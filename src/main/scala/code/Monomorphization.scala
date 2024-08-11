package code

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Context.*
import core.Bindings.*
import core.Types.Type
import core.Terms.*
import core.Bindings.*
import core.TypeChecker.*
import core.Types.*
import core.Instantiations
import core.Context.addBinding
import code.GrinUtils.toContextState
import core.Instantiations.Instantiation
import core.Context
import core.Shifting.*
import parser.Info.UnknownInfo
import core.Desugar.toTypeInstanceMethodID

object Monomorphization {

  /** Replaces each generic function invocation with monomorphic function.
    *
    * To perform replacement it is required to:
    *   - find all instantiations of generic function
    *   - creates a specialized function for each instantiation
    *   - replace each generic function invocation to a specialized function
    *
    * The process also includes replacing type class instance function
    * invocations wtih a specific implementations.
    *
    * @return
    *   monomorphic bindings
    */
  def replace(binds: List[Bind]): List[Bind] =
    replaceM(binds).runEmptyA.value

  def replaceM(binds: List[Bind]): ContextState[List[Bind]] =
    // Collect instantiations.
    val insts = Instantiations.distinct(binds.map(_.insts).flatten)
    insts match {
      case Nil => binds.pure[ContextState]
      case _ =>
        for {
          // Create specialilized functions for each bindig that has an instantiation.
          specializedBinds <- toSpecializedBinds(binds, insts)
          // Replace each generic function invocation with a specialized function.
          modifiedBinds <- specializedBinds.traverse(replaceInstantiations(_))
          // Do a recursive replace until no generic instantiations are found.
          ibinds <- replaceM(modifiedBinds)
        } yield ibinds
    }

  /** Creates specialized functions (binds) for `binds` by using list of
    * instantiations.
    *
    * It iterates over all binds and creates specialization only for binds that
    * have an instantiation for it. Once the instantiation is found specialized
    * binds are inserted into the bind list.
    *
    * Because specialized binds are inserted into the list, the binds that
    * appear after it gotta be shifted. As their variable indxes may point to
    * the symbols before newly inserted binds. That's why a list of shifts is
    * accumulated on iterating the binds list.
    */
  def toSpecializedBinds(
      binds: List[Bind],
      insts: List[Instantiation]
  ): ContextState[List[Bind]] =
    binds
      .foldLeftM((List[Bind](), List[Shift]())) {
        case ((binds, shifts), bind) =>
          getBindInstantiations(bind, insts).flatMap(_ match {
            case Nil =>
              val sbind = shifts.foldLeft(bind) { (b, s) =>
                bindShift(s.d, b, s.c)
              }
              addBinding(sbind.i, sbind.b).map(_ =>
                (binds :+ sbind, incrShifts(shifts))
              )
            case i =>
              i.zipWithIndex
                .traverse((inst, idx) =>
                  for {
                    // NOTE: Specialized binding is shifted based on the number of
                    // bindings built, as they also shift the context.
                    bind <- buildSpecializedBind(bind, inst, idx)
                      .map(bindShift(idx, _))
                    id <- addBinding(bind.i, bind.b)
                  } yield bind
                )
                .map(l =>
                  (binds ::: l, incrShifts(shifts) :+ Shift(l.length - 1, 0))
                )
          })
      }
      .map(_._1)

  /** Finds all bind instantiations in a specified list.
    *
    * In case bind is:
    *   - an ADT all instantiations for that type are associated to it
    *   - a type instance implementation of class method then those
    *     instantiations are associated to it
    */
  def getBindInstantiations(
      bind: Bind,
      insts: List[Instantiation]
  ): ContextState[List[Instantiation]] = for {
    typeNameOption <- getAlgebraicDataTypeName(UnknownInfo, bind.i)
    bindInsts <- typeNameOption match {
      case None => insts.filter(_.i == bind.i).pure[ContextState]
      case Some(typeName) =>
        insts
          .filterA(i =>
            getAlgebraicDataTypeName(UnknownInfo, i.i).map(
              _.map(_ == typeName).getOrElse(false)
            )
          )
          .map(_.map(i => Instantiation(bind.i, i.term, i.tys, i.cls)))
          .map(Instantiations.distinct(_))
    }
  } yield bindInsts.filter(_.tys.forall(_.isPrimitive))

  def buildSpecializedBind(
      bind: Bind,
      inst: Instantiation,
      idx: Int
  ): ContextState[Bind] =
    bind.b match {
      case TermAbbBind(term: TermTAbs, ty) =>
        for {
          // TODO: Use different bind names for specialized binds, to escape collisions
          // with type instance (built-in) named binds.
          name <- toContextState(inst.bindName())
          ctxLength <- State.inspect { (ctx: Context) => ctx._1.length }
          binding = TermAbbBind(
            inst.tys.zipWithIndex.foldRight(term: Term) { case ((ty, idx), t) =>
              specializeTerm(t, idx, ty)
            },
            ty.map(specializeType(_, inst.tys, ctxLength - idx))
          )
          insts <- bind.insts.traverse(i =>
            val i2 = Instantiation(
              i.i,
              termShiftAbove(-1, ctxLength, i.term),
              i.tys.map(specializeType(_, inst.tys, ctxLength - idx)),
              i.cls
            )
            // Try to resolve the resulting index of a instantation binding when
            // specilizing the binding. As resulting method might already exist
            // in the context. Providing the result while iterating & adding
            // bindings instead of doing at the `replaceInstantiations` phase.
            for {
              n <- toContextState(i2.bindName())
              tIdx <- State.inspect { (ctx: Context) => nameToIndex(ctx, n) }
            } yield Instantiation(
              i2.i,
              i2.term,
              i2.tys,
              i2.cls,
              tIdx.map(_ + 1)
            )
          )
        } yield Bind(name, binding, insts)
      /* TODO:
       * 1. Find type class for the class method in the context.
       * 2. Find class instances for the type class.
       * 3. Specilize the class method with type instance term bind.
       *
       * Hmm should we really specialize the class method? We can just replace
       * the instantiation with the specialized type instance term bind.
       * ---
       * It seems like we should specialize the class method in case it's a
       * ganeric function. In a simple function case we can just use a
       * type instance impl.
       * */
      case TermAbbBind(TermClassMethod(_, _, cls), ty) =>
        bind.pure
      // for {
      //   typeInstanceID <- toTypeInstanceMethodID(
      //     bind.i,
      //     inst.tys.head,
      //     cls.name
      //   )
      //   binding <- getBinding(typeInstanceID)
      //
      // } yield Bind(name, binding, insts)
      case _ =>
        throw new RuntimeException(
          s"can't build specialized binding ${inst.i}"
        )
    }

  def specializeTerm(
      term: Term,
      typeVarIndex: Int,
      tyS: Type
  ): Term =
    term match {
      case TermTAbs(_, _, _, body) =>
        termSubstituteType(tyS, typeVarIndex, body)
      case _ =>
        throw new RuntimeException(s"can't specialize term ${term}")
    }

  def specializeType(ty: Type, tys: List[Type], ctxLength: Int): Type =
    tys.zipWithIndex.foldRight(ty) {
      case ((tyS, idx), TypeAll(_, _, _, _, tyT)) =>
        typeSubstitute(tyS, idx, tyT)
      case ((tyS, idx), tyT: TypeVar) =>
        /* NOTE: As we don't have complete context information of the type
         * variable that should be substituted, we calculate its index
         * by using:
         * - curent ctx length
         * - `tyT` context length when it was built
         * - index of the type variable we want to substite
         *
         * The formula is: <ctx_diff> - <type_var_index> - 1
         * */
        val c = (tyT.length - ctxLength) - idx - 1
        typeSubstitute(tyS, c, tyT)
      case _ => throw new RuntimeException(s"can't specialize type ${ty}")
    }

  /** Replaces all instantiations found on specified bind with specialized
    * functions (binds).
    */
  def replaceInstantiations(bind: Bind): ContextState[Bind] =
    bind.insts.foldM(bind)((acc, inst) =>
      for {
        specBindName <- toContextState(inst.bindName())
        specBindIndex <- State.inspect { (ctx: Context) =>
          nameToIndex(ctx, specBindName)
        }
        (replacedBinding, insts) = (
          acc.b,
          inst.term,
          inst.r.orElse(specBindIndex)
        ) match {
          case (TermAbbBind(tT, ty), tC: TermVar, Some(s)) =>
            (
              TermAbbBind(termVarSubstitute(s, tC, tT), ty),
              bind.insts.filterNot(_ == inst)
            )
          case (b, _, _) => (b, bind.insts)
        }
      } yield Bind(bind.i, replacedBinding, insts)
    )

}
