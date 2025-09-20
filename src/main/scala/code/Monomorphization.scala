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
import fuse.SpecializedMethodUtils
import parser.Info.UnknownInfo
import core.Desugar.toTypeInstanceMethodID
import code.GrinUtils.toContextStateOption

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
          // Create specialilized functions for each bindig that has corresponding
          // instantiation.
          specializedBinds <- toSpecializedBinds(binds, insts)
          // Replace each generic function invocation with a specialized function.
          modifiedBinds <- specializedBinds.traverse(replaceInstantiations(_))
          // Do a recursive replace until no generic instantiations are found.
          ibinds = replace(modifiedBinds)
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
    * appear after it gotta be shifted. As their variable indexes may point to
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
              for {
                _ <- addBinding(sbind.i, sbind.b)
                sInsts <- sbind.insts
                  .traverse(instantantionShiftOnContextDiff(_))
                b = binds :+ Bind(sbind.i, sbind.b, sInsts)
              } yield (b, incrShifts(shifts))
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

  /** Finds bind instantiations from a specified list when bind is a a generic
    * function.
    *
    * Once instantations for a bind are found we can build specialized binds for
    * each one of them.
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
    // NOTE: Only get instantations for a bind that represents a generic function.
    //
    // This logic skips class methods that may have instantitations,
    // as we shouldn't build a specialized bind for them.
    genInsts = bind.b match {
      case TermAbbBind(term: TermTAbs, ty) => insts
      case _                               => Nil
    }
    bindInsts <- typeNameOption match {
      case None => genInsts.filter(_.i == bind.i).pure[ContextState]
      case Some(typeName) =>
        genInsts
          .filterA(i =>
            getAlgebraicDataTypeName(UnknownInfo, i.i).map(
              _.map(_ == typeName).getOrElse(false)
            )
          )
          .map(_.map(i => Instantiation(bind.i, i.term, i.tys, i.cls, i.r)))
          .map(Instantiations.distinct(_))
    }
    // Collect only primitive types for bind instantiations. As complex
    // types will be reducted in the subsequent iterations.
    filteredBindInsts <- bindInsts
      .filter(_.tys.forall(_.isPrimitive))
      .filterA(
        instantantionShiftOnContextDiff(_)
          .flatMap(
            _.tys
              .traverse {
                case TypeVar(info, index, length) =>
                  for {
                    b <- toContextStateOption(getBinding(info, index))
                    isPrimType = b match {
                      case Some(TypeAbbBind(t, k)) => t.isPrimitive
                      case _                       => false
                    }
                  } yield isPrimType
                case _ => true.pure[ContextState]
              }
          )
          .map(_.forall(identity))
      )
  } yield filteredBindInsts

  def buildSpecializedBind(
      bind: Bind,
      inst: Instantiation,
      idx: Int
  ): ContextState[Bind] =
    bind.b match {
      case TermAbbBind(term: TermTAbs, ty) =>
        for {
          shiftedInst <- instantantionShiftOnContextDiff(inst)
          // TODO: Use different bind names for specialized binds, to escape collisions
          // with type instance (built-in) named binds.
          name <- toContextState(shiftedInst.bindName())
          ctxlen <- State.inspect { (ctx: Context) => ctx._1.length }
          binding = TermAbbBind(
            shiftedInst.tys.foldLeft(term: Term) { case (t, ty) =>
              specializeTerm(t, idx, ty)
            },
            ty.map(originalTy => {
              specializeType(originalTy, shiftedInst.tys, ctxlen - idx)
            })
          )
          insts <- bind.insts.traverse(i =>
            // Try to resolve the resulting index of an instantation binding when
            // specilizing the binding (this phase). As target method might
            // already exist in the context, the resulting index is resolved while
            // iterating and adding (spec) bindings instead of doing it at the
            // `replaceInstantiations` phase.
            // NOTE: Because this logic is the only one that works! :P
            // Taking the indexes while building the specialized binding will get
            // us the correct values, otherwise the shifting would lose the
            // neccessary information on _replacing_ phase.
            for {
              n <- toContextState(i.bindName())
              tIdx <- State.inspect { (ctx: Context) => nameToIndex(ctx, n) }
            } yield Instantiation(
              i.i,
              termShiftAbove(-1, ctxlen, i.term),
              i.tys.map(originalTy => {
                val specializedTy =
                  specializeType(originalTy, shiftedInst.tys, ctxlen - idx)
                specializedTy
              }),
              i.cls,
              tIdx.map(_ + 1)
            )
          )
        } yield Bind(name, binding, insts)
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
        termSubstituteType(tyS, 0, body)
      case _ =>
        throw new RuntimeException(s"can't specialize term ${term}")
    }

  def specializeType(ty: Type, tys: List[Type], ctxLength: Int): Type =
    tys.zipWithIndex.foldLeft(ty) {
      case (TypeAll(_, _, _, _, tyT), (tyS, _)) =>
        typeSubstitute(tyS, 0, tyT)
      case (tyT: TypeVar, (tyS, idx)) =>
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
      case _ if ty.isPrimitive => ty
      case _ => throw new RuntimeException(s"can't specialize type ${ty}")
    }

  /** Replaces all instantiations found on specified bind with specialized
    * functions (binds).
    */
  def replaceInstantiations(bind: Bind): ContextState[Bind] =
    bind.insts.foldM(bind)((acc, inst) =>
      for {
        specBindName <- toContextState(inst.bindName())
        (specBindIndex, ctxlen) <- State.inspect { (ctx: Context) =>
          (nameToIndex(ctx, specBindName), ctx._1.length)
        }
        (replacedBinding, insts) = (
          acc.b,
          inst.term,
          inst.r.orElse(specBindIndex)
        ) match {
          case (TermAbbBind(tT, ty), tC: TermMethodProj, Some(s)) =>
            handleMethodProjReplacement(tT, ty, tC, specBindName, acc, inst)
          case (TermAbbBind(tT, ty), tC: TermVar, Some(s)) =>
            handleVarReplacement(tT, ty, tC, s, ctxlen, acc, inst)
          case (b, _, _) => (b, acc.insts)
        }
      } yield Bind(bind.i, replacedBinding, insts)
    )

  /** Handle TermMethodProj instantiation replacement */
  private def handleMethodProjReplacement(
      tT: Term,
      ty: Option[Type],
      tC: TermMethodProj,
      methodName: String,
      bind: Bind,
      inst: Instantiation
  ): (Binding, List[Instantiation]) = {
    val replacedTerm = termMap(
      (info, c, k, n) => TermVar(info, k, n),
      (c, ty) => ty,
      (methodProj: TermMethodProj) =>
        if (methodProj.i == tC.i && methodProj.t == tC.t) {
          // Found the matching TermMethodProj - update its method name
          TermMethodProj(methodProj.info, methodProj.t, methodName)
        } else {
          methodProj
        },
      0,
      tT
    )
    (
      TermAbbBind(replacedTerm, ty),
      bind.insts.filterNot(_ == inst)
    )
  }

  /** Handle TermVar instantiation replacement */
  private def handleVarReplacement(
      tT: Term,
      ty: Option[Type],
      tC: TermVar,
      s: Int,
      ctxlen: Int,
      bind: Bind,
      inst: Instantiation
  ): (Binding, List[Instantiation]) = {
    val d = if (inst.r.isDefined) None else Some(ctxlen)
    (
      TermAbbBind(termVarSubstitute(s, d, tC, tT), ty),
      bind.insts.filterNot(_ == inst)
    )
  }
}
