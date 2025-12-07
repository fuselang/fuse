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
import core.Desugar.MethodNamePrefix
import fuse.SpecializedMethodUtils
import parser.Info.UnknownInfo
import core.Desugar
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

  def replaceM(
      binds: List[Bind],
      iterationCount: Int = 0
  ): ContextState[List[Bind]] = {
    if (iterationCount > 20) {
      throw new RuntimeException(
        s"Monomorphization infinite loop detected after $iterationCount iterations"
      )
    }

    // Collect instantiations, filtering out closure parameter instantiations (r == Some(-1))
    // These are only used during specialization in buildSpecializedBind, not for creating new binds
    val allInsts = Instantiations.distinct(binds.map(_.insts).flatten)
    val insts = allInsts.filter(_.r != Some(-1))

    insts match {
      case Nil => binds.pure[ContextState]
      case _   =>
        for {
          // Create specialilized functions for each bindig that has corresponding
          // instantiation.
          specializedBinds <- toSpecializedBinds(binds, insts)
          // Replace each generic function invocation with a specialized function.
          modifiedBinds <- specializedBinds.traverse(replaceInstantiations(_))
          // Do a recursive replace until no generic instantiations are found.
          ibinds <- replaceM(modifiedBinds, iterationCount + 1)
        } yield ibinds
    }
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
          getBindInstantiations(bind, insts).flatMap { bindInsts =>
            bindInsts match {
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
            }
          }
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
      case None =>
        genInsts.filter(_.i == bind.i).pure[ContextState]
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
    // Additional deduplication by generated bind name to catch duplicates that
    // Instantiations.distinct might miss due to Type object reference differences
    dedupedBindInsts <- bindInsts
      .traverse(inst =>
        toContextState(inst.bindName()).map(name => (name, inst))
      )
      .map(_.distinctBy(_._1).map(_._2))
    // Filter out instantiations with unresolved TypeVars that point to type parameters
    // These cannot be monomorphized and will cause errors in later phases
    // However, keep TypeVars that are actually primitive types (will be resolved during specialization)
    filteredBindInsts <- dedupedBindInsts.filterA(
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
          // Separate closure instantiations (r == Some(-1)) from regular ones
          (closureInsts, regularInsts) = bind.insts.partition(_.r == Some(-1))
          // Specialize closure types using SAME substitution as parent function
          specializedClosureInsts = closureInsts.map { cInst =>
            val specializedTys = cInst.tys.map(ty =>
              specializeType(ty, shiftedInst.tys, ctxlen - idx)
            )
            cInst.copy(tys = specializedTys)
          }
          // Apply closure types to term BEFORE term specialization
          termWithClosureTypes = applyClosureTypes(
            term,
            specializedClosureInsts
          )
          // Now specialize the term with closure types already applied
          specializedTerm = shiftedInst.tys.foldLeft(
            termWithClosureTypes: Term
          ) { case (t, ty) =>
            specializeTerm(t, idx, ty)
          }
          binding = TermAbbBind(
            specializedTerm,
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
            } yield {
              val specializedTys = i.tys.map(originalTy =>
                specializeType(originalTy, shiftedInst.tys, ctxlen - idx)
              )
              // Preserve r = Some(-1) for closure instantiations
              val newR = i.r match {
                case Some(-1) => Some(-1) // Closure param, keep marker
                case _        =>
                  tIdx.map(_ + 1) // Regular instantiation, resolve index
              }
              val inst = Instantiation(
                i.i,
                termShiftAbove(-1, ctxlen, i.term),
                specializedTys,
                i.cls,
                newR
              )
              inst
            }
          )
          // Filter out instantiations that still contain TypeVars after specialization
          // These represent cases we can't handle yet
          filteredInsts = insts.filter(inst =>
            !inst.tys.exists(ty => getTypeContextLength(ty).isDefined)
          )
          // Apply distinct to remove partial instantiations (e.g., 1-type vs 2-type for same method)
          dedupedInsts = Instantiations.distinct(filteredInsts)
          // Check for self-recursive calls and add self-instantiation if needed
          hasSelfRecursion = containsAssocProjCall(binding, bind.i)
          selfInstantiation =
            if (hasSelfRecursion) {
              // Create instantiation pointing to the specialized bind itself
              // Use bind.i (original method name) as inst.i, not name (specialized name)
              // This ensures bindName() generates correct name from base + types
              List(
                Instantiation(
                  bind.i, // Original bind name (e.g., !foldRight#List)
                  TermAssocProj(
                    UnknownInfo,
                    inst.tys.head,
                    bind.i
                  ), // Use first type as placeholder
                  inst.tys, // Concrete types from THIS specialization
                  List(),
                  Some(0) // Points to self (index 0 relative to current bind)
                )
              )
            } else List()
          // Merge self-instantiation with inherited instantiations
          allInsts = selfInstantiation ::: dedupedInsts
          finalInsts = Instantiations.distinct(allInsts)
        } yield Bind(name, binding, finalInsts)
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

  /** Extract context length from a type by finding nested TypeVars */
  def getTypeContextLength(ty: Type): Option[Int] = ty match {
    case TypeVar(_, _, n)   => Some(n)
    case TypeApp(_, t1, t2) =>
      getTypeContextLength(t1).orElse(getTypeContextLength(t2))
    case TypeArrow(_, t1, t2) =>
      getTypeContextLength(t1).orElse(getTypeContextLength(t2))
    case TypeAll(_, _, _, _, t) =>
      getTypeContextLength(t)
    case _ => None
  }

  /** Extract all De Bruijn indices from TypeVars in a type */
  def extractTypeVarIndices(ty: Type): List[Int] = ty match {
    case TypeVar(_, idx, _) => List(idx)
    case TypeApp(_, t1, t2) =>
      extractTypeVarIndices(t1) ::: extractTypeVarIndices(t2)
    case TypeArrow(_, t1, t2) =>
      extractTypeVarIndices(t1) ::: extractTypeVarIndices(t2)
    case TypeAll(_, _, _, _, t) => extractTypeVarIndices(t)
    case _                      => Nil
  }

  def specializeType(ty: Type, tys: List[Type], ctxLength: Int): Type =
    tys.zipWithIndex.foldLeft(ty) {
      case (TypeAll(_, _, _, _, tyT), (tyS, _)) =>
        typeSubstitute(tyS, 0, tyT)
      case (tyT: TypeVar, (tyS, idx)) =>
        // For a TypeVar in the instantiation list, directly replace with the concrete type
        // The idx parameter tells us which type parameter this is (0, 1, 2, ...)
        // and tyS is the concrete type to substitute
        tyS
      case (tyT @ TypeApp(info, ctor, param), (tyS, idx)) =>
        // For TypeApp(TypeId, param), only specialize the parameter
        // Type constructor (ctor) is already resolved to TypeId by resolveTypeConstructors
        ctor match {
          case _: TypeId =>
            // Constructor is TypeId, safe to specialize parameter
            val paramIndices = extractTypeVarIndices(param)
            paramIndices.distinct.sorted.reverse.headOption match {
              case Some(c) => TypeApp(info, ctor, typeSubstitute(tyS, c, param))
              case None    => tyT // No TypeVars in param
            }
          case _ =>
            // Constructor is not TypeId (shouldn't happen with resolveTypeConstructors)
            // Fall back to old behavior
            val indices = extractTypeVarIndices(tyT).distinct.sorted.reverse
            indices.headOption match {
              case Some(c) => typeSubstitute(tyS, c, tyT)
              case None    => tyT
            }
        }
      case (tyT @ TypeArrow(_, _, _), (tyS, idx)) =>
        // For TypeArrow, substitute the next TypeVar in descending order
        val indices = extractTypeVarIndices(tyT).distinct.sorted.reverse
        indices.headOption match {
          case Some(c) => typeSubstitute(tyS, c, tyT)
          case None    => tyT // No more TypeVars - already fully substituted
        }
      case (tyT, _) if tyT.isPrimitive => tyT
      case (tyT, _)                    =>
        // Other types that don't contain type parameters to substitute
        tyT
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
          case (TermAbbBind(tT, ty), tC: TermAssocProj, Some(s)) =>
            handleAssocProjReplacement(tT, ty, tC, specBindName, acc, inst)
          case (TermAbbBind(tT, ty), tC: TermVar, Some(s)) =>
            handleVarReplacement(tT, ty, tC, s, ctxlen, acc, inst)
          case (b, _, _) =>
            (b, acc.insts)
        }
      } yield Bind(bind.i, replacedBinding, insts)
    )

  /** Generic handler for term projection replacement */
  private def handleProjReplacement(
      tT: Term,
      ty: Option[Type],
      methodName: String,
      bind: Bind,
      inst: Instantiation,
      methodProjReplacer: TermMethodProj => Term,
      assocProjReplacer: TermAssocProj => Term
  ): (Binding, List[Instantiation]) = {
    val replacedTerm = termMap(
      (info, c, k, n) => TermVar(info, k, n),
      (c, ty) => ty,
      methodProjReplacer,
      assocProjReplacer,
      0,
      tT
    )
    (
      TermAbbBind(replacedTerm, ty),
      bind.insts.filterNot(_ == inst)
    )
  }

  /** Handle TermMethodProj instantiation replacement */
  private def handleMethodProjReplacement(
      tT: Term,
      ty: Option[Type],
      tC: TermMethodProj,
      methodName: String,
      bind: Bind,
      inst: Instantiation
  ): (Binding, List[Instantiation]) = handleProjReplacement(
    tT,
    ty,
    methodName,
    bind,
    inst,
    methodProj =>
      if (methodProj.i == tC.i && methodProj.t == tC.t) {
        // Found the matching TermMethodProj - update its method name
        TermMethodProj(methodProj.info, methodProj.t, methodName)
      } else {
        methodProj
      },
    assocProj => assocProj
  )

  /** Handle TermAssocProj instantiation replacement */
  private def handleAssocProjReplacement(
      tT: Term,
      ty: Option[Type],
      tC: TermAssocProj,
      methodName: String,
      bind: Bind,
      inst: Instantiation
  ): (Binding, List[Instantiation]) = handleProjReplacement(
    tT,
    ty,
    methodName,
    bind,
    inst,
    methodProj => methodProj,
    assocProj => {
      // Strip method prefix from tC.i if present (e.g., !foldRight#List -> foldRight)
      val cleanTCI = if (tC.i.startsWith(MethodNamePrefix)) {
        tC.i.drop(MethodNamePrefix.length).takeWhile(_ != '#')
      } else tC.i
      val methodMatch = assocProj.i == cleanTCI
      val alreadySpecialized = assocProj.i.startsWith(Desugar.MethodNamePrefix)
      // Match on method name only - type equality fails after extractTypeArgs + type substitution
      // Don't replace if already specialized (handles multiple instantiations for same call site)
      if (methodMatch && !alreadySpecialized) {
        TermAssocProj(assocProj.info, assocProj.t, methodName)
      } else {
        assocProj
      }
    }
  )

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

  /** Check if a term contains a TermAssocProj call to a specific method */
  private def containsAssocProjInTerm(term: Term, methodName: String): Boolean =
    term match {
      case TermAssocProj(_, _, method) =>
        // Strip method prefix if present (e.g., !foldRight#List -> foldRight)
        val cleanMethodName = if (methodName.startsWith(MethodNamePrefix)) {
          methodName.drop(MethodNamePrefix.length).takeWhile(_ != '#')
        } else methodName
        method == cleanMethodName
      case TermApp(_, t1, t2) =>
        containsAssocProjInTerm(t1, methodName) || containsAssocProjInTerm(
          t2,
          methodName
        )
      case TermAbs(_, _, _, body, _) =>
        containsAssocProjInTerm(body, methodName)
      case TermMatch(_, t, cases) =>
        containsAssocProjInTerm(t, methodName) || cases.exists { case (_, e) =>
          containsAssocProjInTerm(e, methodName)
        }
      case TermLet(_, _, t1, t2) =>
        containsAssocProjInTerm(t1, methodName) || containsAssocProjInTerm(
          t2,
          methodName
        )
      case TermTAbs(_, _, _, body) => containsAssocProjInTerm(body, methodName)
      case TermTApp(_, t, _)       => containsAssocProjInTerm(t, methodName)
      case TermMethodProj(_, t, _) => containsAssocProjInTerm(t, methodName)
      case TermProj(_, t, _)       => containsAssocProjInTerm(t, methodName)
      case TermRecord(_, fields)   =>
        fields.exists { case (_, t) => containsAssocProjInTerm(t, methodName) }
      case TermTag(_, _, t, _)  => containsAssocProjInTerm(t, methodName)
      case TermAscribe(_, t, _) => containsAssocProjInTerm(t, methodName)
      case TermFix(_, t)        => containsAssocProjInTerm(t, methodName)
      case _                    => false
    }

  /** Check if a binding contains a TermAssocProj call to a specific method */
  private def containsAssocProjCall(
      binding: Binding,
      methodName: String
  ): Boolean = binding match {
    case TermAbbBind(term, _) => containsAssocProjInTerm(term, methodName)
    case _                    => false
  }

  /** Apply specialized closure types to closure parameters in a term.
    *
    * This recursively traverses the term and replaces TermClosure nodes that
    * have no type annotation (None) with versions that have the specialized
    * type from the corresponding closure instantiation.
    *
    * @param term
    *   The term to process
    * @param closureInsts
    *   List of closure instantiations with specialized types
    * @return
    *   The term with closure types applied
    */
  def applyClosureTypes(
      term: Term,
      closureInsts: List[Instantiation]
  ): Term = term match {
    case c @ TermClosure(info, variable, None, body) =>
      // Find matching closure instantiation by variable name
      closureInsts.find(_.i == variable) match {
        case Some(inst) if inst.tys.nonEmpty =>
          // Apply the specialized type without monomorphizing TypeApp(TypeId)
          // Keep TypeApp(TypeId("List"), TypeInt) as-is for now
          val paramType = inst.tys.head
          val bodyWithTypes = applyClosureTypes(body, closureInsts)
          TermClosure(info, variable, Some(paramType), bodyWithTypes)
        case _ =>
          // No matching instantiation, recurse into body
          TermClosure(
            info,
            variable,
            None,
            applyClosureTypes(body, closureInsts)
          )
      }

    // Recursively traverse term structure
    case TermLet(info, name, t1, t2) =>
      TermLet(
        info,
        name,
        applyClosureTypes(t1, closureInsts),
        applyClosureTypes(t2, closureInsts)
      )
    case TermApp(info, t1, t2) =>
      TermApp(
        info,
        applyClosureTypes(t1, closureInsts),
        applyClosureTypes(t2, closureInsts)
      )
    case TermMatch(info, t, clauses) =>
      TermMatch(
        info,
        applyClosureTypes(t, closureInsts),
        clauses.map { case (pattern, term) =>
          (pattern, applyClosureTypes(term, closureInsts))
        }
      )
    case TermAbs(info, variable, variableType, expr, returnType) =>
      TermAbs(
        info,
        variable,
        variableType,
        applyClosureTypes(expr, closureInsts),
        returnType
      )
    case TermTAbs(info, variable, kind, body) =>
      TermTAbs(info, variable, kind, applyClosureTypes(body, closureInsts))
    case TermTApp(info, t, ty) =>
      TermTApp(info, applyClosureTypes(t, closureInsts), ty)
    case TermMethodProj(info, t, method) =>
      TermMethodProj(info, applyClosureTypes(t, closureInsts), method)
    case TermProj(info, t, label) =>
      TermProj(info, applyClosureTypes(t, closureInsts), label)
    case TermRecord(info, fields) =>
      TermRecord(
        info,
        fields.map { case (label, t) =>
          (label, applyClosureTypes(t, closureInsts))
        }
      )
    case TermTag(info, label, t, ty) =>
      TermTag(info, label, applyClosureTypes(t, closureInsts), ty)
    case TermAscribe(info, t, ty) =>
      TermAscribe(info, applyClosureTypes(t, closureInsts), ty)
    case TermFix(info, t) =>
      TermFix(info, applyClosureTypes(t, closureInsts))

    // Atoms: no children to traverse
    case t => t
  }
}
