package code

import cats.data.State
import cats.implicits.*
import core.Bindings.*
import core.Context.*
import core.Desugar
import core.Desugar.{MethodNamePrefix, SelfTypeName}
import core.Instantiations
import core.Instantiations.{
  BindTypeSeparator,
  BindTypeSeparatorChar,
  Instantiation,
  Resolution
}
import core.Shifting.*
import core.TermFold
import core.Terms.*
import core.TypeChecker.*
import core.Types.*
import code.GrinUtils.{toContextState, toContextStateOption, getNameFromType}
import code.MonoTypes.*
import parser.Info.Info
import parser.Info.UnknownInfo

object MonoSpecialize {

  case class ReplacementResult(binding: Binding, insts: List[Instantiation])

  case class SpecializationContext(
      parentTys: List[Type],
      specCtxLength: Int,
      shiftCtxLength: Int,
      specializedTerm: Term,
      regularTypeVarIndices: List[Int],
      deferredDataConstrSpecializations: Map[Instantiation, List[Type]],
      originalBindName: String,
      allBindNames: Set[String] = Set.empty,
      referenceBaseCtxLen: Option[Int] = None
  )

  case class DataConstrContext(
      regularTypeVarIndices: List[Int],
      regularCtxLen: Option[Int],
      numParentTypeParams: Int
  )

  /** Result of computing type variable indices from instantiations. */
  case class TypeVarInfo(
      regularTypeVarIndices: List[Int],
      regularCtxLen: Option[Int],
      numParentTypeParams: Int = 0
  )

  /** Unwrap a TermTAbs and substitute the type variable at index 0. When called
    * with more types than the term has outer TermTAbs wrappers (e.g. a
    * type-class default method body whose inst carries extra receiver-type
    * arguments beyond the term's actual TypeAbs depth), the extra types land
    * here and leave the term unchanged.
    */
  def specializeTerm(
      term: Term,
      typeVarIndex: Int,
      tyS: Type
  ): Term =
    term match {
      case TermTAbs(_, _, _, body) =>
        termSubstituteType(tyS, 0, body)
      case _ =>
        term
    }

  def hasNegativeTypeVarIndex(ty: Type): Boolean = ty match {
    case TypeVar(_, idx, _) => idx < 0
    case TypeApp(_, t1, t2) =>
      hasNegativeTypeVarIndex(t1) || hasNegativeTypeVarIndex(t2)
    case TypeArrow(_, t1, t2) =>
      hasNegativeTypeVarIndex(t1) || hasNegativeTypeVarIndex(t2)
    case TypeAll(_, _, _, _, t) => hasNegativeTypeVarIndex(t)
    case _                      => false
  }

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

  def isTypeFullyResolved(
      ty: Type,
      numBinds: Int
  ): ContextState[Boolean] = ty match {
    case TypeVar(info, index, ctxLen) =>
      (index < 0) match {
        case true  => false.pure[ContextState]
        case false =>
          toContextStateOption(getBinding(info, index)).map {
            case Some(TypeAbbBind(_, _)) => true
            case Some(_)                 => false
            case None                    =>
              numBinds > 0 && (ctxLen.intValue - index) <= numBinds
          }
      }
    case _: TypeEVar =>
      false.pure[ContextState]
    case TypeApp(_, t1, t2) =>
      List(t1, t2).forallM(isTypeFullyResolved(_, numBinds))
    case TypeArrow(_, t1, t2) =>
      List(t1, t2).forallM(isTypeFullyResolved(_, numBinds))
    case _ => true.pure[ContextState]
  }

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
        typeSubstituteTop(tyS, tyT)
      case (tyT: TypeVar, (tyS, idx)) =>
        tyS
      case (tyT @ TypeApp(info, ctor, param), (tyS, idx)) =>
        ctor match {
          case _: TypeId =>
            val paramIndices = extractTypeVarIndices(param)
            paramIndices.distinct.sorted.reverse.headOption match {
              case Some(c) => TypeApp(info, ctor, typeSubstitute(tyS, c, param))
              case None    => tyT
            }
          case _ =>
            val indices = extractTypeVarIndices(tyT).distinct.sorted.reverse
            indices.headOption match {
              case Some(c) => typeSubstitute(tyS, c, tyT)
              case None    => tyT
            }
        }
      case (tyT @ TypeArrow(_, _, _), (tyS, idx)) =>
        val indices = extractTypeVarIndices(tyT).distinct.sorted.reverse
        indices.headOption match {
          case Some(c) => typeSubstitute(tyS, c, tyT)
          case None    => tyT
        }
      case (tyT, _) if tyT.isPrimitive => tyT
      case (tyT, _)                    => tyT
    }

  def specializeInstTys(
      instTys: List[Type],
      parentTys: List[Type],
      ctxLength: Int,
      referenceTypeVarIndices: List[Int] = List(),
      referenceBaseCtxLen: Option[Int] = None
  ): List[Type] = {
    // One tagged walk over instTys yields both views:
    //   - `instTypeVarPairs`: skips TypeApp constructor positions; used for
    //     the primary ordering.
    //   - `allInstTypeVarPairs`: includes TypeApp constructor positions;
    //     needed so the substitution mapping covers the receiver-type
    //     parameter in closure captures of type-class default method bodies
    //     where the receiver constructor itself is a type variable.
    val tagged = instTys.flatMap(collectTypeParamPairs(_))
    val instTypeVarPairs =
      tagged.collect { case (p, false) => p }.distinct
    val allInstTypeVarPairs = tagged.map(_._1).distinct
    referenceTypeVarIndices match {
      case Nil =>
        val allInstTypeVarIndices = instTypeVarPairs.map(_._1).distinct
        val allIndices = allInstTypeVarIndices.distinct.sorted.reverse
        val indexToParentType: Map[Int, Type] =
          allIndices.zipWithIndex.collect {
            case (tvIdx, parentPos) if parentPos < parentTys.length =>
              tvIdx -> parentTys(parentPos)
          }.toMap
        instTys.map(ty => substituteTypeVarsWithMapping(ty, indexToParentType))
      case refIndices =>
        // Primary: map TypeVars that normalize into refIndices (cross-capture-
        // context case — an inner closure nested inside a monadic
        // continuation needs this when its TypeVars carry a deeper ctxLen
        // than the bind's regular insts).
        // Fallback: rawIdx-based ordering for TypeVars that only appear in
        // `instTys` (receiver/type-parameter TypeVars in a type-class
        // default method's closure body). Both paths are necessary.
        val refIndexToPos = refIndices.zipWithIndex.toMap
        val instOwnIndices = instTypeVarPairs.map(_._1).distinct.sorted.reverse
        val instOwnIndexToPos = instOwnIndices.zipWithIndex.toMap
        val indexToParentType: Map[Int, Type] = allInstTypeVarPairs.flatMap {
          case (rawIdx, tvCtxLen) =>
            val normalizedIdx = referenceBaseCtxLen match {
              case Some(base) => rawIdx - (tvCtxLen - base)
              case None       => rawIdx
            }
            refIndexToPos
              .get(normalizedIdx)
              .flatMap(pos => parentTys.lift(pos).map(rawIdx -> _))
              .orElse {
                instOwnIndexToPos
                  .get(rawIdx)
                  .flatMap(pos => parentTys.lift(pos).map(rawIdx -> _))
              }
        }.toMap
        instTys.map(ty => substituteTypeVarsWithMapping(ty, indexToParentType))
    }
  }

  /** Collect `(idx, ctxLen)` pairs for every TypeVar in `ty`, tagged with
    * `inCtor = true` when the TypeVar appears in a TypeApp constructor
    * position. Constructor-position pairs are only relevant when the
    * constructor itself is a type parameter (e.g. a type-class default method
    * body where the receiver constructor is a type variable).
    */
  def collectTypeParamPairs(
      ty: Type,
      inCtor: Boolean = false
  ): List[((Int, Int), Boolean)] = ty match {
    case TypeVar(_, idx, n) =>
      List(((idx.intValue, n.intValue), inCtor))
    case TypeApp(_, t1, t2) =>
      collectTypeParamPairs(t1, true) ::: collectTypeParamPairs(t2, false)
    case TypeArrow(_, t1, t2) =>
      collectTypeParamPairs(t1, false) ::: collectTypeParamPairs(t2, false)
    case _ => Nil
  }

  def extractTypeParamPairs(ty: Type): List[(Int, Int)] =
    collectTypeParamPairs(ty).collect { case (p, false) => p }

  def substituteTypeVarsWithMapping(
      ty: Type,
      mapping: Map[Int, Type]
  ): Type = ty match {
    case TypeVar(_, idx, _) =>
      mapping.getOrElse(idx.intValue, ty)
    case TypeApp(info, t1, t2) =>
      TypeApp(
        info,
        substituteTypeVarsWithMapping(t1, mapping),
        substituteTypeVarsWithMapping(t2, mapping)
      )
    case TypeArrow(info, t1, t2) =>
      TypeArrow(
        info,
        substituteTypeVarsWithMapping(t1, mapping),
        substituteTypeVarsWithMapping(t2, mapping)
      )
    case _ => ty
  }

  def specializeDataConstrInstTys(
      dataConstrInst: Instantiation,
      parentTys: List[Type],
      ctxLength: Int,
      dataConstrCtx: DataConstrContext
  ): List[Type] = {
    val dataConstrCtxLen = dataConstrInst.tys.collectFirst { case tv: TypeVar =>
      tv.length.intValue
    }
    val contextShift = (dataConstrCtx.regularCtxLen, dataConstrCtxLen) match {
      case (Some(regLen), Some(dataConstrLen)) => dataConstrLen - regLen
      case _                                   => dataConstrInst.tys.length
    }
    val referenceIndices = dataConstrCtx.regularTypeVarIndices match {
      case Nil =>
        val typeVarIndices = dataConstrInst.tys.collect { case tv: TypeVar =>
          tv.index.intValue
        }
        typeVarIndices.headOption match {
          case Some(knownIdx) =>
            (0 until dataConstrCtx.numParentTypeParams)
              .map(i => knownIdx + (dataConstrCtx.numParentTypeParams - 1 - i))
              .toList
              .sorted
              .reverse
          case None => Nil
        }
      case indices => indices.map(_ + contextShift)
    }
    specializeInstTys(
      dataConstrInst.tys,
      parentTys,
      ctxLength,
      referenceIndices
    )
  }

  /** Resolve an abstract method ID containing "Self" to its concrete
    * counterpart.
    */
  def resolveAbstractMethodId(
      instId: String,
      parentTys: List[Type],
      allBindNames: Set[String]
  ): ContextState[String] =
    instId.contains(SelfTypeName) match {
      case true =>
        val methodPrefix =
          instId.takeWhile(_ != BindTypeSeparatorChar) + BindTypeSeparator
        parentTys.headOption match {
          case None         => instId.pure[ContextState]
          case Some(selfTy) =>
            for {
              selfTypeName <- getNameFromType(selfTy)
            } yield allBindNames
              .find(n =>
                n.startsWith(methodPrefix) && n.contains(
                  s"${BindTypeSeparator}$selfTypeName${BindTypeSeparator}"
                )
              )
              .getOrElse(instId)
        }
      case false =>
        val methodPrefix =
          instId.takeWhile(_ != BindTypeSeparatorChar) + BindTypeSeparator
        val isKnownBase =
          allBindNames.exists(n =>
            n == instId || n.startsWith(instId + BindTypeSeparator)
          )
        isKnownBase match {
          case true  => instId.pure[ContextState]
          case false =>
            parentTys.foldLeftM(instId: String) { (currentId, parentTy) =>
              (currentId == instId) match {
                case false => currentId.pure[ContextState]
                case true  =>
                  for {
                    tyName <- getNameFromType(parentTy)
                  } yield allBindNames
                    .find(n =>
                      n.startsWith(methodPrefix + tyName + BindTypeSeparator)
                    )
                    .getOrElse(currentId)
              }
            }
        }
    }

  def resolveInstSpecialization(
      inst: Instantiation,
      ctx: SpecializationContext
  ): ContextState[Instantiation] =
    for {
      concreteInstId <- resolveAbstractMethodId(
        inst.i,
        ctx.parentTys,
        ctx.allBindNames
      )
      specializedTys = (inst.r, inst.i == ctx.originalBindName) match {
        case (Resolution.DeferredDataConstr, _) =>
          ctx.deferredDataConstrSpecializations.getOrElse(inst, inst.tys)
        case (_, true) => ctx.parentTys
        case _         =>
          specializeInstTys(
            inst.tys,
            ctx.parentTys,
            ctx.specCtxLength,
            ctx.regularTypeVarIndices,
            ctx.referenceBaseCtxLen
          )
      }
      trimmedTys = (
        concreteInstId != inst.i,
        inst.i.contains(SelfTypeName)
      ) match {
        case (false, _)   => specializedTys
        case (true, true) =>
          ctx.parentTys.headOption match {
            case Some(selfTy) => specializedTys.filterNot(_ == selfTy)
            case None         => specializedTys
          }
        case (true, false) => specializedTys
      }
      resolvedCls = (concreteInstId != inst.i) match {
        case true  => List()
        case false => inst.cls
      }
      n <- toContextState(
        Instantiation(concreteInstId, inst.term, trimmedTys, resolvedCls)
          .bindName()
      )
      tIdx <- State.inspect { (c: Context) => nameToIndex(c, n) }
      specializedDataConstrIdx <- inst.r match {
        case Resolution.DeferredDataConstr =>
          for {
            sn <- toContextState(
              Instantiation(inst.i, inst.term, specializedTys, inst.cls)
                .bindName()
            )
            idx <- State.inspect { (c: Context) => nameToIndex(c, sn) }
          } yield idx
        case _ => None.pure[ContextState]
      }
    } yield {
      val newR: Resolution = inst.r match {
        case Resolution.Closure            => Resolution.Closure
        case Resolution.DeferredDataConstr =>
          specializedDataConstrIdx match {
            case Some(idx) => Resolution.Resolved(idx + 1)
            case None      => Resolution.DeferredDataConstr
          }
        case _ =>
          tIdx
            .map(idx => Resolution.Resolved(idx + 1))
            .getOrElse(Resolution.Unresolved)
      }
      // Align inst.term with the specialized body: specializeTerm shifts
      // body TermVars down by 1 per peeled TypeTAbs. Look up the actual
      // idx/n in the specialized body by info so replaceVar's
      // tC.i1 == x match succeeds; fall back to a -1 shift when the info
      // is not present in the specialized body.
      val actualTerm: Term = inst.term match {
        case tv: TermVar =>
          TermFold.findVarByInfo(ctx.specializedTerm, tv.info) match {
            case Some((actualIdx, actualN, _)) =>
              TermVar(tv.info, actualIdx, actualN)
            case None =>
              termShiftAbove(-1, ctx.shiftCtxLength, tv)
          }
        case other =>
          termShiftAbove(-1, ctx.shiftCtxLength, other)
      }
      Instantiation(concreteInstId, actualTerm, trimmedTys, resolvedCls, newR)
    }

  /** Build a specialized bind for one instantiation of a generic function.
    * Creates the specialized term, resolves all nested instantiations, and
    * produces the final Bind with closure types.
    */
  def buildSpecializedBind(
      bind: Bind,
      inst: Instantiation,
      idx: Int,
      allBindNames: Set[String] = Set.empty
  ): ContextState[Bind] =
    bind.b match {
      case TermAbbBind(term: TermTAbs, ty) =>
        for {
          shiftedInst <- instantantionShiftOnContextDiff(inst)
          name <- toContextState(shiftedInst.bindName())
          ctxlen <- State.inspect { (ctx: Context) => ctx._1.length }
          closureInsts = bind.insts.filter(_.r == Resolution.Closure)
          regularInsts = bind.insts.filter(i =>
            i.r match {
              case Resolution.Closure | Resolution.DeferredDataConstr => false
              case _                                                  => true
            }
          )
          regularTypeVarPairs = regularInsts
            .flatMap(i => i.tys.flatMap(extractTypeParamPairs(_)))
          // Normalize regular insts' TypeVar indices relative to the shallowest
          // ctxLen among them. This keeps the index ordering stable when a
          // regular inst was captured at a slightly deeper ctxLen than another
          // (e.g. two TermApp layers at different depths in the same body).
          baseCtxLen = regularTypeVarPairs.map(_._2).minOption
          regularTypeVarIndices = baseCtxLen match {
            case Some(base) =>
              regularTypeVarPairs
                .map { case (idx, ctxLen) => idx - (ctxLen - base) }
                .filter(_ >= 0)
                .distinct
                .sorted
                .reverse
            case None => List()
          }
          specializedClosureInsts = closureInsts.map { cInst =>
            val specializedTys = specializeInstTys(
              cInst.tys,
              shiftedInst.tys,
              ctxlen - idx,
              regularTypeVarIndices,
              baseCtxLen
            )
            cInst.copy(tys = specializedTys)
          }
          termWithClosureTypes = TermFold.applyClosureTypes(
            term,
            specializedClosureInsts
          )
          specializedTerm = shiftedInst.tys.foldLeft(
            termWithClosureTypes: Term
          )(specializeTerm(_, idx, _))
          deferredDataConstrInsts = bind.insts.filter(
            _.r == Resolution.DeferredDataConstr
          )
          numParentTypeParams = shiftedInst.tys.length
          regularCtxLen = regularInsts
            .flatMap(i => i.tys.flatMap(getTypeContextLength(_)))
            .headOption
          dataConstrCtx = DataConstrContext(
            regularTypeVarIndices,
            regularCtxLen,
            numParentTypeParams
          )
          deferredDataConstrSpecializations = deferredDataConstrInsts.map {
            dataConstrInst =>
              dataConstrInst -> specializeDataConstrInstTys(
                dataConstrInst,
                shiftedInst.tys,
                ctxlen - idx,
                dataConstrCtx
              )
          }.toMap
          binding = TermAbbBind(
            specializedTerm,
            ty.map(originalTy =>
              specializeType(originalTy, shiftedInst.tys, ctxlen - idx)
            )
          )
          specCtx = SpecializationContext(
            shiftedInst.tys,
            ctxlen - idx,
            ctxlen,
            specializedTerm,
            regularTypeVarIndices,
            deferredDataConstrSpecializations,
            bind.i,
            allBindNames,
            baseCtxLen
          )
          insts <- bind.insts.traverse(i =>
            resolveInstSpecialization(i, specCtx)
          )
          syntheticDataConstrInsts <- synthesizeBareDataConstrInsts(
            specializedTerm,
            insts,
            shiftedInst.tys
          )
        } yield finalizeSpecializedBind(
          name,
          binding,
          insts ::: syntheticDataConstrInsts,
          bind.i,
          inst.tys,
          specializedClosureInsts
        )
      case _ =>
        throw new RuntimeException(
          s"can't build specialized binding ${inst.i}"
        )
    }

  /** Synthesize insts for bare TermVars in the specialized body that resolve
    * (via ctx lookup) to an already-specialized data-constructor bind but have
    * no existing `Instantiation` covering them.
    *
    * Source of the gap: `Instantiations.build` early-returns on empty type
    * solutions. Bare data-constructor references inside a type-class default
    * method body carry no local solutions, so no inst is recorded — leaving the
    * TermVar's idx stale when MonoRewrite reorders the bind list, and the
    * backend's codegen resolves to a wrong position.
    *
    * Fix: at specialization time (when `parentTys` is concrete), find bare
    * data-constructor TermVars not yet covered and emit a
    * `BareSpecializedDataConstr` inst whose `bindName()` reconstructs the
    * specialized target. The list-based rewrite path in `replaceInstantiations`
    * then fixes the idx.
    */
  def synthesizeBareDataConstrInsts(
      specializedTerm: Term,
      existingInsts: List[Instantiation],
      parentTys: List[Type]
  ): ContextState[List[Instantiation]] = {
    val coveredInfos = existingInsts.collect {
      case Instantiation(_, tv: TermVar, _, _, _) => tv.info
    }.toSet
    val candidates = TermFold
      .collectBindTermVars(specializedTerm)
      .distinctBy(_.info)
      .filterNot(tv => coveredInfos.contains(tv.info))
    State.inspect { (ctx: Context) =>
      candidates.flatMap(tv =>
        indexToName(ctx, tv.i1.intValue)
          .flatMap(synthesizeForName(tv, _, parentTys))
      )
    }
  }

  /** Build a `BareSpecializedDataConstr` inst for a TermVar whose resolved name
    * has shape `<BaseName>#<ty1>[#<ty2>...]` where BaseName is a
    * data-constructor (starts uppercase, no `#` inside — excludes specialized
    * method names and class-qualifier-only names).
    *
    * `bindName()` of the synthesized inst reconstructs the specialized target
    * from `BaseName` + `parentTys.takeRight(dataConstrArity)`, so list-based
    * rewriting in `replaceInstantiations` finds the right bind.
    *
    * Bare generic data-constructors (name without `#`): not synthesized —
    * existing `DeferredDataConstr` insts from `Instantiations.build` handle
    * those.
    */
  def synthesizeForName(
      tv: TermVar,
      name: String,
      parentTys: List[Type]
  ): Option[Instantiation] =
    name.split(BindTypeSeparatorChar).toList match {
      case baseName :: typeParts
          if typeParts.nonEmpty
            && Instantiations.isDataConstrName(baseName)
            && parentTys.length >= typeParts.length =>
        Some(
          Instantiation(
            i = baseName,
            term = tv,
            tys = parentTys.takeRight(typeParts.length),
            cls = Nil,
            r = Resolution.BareSpecializedDataConstr
          )
        )
      case _ => None
    }

  def finalizeSpecializedBind(
      name: String,
      binding: Binding,
      insts: List[Instantiation],
      originalBindName: String,
      instTys: List[Type],
      specializedClosureInsts: List[Instantiation]
  ): Bind = {
    val filteredInsts = insts.filter(inst =>
      !inst.tys.exists(ty => getTypeContextLength(ty).isDefined)
    )
    // Preserve multiple call sites (different term.info) for the same
    // method+types — each one needs its own replacement in
    // replaceInstantiations (which matches by info).
    val dedupedInsts = Instantiations.distinct(filteredInsts, byTerm = true)
    val selfInstantiation =
      containsAssocProjInBinding(binding, originalBindName) match {
        case true =>
          List(
            Instantiation(
              originalBindName,
              TermAssocProj(UnknownInfo, instTys.head, originalBindName),
              instTys,
              List(),
              Resolution.Resolved(0)
            )
          )
        case false => List()
      }
    val finalInsts = (selfInstantiation ::: dedupedInsts)
      .distinctBy(inst => (inst.i, inst.tys, inst.term))
    val closureTypesMap = specializedClosureInsts
      .flatMap(cInst => cInst.tys.headOption.map(ty => cInst.i -> ty))
      .toMap
    Bind(name, binding, finalInsts, closureTypesMap)
  }

  def containsAssocProjInBinding(
      binding: Binding,
      methodName: String
  ): Boolean = binding match {
    case TermAbbBind(term, _) => TermFold.containsAssocProj(term, methodName)
    case _                    => false
  }

  /** Separate instantiation categories and compute De Bruijn index mapping for
    * regular type parameters.
    */
  def computeTypeVarIndices(
      bindInsts: List[Instantiation]
  ): TypeVarInfo = {
    val regularInsts = bindInsts.filter(i =>
      i.r match {
        case Resolution.Closure | Resolution.DeferredDataConstr => false
        case _                                                  => true
      }
    )
    val regularTypeVarPairs = regularInsts
      .flatMap(i => i.tys.flatMap(extractTypeParamPairs(_)))
    val baseCtxLen = regularTypeVarPairs.map(_._2).minOption
    val regularTypeVarIndices = baseCtxLen match {
      case Some(base) =>
        regularTypeVarPairs
          .map { case (idx, ctxLen) => idx - (ctxLen - base) }
          .filter(_ >= 0)
          .distinct
          .sorted
          .reverse
      case None => List()
    }
    val regularCtxLen = regularInsts
      .flatMap(i => i.tys.flatMap(getTypeContextLength(_)))
      .headOption
    TypeVarInfo(regularTypeVarIndices, regularCtxLen)
  }

  def resolveClosureTypes(
      bindInsts: List[Instantiation],
      parentTys: List[Type],
      ctxLength: Int,
      regularTypeVarIndices: List[Int]
  ): List[Instantiation] = {
    val closureInsts = bindInsts.filter(_.r == Resolution.Closure)
    closureInsts.map { cInst =>
      val specializedTys = specializeInstTys(
        cInst.tys,
        parentTys,
        ctxLength,
        regularTypeVarIndices
      )
      cInst.copy(tys = specializedTys)
    }
  }

  def specializeBody(
      term: TermTAbs,
      closureInsts: List[Instantiation],
      parentTys: List[Type],
      idx: Int
  ): Term = {
    val termWithClosureTypes = TermFold.applyClosureTypes(
      term,
      closureInsts
    )
    parentTys.foldLeft(termWithClosureTypes: Term) { case (t, ty) =>
      specializeTerm(t, idx, ty)
    }
  }

  def resolveDataConstructors(
      bindInsts: List[Instantiation],
      parentTys: List[Type],
      ctxLength: Int,
      typeVarInfo: TypeVarInfo
  ): Map[Instantiation, List[Type]] = {
    val deferredDataConstrInsts =
      bindInsts.filter(_.r == Resolution.DeferredDataConstr)
    val dataConstrCtx = DataConstrContext(
      typeVarInfo.regularTypeVarIndices,
      typeVarInfo.regularCtxLen,
      parentTys.length
    )
    deferredDataConstrInsts.map { dataConstrInst =>
      dataConstrInst -> specializeDataConstrInstTys(
        dataConstrInst,
        parentTys,
        ctxLength,
        dataConstrCtx
      )
    }.toMap
  }
}
