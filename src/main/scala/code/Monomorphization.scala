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
import core.Instantiations.Resolution
import core.Context
import core.Shifting.*
import core.Desugar.MethodNamePrefix
import fuse.SpecializedMethodUtils
import parser.Info.UnknownInfo
import parser.Info.Info
import core.Desugar
import core.Desugar.toTypeInstanceMethodID
import code.GrinUtils.toContextStateOption

object Monomorphization {

  case class ReplacementResult(binding: Binding, insts: List[Instantiation])

  case class SpecializationContext(
      parentTys: List[Type],
      specCtxLength: Int,
      shiftCtxLength: Int,
      specializedTerm: Term,
      regularTypeVarIndices: List[Int],
      deferredDataConstrSpecializations: Map[Instantiation, List[Type]]
  )

  case class DataConstrContext(
      regularTypeVarIndices: List[Int],
      regularCtxLen: Option[Int],
      numParentTypeParams: Int
  )

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
      savedGenericBinds: Map[String, Bind] = Map.empty
  ): ContextState[List[Bind]] = {
    val allInsts = Instantiations.distinct(binds.map(_.insts).flatten)
    val insts = allInsts.filter(i =>
      i.r match {
        case Resolution.Closure | Resolution.DeferredDataConstr => false
        case _                                                  => true
      }
    )

    insts match {
      case Nil => binds.pure[ContextState]
      case _   =>
        // Save generic binds (TermTAbs) before specialization replaces them
        val newSavedGenericBinds = savedGenericBinds ++ binds.collect {
          case b @ Bind(name, TermAbbBind(_: TermTAbs, _), _, _)
              if !name.contains("#") =>
            name -> b
        }.toMap
        for {
          specializedBinds <- toSpecializedBinds(binds, insts)
          bindsWithDataConstrSpecs <- createMissingDataConstrSpecs(
            specializedBinds,
            newSavedGenericBinds
          )
          modifiedBinds <- bindsWithDataConstrSpecs.traverse(
            replaceInstantiations(_)
          )
          ibinds <- replaceM(modifiedBinds, newSavedGenericBinds)
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
    for {
      result <- binds
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
                    b = binds :+ Bind(
                      sbind.i,
                      sbind.b,
                      sInsts,
                      sbind.closureTypes
                    )
                  } yield (b, incrShifts(shifts))
                case i =>
                  for {
                    specializedBindsList <- i.zipWithIndex
                      .traverse((inst, idx) =>
                        for {
                          bind <- buildSpecializedBind(bind, inst, idx)
                            .map(bindShift(idx, _))
                          id <- addBinding(bind.i, bind.b)
                        } yield bind
                      )
                  } yield (
                    binds ::: specializedBindsList,
                    incrShifts(shifts) :+ Shift(
                      specializedBindsList.length - 1,
                      0
                    )
                  )
              }
            }
        }
      (specializedBinds, _) = result
    } yield specializedBinds

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

  /** Create missing data constructor specializations needed by deferred data
    * constructor insts.
    *
    * After toSpecializedBinds, specialized binds may have deferred data
    * constructor insts (r=DeferredDataConstr) with concrete types whose
    * specialized data constructor bind doesn't exist. This function creates
    * those missing data constructor binds and inserts them at the correct
    * position in the bind list (right after the existing data constructor
    * specialization), shifting subsequent binds' indices accordingly.
    */
  def createMissingDataConstrSpecs(
      binds: List[Bind],
      savedGenericBinds: Map[String, Bind]
  ): ContextState[List[Bind]] = {
    val deferredDataConstrInsts = binds.flatMap(b =>
      b.insts.filter(i =>
        i.r == Resolution.DeferredDataConstr && isDataConstrName(i.i) &&
          !i.tys.exists(ty => getTypeContextLength(ty).isDefined)
      )
    )
    val uniqueDataConstrInsts = Instantiations.distinct(deferredDataConstrInsts)
    uniqueDataConstrInsts.foldLeftM(binds) { (currentBinds, dataConstrInst) =>
      for {
        specName <- toContextState(
          Instantiation(
            dataConstrInst.i,
            dataConstrInst.term,
            dataConstrInst.tys,
            dataConstrInst.cls
          ).bindName()
        )
        exists <- State.inspect { (ctx: Context) =>
          nameToIndex(ctx, specName).isDefined
        }
        result <- (exists, savedGenericBinds.get(dataConstrInst.i)) match {
          case (false, Some(genericBind)) =>
            for {
              newBind <- buildSpecializedBind(
                genericBind,
                Instantiation(
                  dataConstrInst.i,
                  dataConstrInst.term,
                  dataConstrInst.tys,
                  dataConstrInst.cls
                ),
                0
              )
              _ <- addBinding(newBind.i, newBind.b)
              insertPos = findDataConstrInsertPos(
                currentBinds,
                dataConstrInst.i
              )
              (beforeInsert, afterInsert) = currentBinds.splitAt(insertPos)
              updatedAfter = afterInsert.zipWithIndex
                .map { case (b, idx) => shiftBindAfterInsert(b, idx) }
                .map(b => redirectDataConstrInBind(b, dataConstrInst))
            } yield beforeInsert ::: (newBind :: updatedAfter)
          case _ => currentBinds.pure[ContextState]
        }
      } yield result
    }
  }

  /** Find the insertion position for a new data constructor specialization.
    * Returns the index right after the last existing specialization of this
    * data constructor, or the end of the list if none exist.
    */
  def findDataConstrInsertPos(
      binds: List[Bind],
      dataConstrName: String
  ): Int = {
    val prefix = dataConstrName + Instantiations.BindTypeSeparator
    val positions = binds.zipWithIndex.collect {
      case (b, idx) if b.i.startsWith(prefix) => idx
    }
    positions.lastOption.map(_ + 1).getOrElse(binds.length)
  }

  /** Shift a bind's terms and inst terms to account for a newly inserted bind.
    * The cutoff determines which De Bruijn indices get shifted: only indices
    * above the cutoff (referencing binds before the insertion point) are
    * shifted +1.
    */
  def shiftBindAfterInsert(bind: Bind, cutoff: Int): Bind = {
    val shiftedBinding = bind.b match {
      case TermAbbBind(term, None) =>
        TermAbbBind(termShiftAbove(1, cutoff, term), None)
      case TermAbbBind(term, Some(ty)) =>
        TermAbbBind(
          termShiftAbove(1, cutoff, term),
          Some(typeShiftAbove(1, cutoff, ty))
        )
      case other => other
    }
    val shiftedInsts = bind.insts.map { inst =>
      inst.copy(term = termShiftAbove(1, cutoff, inst.term))
    }
    Bind(bind.i, shiftedBinding, shiftedInsts, bind.closureTypes)
  }

  /** Redirect a deferred data constructor TermVar in a bind to point at the
    * newly inserted specialization, and remove the matched deferred inst.
    * Returns the bind unchanged if no deferred inst matches.
    */
  def redirectDataConstrInBind(
      bind: Bind,
      dataConstrInst: Instantiation
  ): Bind = {
    val matchingInst = bind.insts.find(i =>
      i.r == Resolution.DeferredDataConstr && i.i == dataConstrInst.i && i.tys == dataConstrInst.tys
    )
    matchingInst match {
      case None       => bind
      case Some(inst) =>
        val termInfo = inst.term match {
          case tv: TermVar => Some(tv.info)
          case _           => None
        }
        termInfo match {
          case None       => bind
          case Some(info) =>
            val redirectedBinding = bind.b match {
              case TermAbbBind(term, ty) =>
                val newTerm = findTermVarByInfoWithDepth(term, info) match {
                  case Some((rawIdx, _, depth)) =>
                    val baseIdx = rawIdx - depth
                    redirectTermVar(term, info, baseIdx, baseIdx - 1)
                  case None => term
                }
                TermAbbBind(newTerm, ty)
              case _ => bind.b
            }
            val updatedInsts = bind.insts.filterNot(i =>
              i.r == Resolution.DeferredDataConstr && i.i == dataConstrInst.i && i.tys == dataConstrInst.tys
            )
            Bind(bind.i, redirectedBinding, updatedInsts, bind.closureTypes)
        }
    }
  }

  /** Redirect a TermVar with specific info from one index to another. Walks the
    * term tree and replaces TermVars matching the info whose current index
    * equals oldIdx with newIdx.
    */
  def redirectTermVar(
      term: Term,
      targetInfo: Info,
      oldIdx: Int,
      newIdx: Int
  ): Term =
    termMap(
      (info, c, k, n) =>
        // Match by info AND by the raw index being oldIdx + depth adjustment
        (info == targetInfo && k == oldIdx + c) match {
          case true  => TermVar(info, newIdx + c, n)
          case false => TermVar(info, k, n)
        },
      (c, ty) => ty,
      0,
      term
    )

  /** Check if a name is a data constructor name (starts with uppercase, not
    * specialized)
    */
  def isDataConstrName(name: String): Boolean =
    name.headOption.exists(_.isUpper) && !name.contains("#")

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
          // Separate closure instantiations (r == Closure) from regular ones
          closureInsts = bind.insts.filter(_.r == Resolution.Closure)
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
          // Specialize the term with closure types already applied
          specializedTerm = shiftedInst.tys.foldLeft(
            termWithClosureTypes: Term
          ) { case (t, ty) =>
            specializeTerm(t, idx, ty)
          }
          // Compute specialized types for deferred data constructor insts
          regularInsts = bind.insts.filter(i =>
            i.r match {
              case Resolution.Closure | Resolution.DeferredDataConstr => false
              case _                                                  => true
            }
          )
          regularTypeVarIndices = regularInsts
            .flatMap(i =>
              i.tys.collect { case tv: TypeVar => tv.index.intValue }
            )
            .distinct
            .sorted
            .reverse
          deferredDataConstrInsts = bind.insts.filter(
            _.r == Resolution.DeferredDataConstr
          )
          numParentTypeParams = shiftedInst.tys.length
          regularCtxLen = regularInsts
            .flatMap(i =>
              i.tys.collect { case tv: TypeVar => tv.length.intValue }
            )
            .headOption
          dcCtx = DataConstrContext(
            regularTypeVarIndices,
            regularCtxLen,
            numParentTypeParams
          )
          deferredDataConstrSpecializations = deferredDataConstrInsts.map {
            dcInst =>
              dcInst -> specializeDataConstrInstTys(
                dcInst,
                shiftedInst.tys,
                ctxlen - idx,
                dcCtx
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
            deferredDataConstrSpecializations
          )
          insts <- bind.insts.traverse(i =>
            resolveInstSpecialization(i, specCtx)
          )
        } yield finalizeSpecializedBind(
          name,
          binding,
          insts,
          bind.i,
          inst.tys,
          specializedClosureInsts
        )
      case _ =>
        throw new RuntimeException(
          s"can't build specialized binding ${inst.i}"
        )
    }

  /** Finalize a specialized bind: filter unresolved insts, add self-recursion,
    * extract closure types map for GRIN generation.
    */
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
    val dedupedInsts = Instantiations.distinct(filteredInsts)
    val selfInstantiation =
      containsAssocProjCall(binding, originalBindName) match {
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
    val finalInsts = Instantiations.distinct(selfInstantiation ::: dedupedInsts)
    val closureTypesMap = specializedClosureInsts
      .flatMap(cInst => cInst.tys.headOption.map(ty => cInst.i -> ty))
      .toMap
    Bind(name, binding, finalInsts, closureTypesMap)
  }

  /** Compute specialized types for a deferred data constructor inst. Data
    * constructor TypeVars may be in a deeper context than regular insts,
    * requiring context shift adjustment before mapping to parent types.
    */
  def specializeDataConstrInstTys(
      dataConstrInst: Instantiation,
      parentTys: List[Type],
      ctxLength: Int,
      dcCtx: DataConstrContext
  ): List[Type] = {
    val dataConstrCtxLen = dataConstrInst.tys.collectFirst { case tv: TypeVar =>
      tv.length.intValue
    }
    val contextShift = (dcCtx.regularCtxLen, dataConstrCtxLen) match {
      case (Some(regLen), Some(dataConstrLen)) => dataConstrLen - regLen
      case _                                   => dataConstrInst.tys.length
    }
    val referenceIndices = dcCtx.regularTypeVarIndices match {
      case Nil =>
        val typeVarIndices = dataConstrInst.tys.collect { case tv: TypeVar =>
          tv.index.intValue
        }
        typeVarIndices.headOption match {
          case Some(knownIdx) =>
            (0 until dcCtx.numParentTypeParams)
              .map(i => knownIdx + (dcCtx.numParentTypeParams - 1 - i))
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

  /** Resolve a single inst into its specialized form during bind
    * specialization. Handles regular insts, closure insts (r=-1), and deferred
    * data constructor insts (r=-2).
    */
  def resolveInstSpecialization(
      inst: Instantiation,
      ctx: SpecializationContext
  ): ContextState[Instantiation] =
    for {
      n <- toContextState(inst.bindName())
      tIdx <- State.inspect { (c: Context) => nameToIndex(c, n) }
      specializedTys = inst.r match {
        case Resolution.DeferredDataConstr =>
          ctx.deferredDataConstrSpecializations.getOrElse(inst, inst.tys)
        case _ =>
          specializeInstTys(
            inst.tys,
            ctx.parentTys,
            ctx.specCtxLength,
            ctx.regularTypeVarIndices
          )
      }
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
      val actualTerm: Term = (inst.r, inst.term) match {
        case (Resolution.DeferredDataConstr, tv: TermVar) =>
          findTermVarByInfoWithDepth(ctx.specializedTerm, tv.info) match {
            case Some((actualIdx, actualN, _)) =>
              TermVar(tv.info, actualIdx, actualN)
            case None =>
              termShiftAbove(-1, ctx.shiftCtxLength, tv)
          }
        case _ =>
          termShiftAbove(-1, ctx.shiftCtxLength, inst.term)
      }
      Instantiation(inst.i, actualTerm, specializedTys, inst.cls, newR)
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
        // For a raw TypeVar (not inside TypeAll), directly substitute
        // This handles cases where a single TypeVar represents a type parameter
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

  /** Specialize a list of inst types, handling raw TypeVars correctly.
    *
    * When inst types are raw TypeVars (from static method calls like
    * fold_right), we need to map each TypeVar to the correct position in the
    * parent's tys. TypeVars with higher De Bruijn indices correspond to outer
    * TAbs positions (earlier in tys), while lower indices correspond to inner
    * positions.
    */
  def specializeInstTys(
      instTys: List[Type],
      parentTys: List[Type],
      ctxLength: Int,
      referenceTypeVarIndices: List[Int] = List()
  ): List[Type] = {
    // Extract TypeVar indices to build a position mapping
    val typeVarIndices = instTys.zipWithIndex.collect {
      case (tv: TypeVar, pos) => (tv.index, pos)
    }
    // If all inst types are TypeVars, use index-based mapping
    val allTypeVars =
      instTys.length == typeVarIndices.length && instTys.nonEmpty
    allTypeVars match {
      case true =>
        // Use reference indices if provided to build a complete mapping
        // Reference indices come from ALL inst TypeVars in the enclosing bind
        val allIndices = (referenceTypeVarIndices ++ typeVarIndices.map(
          _._1.intValue
        )).distinct.sorted.reverse
        // Build mapping from TypeVar index -> parent tys position
        // Highest index = outermost TAbs = tys[0]
        val indexToParentPos: Map[Int, Int] = allIndices.zipWithIndex.map {
          case (tvIdx, parentPos) => tvIdx -> parentPos
        }.toMap
        instTys.zipWithIndex.map { case (ty, pos) =>
          typeVarIndices.find(_._2 == pos) match {
            case Some((tvIdx, _)) =>
              indexToParentPos.get(tvIdx) match {
                case Some(parentPos) if parentPos < parentTys.length =>
                  parentTys(parentPos)
                case _ => ty
              }
            case None => ty
          }
        }
      case false =>
        // Mixed types — use standard per-type specialization
        instTys.map(originalTy =>
          specializeType(originalTy, parentTys, ctxLength)
        )
    }
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
        isDeferredDataConstr = inst.term.isInstanceOf[TermVar] &&
          isDataConstrName(inst.i) &&
          bind.i.contains("#")
        resolvedIndex = inst.r match {
          case Resolution.Resolved(s) => Some(s)
          case Resolution.Unresolved  => specBindIndex
          case _                      => None
        }
        result = isDeferredDataConstr match {
          case true =>
            // Deferred data constructor insts: remove from inst list.
            // The TermVar replacement is handled later in createMissingDataConstrSpecs.
            ReplacementResult(acc.b, acc.insts.filterNot(_ == inst))
          case false =>
            (
              acc.b,
              inst.term,
              resolvedIndex
            ) match {
              case (TermAbbBind(tT, ty), tC: TermMethodProj, Some(s)) =>
                replaceProj(
                  tT,
                  ty,
                  acc,
                  inst,
                  methodProj =>
                    (methodProj.i == tC.i && methodProj.info == tC.info) match {
                      case true =>
                        TermMethodProj(
                          methodProj.info,
                          methodProj.t,
                          specBindName
                        )
                      case false => methodProj
                    },
                  assocProj => assocProj
                )
              case (TermAbbBind(tT, ty), tC: TermAssocProj, Some(s)) =>
                replaceProj(
                  tT,
                  ty,
                  acc,
                  inst,
                  methodProj => methodProj,
                  assocProj => {
                    val cleanTCI = tC.i.startsWith(MethodNamePrefix) match {
                      case true =>
                        tC.i.drop(MethodNamePrefix.length).takeWhile(_ != '#')
                      case false => tC.i
                    }
                    val methodMatch = assocProj.i == cleanTCI
                    val alreadySpecialized =
                      assocProj.i.startsWith(Desugar.MethodNamePrefix)
                    (methodMatch && !alreadySpecialized) match {
                      case true =>
                        TermAssocProj(assocProj.info, assocProj.t, specBindName)
                      case false => assocProj
                    }
                  }
                )
              case (TermAbbBind(tT, ty), tC: TermVar, Some(s)) =>
                replaceVar(tT, ty, tC, s, ctxlen, acc, inst)
              case (b, _, _) =>
                ReplacementResult(b, acc.insts)
            }
        }
      } yield Bind(bind.i, result.binding, result.insts, bind.closureTypes)
    )

  /** Replace projection terms in a binding using termMap with custom replacers
    */
  def replaceProj(
      tT: Term,
      ty: Option[Type],
      bind: Bind,
      inst: Instantiation,
      methodProjReplacer: TermMethodProj => Term,
      assocProjReplacer: TermAssocProj => Term
  ): ReplacementResult = {
    val replacedTerm = termMap(
      (info, c, k, n) => TermVar(info, k, n),
      (c, ty) => ty,
      methodProjReplacer,
      assocProjReplacer,
      0,
      tT
    )
    ReplacementResult(
      TermAbbBind(replacedTerm, ty),
      bind.insts.filterNot(_ == inst)
    )
  }

  /** Replace a TermVar reference with a specialized bind index */
  def replaceVar(
      tT: Term,
      ty: Option[Type],
      tC: TermVar,
      s: Int,
      ctxlen: Int,
      bind: Bind,
      inst: Instantiation
  ): ReplacementResult = {
    val d = inst.r match {
      case Resolution.Resolved(_) => None
      case _                      => Some(ctxlen)
    }
    val resultTerm = termVarSubstitute(s, d, tC, tT)
    ReplacementResult(
      TermAbbBind(resultTerm, ty),
      bind.insts.filterNot(_ == inst)
    )
  }

  /** Find the raw De Bruijn index, context length, and depth of a TermVar
    * matching a given Info. Depth tracks c from termMap (incremented for
    * TermAbs, TermTAbs, TermClosure, TermLet body).
    */
  def findTermVarByInfoWithDepth(
      term: Term,
      targetInfo: Info
  ): Option[(Int, Int, Int)] = {
    def search(t: Term, c: Int): Option[(Int, Int, Int)] = t match {
      case TermVar(info, idx, n) =>
        (info == targetInfo) match {
          case true  => Some((idx, n, c))
          case false => None
        }
      case TermAbs(_, _, _, body, _)  => search(body, c + 1)
      case TermTAbs(_, _, _, body)    => search(body, c + 1)
      case TermClosure(_, _, _, body) => search(body, c + 1)
      case TermLet(_, _, t1, t2)      => search(t1, c).orElse(search(t2, c + 1))
      case TermFix(_, body)           => search(body, c)
      case TermApp(_, f, arg)         => search(f, c).orElse(search(arg, c))
      case TermTApp(_, t, _)          => search(t, c)
      case TermMatch(_, scrutinee, cases) =>
        search(scrutinee, c).orElse(
          cases.view.flatMap { case (_, body) => search(body, c) }.headOption
        )
      case TermProj(_, t, _)       => search(t, c)
      case TermMethodProj(_, t, _) => search(t, c)
      case TermRecord(_, fields)   =>
        fields.view.flatMap { case (_, t) => search(t, c) }.headOption
      case TermTag(_, _, t, _)  => search(t, c)
      case TermAscribe(_, t, _) => search(t, c)
      case _                    => None
    }
    search(term, 0)
  }

  /** Check if a term contains a TermAssocProj call to a specific method */
  def containsAssocProjInTerm(term: Term, methodName: String): Boolean =
    term match {
      case TermAssocProj(_, _, method) =>
        // Strip method prefix if present (e.g., !foldRight#List -> foldRight)
        val cleanMethodName = methodName.startsWith(MethodNamePrefix) match {
          case true =>
            methodName.drop(MethodNamePrefix.length).takeWhile(_ != '#')
          case false => methodName
        }
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

  def containsAssocProjCall(
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
          // Extract param type from the full arrow type stored in closure inst
          val paramType = inst.tys.head match {
            case TypeArrow(_, param, _) => param
            case ty => ty // Backwards compat: handle bare param type
          }
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
