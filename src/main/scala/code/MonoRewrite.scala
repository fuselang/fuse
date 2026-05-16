package code

import cats.data.State
import cats.implicits.*
import core.Bindings.*
import core.Context.*
import core.Instantiations
import core.Instantiations.{
  BindTypeSeparator,
  BindTypeSeparatorChar,
  Instantiation,
  Resolution,
  isDataConstrName
}
import core.Shifting.*
import core.TermFold
import core.Terms.*
import core.Types.*
import code.GrinUtils.{toContextState, getNameFromType}
import code.MonoSpecialize.*
import code.MonoTypes.*
import parser.Info.Info
import parser.Info.UnknownInfo
import core.Desugar.MethodNamePrefix
import core.Desugar
import core.TypeChecker.*

object MonoRewrite {

  /** Insert specialized binds into the bind list. Iterates over all binds and
    * creates specializations for those that have matching instantiations.
    * Accumulates De Bruijn index shifts as new binds are inserted.
    *
    * Returns the specialized bind list and a map from generic-bind name to the
    * shifted source bind. The map is populated for BOTH the i-arm (binds
    * specialised in this pass) and the Nil-arm (generic binds whose bodies are
    * shifted by the accumulator but not specialised here) —
    * `createMissingGenericSpecs` consumes the map to build deferred specs from
    * the iter-shifted source rather than the unshifted `savedGenericBinds`.
    * Without the Nil-arm coverage (Loophole D), a deferred spec built from an
    * unshifted source produces TypeVars whose `n` is δ units below the GRIN
    * context length, drifting names by δ slots at lookup time.
    */
  def toSpecializedBinds(
      binds: List[Bind],
      insts: List[Instantiation],
      allBindNames: Set[String] = Set.empty
  ): ContextState[(List[Bind], Map[String, Bind])] =
    for {
      result <- binds
        .foldLeftM((List[Bind](), List[Shift](), Map[String, Bind]())) {
          case ((binds, shifts, shiftedBinds), bind) =>
            getBindInstantiations(bind, insts).flatMap { bindInsts =>
              bindInsts match {
                case Nil =>
                  val sbind = shifts.foldLeft(bind) { (b, s) =>
                    bindShift(s.d, b, s.c)
                  }
                  // Record the shifted version of every GENERIC bind whose
                  // body gets bumped by the iteration-local accumulator
                  // even when it is not itself specialised in this pass.
                  // `createMissingGenericSpecs` later uses this map to
                  // build deferred specs from the iter-shifted source bind
                  // rather than the unshifted `savedGenericBinds` — without
                  // this, the deferred spec's body retains pre-shift indices
                  // while the bind list around it grew, producing the
                  // cross-iteration drift class (Loophole D).
                  val updatedShiftedBinds = bind.b match {
                    case TermAbbBind(_: TermTAbs, _) =>
                      shiftedBinds + (bind.i -> sbind)
                    case _ => shiftedBinds
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
                  } yield (b, incrShifts(shifts), updatedShiftedBinds)
                case i =>
                  val shiftedSourceBind = shifts.foldLeft(bind) { (b, s) =>
                    bindShift(s.d, b, s.c)
                  }
                  for {
                    specializedBindsList <- i.zipWithIndex
                      .traverse((inst, idx) =>
                        for {
                          bind <- buildSpecializedBind(
                            shiftedSourceBind,
                            inst,
                            idx,
                            allBindNames
                          )
                            .map(bindShift(idx, _))
                          id <- addBinding(bind.i, bind.b)
                        } yield bind
                      )
                  } yield (
                    binds ::: specializedBindsList,
                    incrShifts(shifts) :+ Shift(
                      specializedBindsList.length - 1,
                      1
                    ),
                    shiftedBinds + (bind.i -> shiftedSourceBind)
                  )
              }
            }
        }
      (specializedBinds, _, shiftedGenericBinds) = result
    } yield (specializedBinds, shiftedGenericBinds)

  /** Find instantiations from the given list that match a specific bind. For
    * ADTs, all instantiations for that type are associated. For type instance
    * methods, those instantiations are associated.
    */
  def getBindInstantiations(
      bind: Bind,
      insts: List[Instantiation]
  ): ContextState[List[Instantiation]] = for {
    typeNameOption <- getAlgebraicDataTypeName(UnknownInfo, bind.i)
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
    dedupedBindInsts <- bindInsts
      .traverse(inst =>
        toContextState(inst.bindName()).map(name => (name, inst))
      )
      .map(_.distinctBy(_._1).map(_._2))
    filteredBindInsts <- dedupedBindInsts.filterA(
      instantantionShiftOnContextDiff(_)
        .flatMap(_.tys.forallM(isTypeFullyResolved(_, 0)))
    )
  } yield filteredBindInsts

  /** Create missing data constructor specializations needed by deferred data
    * constructor insts. Inserts them at the correct position in the bind list
    * and shifts subsequent binds' indices accordingly.
    */
  def createMissingDataConstrSpecs(
      binds: List[Bind],
      savedGenericBinds: Map[String, Bind]
  ): ContextState[List[Bind]] = {
    val candidateDataConstrInsts = binds.flatMap(b =>
      b.insts.filter(i =>
        i.r == Resolution.DeferredDataConstr && isDataConstrName(i.i)
      )
    )
    for {
      deferredDataConstrInsts <- candidateDataConstrInsts.filterA(i =>
        i.tys.forallM(isTypeFullyResolved(_, 0))
      )
      uniqueDataConstrInsts = Instantiations.distinct(deferredDataConstrInsts)
      result <- uniqueDataConstrInsts.foldLeftM(binds) {
        (currentBinds, dataConstrInst) =>
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
            innerResult <- (
              exists,
              savedGenericBinds.get(dataConstrInst.i)
            ) match {
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
          } yield innerResult
      }
    } yield result
  }

  def isConcreteType(ty: Type): Boolean = ty match {
    case _: TypeApp    => true
    case _: TypeString => true
    case _: TypeInt    => true
    case _: TypeFloat  => true
    case _: TypeBool   => true
    case _: TypeUnit   => true
    case _: TypeAny    => true
    case _             => false
  }

  /** Create missing specializations for generic functions discovered via
    * transitive instantiation resolution (i.e., after the main
    * `toSpecializedBinds` pass has run). Each missing spec is inserted right
    * after the last existing specialization of the same function.
    *
    * The `shiftedGenericBinds` map (produced by `toSpecializedBinds`) contains
    * the SHIFTED version of each expanded generic bind — i.e., the version with
    * accumulated De Bruijn index shifts applied. These shifted versions carry
    * TypeVar `n` (context-length) fields that are consistent with the position
    * those binds occupied in the context during the main specialization pass.
    * Using the shifted version here is critical: the unshifted original from
    * `savedGenericBinds` has TypeVar `n` values that are one or more units too
    * small, causing `typeShiftOnContextDiff` in the GRIN backend to compute a
    * wrong index offset and resolve type-name references to the wrong binding.
    */
  def createMissingGenericSpecs(
      binds: List[Bind],
      savedGenericBinds: Map[String, Bind],
      shiftedGenericBinds: Map[String, Bind] = Map.empty
  ): ContextState[List[Bind]] = for {
    // Bind-aware unresolvedness: keep insts whose remaining `TypeVar`s
    // refer to top-level concrete types (TypeAbbBind), drop those that
    // refer to still-unresolved generic params (TypeVarBind). See
    // `MonoSpecialize.isFreeOfGenericTypeVar` for the rationale.
    unresolvedConcreteInsts <- binds
      .flatMap(_.insts)
      .filterA(i =>
        i.r match {
          case Resolution.Unresolved =>
            i.tys.forallM(ty => isFreeOfGenericTypeVar(ty)).map { freeOfGen =>
              freeOfGen && i.tys.exists(isConcreteType(_))
            }
          case _ => false.pure[ContextState]
        }
      )
    uniqueInsts = Instantiations.distinct(unresolvedConcreteInsts)
    result <- uniqueInsts.foldLeftM(binds) { (currentBinds, inst) =>
      for {
        specName <- toContextState(inst.bindName())
        exists <- State.inspect { (ctx: Context) =>
          nameToIndex(ctx, specName).isDefined
        }
        // Each iteration of the foldLeft inserts a spec into `currentBinds`
        // and shifts every bind after the insertion via `shiftBindAfterInsert`.
        // The `shiftedGenericBinds` map's values are static snapshots taken
        // BEFORE this loop began, so when a later iteration looks up the same
        // (or another) generic, the snapshot lacks the prior iteration's
        // insertion shift. Re-locate the generic in `currentBinds` (filtered to
        // `TermAbbBind(TermTAbs, _)` so we don't accidentally match a non-
        // generic same-named bind like a record constructor) so the newly-built
        // spec body inherits all accumulated shifts.
        currentGenericBind = currentBinds.find(b =>
          b.i == inst.i && (b.b match {
            case TermAbbBind(_: TermTAbs, _) => true
            case _                           => false
          })
        )
        result <- (
          exists,
          currentGenericBind.orElse(
            shiftedGenericBinds
              .get(inst.i)
              .orElse(savedGenericBinds.get(inst.i))
          )
        ) match {
          case (false, Some(genericBind)) =>
            for {
              newBind <- buildSpecializedBind(genericBind, inst, 0)
              _ <- addBinding(newBind.i, newBind.b)
              insertPos = findGenericSpecInsertPos(currentBinds, inst.i)
              (beforeInsert, afterInsert) = currentBinds.splitAt(insertPos)
              updatedAfter = afterInsert.zipWithIndex.map { case (b, idx) =>
                shiftBindAfterInsert(b, idx)
              }
            } yield beforeInsert ::: (newBind :: updatedAfter)
          case _ => currentBinds.pure[ContextState]
        }
      } yield result
    }
  } yield result

  /** Find the insertion position for a new generic specialization. Returns the
    * index right after the last existing specialization of this function, or
    * after the original generic bind if none exist.
    */
  def findGenericSpecInsertPos(binds: List[Bind], baseName: String): Int = {
    val prefix = baseName + BindTypeSeparator
    val positions = binds.zipWithIndex.collect {
      case (b, idx) if b.i == baseName || b.i.startsWith(prefix) =>
        idx
    }
    positions.lastOption.map(_ + 1).getOrElse(binds.length)
  }

  /** Find the insertion position for a new data constructor specialization.
    * Returns the index right after the last existing specialization of this
    * data constructor, or the end of the list if none exist.
    */
  def findDataConstrInsertPos(
      binds: List[Bind],
      dataConstrName: String
  ): Int = {
    val prefix = dataConstrName + BindTypeSeparator
    val positions = binds.zipWithIndex.collect {
      case (b, idx) if b.i.startsWith(prefix) => idx
    }
    positions.lastOption.map(_ + 1).getOrElse(binds.length)
  }

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

  def redirectDataConstrInBind(
      bind: Bind,
      dataConstrInst: Instantiation
  ): Bind = {
    def isMatching(i: Instantiation): Boolean =
      i.r == Resolution.DeferredDataConstr &&
        i.i == dataConstrInst.i &&
        i.tys == dataConstrInst.tys
    val rewritten = for {
      inst <- bind.insts.find(isMatching)
      info <- inst.term match {
        case tv: TermVar => Some(tv.info)
        case _           => None
      }
    } yield {
      val redirectedBinding = bind.b match {
        case TermAbbBind(term, ty) =>
          val newTerm = TermFold.findVarByInfo(term, info) match {
            case Some((rawIdx, _, depth)) =>
              val baseIdx = rawIdx - depth
              redirectTermVar(term, info, baseIdx, baseIdx - 1)
            case None => term
          }
          TermAbbBind(newTerm, ty)
        case other => other
      }
      Bind(
        bind.i,
        redirectedBinding,
        bind.insts.filterNot(isMatching),
        bind.closureTypes
      )
    }
    rewritten.getOrElse(bind)
  }

  def redirectTermVar(
      term: Term,
      targetInfo: Info,
      oldIdx: Int,
      newIdx: Int
  ): Term =
    termMap(
      (info, c, k, n) => {
        val resolvedK = (info == targetInfo && k == oldIdx + c) match {
          case true  => newIdx + c
          case false => k
        }
        TermVar(info, resolvedK, n)
      },
      (c, ty) => ty,
      0,
      term
    )

  /** Align a TermVar inst's term with the corresponding body TermVar by info.
    * Without this, inst.term's idx/n can drift from the body's TermVar idx/n
    * when the body gets modified by prior shifts or specializations but the
    * inst's term does not, causing replaceVar's tC.i1 == x match to fail.
    *
    * Only aligns when there is NO body TermVar with both matching info and
    * matching idx — if such a TermVar exists, inst is already aligned and we
    * must not rewrite to a different occurrence that merely shares info.
    */
  def alignInstToBody(bind: Bind, inst: Instantiation): Instantiation =
    (bind.b, inst.term) match {
      case (TermAbbBind(term, _), tv: TermVar) =>
        val hasStrictMatch = TermFold.findVarByInfoAndIdx(term, tv.info, tv.i1)
        hasStrictMatch match {
          case true  => inst
          case false =>
            TermFold.findVarByInfo(term, tv.info) match {
              case Some((actualIdx, actualN, _)) =>
                inst.copy(term = TermVar(tv.info, actualIdx, actualN))
              case None => inst
            }
        }
      case _ => inst
    }

  /** Replace all instantiations in a bind with references to their specialized
    * binds. Handles TermMethodProj, TermAssocProj, and TermVar replacements.
    */
  def replaceInstantiations(
      bind: Bind,
      allBinds: List[Bind] = List.empty
  ): ContextState[Bind] =
    bind.insts.foldM(bind)((acc, origInst) => {
      // Align inst.term idx/n to the current body by info, so replaceVar's
      // tC.i1 == x match succeeds even when the body was modified by prior
      // shifts (e.g. non-specialized binds like main whose body was
      // incrementally rewritten across worklist iterations).
      val inst = alignInstToBody(acc, origInst)
      for {
        specBindName <- toContextState(inst.bindName())
        (specBindIndex, ctxlen) <- State.inspect { (ctx: Context) =>
          (nameToIndex(ctx, specBindName), ctx._1.length)
        }
        // Compute idx relative to the bind's list position. MonoRewrite's
        // ctx may order binds differently from the final list (insertions by
        // createMissingGenericSpecs), so ctx-based idx becomes stale at
        // codegen time (ctx grows sequentially through the list). List-based
        // idx stays valid because both phases share the same list. Applied
        // for Resolved data-constructor insts (specialized constructors
        // whose fields contain function types), Unresolved regular insts
        // targeting a TermVar, and BareSpecializedDataConstr synthetic insts for
        // bare data-constructor references in type-class default match arms.
        listBasedIndex = (
          bind.b.isInstanceOf[TermAbbBind] && (inst.r match {
            case Resolution.Resolved(_) => isDataConstrName(inst.i)
            case Resolution.Unresolved  =>
              inst.term.isInstanceOf[TermVar]
            case Resolution.BareSpecializedDataConstr => true
            case _                                    => false
          })
        ) match {
          case false => None
          case true  =>
            (
              allBinds.indexWhere(_.i == bind.i),
              allBinds.indexWhere(_.i == specBindName)
            ) match {
              case (bp, tp) if bp >= 0 && tp >= 0 && tp < bp =>
                Some(bp - tp - 1)
              case _ => None
            }
        }
        isDeferredDataConstr = inst.term.isInstanceOf[TermVar] &&
          isDataConstrName(inst.i) &&
          bind.i.contains(BindTypeSeparator) &&
          listBasedIndex.isEmpty
        resolvedIndex = inst.r match {
          case Resolution.Resolved(s) => Some(s)
          case Resolution.Unresolved  => specBindIndex
          // Synthetic DataConstr insts have no meaningful ctx idx — only
          // the list-based path is valid. Provide a dummy Some iff list-
          // based lookup succeeded (ignored by replaceVar's list-based
          // branch); otherwise None so the downstream match drops the inst.
          case Resolution.BareSpecializedDataConstr =>
            listBasedIndex.map(_ => 0)
          case _ => None
        }
        result = isDeferredDataConstr match {
          case true =>
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
                        tC.i
                          .drop(MethodNamePrefix.length)
                          .takeWhile(_ != BindTypeSeparatorChar)
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
                replaceVar(tT, ty, tC, s, ctxlen, acc, inst, listBasedIndex)
              case (b, _, _) =>
                ReplacementResult(b, acc.insts)
            }
        }
      } yield Bind(bind.i, result.binding, result.insts, bind.closureTypes)
    })

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

  def replaceVar(
      tT: Term,
      ty: Option[Type],
      tC: TermVar,
      s: Int,
      ctxlen: Int,
      bind: Bind,
      inst: Instantiation,
      listBasedIndex: Option[Int] = None
  ): ReplacementResult = {
    val resultTerm = listBasedIndex match {
      case Some(lIdx) =>
        // List-based idx: at codegen ctxlen=bp (bind's list pos), target at
        // list pos tp resolves via idx=lIdx=bp-tp-1. Inside abstractions,
        // ctx grows by depth j, so the idx shifts by +j to stay valid.
        termMap(
          (info, j, x, n) =>
            if (tC.info == info && tC.i1 == x)
              TermVar(info, lIdx + j, n)
            else TermVar(info, x, n),
          (_, ty) => ty,
          0,
          tT
        )
      case None =>
        val d = inst.r match {
          case Resolution.Resolved(_) => None
          case _                      => Some(ctxlen)
        }
        termVarSubstitute(s, d, tC, tT)
    }
    ReplacementResult(
      TermAbbBind(resultTerm, ty),
      bind.insts.filterNot(_ == inst)
    )
  }
}
