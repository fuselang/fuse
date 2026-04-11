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
  Resolution
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
    */
  def toSpecializedBinds(
      binds: List[Bind],
      insts: List[Instantiation],
      allBindNames: Set[String] = Set.empty
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
                          bind <- buildSpecializedBind(
                            bind,
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
                      0
                    )
                  )
              }
            }
        }
      (specializedBinds, _) = result
    } yield specializedBinds

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

  def createMissingGenericSpecs(
      binds: List[Bind],
      savedGenericBinds: Map[String, Bind]
  ): ContextState[List[Bind]] = {
    val unresolvedConcreteInsts = binds.flatMap(b =>
      b.insts.filter(i =>
        i.r == Resolution.Unresolved &&
          !i.tys.exists(ty => getTypeContextLength(ty).isDefined) &&
          i.tys.exists(_.isInstanceOf[TypeApp])
      )
    )
    val uniqueInsts = Instantiations.distinct(unresolvedConcreteInsts)
    uniqueInsts.foldLeftM(binds) { (currentBinds, inst) =>
      for {
        specName <- toContextState(inst.bindName())
        exists <- State.inspect { (ctx: Context) =>
          nameToIndex(ctx, specName).isDefined
        }
        result <- (exists, savedGenericBinds.get(inst.i)) match {
          case (false, Some(genericBind)) =>
            for {
              newBind <- buildSpecializedBind(genericBind, inst, 0)
              _ <- addBinding(newBind.i, newBind.b)
            } yield currentBinds :+ newBind
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
                val newTerm = TermFold.findVarByInfo(term, info) match {
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

  def redirectTermVar(
      term: Term,
      targetInfo: Info,
      oldIdx: Int,
      newIdx: Int
  ): Term =
    termMap(
      (info, c, k, n) =>
        (info == targetInfo && k == oldIdx + c) match {
          case true  => TermVar(info, newIdx + c, n)
          case false => TermVar(info, k, n)
        },
      (c, ty) => ty,
      0,
      term
    )

  /** Replace all instantiations in a bind with references to their specialized
    * binds. Handles TermMethodProj, TermAssocProj, and TermVar replacements.
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
          bind.i.contains(BindTypeSeparator)
        resolvedIndex = inst.r match {
          case Resolution.Resolved(s) => Some(s)
          case Resolution.Unresolved  => specBindIndex
          case _                      => None
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
                replaceVar(tT, ty, tC, s, ctxlen, acc, inst)
              case (b, _, _) =>
                ReplacementResult(b, acc.insts)
            }
        }
      } yield Bind(bind.i, result.binding, result.insts, bind.closureTypes)
    )

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
}
