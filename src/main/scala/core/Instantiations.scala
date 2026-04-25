package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Context.*
import core.Bindings.*
import core.Types.Type
import core.Terms.*
import core.TypeChecker.*
import core.Types.*
import parser.Info.Info
import core.Desugar.{MethodNamePrefix, SelfTypeName}
import parser.Info.ShowInfo.ShowInfoOps
import scala.annotation.tailrec
import parser.Info.UnknownInfo
import code.GrinUtils.getNameFromType

object Instantiations {
  val BindTypeSeparator = "#"
  val BindTypeSeparatorChar = '#'

  def isDataConstrName(name: String): Boolean =
    name.headOption.exists(_.isUpper) && !name.contains(BindTypeSeparator)

  def getTypeArrity(ty: Type): Int = ty match {
    case TypeAll(_, _, _, _, a: TypeAll) => 1 + getTypeArrity(a)
    case TypeAll(_, _, _, _, _)          => 1
    case _                               => 0
  }

  def getValueArity(ty: Type): Int = ty match {
    case TypeAll(_, _, _, _, t) => getValueArity(t)
    case TypeArrow(_, _, t2)    => 1 + getValueArity(t2)
    case _                      => 0
  }

  def isAllPureTypeVars(tys: List[Type]): Boolean = tys.forall {
    case _: TypeVar => true
    case _          => false
  }

  /** Find the trait instance whose method set contains `method` for the
    * concrete type `typeName`, if any.
    */
  def findMethodTrait(
      typeName: String,
      method: String
  ): StateEither[Option[TypeClass]] = for {
    typeInstances <- EitherT.liftF(getTypeInstances(typeName))
    matching <- EitherT.liftF(
      typeInstances.findM(instance =>
        getAllTypeClassMethods(instance.name).map(_.exists(_ == method))
      )
    )
  } yield matching

  /** Resolve `typeName.method` to a concrete method id. When `traitOpt` is
    * given, prefer a type-specific override and fall back to the trait default.
    * Returns `(methodID, isTraitDefault)` — the second field is true when we
    * fell back to the trait default, which requires Self-prepending later.
    */
  def resolveMethodId(
      typeName: String,
      method: String,
      traitOpt: Option[TypeClass]
  ): StateEither[(String, Boolean)] = traitOpt match {
    case Some(traitInstance) =>
      val typeSpecificID =
        Desugar.toTypeInstanceMethodID(method, typeName, traitInstance.name)
      val traitDefaultID = Desugar.toMethodID(method, traitInstance.name)
      EitherT.liftF(State.inspect { (ctx: Context) =>
        nameToIndex(ctx, typeSpecificID)
          .map(_ => (typeSpecificID, false))
          .getOrElse((traitDefaultID, true))
      })
    case None =>
      (Desugar.toMethodID(method, typeName), false).pure[StateEither]
  }

  enum Resolution {
    case Unresolved
    case Closure
    case DeferredDataConstr
    case Resolved(index: Int)
    // Synthesized marker for bare TermVars that resolve (via ctx lookup) to
    // an already-specialized data-constructor bind. The inst carries the
    // generic base name and the DataConstr's concrete tys so that
    // `bindName()` reconstructs the specialized target. Only the list-based
    // rewrite path in `replaceInstantiations` is valid for these — there is
    // no meaningful ctx-based index to fall back to, so the inst is dropped
    // when list-based lookup fails.
    case BareSpecializedDataConstr
  }

  case class Instantiation(
      i: String,
      term: Term,
      tys: List[Type],
      cls: List[TypeClass] = List(),
      r: Resolution = Resolution.Unresolved
  ) {
    def isTypeResolved(): StateEither[Boolean] =
      for {
        idx <- EitherT.liftF(State.inspect { (ctx: Context) =>
          nameToIndex(ctx, i)
        })
        ty <- idx match {
          case Some(x) => getType(UnknownInfo, x).map(Some(_))
          case _       => None.pure[StateEither]
        }
      } yield ty.map(getTypeArrity(_) == tys.length).getOrElse(false)

    def bindName(): StateEither[String] =
      for {
        typeNames <- tys
          .traverse(Representation.typeToString(_))
          .map(_.mkString(BindTypeSeparator))
        baseName = s"$i$BindTypeSeparator$typeNames"
        name = cls match {
          // NOTE: When class is set on the instantiation it points to a type
          // instances's method. Hence we need to add a method prefix.
          case v if v.length > 0 =>
            val cls = v.map(_.name).mkString(BindTypeSeparator)
            s"$MethodNamePrefix$baseName$BindTypeSeparator$cls"
          case _ => baseName
        }
      } yield name
  }

  def build(
      t: Term,
      solutions: List[TypeESolutionBind],
      acc: List[Instantiation]
  ): StateEither[List[Instantiation]] = {
    val tys = solutions.map(_.t)
    val cls = solutions.map(_.cls).flatten
    val rootTerm = findRootTerm(t)
    (t, rootTerm, solutions) match {
      case (app: TermApp, assocProj @ TermAssocProj(_, ty, method), Nil) =>
        // Static method calls without type solutions - no instantiation needed
        acc.pure[StateEither]
      // Explicit type application carries its inst ty directly — handle before
      // the generic Nil-solutions early return so phantom-only type params
      // (generic functions whose type parameter does not flow through any
      // value argument) still get collected when value-arg unification
      // yields no solutions.
      case (TermTApp(info, termVar @ TermVar(_, idx, c), typ), _, _) =>
        for {
          optionName <- EitherT.liftF(State.inspect(indexToName(_, idx)))
          name <- optionName match {
            case Some(name) => name.pure[StateEither]
            case None       => TypeError.format(NotFoundTypeError(info))
          }
        } yield acc :+ Instantiation(name, termVar, List(typ), cls)
      case (_, _, Nil) =>
        acc.pure[StateEither]
      case (assocProj @ TermAssocProj(_, ty, method), _, _) if tys.nonEmpty =>
        // Direct static method term (not yet wrapped in TermApp)
        // Captures type solutions from innermost TypeAll unwrapping
        for {
          typeName <- EitherT.liftF(getNameFromType(ty))
          methodID = Desugar.toMethodID(method, typeName)
          resolvedTys <- tys.traverse(resolveTypeConstructorsInType(_))
        } yield acc :+ Instantiation(methodID, assocProj, resolvedTys, cls)
      case (
            methodTerm @ TermMethodProj(_, obj, method),
            _: TermMethodProj,
            _
          ) if tys.nonEmpty =>
        // Direct method projection with type solutions (not wrapped in
        // TermApp). Only fires for method calls with a single value
        // argument: multi-argument method calls are caught by the outer
        // `TermApp`/`TermMethodProj` case below, so collecting here would
        // double-add them.
        for {
          (objType, _) <- pureInfer(obj)
          typeName <- EitherT.liftF(getNameFromType(objType))
          traitOpt <- findMethodTrait(typeName, method)
          (methodID, isTraitDefault) <- resolveMethodId(
            typeName,
            method,
            traitOpt
          )
          methodIdx <- EitherT.liftF(
            State.inspect { (ctx: Context) => nameToIndex(ctx, methodID) }
          )
          methodArity <- methodIdx match {
            case None      => Option.empty[Int].pure[StateEither]
            case Some(idx) =>
              getType(UnknownInfo, idx).map(t => Some(getValueArity(t)))
          }
          result <- methodArity.contains(1) match {
            case false => acc.pure[StateEither]
            case true  =>
              for {
                resolvedTys <- tys.traverse(resolveTypeConstructorsInType(_))
                finalTys = isTraitDefault match {
                  case true  => TypeId(UnknownInfo, typeName) +: resolvedTys
                  case false => resolvedTys
                }
              } yield acc :+ Instantiation(methodID, methodTerm, finalTys, cls)
          }
        } yield result
      case (_: TermApp, methodTerm @ TermMethodProj(_, obj, method), _) =>
        for {
          (objType, _) <- pureInfer(obj) // type of the object, not the method
          typeName <- EitherT.liftF(getNameFromType(objType))
          traitOpt <- findMethodTrait(typeName, method)
          (methodID, isTraitDefault) <- resolveMethodId(
            typeName,
            method,
            traitOpt
          )
          // For method calls whose receiver has a bounded-generic type
          // (a type variable constrained by a type class), extract the
          // type argument from the object type to include in the inst
          // types. Without this, the inst would only capture the outer
          // application's type solution and miss the inner application's
          // solution. Skip when a trait matched (handled by the
          // isTraitDefault path) or when the object type name is the
          // trait receiver placeholder.
          objTypeArg <- (traitOpt, typeName) match {
            case (None, name) if name != SelfTypeName =>
              findRootTypeVar(objType) match {
                case Some(tv) =>
                  EitherT.liftF(getTypeBounds(tv)).map { bounds =>
                    bounds.nonEmpty match {
                      case true =>
                        objType match {
                          case TypeApp(_, _, arg) => Some(arg)
                          case _                  => None
                        }
                      case false => None
                    }
                  }
                case None => None.pure[StateEither]
              }
            case _ => None.pure[StateEither]
          }
          // For trait default methods, prepend the receiver's concrete
          // type because the trait default has an extra outermost TermTAbs
          // for the receiver-type parameter.
          finalTys = isTraitDefault match {
            case true  => TypeId(UnknownInfo, typeName) +: tys
            case false =>
              objTypeArg match {
                case Some(arg) => arg +: tys
                case None      => tys
              }
          }
        } yield acc :+ Instantiation(methodID, methodTerm, finalTys, cls)
      case (_: TermApp, assocProj @ TermAssocProj(_, ty, method), _)
          if tys.nonEmpty =>
        // Handle static method calls with type solutions
        // Type solutions preserve their full structure (including TypeApp wrappers)
        // so that distinct specializations get distinct monomorphized names
        for {
          typeName <- EitherT.liftF(getNameFromType(ty))
          methodID = Desugar.toMethodID(method, typeName)
          resolvedTys <- tys.traverse(resolveTypeConstructorsInType(_))
          existingInst = acc.find(_.i == methodID)
          // Look up the method's type arity to limit accumulation
          typeArity <- existingInst match {
            case Some(_) =>
              for {
                idx <- EitherT.liftF(State.inspect { (ctx: Context) =>
                  nameToIndex(ctx, methodID)
                })
                arity <- idx match {
                  case Some(i) =>
                    getType(UnknownInfo, i).map(getTypeArrity).recover {
                      case _ => 0
                    }
                  case None => 0.pure[StateEither]
                }
              } yield arity
            case None => 0.pure[StateEither]
          }
        } yield existingInst match {
          // If an inst already exists (from direct TermAssocProj case or prior TermApp layer),
          // append new types only if we haven't reached the type arity yet
          case Some(existing) if existing.tys.length < typeArity =>
            acc.map {
              case i if i.i == methodID =>
                val remaining = typeArity - i.tys.length
                Instantiation(
                  i.i,
                  i.term,
                  i.tys ::: resolvedTys.take(remaining),
                  i.cls
                )
              case e => e
            }
          case Some(_) => acc // Already has enough types
          case None    =>
            acc :+ Instantiation(methodID, assocProj, resolvedTys, cls)
        }

      case (app: TermApp, _, _) =>
        for {
          isFormedSolution <- EitherT.liftF(
            tys
              .traverse(isWellFormed(_))
              .map(_.reduce(_ && _))
          )
          fInsts <- acc.filterA(_.isTypeResolved().map(_ == false))
          r = fInsts.find(_.term == rootTerm) match {
            case Some(existing) =>
              // Don't accumulate types onto TermAssocProj instantiations -
              // they already have their types managed by the TermAssocProj case above
              existing.term match {
                case _: TermAssocProj => acc
                case _                =>
                  acc.map {
                    case i if i.term == rootTerm && isFormedSolution =>
                      Instantiation(i.i, i.term, i.tys ::: tys, i.cls)
                    case e => e
                  }
              }
            case _ => acc
          }
        } yield r
      case (termVar @ TermVar(info, idx, c), _, _) =>
        for {
          optionName <- EitherT.liftF(State.inspect(indexToName(_, idx)))
          name <- optionName match {
            case Some(name) => name.pure[StateEither]
            case None       => TypeError.format(NotFoundTypeError(info))
          }
          // Data constructors are uppercase and don't contain specialized suffix
          isDataConstructor = isDataConstrName(name)
          // Skip closure parameters - these are runtime values, not generic functions
          // Use semantic binding-based check instead of name pattern matching
          isClosureParam <- EitherT.liftF(
            State.inspect(Context.isClosureParameter(_, idx))
          )
          resolvedTys <- tys.traverse(resolveTypeConstructorsInType(_))
          // Limit collected types to the binding's actual type arity.
          //
          // When a closure is passed directly as an argument to a
          // constructor or generic function, the type checker's closure-
          // check rule prepends the closure's argument type as an extra
          // `TypeESolutionBind`. The consumer's declared type arity is
          // unchanged, so `resolvedTys.length` can exceed the arity.
          //
          // Those prepended solutions carry `fromClosurePrepend = true`,
          // so we drop them structurally (front-first, up to the surplus)
          // instead of positionally with `takeRight`. The invariant lives
          // in the data rather than in ordering arithmetic.
          arity <- getType(info, idx)
            .map(getTypeArrity)
            .recover { case _ => resolvedTys.length }
          arityLimitedTys = dropClosurePrependSurplus(
            resolvedTys,
            solutions,
            arity
          )
          result <- (
            isClosureParam,
            isAllPureTypeVars(arityLimitedTys) && isDataConstructor
          ) match {
            case (true, _) => acc.pure[StateEither]
            case (_, true) =>
              // Deferred data constructor instantiation - these will be specialized
              // during buildSpecializedBind with concrete types
              (acc :+ Instantiation(
                name,
                termVar,
                arityLimitedTys,
                cls,
                Resolution.DeferredDataConstr
              )).pure[StateEither]
            case _ =>
              (acc :+ Instantiation(name, termVar, arityLimitedTys, cls))
                .pure[StateEither]
          }
        } yield result
      case _ => acc.pure[StateEither]
    }
  }

  /** Drop closure-prepended solutions from `resolvedTys` so the collected type
    * list matches the bind's declared `arity`.
    *
    * `resolvedTys(i)` corresponds to `solutions(i)` (both produced in the same
    * order by `build`). When `solutions.length > arity`, the checker has added
    * more entries than the consumer expects — those extras are the
    * closure-prepends (`sol.fromClosurePrepend = true`) emitted when a closure
    * is type-checked against an expected type. We drop exactly the surplus
    * worth of closure prepends from the front. If the surplus cannot be
    * explained by closure prepends (e.g. a future codepath adds another kind of
    * prepend), we fall back to the positional `takeRight` so behavior is never
    * worse than before.
    */
  def dropClosurePrependSurplus(
      resolvedTys: List[Type],
      solutions: List[TypeESolutionBind],
      arity: Int
  ): List[Type] = (arity > 0 && resolvedTys.length > arity) match {
    case false => resolvedTys
    case true  =>
      val surplus = resolvedTys.length - arity
      val closurePrependIdxs = solutions.zipWithIndex.collect {
        case (sol, i) if sol.fromClosurePrepend => i
      }
      closurePrependIdxs.length >= surplus match {
        case true =>
          val toDrop = closurePrependIdxs.take(surplus).toSet
          resolvedTys.zipWithIndex.collect {
            case (t, i) if !toDrop.contains(i) => t
          }
        case false => resolvedTys.takeRight(arity)
      }
  }

  /** Resolve TypeVar type constructors inside TypeApp to TypeId.
    *
    * Type solutions may contain TypeApp(TypeVar(List_idx), TypeVar(B)) where
    * the outer TypeVar points to a type constructor (TypeAbbBind). These are
    * resolved to TypeApp(TypeId("List"), TypeVar(B)) so that during
    * monomorphization the constructor name is stable across context changes.
    */
  def resolveTypeConstructorsInType(ty: Type): StateEither[Type] = ty match {
    case TypeApp(info, tv @ TypeVar(tvInfo, idx, _), ty2) =>
      for {
        binding <- getBinding(tvInfo, idx).recover { case _ =>
          TermAbbBind(TermUnit(UnknownInfo), None)
        }
        name <- EitherT.liftF(State.inspect(indexToName(_, idx)))
        resolvedCtor = (binding, name) match {
          case (TypeAbbBind(_, _), Some(n)) => TypeId(tvInfo, n)
          case _                            => tv
        }
        resolvedTy2 <- resolveTypeConstructorsInType(ty2)
      } yield TypeApp(info, resolvedCtor, resolvedTy2)
    case TypeApp(info, ty1, ty2) =>
      for {
        resolvedTy1 <- resolveTypeConstructorsInType(ty1)
        resolvedTy2 <- resolveTypeConstructorsInType(ty2)
      } yield TypeApp(info, resolvedTy1, resolvedTy2)
    case tv @ TypeVar(tvInfo, idx, _) =>
      // Resolve bare TypeVars pointing to concrete TypeAbbBind (user-defined ADTs)
      // to stable TypeId names so they survive context changes in monomorphization.
      for {
        binding <- getBinding(tvInfo, idx).recover { case _ =>
          TermAbbBind(TermUnit(UnknownInfo), None)
        }
        name <- EitherT.liftF(State.inspect(indexToName(_, idx)))
        resolved = (binding, name) match {
          case (TypeAbbBind(_, _), Some(n)) => TypeId(tvInfo, n)
          case _                            => tv
        }
      } yield resolved
    case _ => ty.pure[StateEither]
  }

  @tailrec
  def findRootTerm(t: Term): Term = t match {
    case v @ TermVar(_, _, _) => v
    case TermApp(_, t1, t2)   => findRootTerm(t1)
    case _                    => t
  }

  /** Deduplicate instantiations after monomorphization-related collection.
    *
    * Always groups by method id and keeps only entries with the maximum
    * type-arg count (drops partial instantiations from incremental solving).
    *
    * `byTerm = true` keeps distinct call sites (different `term.info`) for the
    * same `(i, tys)` — required when `replaceInstantiations` matches by info.
    * `byTerm = false` collapses them into one — the default for callers that
    * only need one entry per `(method, types)` pair.
    */
  def distinct(
      insts: List[Instantiation],
      byTerm: Boolean = false
  ): List[Instantiation] = {
    val maxByMethod = insts
      .groupBy(_.i)
      .flatMap { case (_, group) =>
        val maxTypeCount = group.map(_.tys.length).maxOption.getOrElse(0)
        group.filter(_.tys.length == maxTypeCount)
      }
      .toList
    byTerm match {
      case true => maxByMethod.distinctBy(inst => (inst.i, inst.tys, inst.term))
      case false => maxByMethod.distinctBy(inst => (inst.i, inst.tys))
    }
  }

}
