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

  def getTypeArrity(ty: Type): Int = ty match {
    case TypeAll(_, _, _, _, a: TypeAll) => 1 + getTypeArrity(a)
    case TypeAll(_, _, _, _, _)          => 1
    case _                               => 0
  }

  def isAllPureTypeVars(tys: List[Type]): Boolean = tys.forall {
    case _: TypeVar => true
    case _          => false
  }

  enum Resolution {
    case Unresolved
    case Closure
    case DeferredDataConstr
    case Resolved(index: Int)
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
      case (_: TermApp, methodTerm @ TermMethodProj(_, obj, method), _) =>
        for {
          (objType, _) <- pureInfer(
            obj
          ) // Infer type of the object, not the method
          typeName <- EitherT.liftF(getNameFromType(objType))
          // Enhanced: Detect if this is a trait method vs direct type method
          // Use getAllTypeClassMethods to include default implementations too
          typeInstances <- EitherT.liftF(getTypeInstances(typeName))
          matchingTraitOpt <- EitherT.liftF(
            typeInstances.findM(instance =>
              getAllTypeClassMethods(instance.name).map(_.exists(_ == method))
            )
          )
          // Choose method ID: prefer type-specific override, fall back to trait default
          // Also track whether we're using a trait default (needs Self type prepended)
          (methodID, isTraitDefault) <- matchingTraitOpt match {
            case Some(traitInstance) =>
              val typeSpecificID = Desugar.toTypeInstanceMethodID(
                method,
                typeName,
                traitInstance.name
              )
              val traitDefaultID =
                Desugar.toMethodID(method, traitInstance.name)
              EitherT.liftF(State.inspect { (ctx: Context) =>
                nameToIndex(ctx, typeSpecificID)
                  .map(idx => (typeSpecificID, false))
                  .getOrElse((traitDefaultID, true))
              })
            case None =>
              (Desugar.toMethodID(method, typeName), false).pure[StateEither]
          }
          // For bounded type parameter method calls (e.g., c.map(f) where c: F[A]
          // and F: Functor), extract the type argument from the object type to include
          // in the inst types. Without this, the inst would only capture the outer
          // TermApp's solution (B) but miss the inner TermApp's solution (A).
          // Skip when: matchingTraitOpt found, or typeName is "Self" (trait default
          // context where Self methods are handled by resolveAbstractMethodId).
          objTypeArg <- (matchingTraitOpt, typeName) match {
            case (None, name) if name != SelfTypeName =>
              val rootTV = findRootTypeVar(objType)
              rootTV match {
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
          // For trait default methods, prepend the Self type (e.g., TypeId("Option"))
          // because the trait default has an extra outermost TermTAbs for Self.
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
      case (TermTApp(info, termVar @ TermVar(_, idx, c), typ), _, _) =>
        for {
          optionName <- EitherT.liftF(State.inspect(indexToName(_, idx)))
          name <- optionName match {
            case Some(name) => name.pure[StateEither]
            case None       => TypeError.format(NotFoundTypeError(info))
          }
          // Handle explicit type applications to data constructors (e.g., Nil[B])
          isDataConstructor = name.headOption.exists(_.isUpper) && !name
            .contains(BindTypeSeparator)
          result <- isDataConstructor match {
            case true =>
              (acc :+ Instantiation(name, termVar, List(typ), cls))
                .pure[StateEither]
            case false => acc.pure[StateEither] // Non-constructor, skip
          }
        } yield result
      case (termVar @ TermVar(info, idx, c), _, _) =>
        for {
          optionName <- EitherT.liftF(State.inspect(indexToName(_, idx)))
          name <- optionName match {
            case Some(name) => name.pure[StateEither]
            case None       => TypeError.format(NotFoundTypeError(info))
          }
          // Data constructors are uppercase and don't contain specialized suffix
          isDataConstructor = name.headOption.exists(_.isUpper) && !name
            .contains(BindTypeSeparator)
          // Skip closure parameters - these are runtime values, not generic functions
          // Use semantic binding-based check instead of name pattern matching
          isClosureParam <- EitherT.liftF(
            State.inspect(Context.isClosureParameter(_, idx))
          )
          resolvedTys <- tys.traverse(resolveTypeConstructorsInType(_))
          result <- (
            isClosureParam,
            isAllPureTypeVars(resolvedTys) && isDataConstructor
          ) match {
            case (true, _) => acc.pure[StateEither]
            case (_, true) =>
              // Deferred data constructor instantiation - these will be specialized
              // during buildSpecializedBind with concrete types
              (acc :+ Instantiation(
                name,
                termVar,
                resolvedTys,
                cls,
                Resolution.DeferredDataConstr
              )).pure[StateEither]
            case _ =>
              (acc :+ Instantiation(name, termVar, resolvedTys, cls))
                .pure[StateEither]
          }
        } yield result
      case _ => acc.pure[StateEither]
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

  def distinct(insts: List[Instantiation]): List[Instantiation] =
    insts
      .groupBy(_.i) // Group by method ID
      .flatMap { case (methodID, group) =>
        // For each method, keep only instantiations with max type count
        // This filters out partial instantiations created during incremental type solving
        val maxTypeCount = group.map(_.tys.length).maxOption.getOrElse(0)
        group.filter(_.tys.length == maxTypeCount)
      }
      .toList
      .distinctBy(inst =>
        (inst.i, inst.tys)
      ) // Still deduplicate exact duplicates

}
