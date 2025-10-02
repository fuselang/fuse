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
import core.Desugar.MethodNamePrefix
import parser.Info.ShowInfo.ShowInfoOps
import scala.annotation.tailrec
import parser.Info.UnknownInfo
import code.GrinUtils.getNameFromType

object Instantiations {
  val BindTypeSeparator = "#"

  def getTypeArrity(ty: Type): Int = ty match {
    case TypeAll(_, _, _, _, a: TypeAll) => 1 + getTypeArrity(a)
    case TypeAll(_, _, _, _, _)          => 1
    case _                               => 0
  }

  def isAllPureTypeVars(tys: List[Type]): Boolean = tys.forall {
    case _: TypeVar => true
    case _ => false
  }

  case class Instantiation(
      i: String,
      term: Term,
      tys: List[Type],
      cls: List[TypeClass] = List(),
      r: Option[Int] = None
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
      case (_: TermApp, methodTerm @ TermMethodProj(_, obj, method), _) =>
        for {
          (objType, _) <- pureInfer(obj) // Infer type of the object, not the method
          typeName <- EitherT.liftF(getNameFromType(objType))
          // Enhanced: Detect if this is a trait method vs direct type method
          typeInstances <- EitherT.liftF(getTypeInstances(typeName))
          matchingTraitOpt <- EitherT.liftF(typeInstances.findM(instance =>
            getTypeClassMethods(instance.name).map(_.exists(_ == method))
          ))
          methodID = matchingTraitOpt match {
            case Some(traitInstance) =>
              Desugar.toTypeInstanceMethodID(method, typeName, traitInstance.name)
            case None =>
              Desugar.toMethodID(method, typeName)
          }
        } yield acc :+ Instantiation(methodID, methodTerm, tys, cls)
      case (_: TermApp, assocProj @ TermAssocProj(_, ty, method), _) if tys.nonEmpty =>
        // Handle static method calls with type solutions
        // Extract type arguments from TypeApp structures to get actual type parameter values
        // For foldRight[A, B], solutions may contain [TypeApp(List, A), B]
        // We need to extract to get [A, B] for correct bind naming
        for {
          typeName <- EitherT.liftF(getNameFromType(ty))
          methodID = Desugar.toMethodID(method, typeName)
          extractedTys = extractTypeArgs(tys)
        } yield acc :+ Instantiation(methodID, assocProj, extractedTys, cls)

      case (app: TermApp, _, _) =>
        for {
          isFormedSolution <- EitherT.liftF(
            tys
              .traverse(isWellFormed(_))
              .map(_.reduce(_ && _))
          )
          fInsts <- acc.filterA(_.isTypeResolved().map(_ == false))
          r = fInsts.find(_.term == rootTerm) match {
            case Some(_) =>
              acc.map {
                case i if i.term == rootTerm && isFormedSolution =>
                  Instantiation(i.i, i.term, i.tys ::: tys, i.cls)
                case e => e
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
          isDataConstructor = name.headOption.exists(_.isUpper) && !name.contains("#")
          result <- isDataConstructor match {
            case true  => (acc :+ Instantiation(name, termVar, List(typ), cls)).pure[StateEither]
            case false => acc.pure[StateEither]  // Non-constructor, skip
          }
        } yield result
      case (termVar @ TermVar(info, idx, c), _, _) =>
        for {
          optionName <- EitherT.liftF(State.inspect(indexToName(_, idx)))
          name <- optionName match {
            case Some(name) => name.pure[StateEither]
            case None       => TypeError.format(NotFoundTypeError(info))
          }
          // Skip data constructors with unresolved type parameters
          // Data constructors are uppercase and don't contain specialized suffix
          isDataConstructor = name.headOption.exists(_.isUpper) && !name.contains("#")
          // Skip closure parameters - these are runtime values, not generic functions
          // Use semantic binding-based check instead of name pattern matching
          isClosureParam <- EitherT.liftF(State.inspect(Context.isClosureParameter(_, idx)))
          shouldSkip = (isAllPureTypeVars(tys) && isDataConstructor) || isClosureParam
          result <- shouldSkip match {
            case true  => acc.pure[StateEither]
            case false => (acc :+ Instantiation(name, termVar, tys, cls)).pure[StateEither]
          }
        } yield result
      case _ => acc.pure[StateEither]
    }
  }

  @tailrec
  def findRootTerm(t: Term): Term = t match {
    case v @ TermVar(_, _, _) => v
    case TermApp(_, t1, t2)   => findRootTerm(t1)
    case _                    => t
  }

  def distinct(insts: List[Instantiation]): List[Instantiation] =
    insts
      .groupBy(_.i)  // Group by method ID
      .flatMap { case (methodID, group) =>
        // For each method, keep only instantiations with max type count
        // This filters out partial instantiations created during incremental type solving
        val maxTypeCount = group.map(_.tys.length).maxOption.getOrElse(0)
        group.filter(_.tys.length == maxTypeCount)
      }
      .toList
      .distinctBy(inst => (inst.i, inst.tys))  // Still deduplicate exact duplicates

  /** Extract type arguments from TypeApp structures for static method instantiations.
    *
    * For static method calls like `List::foldRight[A, B](...)`, type solutions
    * may contain TypeApp(List, A) instead of just A. This function extracts the
    * type arguments to get the actual type parameter values.
    */
  def extractTypeArgs(tys: List[Type]): List[Type] =
    tys.flatMap {
      case TypeApp(_, t1, t2) =>
        // For TypeApp(TypeId("List"), TypeInt), extract both arguments
        // This handles cases like List[A] -> [List, A]
        // We want just the type argument (A), not the type constructor
        extractTypeArgs(List(t2))
      case ty => List(ty)
    }
}
