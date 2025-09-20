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
  ): StateEither[List[Instantiation]] =
    val tys = solutions.map(_.t)
    val cls = solutions.map(_.cls).flatten
    val rootTerm = findRootTerm(t)
    (t, rootTerm, solutions) match {
      case (_, _, Nil) => acc.pure
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
        } yield acc :+ Instantiation(methodID, methodTerm, tys, cls) // Store the whole TermMethodProj
      
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
      case (termVar @ TermVar(info, idx, c), _, _) =>
        for {
          optionName <- EitherT.liftF(State.inspect { (ctx: Context) =>
            indexToName(ctx, idx)
          })
          name <- optionName match {
            case Some(name) => name.pure[StateEither]
            case None       => TypeError.format(NotFoundTypeError(info))
          }
        } yield acc :+ Instantiation(name, termVar, tys, cls)
      case _ => acc.pure
    }

  @tailrec
  def findRootTerm(t: Term): Term = t match {
    case v @ TermVar(_, _, _) => v
    case TermApp(_, t1, t2)   => findRootTerm(t1)
    case _                    => t
  }

  def getTypeArrity(ty: Type): Int = ty match {
    case TypeAll(_, _, _, _, a: TypeAll) => 1 + getTypeArrity(a)
    case TypeAll(_, _, _, _, _)          => 1
    case _                               => 0
  }

  def distinct(insts: List[Instantiation]): List[Instantiation] =
    insts.distinctBy(inst => (inst.i, inst.tys))
}
