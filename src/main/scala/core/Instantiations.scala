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
import fuse.Utils.debug
import scala.annotation.tailrec
import parser.Info.UnknownInfo

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
    debug("t", t)
    debug("solutions", solutions)
    debug("acc", acc)
    val tys = solutions.map(_.t)
    val cls = solutions.map(_.cls).flatten
    (t, solutions) match {
      case (_, Nil) => acc.pure
      case (app: TermApp, _) =>
        val rootTerm = findRootTerm(app)
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
      case (termVar @ TermVar(info, idx, c), _) =>
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
