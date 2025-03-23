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

object Instantiations {
  val BindTypeSeparator = "#"

  case class Instantiation(
      i: String,
      term: Term,
      tys: List[Type],
      cls: List[TypeClass] = List(),
      r: Option[Int] = None
  ) {
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
      solutions: List[TypeESolutionBind]
  ): StateEither[List[Instantiation]] = (t, solutions) match {
    case (_, Nil) => Nil.pure
    case (TermVar(info, idx, c), _) =>
      for {
        optionName <- EitherT.liftF(State.inspect { (ctx: Context) =>
          indexToName(ctx, idx)
        })
        name <- optionName match {
          case Some(name) => name.pure[StateEither]
          case None       => TypeError.format(NotFoundTypeError(info))
        }
        tys = solutions.map(_.t)
        cls = solutions.map(_.cls).flatten
      } yield List(Instantiation(name, TermVar(info, idx, c), tys, cls))
    case _ => Nil.pure
  }

  def distinct(insts: List[Instantiation]): List[Instantiation] =
    insts.distinctBy(inst => (inst.i, inst.tys))

}
