package core

import cats.data.EitherT
import cats.data.State
import cats.implicits.*
import core.Types.*
import core.Bindings.*
import Context.*

object Representation {
  def typeRepresentation(
      types: List[Bind]
  ): Either[String, List[String]] =
    types
      .traverse(bind => {
        val repr = bindingToType(bind.b)
        repr.map2(EitherT.liftF(addName(bind.i))) { case (repr, id) =>
          s"$id: $repr"
        }
      })
      .value
      .runEmptyA
      .value

  def bindingToType(b: Binding): StateEither[String] = b match {
    case TypeVarBind(k, _) =>
      Representation.kindToString(k).pure[StateEither]
    case VarBind(ty) =>
      Context.runE(Representation.typeToString(ty, buildContext = true))
    case TypeAbbBind(_, Some(k)) =>
      Representation.kindToString(k).pure[StateEither]
    case TypeEMarkBind            => "[mark]".pure
    case TypeEFreeBind            => s"[free]".pure
    case TypeESolutionBind(ty, _) =>
      Context
        .runE(Representation.typeToString(ty, buildContext = true))
        .map(s => s"[solution] $s")
    case TermAbbBind(_, Some(ty)) =>
      Context.runE(Representation.typeToString(ty, buildContext = true))
    case _ => b.toString.pure
  }

  def typeToString(
      t: Type,
      buildContext: Boolean = false
  ): StateEither[String] =
    t match {
      case TypeVar(_, idx, n) =>
        EitherT(State.inspect { ctx =>
          Context
            .indexToName(ctx, idx)
            .toRight("Repr: Type variable not found.")
        })
      case TypeEVar(_, n, cls) =>
        val typeBounds = cls match {
          case Nil => ""
          case _   => s": ${cls.map(_.name).mkString(" + ")}"
        }
        s"{unknown}${typeBounds}".pure
      case TypeId(_, id)           => id.pure
      case TypeAny(_)              => "any".pure
      case TypeClass(_, n)         => n.pure
      case TypeAbs(_, typeVar, ty) =>
        for {
          _ <- addNameToContext(typeVar, buildContext)
          ty1 <- Context.runE(typeToString(ty, buildContext))
        } yield s"λ $typeVar. $ty1"
      case TypeArrow(_, t1, t2) =>
        for {
          s1 <- typeToString(t1, buildContext)
          s2 <- typeToString(t2, buildContext)
        } yield s"$s1 -> $s2"
      case TypeAll(_, typeVar, kind, cls, ty) =>
        for {
          _ <- addNameToContext(typeVar, buildContext)
          k1 <- kindToString(kind).pure[StateEither]
          ty1 <- Context.runE(typeToString(ty, buildContext))
          typeBounds = cls match {
            case Nil => ""
            case _   => s": ${cls.map(_.name).mkString(" + ")}"
          }
          sep = ty match {
            case _: TypeAll => ", "
            case _          => " => "
          }
        } yield s"[$typeVar::$k1]$typeBounds$sep$ty1"
      case TypeApp(_, ty1, ty2) =>
        for {
          ty1s <- typeToString(ty1, buildContext)
          ty2s <- typeToString(ty2, buildContext)
        } yield s"$ty1s[$ty2s]"
      case TypeRecord(_, fields) =>
        fields
          .traverse { case (fieldName, ty) =>
            typeToString(ty, buildContext)
              .map(typeString => s"$fieldName: $typeString")
          }
          .map(fieldTypes => s"{${fieldTypes.mkString(", ")}}")
      case TypeVariant(_, fields) =>
        fields
          .traverse { case (fieldName, ty) =>
            typeToString(ty, buildContext)
              .map(typeString => s"$fieldName$typeString")
          }
          .map(fieldTypes => s"${fieldTypes.mkString(" | ")}")
      case TypeRec(_, tyX, _, ty) =>
        for {
          _ <- addNameToContext(tyX.stripPrefix("@"), buildContext)
          ty1 <- typeToString(ty, buildContext)
        } yield ty1
      case TypeUnit(_)   => "Unit".pure[StateEither]
      case TypeInt(_)    => "i32".pure[StateEither]
      case TypeFloat(_)  => "f32".pure[StateEither]
      case TypeString(_) => "str".pure[StateEither]
      case TypeBool(_)   => "bool".pure[StateEither]
    }

  def addNameToContext(name: String, skip: Boolean): StateEither[String] =
    skip match {
      case true  => EitherT.liftF(Context.addName(name))
      case false => "".pure[StateEither]
    }

  def kindToString(k: Kind): String = k match {
    case KindStar          => "*"
    case KindArrow(k1, k2) =>
      s"${kindToString(k1)} -> ${kindToString(k2)}"
  }
}
