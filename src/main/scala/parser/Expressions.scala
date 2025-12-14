package parser

import org.parboiled2.*
import scala.util.Either
import org.parboiled2.Parser

object Expressions {
  import Identifiers.*
  import Types.*
  import Info.*
  import Info.ShowInfo.*

  sealed trait FExpr

  case class FLetExpr(
      info: Info,
      i: FIdentifier,
      t: Option[FType],
      e: Seq[FExpr]
  ) extends FExpr

  case class FBinding(info: Info, i: FIdentifier, t: Option[FType] = None)
  case class FAbs(
      info: Info,
      params: Seq[FBinding],
      t: Option[FType],
      e: Seq[FExpr]
  ) extends FExpr

  sealed trait FPattern
  case class FIdentifierPattern(
      info: Info,
      value: String,
      p: Option[FPattern] = None
  ) extends FPattern
  case class FWildCardPattern(info: Info) extends FPattern
  case class FTuplePattern(info: Info, s: Seq[FPattern]) extends FPattern
  case class FVariantOrRecordPattern(
      info: Info,
      i: FIdentifier,
      s: Seq[FPattern]
  ) extends FPattern
  case class FCase(
      info: Info,
      p: Seq[FPattern],
      guard: Option[FInfixExpr],
      e: Seq[FExpr]
  )
  case class FMatch(info: Info, e: FInfixExpr, c: Seq[FCase]) extends FExpr

  sealed trait FDoAction
  case class FAssign(
      info: Info,
      i: FIdentifier,
      e: Seq[FExpr]
  ) extends FDoAction
  case class FDo(info: Info, a: Seq[FDoAction]) extends FExpr

  sealed trait FInfixExpr extends FExpr with FDoAction
  case class FAddition(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FSubtraction(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FMultiplication(lhs: FInfixExpr, rhs: FInfixExpr)
      extends FInfixExpr
  case class FDivision(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FModulo(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FEquality(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FNotEquality(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FAnd(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FOr(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FLessThan(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FLessThanEqual(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FGreaterThan(lhs: FInfixExpr, rhs: FInfixExpr) extends FInfixExpr
  case class FGreaterThanEqual(lhs: FInfixExpr, rhs: FInfixExpr)
      extends FInfixExpr

  case class FVar(info: Info, value: String) extends FInfixExpr
  case class FProj(info: Info, e: FInfixExpr, ids: Seq[FVar]) extends FInfixExpr
  type FArguments = Option[Seq[FExpr]]
  type FTypeArguments = Option[Seq[FType]]
  // NOTE: This is the actual application, but the arguments can be optional
  // indicating the abstraction accepts a unit.
  case class FApp(
      info: Info,
      e: FExpr,
      typeArguments: FTypeArguments = None,
      args: Seq[FArguments]
  ) extends FInfixExpr
  case class FMethodApp(
      info: Info,
      e: FProj,
      typeArguments: FTypeArguments = None,
      args: Seq[FArguments]
  ) extends FInfixExpr
  case class FAssocApp(
      info: Info,
      t: FVar,
      id: FVar,
      typeArguments: FTypeArguments = None,
      args: Seq[FArguments]
  ) extends FInfixExpr

  // Literals
  sealed trait FLiteral extends FInfixExpr with FPattern
  case class FBool(info: Info, b: Boolean) extends FLiteral
  case class FInt(info: Info, i: Integer) extends FLiteral
  case class FFloat(info: Info, f: Float) extends FLiteral
  case class FString(info: Info, s: String) extends FLiteral
  case class FUnit(info: Info) extends FLiteral

  implicit val showExprInfo: ShowInfo[FExpr] = ShowInfo.info(_ match {
    case FLetExpr(info, _, _, _)     => info
    case FAbs(info, _, _, _)         => info
    case FMatch(info, _, _)          => info
    case FDo(info, _)                => info
    case FVar(info, _)               => info
    case FProj(info, _, _)           => info
    case FApp(info, _, _, _)         => info
    case FMethodApp(info, _, _, _)   => info
    case FAssocApp(info, _, _, _, _) => info
    case FBool(info, _)              => info
    case FInt(info, _)               => info
    case FFloat(info, _)             => info
    case FString(info, _)            => info
    case FUnit(info)                 => info
    case FAddition(lhs, _)           => (lhs: FExpr).info
    case FSubtraction(lhs, _)        => (lhs: FExpr).info
    case FMultiplication(lhs, _)     => (lhs: FExpr).info
    case FDivision(lhs, _)           => (lhs: FExpr).info
    case FModulo(lhs, _)             => (lhs: FExpr).info
    case FEquality(lhs, _)           => (lhs: FExpr).info
    case FNotEquality(lhs, _)        => (lhs: FExpr).info
    case FAnd(lhs, _)                => (lhs: FExpr).info
    case FOr(lhs, _)                 => (lhs: FExpr).info
    case FLessThan(lhs, _)           => (lhs: FExpr).info
    case FLessThanEqual(lhs, _)      => (lhs: FExpr).info
    case FGreaterThan(lhs, _)        => (lhs: FExpr).info
    case FGreaterThanEqual(lhs, _)   => (lhs: FExpr).info
  })

  implicit val showPatternInfo: ShowInfo[FPattern] = ShowInfo.info(_ match {
    case FBool(info, _)                      => info
    case FInt(info, _)                       => info
    case FFloat(info, _)                     => info
    case FString(info, _)                    => info
    case FUnit(info)                         => info
    case FIdentifierPattern(info, _, _)      => info
    case FWildCardPattern(info)              => info
    case FTuplePattern(info, _)              => info
    case FVariantOrRecordPattern(info, _, _) => info
  })
}

abstract class Expressions(fileName: String) extends Types(fileName) {
  import Expressions.*
  import Identifiers.*

  def BlockExpr = oneOrMoreWithIndent(ExprVal, "expression")
  val ExprVal = () => Expr
  def Expr: Rule1[FExpr] = rule {
    LetExpr.named("let expression") |
      LambdaExpr.named("lambda expression") |
      MatchExpr.named("match expression") |
      DoExpr.named("do expression") |
      InfixExpr.named("infix expression")
  }

  def InlineExpr: Rule1[Seq[FExpr]] = {
    def InlineBlockExpr = rule {
      '{' ~ BlockExpr ~ NewLine ~ Indent ~ '}' ~> (
        (e: Seq[FExpr], i: FIndent) => {
          validateIndents(Seq(i), true) ~ push(e) | failX(
            "correctly indented inline block"
          )
        }
      )
    }
    rule {
      LambdaExpr.named("lambda expression") ~> (Seq(_)) |
        InfixExpr.named("infix expression") ~> (Seq(_)) | InlineBlockExpr
    }
  }
  def LetExpr = rule {
    info ~ `let` ~ identifier ~ (`:` ~ `Type`).? ~ `=` ~ InlineExpr ~> FLetExpr.apply
  }
  def LambdaExpr = {
    def Binding = rule {
      info ~ identifier ~ (`:` ~ `Type`).? ~> FBinding.apply
    }
    def Bindings = rule { '(' ~ Binding.*(",") ~ ')' }
    def ReturnType = rule { `->` ~ Type }
    rule {
      info ~ (Bindings | info ~ identifier ~> ((i, id) =>
        Seq(FBinding(i, id))
      )) ~ ReturnType.? ~ `=>` ~ InlineExpr ~> FAbs.apply
    }
  }

  def MatchExpr = {
    def Pattern: Rule1[FPattern] = rule {
      info ~ identifier ~ '(' ~ Patterns ~ ')' ~> FVariantOrRecordPattern.apply |
        info ~ '(' ~ Patterns ~ ')' ~> FTuplePattern.apply |
        Literal.named("literal") |
        info ~ capture('_') ~> ((i, _) => FWildCardPattern(i)) |
        info ~ capture(
          IdentifierPart.named("identifier")
        ) ~ (`@` ~ Pattern).? ~>
        FIdentifierPattern.apply
    }
    def Patterns = rule {
      Pattern.*(',')
    }
    def Guard = rule { `if` ~ InfixExpr }
    def ArmPatterns = rule {
      Pattern.+("|")
    }
    def Case = () =>
      rule {
        info ~ ArmPatterns ~ Guard.? ~ `=>` ~ InlineExpr ~> FCase.apply
      }
    rule {
      info ~ `match` ~ InfixExpr ~ ':' ~ oneOrMoreWithIndent(
        Case,
        "case"
      ) ~> FMatch.apply
    }
  }

  def DoExpr = {
    def Assign = rule {
      info ~ identifier ~ `<-` ~ InlineExpr ~> FAssign.apply
    }
    def Action = () =>
      rule {
        Assign | InfixExpr
      }
    rule {
      info ~ `do` ~ ':' ~ oneOrMoreWithIndent(Action, "action") ~> FDo.apply
    }
  }

  def InfixExpr: Rule1[FInfixExpr] = {
    def SimpleExpr = rule {
      Literal | ExprId | '(' ~ InfixExpr ~ ')'
    }

    def ArgumentExpr = rule { LambdaExpr | InfixExpr }
    def ArgumentList = rule { ArgumentExpr.+(',') }
    def Arguments = rule { '(' ~ ArgumentList.? ~ ')' }
    def TypeArguments = rule { '[' ~ Type.named("type").+(',') ~ ']' }

    def CallExpr = rule {
      info ~ SimpleExpr ~ TypeArguments.named("type arguments").? ~ Arguments
        .named("arguments")
        .* ~> FApp.apply
    }

    def MethodExpr = rule {
      info ~ Proj ~ TypeArguments.named("type arguments").? ~ Arguments
        .named("arguments")
        .+ ~> FMethodApp.apply
    }

    def AssocCallExpr = rule {
      info ~ AssocProj ~ TypeArguments.named("type arguments").? ~ Arguments
        .named("arguments")
        .+ ~> FAssocApp.apply
    }

    def Proj = rule {
      info ~ (CallExpr | SimpleExpr) ~ ('.' ~ ExprId).+ ~> FProj.apply
    }

    def AssocProj = rule {
      ExprId ~ "::" ~ ExprId
    }

    def PrimaryExpr: Rule1[FInfixExpr] = rule {
      MethodExpr | AssocCallExpr | Proj | CallExpr | SimpleExpr
    }

    def MultiplicativeExpr = rule {
      PrimaryExpr ~ (`*` ~ PrimaryExpr ~> FMultiplication.apply |
        `/` ~ PrimaryExpr ~> FDivision.apply |
        `%` ~ PrimaryExpr ~> FModulo.apply).*
    }
    def AdditiveExpr = rule {
      MultiplicativeExpr ~ (`+ ` ~ MultiplicativeExpr ~> FAddition.apply |
        `- ` ~ MultiplicativeExpr ~> FSubtraction.apply).*
    }
    def RelationExpr = rule {
      AdditiveExpr ~ (`<=` ~ AdditiveExpr ~> FLessThanEqual.apply |
        `>=` ~ AdditiveExpr ~> FGreaterThanEqual.apply |
        `<` ~ AdditiveExpr ~> FLessThan.apply |
        `>` ~ AdditiveExpr ~> FGreaterThan.apply).*
    }
    def EqualityExpr = rule {
      RelationExpr ~ (`==` ~ RelationExpr ~> FEquality.apply |
        `!=` ~ RelationExpr ~> FNotEquality.apply).*
    }
    def LogicalAnd = rule {
      EqualityExpr ~ (`&&` ~ EqualityExpr ~> FAnd.apply).*
    }
    def LogicalOr = rule {
      LogicalAnd ~ (`||` ~ LogicalAnd ~> FOr.apply).*
    }

    LogicalOr
  }

  // Literals
  def Literal: Rule1[FLiteral] = rule { Bool | Float | Int | String | unit }
  def unit = rule {
    info ~ "()" ~> FUnit.apply
  }
  def Bool = rule {
    info ~ capture(`true` | `false`) ~> ((i, s) => FBool(i, s.trim().toBoolean))
  }
  def Float = rule {
    info ~ capture(DecimalInteger ~ '.' ~ CharPredicate.Digit.*) ~> ((i, f) =>
      FFloat(i, f.toFloat)
    )
  }
  def DecimalInteger = rule {
    '0' | (CharPredicate.Digit19 ~ CharPredicate.Digit.*)
  }
  def Int = rule {
    info ~ capture(CharPredicate.Digit.+) ~> ((i, integer) =>
      FInt(i, integer.toInt)
    )
  }
  def String = {
    def Raw = rule(!'\"' ~ ANY)
    rule {
      info ~ '"' ~ capture(Raw.*) ~ '"' ~> FString.apply
    }
  }

  private def ExprId = rule {
    info ~ quiet(Spacing.*) ~ capture(!Keyword ~ IdentifierPart) ~> FVar.apply
  }
}
