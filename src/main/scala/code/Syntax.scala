package code

import cats.Show
import cats.implicits.*
import core.Types.Type

object Syntax {
  // GRIN AST
  sealed trait Expr

  case class Value(e: String) extends Expr
  case class ClosureValue(
      f: String,
      arity: Int,
      freeVarParameters: List[String],
      binding: LambdaBinding,
      typeKey: String = "", // Type signature for closureMap lookup
      freeVarIsFunction: List[Boolean] = List()
  ) extends Expr
  case class FunctionValue(
      f: String,
      arity: Int,
      ptag: Option[String] =
        None, // Track which P-tag this variable holds (e.g., "P2c18")
      typeKey: String = "" // Type signature for closureMap lookup
  ) extends Expr
  case class PartialFunValue(
      f: String,
      arity: Int,
      numOfAppliedVars: Int = 0,
      typeKey: String = "",
      resolvedType: Option[Type] =
        None // Carry type from TypeChecker/Monomorphization
  ) extends Expr
  case class AppExpr(
      e: String,
      resultArity: Int = 0,
      indent: Int = 1,
      resultPtag: Option[String] = None,
      resultClosures: List[String] = Nil // Closure names for tKey propagation
  ) extends Expr
  case class BindExpr(
      repr: String,
      variable: String,
      values: List[Expr],
      indent: Int = 1
  ) extends Expr
  case class MultiLineExpr(exprs: List[BindExpr], result: Expr) extends Expr
  case class CaseClause(pattern: String, expr: Expr, indent: Int = 2)
      extends Expr
  case class CaseExpr(expr: Expr, cases: List[CaseClause], indent: Int = 1)
      extends Expr
  case class PureExpr(expr: Expr, indent: Int = 1) extends Expr
  case class DoExpr(expr: Expr, indent: Int = 1) extends Expr
  case class Abs(variable: String, expr: Expr) extends Expr

  case class LambdaBinding(name: String, expr: Expr)

  // ArityFact stores explicit arity information per closure/function
  // Following GHC-GRIN pattern of storing arity at definition time
  case class ArityFact(
      functionName: String,
      totalParams: Int, // Total parameters the function expects
      capturedVars: Int, // Number of captured free variables
      returnTypeKey: String, // Return type for grouping apply functions
      paramTypeKeys: List[String] // Per-level param types for grouping
  )

  /** Environment containing all closure-related maps for GRIN code generation.
    * Used as a single implicit parameter instead of multiple scattered maps.
    */
  case class Env(
      closureMap: Map[String, List[String]] = Map.empty,
      arityFactsMap: Map[String, ArityFact] = Map.empty,
      closureTypesFromBind: Map[String, Type] = Map.empty,
      closureTypesFallback: Map[String, Type] = Map.empty,
      // typeInstances maps typeName -> set of trait class names it impls.
      // Built once over the full bind list so forward references work during
      // sequential rendering (trait default method specializations can call
      // impl methods that appear later in the bind list).
      typeInstances: Map[String, Set[String]] = Map.empty,
      // typeInstanceMethods maps (typeName, className) -> set of method names
      // exposed by that instance.
      typeInstanceMethods: Map[(String, String), Set[String]] = Map.empty
  )

  object Env {
    val empty: Env = Env()
  }

  // Helper functions
  def indent(indent: Int, instr: String) = {
    val space = List.fill(indent)(" ").mkString
    val indentedInstr = instr.split("\n").map(i => s"$space$i").mkString("\n")
    instr.endsWith("\n") match {
      case true => s"$indentedInstr\n"
      case _    => indentedInstr
    }
  }

  def indentExpr(indent: Int, e: Expr): Expr = e match {
    case BindExpr(expr, variable, value, _) =>
      BindExpr(expr, variable, value, indent)
    case AppExpr(expr, arity, _, ptag, closures) =>
      AppExpr(expr, arity, indent, ptag, closures)
    case CaseExpr(expr, cases, _) =>
      val t = cases.map(c => indentExpr(indent + 1, c)).map {
        case c: CaseClause => c
      }
      CaseExpr(indentExpr(indent, expr), t, indent)
    case CaseClause(pattern, expr, _) =>
      CaseClause(pattern, expr, indent)
    case MultiLineExpr(exprs, expr) =>
      MultiLineExpr(
        exprs.map(l => BindExpr(l.repr, l.variable, l.values, indent)),
        indentExpr(indent, expr)
      )
    case PureExpr(expr, _) => PureExpr(indentExpr(indent, expr), indent)
    case DoExpr(expr, _)   => DoExpr(expr, indent)
    case _                 => e
  }

  // Show instances
  implicit val showExpr: Show[Expr] =
    Show.show(_ match {
      case Value(e)                              => e
      case FunctionValue(f, _, _, _)             => f
      case ClosureValue(f, _, freeVars, _, _, _) =>
        s"$f ${freeVars.mkString(" ")}"
      case PartialFunValue(f, _, _, _, _) => f
      case AppExpr(e, _, i, _, _)         => indent(i, e)
      case BindExpr(e, _, _, i)           => indent(i, e)
      case MultiLineExpr(exprs, result)   =>
        (exprs.filter(b => !b.repr.isBlank) :+ result)
          .map((_: Expr).show)
          .mkString("\n")
      case CaseClause(pattern, e, i) =>
        val patternLine = indent(i, s"$pattern ->\n")
        val indentedExpr = indentExpr(i + 1, e)
        show"$patternLine$indentedExpr"
      case CaseExpr(expr, cases, i) =>
        val matchLine = expr match {
          case MultiLineExpr(exprs, result) =>
            val prepExpr = exprs
              .filter(!_.repr.isBlank)
              .map(e => indentExpr(i, e).show)
              .mkString("\n")
            val caseOf = indent(i, show"case $result of")
            if (prepExpr.isEmpty) caseOf else s"$prepExpr\n$caseOf"
          case _ => indent(i, show"case $expr of")
        }
        val caseLines = cases.map((_: Expr).show).mkString("\n")
        show"$matchLine\n$caseLines"
      case PureExpr(expr, i) =>
        expr match {
          case Value(e)                => indent(i, s"pure $e")
          case MultiLineExpr(exprs, r) =>
            val pureExpr = (PureExpr(r): Expr)
            (MultiLineExpr(exprs, pureExpr): Expr).show
          case _ => expr.show
        }
      case DoExpr(expr, i) =>
        val doLine = "do\n"
        val iExpr = indentExpr(i + 1, expr)
        show"$doLine$iExpr"
      case Abs(variable, e: Abs) => show"$variable $e".strip()
      case Abs(variable, e)      =>
        // Don't wrap store operations in PureExpr (they're already effects)
        e match {
          case MultiLineExpr(prepExprs, AppExpr(expr, _, _, _, _))
              if expr.trim.startsWith("store ") =>
            val preps = prepExprs
              .filter(!_.repr.isBlank)
              .map(indentExpr(1, _).show)
              .mkString("\n")
            preps.isEmpty match {
              case true  => show"$variable =\n${indent(1, expr)}"
              case false => show"$variable =\n$preps\n${indent(1, expr)}"
            }
          case ClosureValue(f, arity, freeVars, _, _, _) =>
            val tag = GrinUtils.pTag(arity, f)
            val body = freeVars match {
              case Nil  => s"pure ($tag)"
              case args => s"pure ($tag ${args.mkString(" ")})"
            }
            show"$variable =\n${indent(1, body)}"
          case _ =>
            show"$variable =\n${PureExpr(e)}"
        }
    })

  implicit val showLambdaBinding: Show[LambdaBinding] = Show.show(_ match {
    case LambdaBinding(name, a: Abs) => show"$name $a"
    case LambdaBinding(name, e)      => show"$name = $e"
  })
}
