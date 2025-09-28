package code

import cats.Show
import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Bindings.*
import core.Context
import core.Context.*
import core.Desugar
import core.Terms.*
import core.TypeChecker
import core.Types.*
import parser.Info.UnknownInfo

import GrinUtils.*
import core.TypeChecker.findRootTypeVar
import fuse.SpecializedMethodUtils

object Grin {
  val MainFunction = "grinMain"
  val PartialFunSuffix = "''"

  // Move the Grin AST into a separate object.
  sealed trait Expr

  case class Value(e: String) extends Expr
  case class ClosureValue(
      f: String,
      arity: Int,
      freeVarParameters: List[String],
      binding: LambdaBinding
  ) extends Expr
  case class FunctionValue(
      f: String,
      arity: Int
  ) extends Expr
  case class PartialFunValue(f: String, arity: Int, numOfAppliedVars: Int = 0)
      extends Expr
  case class AppExpr(e: String, resultArity: Int = 0, indent: Int = 1)
      extends Expr
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
    case AppExpr(expr, arity, _) => AppExpr(expr, arity, indent)
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

  implicit val showExpr: Show[Expr] =
    Show.show(_ match {
      case Value(e)                        => e
      case FunctionValue(f, _)             => f
      case ClosureValue(f, _, freeVars, _) => s"$f ${freeVars.mkString(" ")}"
      case AppExpr(e, _, i)                => indent(i, e)
      case BindExpr(e, _, _, i)            => indent(i, e)
      case MultiLineExpr(exprs, result) =>
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
          case Value(e) => indent(i, s"pure $e")
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
      case Abs(variable, e)      => show"$variable =\n${PureExpr(e)}"
    })

  implicit val showLambdaBinding: Show[LambdaBinding] = Show.show(_ match {
    case LambdaBinding(name, a: Abs) => show"$name $a"
    case LambdaBinding(name, e)      => show"$name = $e"
  })

  def generate(bindings: List[Bind]): String = {
    val s = for {
      values <- bindings
        .traverse(bind =>
          for {
            value <- toLambdaBinding(bind)
            id <- Context.addBinding(bind.i, bind.b)
          } yield value
        )
      (lambdaBindings, partialFunctions) = values.flatten.unzip
      applyFunction <- buildApply(partialFunctions.flatten)
    } yield (lambdaBindings.flatten.map(_.show) :+ applyFunction)
      .mkString("\n\n")
    s.runEmptyA.value
  }

  def buildApply(partialFun: List[PartialFunValue]): ContextState[String] =
    partialFun.isEmpty match {
      case false =>
        for {
          funVariable <- addTempVariable()
          funParameter <- addTempVariable()
          caseClauses <- partialFun.traverse(partialFunToCase(_, funParameter))
          caseExpr = CaseExpr(Value(funVariable), caseClauses.flatten)
          abs = Abs(funVariable, Abs(funParameter, caseExpr))
        } yield show"apply $abs"
      case true => "".pure[ContextState]
    }

  def partialFunToCase(
      partialFun: PartialFunValue,
      parameter: String
  ): ContextState[List[CaseClause]] = {
    def iter(arity: Int): ContextState[List[CaseClause]] = arity match {
      case 0 => State.pure(Nil)
      case _ =>
        for {
          acc <- iter(arity - 1)
          variables <- List
            .fill(partialFun.arity - arity + partialFun.numOfAppliedVars)("")
            .traverse(_ => addTempVariable())
          tag = pTag(arity, partialFun.f)
          vars = variables.mkString(" ").strip()
          pattern = s"($tag $vars)"
          expr = arity - 1 match {
            case 0 => AppExpr(s"${partialFun.f} $vars $parameter")
            case v =>
              val lowerArityTag = pTag(v, partialFun.f)
              AppExpr(s"pure ($lowerArityTag $vars $parameter)")
          }
        } yield CaseClause(pattern, expr) :: acc
    }
    iter(partialFun.arity)
  }

  def toLambdaBinding(
      binding: Bind
  ): ContextState[Option[(List[LambdaBinding], List[PartialFunValue])]] =
    binding.b match {
      case TermAbbBind(expr: (TermBuiltin | TermClassMethod), _) =>
        State.pure(None)
      case TermAbbBind(expr: TermTAbs, _) =>
        // Skip uninstantiated generic functions - these are generic functions
        // that were never called with concrete types (e.g., unused Option[A].map)
        // and therefore cannot be code-generated
        State.pure(None)
      case TermAbbBind(expr, _) =>
        pureToExpr(expr).map(e => {
          val partialFun = extractPartialFun(e)
          val closures = lambdaLift(e)
          val lambda = LambdaBinding(nameBind(binding.i), e)
          Some(lambda :: closures, partialFun)
        })
      case _ => State.pure(None)
    }

  def extractPartialFun(e: Expr): List[PartialFunValue] = e match {
    case p: PartialFunValue       => List(p)
    case BindExpr(_, _, exprs, _) => exprs.map(extractPartialFun(_)).flatten
    case DoExpr(e, _)             => extractPartialFun(e)
    case CaseClause(_, e, _)      => extractPartialFun(e)
    case PureExpr(e, _)           => extractPartialFun(e)
    case Abs(_, e)                => extractPartialFun(e)
    case CaseExpr(e, cases, _) =>
      val casesPartialFunctions = cases.map(extractPartialFun(_)).flatten
      extractPartialFun(e) :++ casesPartialFunctions
    case MultiLineExpr(exprs, r) =>
      exprs.map(extractPartialFun(_)).flatten :++ extractPartialFun(r)
    case _ => Nil
  }

  def lambdaLift(e: Expr): List[LambdaBinding] = e match {
    case ClosureValue(_, _, _, binding) => List(binding)
    case BindExpr(_, _, exprs, _)       => exprs.map(lambdaLift(_)).flatten
    case DoExpr(e, _)                   => lambdaLift(e)
    case CaseClause(_, e, _)            => lambdaLift(e)
    case PureExpr(e, _)                 => lambdaLift(e)
    case Abs(_, e)                      => lambdaLift(e)
    case CaseExpr(e, cases, _) =>
      val casesPartialFunctions = cases.map(lambdaLift(_)).flatten
      lambdaLift(e) :++ casesPartialFunctions
    case MultiLineExpr(exprs, r) =>
      exprs.map(lambdaLift(_)).flatten :++ lambdaLift(r)
    case _ => Nil
  }

  def nameBind(name: String): String = name match {
    case b if b.startsWith(Desugar.MethodNamePrefix) =>
      methodToName(b)
    case b if b.startsWith(Desugar.RecordConstrPrefix) =>
      recordConstrToName(b)
    case TypeChecker.MainFunction => MainFunction
    case n                        => n
  }

  def pureToExpr(expr: Term)(implicit
      substFunc: String => String = identity
  ): ContextState[Expr] =
    Context.run(toExpr(expr))

  def toExpr(
      expr: Term
  )(implicit substFunc: String => String): ContextState[Expr] =
    expr match {
      case TermBuiltin(_)       => StateT.pure(Value(""))
      case TermAscribe(_, t, _) => toExpr(t)
      case TermFix(_, body)     => pureToExpr(body)
      case TermAbs(_, variable, variableType, c: TermClosure, _) =>
        Context
          .addBinding("c", VarBind(variableType))
          .flatMap(name => toClosureValue(name, c))
      case c: TermClosure =>
        addTempVariable("c").flatMap(name => toClosureValue(name, c))
      case TermAbs(_, variable, variableType, body, _) =>
        for {
          variable1 <- includeFunctionSuffix(variable, variableType)
          variable2 <- Context.addBinding(variable1, VarBind(variableType))
          b <- pureToExpr(body)
        } yield Abs(toParamVariable(variable2), b)
      case TermApp(_, TermFold(_, ty), r: TermRecord) =>
        for {
          tyD <- typeShiftOnContextDiff(ty)
          typeName <- getNameFromType(tyD)
          recordExpr <- pureToExpr(r)
          (prepExprs, parameters) <- prepParameters(recordExpr)
          constr = show"pure (${cTag(typeName)} $parameters)"
        } yield MultiLineExpr(prepExprs, AppExpr(constr))
      case TermApp(_, value, svalue) =>
        for {
          v <- pureToExpr(value)
          sval <- pureToExpr(svalue)
          (prepExprs1, result) <- getResult(v)
          (prepExprs2, parameter) <- prepParameters(sval)
          prepExprs = prepExprs1 :++ prepExprs2
          app = result match {
            case FunctionValue(f, arity) if isVariablePartialFun(f) =>
              AppExpr(show"apply $result $parameter".strip(), arity - 1)
            case _ =>
              AppExpr(show"$result $parameter".strip())
          }
        } yield MultiLineExpr(prepExprs, app)
      case TermLet(_, variable, t1, t2) =>
        for {
          letValue <- pureToExpr(t1)
          (prepExprs, result) <- getResult(letValue)
          // Get type, using cached type for specialized methods to avoid re-inference
          variableType <- getSpecializedTermType(t1).flatMap(_.fold(typeCheck(t1))(State.pure))
          fVar <- result match {
            case c: ClosureValue =>
              Context.addBinding(
                (c: Expr).show,
                TermAbbBind(t1, Some(variableType))
              )
            case _ => Context.addBinding(variable, VarBind(variableType))
          }
          prepExprs2 = result match {
            case _: ClosureValue =>
              prepExprs :+ BindExpr("", "", List(result))
            case _ =>
              prepExprs :+ BindExpr(
                show"$fVar <- ${PureExpr(result)}".strip(),
                fVar,
                List(result)
              )
          }
          expr <- pureToExpr(t2)
        } yield MultiLineExpr(prepExprs2, expr)
      case TermTag(_, tag, TermUnit(_), _) =>
        State.pure(Value(s"(${cTag(tag)})"))
      case TermTag(_, tag, term, _) =>
        for {
          params <- toExpr(term)
          (prepExprs, values) <- prepParameters(params)
          constr = show"(${cTag(tag)} $values)"
        } yield MultiLineExpr(prepExprs, Value(constr))
      case TermRecord(_, fields) =>
        fields
          .traverse { case (_, term) =>
            for {
              e <- toExpr(term)
              isNode <- isNodeValue(term)
              v: Tuple2[Option[BindExpr], Expr] <- isNode match {
                case true =>
                  addTempVariable().map(p =>
                    (
                      Some(BindExpr(show"$p <- store $e", p, List(e))),
                      Value(p): Expr
                    )
                  )
                case false =>
                  State.pure[Context, Tuple2[Option[BindExpr], Expr]]((None, e))
              }
            } yield v
          }
          .map(exprs => {
            val prepExprs = exprs.map(_._1).flatten
            MultiLineExpr(
              prepExprs,
              Value(exprs.map { case (_, p) => p.show }.mkString(" "))
            )
          })
      case TermProj(_, t, label) =>
        for {
          e <- pureToExpr(t)
          ty <- typeCheck(t)
          tyD <- typeShiftOnContextDiff(ty)
          typeName <- getNameFromType(tyD)
          tyS <- TypeChecker.simplifyType(tyD)
          (variables, labelVariable) <- tyS match {
            case TypeRec(_, _, _, TypeRecord(_, fields)) =>
              fields
                .traverse(_ => addTempVariable())
                .map(v =>
                  (
                    v.mkString(" "),
                    // TODO: We probably need to fetch a node value here.
                    v.get(fields.indexWhere { case (f, _) => f == label }).get
                  )
                )
            case _ =>
              throw new RuntimeException(
                s"failed to map type on field access for grin generation $tyS; $typeName"
              )
          }
          doExpr: Expr = DoExpr(
            CaseExpr(
              e,
              List(
                CaseClause(
                  s"(${cTag(typeName)} $variables)",
                  PureExpr(Value(labelVariable))
                )
              )
            )
          )
          bindVar <- addTempVariable()
        } yield MultiLineExpr(
          List(BindExpr(show"$bindVar <- $doExpr", bindVar, List(doExpr))),
          Value(bindVar)
        )
      case TermMatch(_, e, patterns) =>
        for {
          ty1 <- typeCheck(e)
          tyT1D <- typeShiftOnContextDiff(ty1)
          exprType <- TypeChecker.unfoldType(tyT1D)
          t <- pureToExpr(e)
          (prepExprs, result) <- prepParameters(t)
          p <- patterns.traverse { case (p, e) =>
            Context.run(toCaseClause(p, e, exprType))
          }
        } yield CaseExpr(MultiLineExpr(prepExprs, Value(result)), p)
      case TermMethodProj(_, t, method) =>
        method match {
          case m if SpecializedMethodUtils.isSpecializedMethod(m) =>
            // For specialized methods, return as FunctionValue to preserve type information
            State.pure(FunctionValue(s"${methodToName(m)}", 2))
          case m =>
            // Not specialized - need type checking and method ID computation
            for {
              binding <- t match {
                case TermVar(_, idx, _) =>
                  toContextState(Context.getBinding(UnknownInfo, idx))
                    .map(b => Some(b))
                case _ => State.pure(None)
              }
              tyT1 <- typeCheck(t)
              tyT1D <- typeShiftOnContextDiff(tyT1)
              tyT1S <- TypeChecker.simplifyType(tyT1D)
              typeName <- getNameFromType(tyT1D)
              instances <- getTypeInstances(typeName)
              cls <- instances.findM(c =>
                getTypeClassMethods(c.name).map(_.exists(_ == m))
              )
              f = cls match {
                case Some(value) =>
                  Desugar.toTypeInstanceMethodID(m, typeName, value.name)
                case None => Desugar.toMethodID(m, typeName)
              }
            } yield Value(s"${methodToName(f)}")
        }
      case TermFold(_, _)   => StateT.pure(Value("pure "))
      case TermInt(_, i)    => StateT.pure(Value(i.toString))
      case TermFloat(_, f)  => StateT.pure(Value(f.toString))
      case TermString(_, s) => StateT.pure(Value(s"""#"$s""""))
      case TermTrue(_)      => StateT.pure(Value("#True"))
      case TermFalse(_)     => StateT.pure(Value("#False"))
      // NOTE: There's an issue with grin llvm code generation, where
      // an error would be thrown if we use the grin `()` unit value
      // for return in functions. Thus we use the integer `0` instead.
      case TermUnit(_) => State.pure(Value("0"))
      case TermTAbs(_, _, _, _) =>
        // Uninstantiated generic functions should not reach here during normal compilation
        // This can happen when a generic function is defined but never used with concrete types
        // Return a placeholder to avoid match errors during processing
        State.pure(Value("#UNINSTANTIATED_GENERIC"))
      case TermAssocProj(_, _, method) =>
        // Associated function projections (like Option::to_str when used as a value)
        State.pure(Value(s"${methodToName(method)}"))
      case TermVar(info, idx, ctxLen) =>
        for {
          variable <- toVariable(idx)
          sVariable = substFunc(variable)
          binding <- toContextState(Context.getBinding(UnknownInfo, idx))
          expr <- (sVariable, binding) match {
            case (v, VarBind(ty)) =>
              isFunctionType(ty).map(_ match {
                case false => Value(v)
                case true  => FunctionValue(v, getFunctionArity(ty))
              })
            case (v, TermAbbBind(t, Some(ty))) =>
              FunctionValue(v, getFunctionArity(ty)).pure[ContextState]
            case (v, _) =>
              Value(v).pure[ContextState]
          }
        } yield expr
    }

  def toClosureValue(name: String, c: TermClosure): ContextState[Expr] = for {
    substVars <- freeVars(c).map(_.filter(_._1 != name))
    (freeVars, tempVars) = substVars.unzip
    substFunc = (variable: String) => {
      name == variable match {
        case false =>
          substVars.find(_._1 == variable).map(_._2).getOrElse(variable)
        case true => s"$name ${tempVars.mkString(" ")}"
      }
    }
    expr <- Context.run(toClosureAbs(c)(substFunc))
    closure = tempVars.foldLeft(expr) { (term, v) => Abs(v, term) }
    ty <- typeCheck(c)
    arity = getFunctionArity(ty)
  } yield ClosureValue(name, arity, freeVars, LambdaBinding(name, closure))

  def toClosureAbs(
      closure: Term
  )(implicit substFunc: String => String): ContextState[Expr] =
    closure match {
      case TermClosure(_, variable, Some(variableType), body) =>
        for {
          variable1 <- includeFunctionSuffix(variable, variableType)
          variable2 <- Context.addBinding(variable1, VarBind(variableType))
          b <- toClosureAbs(body)
        } yield Abs(toParamVariable(variable2), b)
      case TermClosure(_, variable, None, body) =>
        // TODO: Remove this simplification of variable type for closure,
        // once all term closures are populated with var types during
        // monomorphization phase. This logic is primarly used to test
        // building inline lambdas without type annotations.
        val variableType = TypeUnit(UnknownInfo)
        for {
          variable1 <- includeFunctionSuffix(variable, variableType)
          variable2 <- Context.addBinding(variable1, VarBind(variableType))
          b <- toClosureAbs(body)
        } yield Abs(toParamVariable(variable2), b)
      case t => pureToExpr(t)(substFunc)
    }

  def toCaseClause(
      p: Pattern,
      e: Term,
      matchExprType: Type
  )(implicit substFunc: String => String): ContextState[CaseClause] = p match {
    case PatternNode(_, node, vars) =>
      for {
        (_, bindVariables, _) <- toContextState(
          TypeChecker.inferPattern(p, matchExprType)(checking = false)
        )
        cpat = s"(${cTag(node)} ${bindVariables.mkString(" ")})"
        caseClause <- buildCaseClause(cpat, e)
      } yield caseClause
    case PatternDefault(_) => buildCaseClause("#default", e)
    case t: Term => pureToExpr(t).flatMap(cpat => buildCaseClause(cpat.show, e))
  }

  def buildCaseClause(cpat: String, t: Term)(implicit
      substFunc: String => String
  ): ContextState[CaseClause] = for {
    caseExpr <- pureToExpr(t)
    (prepExprs, parameter) <- prepParameters(caseExpr)
  } yield CaseClause(
    cpat,
    MultiLineExpr(prepExprs, PureExpr(Value(parameter)))
  )

  def prepParameters(
      expr: Expr
  ): ContextState[Tuple2[List[BindExpr], String]] =
    expr match {
      case b @ BindExpr(e, v, _, _) =>
        ((List(b), v)).pure[ContextState]
      case MultiLineExpr(exprs, r) =>
        prepParameters(r).map { case (prepExprs, result) =>
          (exprs :++ prepExprs, result)
        }
      case c @ AppExpr(e, _, i) =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- $e", p, List(c), i))
        )
      case v @ FunctionValue(f, 0) =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- $f", p, List(v)))
        )
      case FunctionValue(f, arity) if !isVariablePartialFun(f) =>
        addTempVariable().flatMap(p =>
          prepParameters(
            BindExpr(
              s"$p <- pure (${pTag(arity, f)})",
              p,
              List(PartialFunValue(f, arity))
            )
          )
        )
      case FunctionValue(f, _) => State.pure(List(), f)
      case c @ ClosureValue(f, arity, freeVars, _) =>
        addTempVariable().flatMap(p =>
          prepParameters(
            BindExpr(
              s"$p <- pure (${pTag(arity, f)} ${freeVars.mkString(" ")})",
              p,
              List(c, PartialFunValue(f, arity, freeVars.length))
            )
          )
        )
      case c: CaseExpr =>
        addTempVariable().flatMap(p => {
          val doExpr = DoExpr(c, c.indent)
          prepParameters(
            BindExpr(show"$p <- $doExpr", p, List(doExpr))
          )
        })
      case v @ Value(s) if s.contains("'") =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- fetch $s", p, List(v)))
        )
      case Value(v) => State.pure(List(), v)
    }

  def getResult(
      expr: Expr
  ): ContextState[Tuple2[List[BindExpr], Expr]] = expr match {
    case l: BindExpr => State.pure(List(l), Value(l.variable))
    case MultiLineExpr(exprs, r) =>
      getResult(r).map { case (prepExprs, v) =>
        (exprs :++ prepExprs, v)
      }
    case i: Value        => State.pure(List(), i)
    case c: ClosureValue => State.pure(List(), c)
    case v @ FunctionValue(f, 0) =>
      addTempVariable().flatMap(p =>
        getResult(BindExpr(s"$p <- $f", p, List(v)))
      )
    case f: FunctionValue => State.pure(List(), f)
    case a @ AppExpr(e, arity, _) if arity >= 1 =>
      addTempVariable().map(p => {
        val f = toPartialFunVariable(p)
        (
          List(BindExpr(show"$f <- $e", f, List(a))),
          FunctionValue(s"$f", arity)
        )
      })
    case e: AppExpr  => State.pure(List(), e)
    case c: CaseExpr => State.pure(List(), DoExpr(c, c.indent))
  }

  def toParamVariable(v: String): String = v match {
    case WildcardName                                            => ""
    case s if s.startsWith(Desugar.RecursiveFunctionParamPrefix) => ""
    case _                                                       => v
  }

  def includeFunctionSuffix(v: String, ty: Type): ContextState[String] =
    isFunctionType(ty).map(_ match {
      case true
          if !v.isBlank && !v
            .startsWith(Desugar.RecursiveFunctionParamPrefix) =>
        toPartialFunVariable(v)
      case _ => v
    })

  def isVariablePartialFun(v: String): Boolean =
    v.contains(PartialFunSuffix) && !v.contains(" ")

  def toPartialFunVariable(p: String): String = s"$p$PartialFunSuffix"

  def toVariable(idx: Integer): ContextState[String] =
    getNameFromIndex(idx).map(_ match {
      case PrimOp(ty, op) => s"_prim_${ty}_${op}"
      case "print"        => "_prim_string_print"
      case "int_to_str"   => "_prim_int_str"
      case v if v.startsWith(Desugar.RecursiveFunctionParamPrefix) =>
        v.stripPrefix(Desugar.RecursiveFunctionParamPrefix)
      case v if v.startsWith(Desugar.MethodNamePrefix) => methodToName(v)
      case v if v.startsWith(Desugar.RecordConstrPrefix) =>
        recordConstrToName(v)
      case s => s
    })

  def methodToName(n: String): String =
    s"${n.filter(_.isLetterOrDigit)}'"

  def recordConstrToName(n: String): String =
    n.stripPrefix(Desugar.RecordConstrPrefix)
}
