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
      binding: LambdaBinding,
      typeKey: String = "" // Type signature for closureMap lookup
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
      typeKey: String = ""
  ) extends Expr
  case class AppExpr(
      e: String,
      resultArity: Int = 0,
      indent: Int = 1,
      resultPtag: Option[String] = None
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
    case AppExpr(expr, arity, _, ptag) => AppExpr(expr, arity, indent, ptag)
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
      case Value(e)                           => e
      case FunctionValue(f, _, _, _)          => f
      case ClosureValue(f, _, freeVars, _, _) => s"$f ${freeVars.mkString(" ")}"
      case PartialFunValue(f, _, _, _)        => f
      case AppExpr(e, _, i, _)                => indent(i, e)
      case BindExpr(e, _, _, i)               => indent(i, e)
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
      case Abs(variable, e)      =>
        // Don't wrap store operations in PureExpr (they're already effects)
        e match {
          case MultiLineExpr(prepExprs, AppExpr(expr, _, _, _))
              if expr.trim.startsWith("store ") =>
            val preps = prepExprs
              .filter(!_.repr.isBlank)
              .map(indentExpr(1, _).show)
              .mkString("\n")
            if (preps.isEmpty) show"$variable =\n${indent(1, expr)}"
            else show"$variable =\n$preps\n${indent(1, expr)}"
          case _ =>
            show"$variable =\n${PureExpr(e)}"
        }
    })

  implicit val showLambdaBinding: Show[LambdaBinding] = Show.show(_ match {
    case LambdaBinding(name, a: Abs) => show"$name $a"
    case LambdaBinding(name, e)      => show"$name = $e"
  })

  def generateMissingFFI(grinCode: String): String = {
    val missingOps = List(
      ("_prim_int_mod", "_prim_int_mod :: T_Int64 -> T_Int64 -> T_Int64"),
      ("_prim_float_mod", "_prim_float_mod :: T_Float -> T_Float -> T_Float"),
      ("_prim_bool_and", "_prim_bool_and :: T_Bool -> T_Bool -> T_Bool"),
      ("_prim_bool_or", "_prim_bool_or :: T_Bool -> T_Bool -> T_Bool"),
      ("_prim_string_ne", "_prim_string_ne :: T_String -> T_String -> T_Int64")
    )

    val usedOps = missingOps.filter { case (opName, _) =>
      grinCode.contains(opName)
    }

    if (usedOps.isEmpty) ""
    else {
      val declarations = usedOps.map(_._2).mkString("\n  ")
      s"ffi pure\n  $declarations"
    }
  }

  // Two-pass code generation.
  // Pass 1: Collect all partial functions to build type-to-closure map
  // Pass 2: Generate code with P-tag inference
  def generate(bindings: List[Bind]): String = {
    // Pass 1: Collect partial functions (using empty maps - no P-tag inference)
    val emptyClosureMap: Map[String, List[String]] = Map.empty
    val emptyArityMap: Map[Int, List[String]] = Map.empty
    val collectPass = for {
      values <- bindings.traverse(bind =>
        for {
          value <- toLambdaBinding(bind)(emptyClosureMap, emptyArityMap)
          _ <- Context.addBinding(bind.i, bind.b)
        } yield value
      )
    } yield values.flatten.flatMap(_._2)

    val (ctx1, partialFunctions) = collectPass.run(Context.emptyContext).value

    // Build type-to-closure map for P-tag inference
    // Map from typeKey -> list of closure names with that type signature
    // This allows distinguishing closures with same arity but different types
    val typeToClosures: Map[String, List[String]] = partialFunctions
      .filter(_.typeKey.nonEmpty)
      .groupBy(_.typeKey)
      .map { case (typeKey, pfs) => (typeKey, pfs.map(_.f).distinct) }

    // Build arity-to-closure map for fallback when type matching fails
    // Map from arity -> list of closure names with that arity
    val arityToClosures: Map[Int, List[String]] = partialFunctions
      .groupBy(_.arity)
      .map { case (arity, pfs) => (arity, pfs.map(_.f).distinct) }

    // Pass 2: Generate code with P-tag inference
    // IMPORTANT: Start from ctx1 (context after Pass 1) to ensure consistent temp variable names
    // Pass both typeToClosures (primary) and arityToClosures (fallback) for inference
    implicit val closureMap: Map[String, List[String]] = typeToClosures
    implicit val arityMap: Map[Int, List[String]] = arityToClosures

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
    } yield {
      val grinCode = (lambdaBindings.flatten.map(_.show) :+ applyFunction)
        .mkString("\n\n")
        .replaceAll(
          "([a-zA-Z0-9])#",
          "$1'"
        ) // Replace # with ' in type specializations only
      val ffi = generateMissingFFI(grinCode)
      if (ffi.isEmpty) grinCode else s"$ffi\n\n$grinCode"
    }
    // Use empty context - both passes should generate same variable names when starting fresh
    s.runEmptyA.value
  }

  // Generate specialized apply functions for each P-tag (e.g., apply_P2c18, apply_P1c18)
  // Each specialized apply handles only ONE P-tag, avoiding HPT mixed return type issues
  def buildApply(partialFun: List[PartialFunValue]): ContextState[String] =
    partialFun.isEmpty match {
      case false =>
        for {
          // Generate specialized apply for each P-tag at each arity level
          specializedApplies <- partialFun
            .flatMap { pf =>
              (1 to pf.arity).map(arity => (pf, arity))
            }
            .traverse { case (pf, arity) =>
              buildSpecializedApply(pf, arity)
            }
        } yield specializedApplies.mkString("\n\n")
      case true => "".pure[ContextState]
    }

  // Build a specialized apply function for a single P-tag at a specific arity
  def buildSpecializedApply(
      partialFun: PartialFunValue,
      arity: Int
  ): ContextState[String] =
    for {
      funVariable <- addTempVariable()
      funParameter <- addTempVariable()
      caseClause <- buildApplyCase(partialFun, arity, funParameter)
      tag = pTag(arity, partialFun.f)
      caseExpr = CaseExpr(Value(funVariable), List(caseClause))
      abs = Abs(funVariable, Abs(funParameter, caseExpr))
    } yield show"apply_$tag $abs"

  // Build a single case clause for a specialized apply function
  def buildApplyCase(
      partialFun: PartialFunValue,
      arity: Int,
      parameter: String
  ): ContextState[CaseClause] =
    for {
      variables <- List
        .fill(partialFun.arity - arity + partialFun.numOfAppliedVars)("")
        .traverse(_ => addTempVariable())
      tag = pTag(arity, partialFun.f)
      vars = variables.mkString(" ").strip()
      pattern = s"($tag $vars)"
      expr = (arity - 1) match {
        case 0 => AppExpr(s"${partialFun.f} $vars $parameter")
        case v =>
          val lowerArityTag = pTag(v, partialFun.f)
          AppExpr(s"pure ($lowerArityTag $vars $parameter)")
      }
    } yield CaseClause(pattern, expr)

  def toLambdaBinding(
      binding: Bind
  )(implicit
      closureMap: Map[String, List[String]] = Map.empty,
      arityMap: Map[Int, List[String]] = Map.empty
  ): ContextState[Option[(List[LambdaBinding], List[PartialFunValue])]] = {
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
          val lambdaName = nameBind(binding.i)
          val lambda = LambdaBinding(lambdaName, e)
          Some(lambda :: closures, partialFun)
        })
      case _ => State.pure(None)
    }
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
    case ClosureValue(_, _, _, binding, _) => List(binding)
    case BindExpr(_, _, exprs, _)          => exprs.map(lambdaLift(_)).flatten
    case DoExpr(e, _)                      => lambdaLift(e)
    case CaseClause(_, e, _)               => lambdaLift(e)
    case PureExpr(e, _)                    => lambdaLift(e)
    case Abs(_, e)                         => lambdaLift(e)
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

  /** Add fetch operation if value is heap-allocated, otherwise pass through.
    * This ensures values stored on the heap are fetched before use.
    */
  def addFetchIfNeeded(
      prepExprs: List[BindExpr],
      result: String,
      isHeapAllocated: Boolean
  ): ContextState[(List[BindExpr], String)] = {
    if (isHeapAllocated) {
      addTempVariable().map(fetchVar =>
        (
          prepExprs :+ BindExpr(
            show"$fetchVar <- fetch $result",
            fetchVar,
            List(Value(result))
          ),
          fetchVar
        )
      )
    } else {
      State.pure((prepExprs, result))
    }
  }

  def pureToExpr(expr: Term)(implicit
      substFunc: String => String = identity,
      closureMap: Map[String, List[String]] = Map.empty,
      arityMap: Map[Int, List[String]] = Map.empty
  ): ContextState[Expr] =
    Context.run(toExpr(expr))

  def toExpr(
      expr: Term
  )(implicit
      substFunc: String => String,
      closureMap: Map[String, List[String]] = Map.empty,
      arityMap: Map[Int, List[String]] = Map.empty
  ): ContextState[Expr] =
    expr match {
      case TermBuiltin(_)       => StateT.pure(Value(""))
      case TermAscribe(_, t, _) => toExpr(t)
      case TermFix(_, body)     => pureToExpr(body)
      case TermAbs(_, variable, variableType, c: TermClosure, _) =>
        Context
          .addBinding("c", VarBind(variableType))
          .flatMap(name => toClosureValue(name, name, c))
      case c: TermClosure =>
        addTempVariable("c").flatMap(name => toClosureValue(name, name, c))
      case TermAbs(_, variable, variableType, body, _) =>
        for {
          variable1 <- includeFunctionSuffix(variable, variableType)
          variable2 <- Context.addBinding(variable1, VarBind(variableType))
          b <- pureToExpr(body)
        } yield Abs(toParamVariable(variable2), b)
      case TermApp(_, TermFold(_, ty), tag: TermTag) =>
        // Constructor application with TermFold - use store for heap allocation
        for {
          tyD <- typeShiftOnContextDiff(ty)
          typeName <- getNameFromType(tyD)
          // Handle empty constructors (TermUnit should not generate parameters)
          result <- tag.t match {
            case TermUnit(_) =>
              // Empty constructor like Nil - no parameters
              val constr = show"store (${cTag(tag.i)})"
              State.pure(MultiLineExpr(List(), AppExpr(constr)))
            case _ =>
              // Constructor with fields
              for {
                tagExpr <- pureToExpr(tag.t)
                (prepExprs, parameters) <- prepParameters(tagExpr)
                constr = show"store (${cTag(tag.i)} $parameters)"
              } yield MultiLineExpr(prepExprs, AppExpr(constr))
          }
        } yield result
      case TermApp(_, TermFold(_, ty), r: TermRecord) =>
        // Handle record type constructors (e.g., Tweet, X[T], Point[T,V], Tuple[A,B])
        for {
          tyD <- typeShiftOnContextDiff(ty)
          typeName <- getNameFromType(tyD)
          recordExpr <- pureToExpr(r)
          (prepExprs, parameters) <- prepParameters(recordExpr)
          constr = show"store (C${typeName} $parameters)"
        } yield MultiLineExpr(prepExprs, AppExpr(constr))
      case TermApp(_, value, svalue) =>
        for {
          v <- pureToExpr(value)
          sval <- pureToExpr(svalue)
          (prepExprs1, result) <- getResult(v)
          (prepExprs2, parameter) <- prepParameters(sval)
          prepExprs = prepExprs1 :++ prepExprs2
          app = result match {
            case FunctionValue(f, arity, ptag, tKey)
                if isVariablePartialFun(f) =>
              val resolvedTag = ptag.orElse(
                lookupSingleClosure(tKey, arity).map(pTag(arity, _))
              )
              resolvedTag match {
                case Some(tag) =>
                  makeApplyCall(tag, result.show, parameter, arity)
                case None =>
                  lookupClosures(tKey, arity) match {
                    case cs if cs.nonEmpty =>
                      makeDispatch(f, cs, arity, parameter)
                    case _ =>
                      AppExpr(s"apply $result $parameter", arity - 1, 1, None)
                  }
              }
            case _ =>
              AppExpr(show"$result $parameter".strip())
          }
        } yield MultiLineExpr(prepExprs, app)
      case TermLet(_, variable, t1, t2) =>
        // Special handling for TermFix(TermClosure): use original variable name instead of temp name
        // This makes recursive calls work naturally (e.g., "iter" instead of "c7")
        t1 match {
          case TermFix(_, c: TermClosure) =>
            for {
              // Generate unique numbered name for closure to prevent collisions with top-level functions
              counter <- addTempVariable().map(_.filter(_.isDigit))
              closureName = s"$variable$counter" // e.g., "iter" -> "iter7"
              // Use original variable name for matching self-references, numbered name for output
              letValue <- toClosureValue(variable, closureName, c)
              (prepExprs, result) <- getResult(letValue)
              // Get type, extracting from closure structure to avoid re-inference
              closureType <- GrinUtils.getClosureType(c)
              specializedType <- getSpecializedTermType(t1)
              variableTypeRaw <- specializedType
                .orElse(closureType)
                .fold(typeCheck(t1))(State.pure)
              variableType <- typeShiftOnContextDiff(variableTypeRaw)
              // Store the unwrapped TermClosure (not TermFix) to avoid infinite recursion when referenced
              // Use numbered name to prevent collision with top-level functions
              fVar <- Context.addBinding(
                closureName,
                TermAbbBind(c, Some(variableType))
              )
              prepExprs2 = prepExprs :+ BindExpr("", "", List(result))
              expr <- pureToExpr(t2)
            } yield MultiLineExpr(prepExprs2, expr)
          case _ =>
            // Regular TermLet processing
            for {
              letValue <- pureToExpr(t1)
              (prepExprs, result) <- getResult(letValue)
              // Get type, using cached type for specialized methods to avoid re-inference
              specializedType <- getSpecializedTermType(t1)
              // For closures, try to extract type from structure without type-checking
              closureType <- t1 match {
                case c: TermClosure => GrinUtils.getClosureType(c)
                case TermFix(_, c)  => GrinUtils.getClosureType(c)
                case _              => State.pure(None)
              }
              variableTypeRaw <- specializedType
                .orElse(closureType)
                .fold(typeCheck(t1))(State.pure)
              variableType <- typeShiftOnContextDiff(variableTypeRaw)
              fVar <- Context.addBinding(variable, VarBind(variableType))
              prepExprs2 = prepExprs :+ BindExpr(
                show"$fVar <- ${PureExpr(result)}".strip(),
                fVar,
                List(result)
              )
              expr <- pureToExpr(t2)
            } yield MultiLineExpr(prepExprs2, expr)
        }
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
          exprType <- TypeChecker.unfoldType(tyD)
          typeName <- getNameFromType(tyD)
          tyS <- TypeChecker.simplifyType(tyD)
          (prepExprs, result) <- prepParameters(e)
          // Add fetch operation if accessing heap-allocated value
          isNode = isHeapAllocatedType(exprType)
          (finalPrepExprs, finalResult) <- addFetchIfNeeded(
            prepExprs,
            result,
            isNode
          )
          (variables, labelVariable, fieldType) <- tyS match {
            case TypeRec(_, _, _, TypeRecord(_, fields)) =>
              val fieldIndex = fields.indexWhere { case (f, _) => f == label }
              fields
                .traverse(_ => addTempVariable())
                .map(v =>
                  (
                    v.mkString(" "),
                    v.get(fieldIndex).get,
                    fields(fieldIndex)._2
                  )
                )
            case _ =>
              throw new RuntimeException(
                s"failed to map type on field access for grin generation $tyS; $typeName"
              )
          }
          isFunctionField <- isFunctionType(fieldType)
          result2 <- isFunctionField match {
            case true =>
              // For function fields, return as FunctionValue (partial application)
              // The extracted value is a partial application tag like (P1c24)
              // which needs to be called via the 'apply' function
              // Mark variable with PartialFunSuffix so TermApp knows to use 'apply'
              val arity = getFunctionArity(fieldType)
              for {
                bindVarRaw <- addTempVariable()
                bindVar = toPartialFunVariable(bindVarRaw) // Add '' suffix
                caseExpr = DoExpr(
                  CaseExpr(
                    Value(finalResult),
                    List(
                      CaseClause(
                        s"(${cTag(typeName)} $variables)",
                        PureExpr(Value(labelVariable))
                      )
                    )
                  )
                )
              } yield MultiLineExpr(
                finalPrepExprs :+ BindExpr(
                  show"$bindVar <- $caseExpr",
                  bindVar,
                  List(caseExpr)
                ),
                FunctionValue(bindVar, arity)
              )
            case false =>
              // For non-function fields, use original do/pure wrapping
              for {
                bindVar <- addTempVariable()
                caseExpr = DoExpr(
                  CaseExpr(
                    Value(finalResult),
                    List(
                      CaseClause(
                        s"(${cTag(typeName)} $variables)",
                        PureExpr(Value(labelVariable))
                      )
                    )
                  )
                )
              } yield MultiLineExpr(
                finalPrepExprs :+ BindExpr(
                  show"$bindVar <- $caseExpr",
                  bindVar,
                  List(caseExpr)
                ),
                Value(bindVar)
              )
          }
        } yield result2
      case TermMatch(_, e, patterns) =>
        for {
          ty1 <- typeCheck(e)
          tyT1D <- typeShiftOnContextDiff(ty1)
          exprType <- TypeChecker.unfoldType(tyT1D)
          t <- pureToExpr(e)
          (prepExprs, result) <- prepParameters(t)
          // Add fetch operation if matching on heap-allocated value
          isNode = isHeapAllocatedType(exprType)
          (finalPrepExprs, finalResult) <- addFetchIfNeeded(
            prepExprs,
            result,
            isNode
          )
          p <- patterns.zipWithIndex.traverse { case ((p, e), idx) =>
            Context.run(
              toCaseClause(
                p,
                e,
                exprType,
                isLastPattern = idx == patterns.length - 1
              )
            )
          }
        } yield CaseExpr(MultiLineExpr(finalPrepExprs, Value(finalResult)), p)
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
      case TermUnit(_)          => State.pure(Value("0"))
      case TermTAbs(_, _, _, _) =>
        // Uninstantiated generic functions should not reach here during normal compilation
        // This can happen when a generic function is defined but never used with concrete types
        // Return a placeholder to avoid match errors during processing
        State.pure(Value("#UNINSTANTIATED_GENERIC"))
      case TermTApp(_, t, _) =>
        // Type applications like Nil[B] reach here as TermTApp nodes
        // Process the underlying term (the constructor/variable)
        toExpr(t)
      case TermAssocProj(_, ty, method) =>
        // Associated function projections (like List::foldRight when used as a value)
        method match {
          case m if SpecializedMethodUtils.isSpecializedMethod(m) =>
            // Specialized methods already have full type info in name
            State.pure(Value(s"${methodToName(m)}"))
          case m =>
            // Non-specialized methods need type name added
            for {
              typeName <- getNameFromType(ty)
              methodID = Desugar.toMethodID(m, typeName)
            } yield Value(s"${methodToName(methodID)}")
        }
      case TermVar(info, idx, ctxLen) =>
        for {
          variable <- toVariable(idx)
          sVariable = substFunc(variable)
          binding <- toContextState(Context.getBinding(UnknownInfo, idx))
          expr <- (sVariable, binding) match {
            case (v, VarBind(ty)) =>
              isFunctionType(ty).map {
                case false => Value(v)
                case true =>
                  val arity = getFunctionArity(ty)
                  val tKey = typeToKey(ty)
                  val inferredPtag =
                    lookupSingleClosure(tKey, arity).map(pTag(arity, _))
                  FunctionValue(v, arity, inferredPtag, tKey)
              }
            case (v, TermAbbBind(c: TermClosure, Some(ty))) =>
              // When referencing a TermClosure binding (from TermFix in TermLet),
              // extract free variables and create ClosureValue for partial application
              GrinUtils.freeVars(c).map { fvs =>
                val freeVars = fvs.filter(_._1 != v).map(_._1)
                val arity = GrinUtils.getClosureArity(c)
                if (freeVars.isEmpty) {
                  FunctionValue(v, arity)
                } else {
                  // Dummy lambda is safe: ClosureValue only used to extract name/freeVars in TermApp (not lifted)
                  ClosureValue(
                    v,
                    arity,
                    freeVars,
                    LambdaBinding(v, DoExpr(Value(""), 0))
                  )
                }
              }
            case (v, TermAbbBind(t, Some(ty))) =>
              FunctionValue(v, getFunctionArity(ty)).pure[ContextState]
            case (v, _) =>
              Value(v).pure[ContextState]
          }
        } yield expr
    }

  // Helper: Extract closure name from P-tag (e.g., "P2c18" -> "c18")
  def closureFromTag(tag: String): String =
    tag.dropWhile(c => c == 'P' || c.isDigit)

  // Helper: Calculate next P-tag after one application
  def nextPtag(tag: String, arity: Int): Option[String] =
    (arity - 1) match {
      case 0 => None
      case n => Some(pTag(n, closureFromTag(tag)))
    }

  // Helper: Build specialized apply call
  def makeApplyCall(
      tag: String,
      result: String,
      param: String,
      arity: Int
  ): AppExpr =
    AppExpr(s"apply_$tag $result $param", arity - 1, 1, nextPtag(tag, arity))

  // Helper: Build inline dispatch for multiple closures
  def makeDispatch(
      f: String,
      closures: List[String],
      arity: Int,
      param: String
  ): Expr = {
    val clauses = closures.map { cn =>
      val tag = pTag(arity, cn)
      val call = (arity - 1) match {
        case 0 => s"$cn $param"
        case n => s"pure (${pTag(n, cn)} $param)"
      }
      CaseClause(s"($tag)", AppExpr(call), 1)
    }
    DoExpr(CaseExpr(Value(f), clauses))
  }

  // Helper: Lookup closures with fallback
  def lookupClosures(tKey: String, arity: Int)(implicit
      closureMap: Map[String, List[String]],
      arityMap: Map[Int, List[String]]
  ): List[String] =
    closureMap.getOrElse(tKey, arityMap.getOrElse(arity, Nil))

  // Helper: Lookup single closure (for direct P-tag inference)
  def lookupSingleClosure(tKey: String, arity: Int)(implicit
      closureMap: Map[String, List[String]],
      arityMap: Map[Int, List[String]]
  ): Option[String] =
    lookupClosures(tKey, arity) match {
      case List(single) => Some(single)
      case _            => None
    }

  def toClosureValue(matchName: String, outputName: String, c: TermClosure)(
      implicit
      closureMap: Map[String, List[String]] = Map.empty,
      arityMap: Map[Int, List[String]] = Map.empty
  ): ContextState[Expr] = for {
    substVars <- freeVars(c).map(_.filter(_._1 != matchName))
    (freeVars, tempVars) = substVars.unzip
    substFunc = (variable: String) => {
      matchName == variable match {
        case false =>
          substVars.find(_._1 == variable).map(_._2).getOrElse(variable)
        case true => s"$outputName ${tempVars.mkString(" ")}"
      }
    }
    expr <- Context.run(toClosureAbs(c)(substFunc, closureMap, arityMap))
    closure = tempVars.foldLeft(expr) { (term, v) => Abs(v, term) }
    // Compute arity from closure structure instead of type-checking
    // (avoids De Bruijn index issues when closure has stale indices from type-checking phase)
    arity = getClosureArity(c)
    // Compute typeKey from closure type for closureMap lookup
    // Don't use Context.run - we need access to type variables (A, B) from outer context
    closureTypeOpt <- getClosureType(c)
    tKey = closureTypeOpt.map(typeToKey).getOrElse("")
  } yield ClosureValue(
    outputName,
    arity,
    freeVars,
    LambdaBinding(outputName, closure),
    tKey
  )

  def toClosureAbs(
      closure: Term
  )(implicit
      substFunc: String => String,
      closureMap: Map[String, List[String]] = Map.empty,
      arityMap: Map[Int, List[String]] = Map.empty
  ): ContextState[Expr] =
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
      case t => pureToExpr(t)(substFunc, closureMap, arityMap)
    }

  def toCaseClause(
      p: Pattern,
      e: Term,
      matchExprType: Type,
      isLastPattern: Boolean = false
  )(implicit
      substFunc: String => String,
      closureMap: Map[String, List[String]] = Map.empty,
      arityMap: Map[Int, List[String]] = Map.empty
  ): ContextState[CaseClause] = p match {
    case PatternNode(_, node, vars) if vars.isEmpty && isLastPattern =>
      // Empty constructors (like Nil, None) cause GRIN LLVM backend errors.
      // Use #default for the LAST empty constructor pattern to avoid the bug.
      // Other empty constructors must remain explicit to maintain match semantics.
      // See: docs/grin-llvm-backend-issue.md
      buildCaseClause("#default", e)
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
      substFunc: String => String,
      closureMap: Map[String, List[String]] = Map.empty,
      arityMap: Map[Int, List[String]] = Map.empty
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
      case c @ AppExpr(e, _, i, _) =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- $e", p, List(c), i))
        )
      case v @ FunctionValue(f, 0, _, _) =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(s"$p <- $f", p, List(v)))
        )
      case FunctionValue(f, arity, _, tKey) if !isVariablePartialFun(f) =>
        addTempVariable().flatMap(p =>
          prepParameters(
            BindExpr(
              s"$p <- pure (${pTag(arity, f)})",
              p,
              List(PartialFunValue(f, arity, 0, tKey))
            )
          )
        )
      case FunctionValue(f, _, _, _) => State.pure(List(), f)
      case c @ ClosureValue(f, arity, freeVars, _, tKey) =>
        addTempVariable().flatMap(p =>
          prepParameters(
            BindExpr(
              s"$p <- pure (${pTag(arity, f)} ${freeVars.mkString(" ")})",
              p,
              List(c, PartialFunValue(f, arity, freeVars.length, tKey))
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
      case d: DoExpr =>
        addTempVariable().flatMap(p =>
          prepParameters(BindExpr(show"$p <- $d", p, List(d)))
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
    case v @ FunctionValue(f, 0, _, _) =>
      addTempVariable().flatMap(p =>
        getResult(BindExpr(s"$p <- $f", p, List(v)))
      )
    case f: FunctionValue => State.pure(List(), f)
    case a @ AppExpr(e, arity, _, resultPtag) if arity >= 1 =>
      addTempVariable().map(p => {
        val f = toPartialFunVariable(p)
        (
          List(BindExpr(show"$f <- $e", f, List(a))),
          FunctionValue(
            s"$f",
            arity,
            resultPtag,
            ""
          ) // Propagate P-tag from apply result
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

  def mapPrimitiveToGrin(ty: String, op: String): String = {
    val grinType = ty match {
      case "i32"  => "int"
      case "f32"  => "float"
      case "str"  => "string"
      case "bool" => "bool"
      case other  => other
    }

    val grinOp = (grinType, op) match {
      case ("string", "add")    => "concat"
      case (_, "noteq")         => "ne"
      case (_, "lessthan")      => "lt"
      case (_, "greaterthan")   => "gt"
      case (_, "lessthaneq")    => "le"
      case (_, "greaterthaneq") => "ge"
      case (_, other)           => other
    }

    s"_prim_${grinType}_${grinOp}"
  }

  def toVariable(idx: Integer): ContextState[String] =
    getNameFromIndex(idx).map(_ match {
      case PrimOp(ty, op) => mapPrimitiveToGrin(ty, op)
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
