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
import Syntax.*
import core.TypeChecker.findRootTypeVar
import fuse.SpecializedMethodUtils

object Grin {
  val MainFunction = "grinMain"
  val PartialFunSuffix = "''"

  // Two-pass code generation.
  // Pass 1: Collect all partial functions to build type-to-closure map
  // Pass 2: Generate code with P-tag inference
  def generate(bindings: List[Bind]): String = {
    // Build Env (Pass 1: collect partial functions + build maps)
    implicit val env: Env = buildEnv(bindings)
    // Pass 2: Generate code with P-tag inference
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
      ffi.isEmpty match {
        case true  => grinCode
        case false => s"$ffi\n\n$grinCode"
      }
    }
    s.runEmptyA.value
  }

  /** Build Env by collecting partial functions from bindings and constructing
    * all closure-related maps.
    */
  def buildEnv(bindings: List[Bind]): Env = {
    val collectPass = for {
      values <- bindings.traverse(bind =>
        for {
          value <- toLambdaBinding(bind)(Env.empty)
          _ <- Context.addBinding(bind.i, bind.b)
        } yield value
      )
    } yield values.flatten.flatMap(_._2)
    val partialFunctions = collectPass.run(Context.emptyContext).value._2

    // Build type-to-closure map for P-tag inference
    // Map from typeKey -> list of closure names with that type signature
    val closureMap: Map[String, List[String]] = partialFunctions
      .filter(_.typeKey.nonEmpty)
      .groupBy(_.typeKey)
      .map { case (typeKey, pfs) => (typeKey, pfs.map(_.f).distinct) }

    val arityFactsMap: Map[String, ArityFact] = partialFunctions
      .map(pf =>
        pf.f -> ArityFact(
          pf.f,
          pf.arity,
          pf.numOfAppliedVars,
          extractReturnType(pf.typeKey)
        )
      )
      .toMap

    Env(
      closureMap = closureMap,
      arityFactsMap = arityFactsMap
    )
  }

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

    usedOps.isEmpty match {
      case true  => ""
      case false =>
        val declarations = usedOps.map(_._2).mkString("\n  ")
        s"ffi pure\n  $declarations"
    }
  }

  // Generate return-type-grouped apply functions following GHC-GRIN pattern.
  // Closures are grouped by (arity, return type) to ensure HPT type homogeneity.
  // Each apply function handles all closures with the same return type at a given arity.
  def buildApply(partialFun: List[PartialFunValue]): ContextState[String] =
    partialFun.isEmpty match {
      case false =>
        for {
          // Group by (remaining arity, return type) for return-type-grouped apply functions
          groupedApplies <- buildReturnTypeGroupedApply(partialFun)
        } yield groupedApplies
      case true => "".pure[ContextState]
    }

  // Build return-type-grouped apply functions
  def buildReturnTypeGroupedApply(
      partialFuns: List[PartialFunValue]
  ): ContextState[String] = {
    // Step 1: Group by (remaining arity, return type)
    // Use "unknown" as fallback when return type is empty
    val byArityAndReturnType: Map[(Int, String), List[PartialFunValue]] =
      partialFuns
        .flatMap { pf =>
          // For each closure, generate entries for all arity levels 1..pf.arity
          (1 to pf.arity).map { remaining =>
            val returnType = getPartialFunReturnType(pf)
            ((remaining, returnType), pf)
          }
        }
        .groupBy(_._1)
        .map { case (key, pairs) =>
          (key, pairs.map(_._2).distinctBy(_.f))
        }

    // Step 2: Generate one apply function per (arity, return type) combination
    byArityAndReturnType.toList
      .sortBy(_._1)
      .traverse { case ((remaining, retType), closures) =>
        buildApplyForArityAndReturnType(remaining, retType, closures)
      }
      .map(_.mkString("\n\n"))
  }

  // Get return type for a PartialFunValue
  // Tries resolvedType first, then typeKey, then falls back to "unknown"
  def getPartialFunReturnType(pf: PartialFunValue): String = {
    // First try resolvedType if available
    val fromResolvedType = pf.resolvedType.map(typeToKey).map(extractReturnType)
    fromResolvedType match {
      case Some(rt) if rt.nonEmpty => rt
      case _                       =>
        // Fall back to typeKey
        val fromTypeKey = extractReturnType(pf.typeKey)
        fromTypeKey.isEmpty match {
          case true  => "unknown"
          case false => fromTypeKey
        }
    }
  }

  // Sanitize return type name for use in function names (replace special chars)
  def sanitizeTypeName(typeName: String): String =
    typeName
      .replace("[", "_")
      .replace("]", "_")
      .replace("->", "_to_")
      .replace("{", "_")
      .replace("}", "_")
      .replace(",", "_")
      .replace(":", "_")
      .replace("|", "_")
      .stripSuffix("_")

  // Build a unified apply function for all closures with given arity and return type
  def buildApplyForArityAndReturnType(
      remaining: Int,
      returnType: String,
      closures: List[PartialFunValue]
  ): ContextState[String] = {
    val sanitizedRetType = sanitizeTypeName(returnType)
    for {
      ptagParam <- addTempVariable()
      argParam <- addTempVariable()
      clauses <- closures.traverse(pf =>
        buildUnifiedClause(pf, remaining, argParam)
      )
    } yield {
      // Function name includes arity and return type: apply1_i32, apply1_unit, apply2_List_i32_
      s"apply${remaining}_$sanitizedRetType $ptagParam $argParam =\n case $ptagParam of\n${clauses.mkString("\n")}"
    }
  }

  // Build a single case clause for a unified apply function
  def buildUnifiedClause(
      pf: PartialFunValue,
      remaining: Int,
      argParam: String
  ): ContextState[String] = {
    val tag = pTag(remaining, pf.f)
    val totalCaptured = pf.arity - remaining + pf.numOfAppliedVars

    for {
      capturedVars <- (1 to totalCaptured).toList.traverse(_ =>
        addTempVariable()
      )
      capturedStr = capturedVars.mkString(" ").strip()
      pattern = capturedStr.isEmpty match {
        case true  => s"($tag)"
        case false => s"($tag $capturedStr)"
      }
    } yield {
      remaining match {
        case 1 =>
          // Final application - call the function
          val allArgs = (capturedVars :+ argParam).mkString(" ").strip()
          s"  $pattern ->\n   ${pf.f} $allArgs"
        case n =>
          // Partial application - create new P-tag with one less remaining
          val newTag = pTag(n - 1, pf.f)
          val allArgs = (capturedVars :+ argParam).mkString(" ")
          s"  $pattern ->\n   pure ($newTag $allArgs)"
      }
    }
  }

  def toLambdaBinding(
      binding: Bind
  )(implicit
      env: Env = Env.empty
  ): ContextState[Option[(List[LambdaBinding], List[PartialFunValue])]] = {
    // Pass closure types from the Bind to GRIN generation
    implicit val localEnv: Env =
      env.copy(closureTypesFromBind = binding.closureTypes)
    binding.b match {
      case TermAbbBind(expr: (TermBuiltin | TermClassMethod), _) =>
        State.pure(None)
      case TermAbbBind(expr: TermTAbs, _) =>
        // Skip uninstantiated generic functions - these are generic functions
        // that were never called with concrete types (e.g., unused Option[A].map)
        // and therefore cannot be code-generated
        State.pure(None)
      case TermAbbBind(expr, _) =>
        pureToExpr(expr)(identity, localEnv).map(e => {
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
    case CaseExpr(e, cases, _)    =>
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
    case CaseExpr(e, cases, _)             =>
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
  ): ContextState[(List[BindExpr], String)] =
    isHeapAllocated match {
      case true =>
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
      case false =>
        State.pure((prepExprs, result))
    }

  def pureToExpr(expr: Term)(implicit
      substFunc: String => String = identity,
      env: Env = Env.empty
  ): ContextState[Expr] =
    Context.run(toExpr(expr))

  def toExpr(
      expr: Term
  )(implicit
      substFunc: String => String,
      env: Env = Env.empty
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
          app <- result match {
            case FunctionValue(f, arity, ptag, tKey)
                if isVariablePartialFun(f) =>
              val resolvedTag = ptag.orElse(
                lookupSingleClosure(tKey, arity).map(pTag(arity, _))
              )
              resolvedTag match {
                case Some(tag) =>
                  // Pass tKey to makeApplyCall for return-type-based apply function selection
                  State.pure(
                    makeApplyCall(tag, result.show, parameter, arity, tKey)
                  )
                case None =>
                  lookupClosures(tKey, arity) match {
                    case cs if cs.nonEmpty =>
                      // Pass tKey to makeDispatch for return-type-based apply function selection
                      makeDispatch(f, cs, arity, parameter, tKey)
                    case _ =>
                      State.pure(
                        AppExpr(s"apply $result $parameter", arity - 1, 1, None)
                      )
                  }
              }
            case _ =>
              State.pure(AppExpr(show"$result $parameter".strip()))
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
              closureType <- getClosureTypeWithFallback(c)
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
                case c: TermClosure => getClosureTypeWithFallback(c)
                case TermFix(_, c)  => getClosureTypeWithFallback(c)
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
                case true  =>
                  val arity = getFunctionArity(ty)
                  val tKey = typeToKey(ty)
                  val inferredPtag =
                    lookupSingleClosure(tKey, arity).map(pTag(arity, _))
                  FunctionValue(v, arity, inferredPtag, tKey)
              }
            case (v, TermAbbBind(c: TermClosure, Some(ty))) =>
              // When referencing a TermClosure binding (from TermFix in TermLet),
              // extract free variables and create ClosureValue for partial application
              freeVars(c).map { fvs =>
                val freeVars = fvs.filter(_._1 != v).map(_._1)
                val arity = getClosureArity(c)
                freeVars match {
                  case Nil =>
                    FunctionValue(v, arity)
                  case _ =>
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

  // Helper: Build return-type-based apply call (e.g., "apply1_i32 f x")
  // Uses return type from typeKey to determine the correct apply function
  def makeApplyCall(
      tag: String,
      result: String,
      param: String,
      arity: Int,
      tKey: String = ""
  )(implicit env: Env): AppExpr = {
    // Extract closure name from tag (e.g., "P2c18" -> "c18")
    val closureName = closureFromTag(tag)

    // If tKey is a closure name (no arrows), look up its typeKey from closureMap
    val closureToTypeKey: Map[String, String] = env.closureMap.flatMap {
      case (typeKey, closureNames) =>
        closureNames.map(cn => cn -> typeKey)
    }

    // Use closure name to look up typeKey, which contains the accurate return type info
    // This ensures we use the same typeKey that was used to generate the apply function
    val effectiveTKey = closureToTypeKey.getOrElse(closureName, tKey)

    val rawReturnType = extractReturnType(effectiveTKey)
    val returnType = rawReturnType.isEmpty match {
      case true  => "unknown"
      case false => rawReturnType
    }
    val sanitizedRetType = sanitizeTypeName(returnType)
    val applyFn = s"apply${arity}_$sanitizedRetType"
    AppExpr(
      s"$applyFn $result $param",
      arity - 1,
      1,
      nextPtag(tag, arity),
      List(closureName)
    )
  }

  // Helper: Build inline dispatch for multiple closures with different return types
  // Dispatches to the appropriate return-type-grouped apply function based on closure's actual return type
  def makeDispatch(
      f: String,
      closures: List[String],
      arity: Int,
      param: String,
      tKey: String
  )(implicit env: Env): ContextState[Expr] = {
    val remainingArity = arity - 1
    // For each closure, look up its actual return type from closureMap (typeKey -> closures)
    // by finding which typeKey contains this closure, then extracting the return type
    val closureToTypeKey: Map[String, String] = env.closureMap.flatMap {
      case (typeKey, closureNames) =>
        closureNames.map(cn => cn -> typeKey)
    }

    // For each closure, dispatch to the appropriate apply function
    val clauses = closures.map { cn =>
      val tag = pTag(arity, cn)
      val pattern = s"($tag)"
      // Get the closure's actual return type by finding its typeKey
      val closureReturnType = closureToTypeKey
        .get(cn)
        .map(extractReturnType)
        .filter(_.nonEmpty)
        .getOrElse {
          // Fallback to tKey's return type if closure not found
          val raw = extractReturnType(tKey)
          raw.isEmpty match {
            case true  => "unknown"
            case false => raw
          }
        }
      val sanitizedRetType = sanitizeTypeName(closureReturnType)
      val applyFn = s"apply${arity}_$sanitizedRetType"
      val call = s"$applyFn $f $param"
      // Pass closure name for subsequent dispatch
      CaseClause(
        pattern,
        AppExpr(call, remainingArity, 1, nextPtag(tag, arity), List(cn)),
        1
      )
    }
    State.pure(DoExpr(CaseExpr(Value(f), clauses)))
  }

  // Helper: Get closures by arity (derived from arityFactsMap)
  def getClosuresByArity(arity: Int)(implicit env: Env): List[String] =
    env.arityFactsMap.collect {
      case (name, fact) if fact.totalParams == arity => name
    }.toList

  // Helper: Lookup closures with fallback (no specialized variant expansion)
  def lookupClosures(tKey: String, arity: Int)(implicit
      env: Env
  ): List[String] = {
    // Check if tKey is a comma-separated list of closures (from DoExpr dispatch extraction)
    tKey.contains(",") match {
      case true  => tKey.split(",").toList
      case false =>
        env.closureMap.getOrElse(tKey, getClosuresByArity(arity))
    }
  }

  // Helper: Lookup single closure (for direct P-tag inference)
  def lookupSingleClosure(tKey: String, arity: Int)(implicit
      env: Env
  ): Option[String] =
    lookupClosures(tKey, arity) match {
      case List(single) => Some(single)
      case _            => None
    }

  // Helper: Get return type for a closure from ArityFacts
  def getClosureReturnType(
      closureName: String
  )(implicit env: Env): Option[String] = {
    // Handle specialized closures (c28_c47 -> look up c28)
    val baseName = closureName.contains("_") match {
      case true  => closureName.split("_")(0)
      case false => closureName
    }
    env.arityFactsMap.get(baseName).map(_.returnTypeKey).filter(_.nonEmpty)
  }

  // Helper: Check if all closures in a list have the same return type
  def hasUniformReturnType(
      closures: List[String]
  )(implicit env: Env): Option[String] =
    closures.flatMap(getClosureReturnType).distinct match {
      case List(single) => Some(single)
      case _            => None
    }

  // Helper: Get closures by return type for dispatch optimization
  def getClosuresByReturnType(
      returnType: String,
      returnTypeMap: Map[String, List[String]]
  ): List[String] =
    returnTypeMap.getOrElse(returnType, Nil)

  def toClosureValue(matchName: String, outputName: String, c: TermClosure)(
      implicit env: Env = Env.empty
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
    expr <- Context.run(toClosureAbs(c)(substFunc, env))
    closure = tempVars.foldLeft(expr) { (term, v) => Abs(v, term) }
    // Compute arity from closure structure instead of type-checking
    // (avoids De Bruijn index issues when closure has stale indices from type-checking phase)
    arity = getClosureArity(c)
    // Compute typeKey from closure type - try multiple sources:
    // 1. First try closureTypesFromBind (from monomorphization)
    // 2. Then try extracting from closure structure
    // 3. Finally fall back to type-checking the closure
    closureTypeOpt <- getClosureTypeWithFallback(c)
    // If no type from structure, try type-checking (may fail for some closures)
    closureTypeFromTC <- closureTypeOpt match {
      case Some(ty) => State.pure(Some(ty))
      case None     =>
        toContextStateOption(
          TypeChecker.pureInfer(c)(checking = false).map(_._1)
        )
    }
    tKey = closureTypeFromTC.map(typeToKey).getOrElse("")
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
      env: Env = Env.empty
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
      case t => pureToExpr(t)
    }

  def toCaseClause(
      p: Pattern,
      e: Term,
      matchExprType: Type,
      isLastPattern: Boolean = false
  )(implicit
      substFunc: String => String,
      env: Env = Env.empty
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
      env: Env = Env.empty
  ): ContextState[CaseClause] = for {
    caseExpr <- pureToExpr(t)
    (prepExprs, parameter) <- prepParameters(caseExpr)
  } yield CaseClause(
    cpat,
    MultiLineExpr(prepExprs, PureExpr(Value(parameter)))
  )

  def prepParameters(
      expr: Expr
  )(implicit
      env: Env = Env.empty
  ): ContextState[Tuple2[List[BindExpr], String]] =
    expr match {
      case b @ BindExpr(e, v, _, _) =>
        ((List(b), v)).pure[ContextState]
      case MultiLineExpr(exprs, r) =>
        prepParameters(r).map { case (prepExprs, result) =>
          (exprs :++ prepExprs, result)
        }
      case c @ AppExpr(e, _, i, _, _) =>
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
        // GHC-GRIN style: Always store all freeVars (including closures) as P-tag fields
        // HPT can track values in P-tag fields - they're treated as node values
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
    case l: BindExpr             => State.pure(List(l), Value(l.variable))
    case MultiLineExpr(exprs, r) =>
      getResult(r).map { case (prepExprs, v) =>
        (exprs :++ prepExprs, v)
      }
    case i: Value                      => State.pure(List(), i)
    case c: ClosureValue               => State.pure(List(), c)
    case v @ FunctionValue(f, 0, _, _) =>
      addTempVariable().flatMap(p =>
        getResult(BindExpr(s"$p <- $f", p, List(v)))
      )
    case f: FunctionValue => State.pure(List(), f)
    case a @ AppExpr(e, arity, _, resultPtag, resultClosures) if arity >= 1 =>
      addTempVariable().map(p => {
        val f = toPartialFunVariable(p)
        // Use closure names as tKey for subsequent dispatch
        val tKey = resultClosures.mkString(",")
        (
          List(BindExpr(show"$f <- $e", f, List(a))),
          FunctionValue(
            s"$f",
            arity,
            resultPtag,
            tKey
          ) // Propagate P-tag and closure names from apply result
        )
      })
    case e: AppExpr => State.pure(List(), e)
    case d: DoExpr  =>
      // Extract arity and closure names from DoExpr if it contains a dispatch with partial applications
      // New format: "apply2_T8_i32 f x" or "apply1_i32 f x"
      val dispatchInfo = d.expr match {
        case CaseExpr(_, clauses, _) =>
          // Extract arity and closure names from case clause patterns
          // Pattern like "(P2c28)" indicates closure c28 at arity 2
          val clauseInfo = clauses.flatMap {
            case CaseClause(pattern, AppExpr(repr, arity, _, _, closures), _)
                if arity > 0 =>
              // Extract closure name from pattern like "(P2c28)"
              val ptagPattern = """\(P(\d+)(\w+)\)""".r
              ptagPattern.findFirstMatchIn(pattern).map { m =>
                val closureName = m.group(2) // e.g., "c28"
                (arity, closureName)
              }
            case _ => None
          }
          clauseInfo.headOption.map { case (arity, _) =>
            val closures = clauseInfo.map(_._2).distinct
            (arity, closures)
          }
        case _ => None
      }
      addTempVariable().flatMap(p => {
        dispatchInfo match {
          case Some((arity, closures)) if arity > 0 =>
            // Result is a partial application - use FunctionValue with '' suffix
            // Use closure names as tKey for correct subsequent dispatch
            val varName = toPartialFunVariable(p)
            val tKey = closures.mkString(",")
            State.pure(
              (
                List(BindExpr(show"$varName <- $d", varName, List(d))),
                FunctionValue(varName, arity, None, tKey)
              )
            )
          case _ =>
            // Result is a value, not a partial application
            getResult(BindExpr(show"$p <- $d", p, List(d)))
        }
      })
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
      case v if v.startsWith(Desugar.MethodNamePrefix)   => methodToName(v)
      case v if v.startsWith(Desugar.RecordConstrPrefix) =>
        recordConstrToName(v)
      case s => s
    })

  def methodToName(n: String): String =
    s"${n.filter(_.isLetterOrDigit)}'"

  def recordConstrToName(n: String): String =
    n.stripPrefix(Desugar.RecordConstrPrefix)
}
