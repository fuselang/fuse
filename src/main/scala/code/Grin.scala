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

  /** Checks if a De Bruijn-indexed variable is referenced in a term. Used to
    * detect dead let-bindings for unused closures.
    */
  def isVarUsed(idx: Int, term: Term): Boolean = term match {
    case TermVar(_, i, _)           => i == idx
    case TermApp(_, t1, t2)         => isVarUsed(idx, t1) || isVarUsed(idx, t2)
    case TermClosure(_, _, _, body) => isVarUsed(idx + 1, body)
    case TermAbs(_, _, _, body, _)  => isVarUsed(idx + 1, body)
    case TermLet(_, _, t1, t2)  => isVarUsed(idx, t1) || isVarUsed(idx + 1, t2)
    case TermFix(_, t)          => isVarUsed(idx + 1, t)
    case TermMatch(_, t, cases) =>
      isVarUsed(idx, t) || cases.exists { case (_, body) =>
        isVarUsed(idx, body)
      }
    case TermTApp(_, t, _)     => isVarUsed(idx, t)
    case TermTAbs(_, _, _, t)  => isVarUsed(idx, t)
    case TermTag(_, _, t, _)   => isVarUsed(idx, t)
    case TermRecord(_, fields) =>
      fields.exists { case (_, t) => isVarUsed(idx, t) }
    case TermAscribe(_, t, _)    => isVarUsed(idx, t)
    case TermMethodProj(_, t, _) => isVarUsed(idx, t)
    case TermFold(_, _)          => false
    case _                       => false
  }

  def isClosureTerm(t: Term): Boolean = t match {
    case _: TermClosure             => true
    case TermFix(_, c: TermClosure) => true
    case TermFix(_, _)              => true
    case _                          => false
  }
  val PartialFunSuffix = "''"
  // GRIN optimizer hardcodes these function names for special optimization passes.
  // User-defined functions with these names must be renamed to avoid InlineEval/InlineApply
  // targeting them and creating undefined specialized versions (e.g., eval.0).
  val GrinReservedFunctionNames: Set[String] = Set("eval", "apply")

  // Built-in identifiers that map directly to GRIN runtime primops.
  // Derived from `core.Primops.specs` — the single source of truth for
  // user-visible primop names.
  val BuiltInPrimopNames: Map[String, String] = core.Primops.fuseToGrinName

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
        .replaceAll("[\\[\\]]", "") // Remove brackets from TypeApp names
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
    *
    * Two-pass scan: the first pass primes partial-function names with an env
    * that has no closureMap/arityFactsMap; the second re-collects using the
    * populated maps so env-driven branches (direct call vs dispatch) match
    * final codegen. Without this, temp-variable counters diverge between the
    * collect pass and codegen, and lifted closure names in closureMap don't
    * appear in emitted output.
    */
  def buildEnv(bindings: List[Bind]): Env = {
    val typeInstancesMap = buildTypeInstancesMap(bindings)
    val typeInstanceMethodsMap = buildTypeInstanceMethodsMap(bindings)
    val primingEnv = Env(
      typeInstances = typeInstancesMap,
      typeInstanceMethods = typeInstanceMethodsMap
    )
    val primingFunctions = collectPartialFunctions(bindings, primingEnv)
    val primingEnvWithFunctions =
      envWithPartialFunctions(primingEnv, primingFunctions)
    val partialFunctions =
      collectPartialFunctions(bindings, primingEnvWithFunctions)
    envWithPartialFunctions(primingEnvWithFunctions, partialFunctions)
  }

  def collectPartialFunctions(
      bindings: List[Bind],
      env: Env
  ): List[PartialFunValue] = {
    val pass = for {
      values <- bindings.traverse(bind =>
        for {
          value <- toLambdaBinding(bind)(env)
          _ <- Context.addBinding(bind.i, bind.b)
        } yield value
      )
    } yield values.flatten.flatMap(_._2)
    pass.run(Context.emptyContext).value._2
  }

  def envWithPartialFunctions(
      env: Env,
      partialFunctions: List[PartialFunValue]
  ): Env = {
    val closureMap: Map[String, List[String]] = partialFunctions
      .filter(_.typeKey.nonEmpty)
      .groupBy(_.typeKey)
      .map { case (typeKey, pfs) => (typeKey, pfs.map(_.f).distinct) }
    // Per-closure facts store raw parameter and return types. Merging of
    // unresolved parameter types into concrete sibling groups happens in
    // buildGroupedApply (via duplication) and in applyFnForClosure (at
    // call sites); the env is just the structural source of truth.
    val arityFactsMap: Map[String, ArityFact] = partialFunctions.map { pf =>
      pf.f -> ArityFact(
        pf.f,
        pf.arity,
        pf.numOfAppliedVars,
        extractReturnType(pf.typeKey),
        (0 until pf.arity).toList.map(i => extractParamTypeAt(pf.typeKey, i))
      )
    }.toMap
    env.copy(
      closureMap = closureMap,
      arityFactsMap = arityFactsMap
    )
  }

  /** Scan all bindings to build a map from a concrete type name to the set of
    * type-class names that the type implements. Matches on names of the form
    * `#<typeClass>#<typeName>` (see [[Desugar.toTypeInstanceBindID]]).
    */
  def buildTypeInstancesMap(bindings: List[Bind]): Map[String, Set[String]] =
    bindings.foldLeft(Map.empty[String, Set[String]]) { (acc, bind) =>
      bind.i match {
        case Desugar.TypeInstancePattern(cls, typeName) =>
          acc.updated(typeName, acc.getOrElse(typeName, Set.empty) + cls)
        case _ => acc
      }
    }

  /** Scan all bindings to build a map from `(typeName, className)` to the set
    * of method names exposed. Matches on bind names for instance methods
    * (`!<method>#<typeName>#<className>`) and for type-class default methods
    * (`!<method>#<className>`) prior to specialization.
    */
  def buildTypeInstanceMethodsMap(
      bindings: List[Bind]
  ): Map[(String, String), Set[String]] =
    bindings.foldLeft(Map.empty[(String, String), Set[String]]) { (acc, bind) =>
      bind.i match {
        case Desugar.TypeInstanceMethodPattern(method, typeName, cls) =>
          val key = (typeName, cls)
          acc.updated(key, acc.getOrElse(key, Set.empty) + method)
        case _ => acc
      }
    }

  def generateMissingFFI(grinCode: String): String = {
    val usedOps = GrinPrelude.missingFFI.filter { case (opName, _) =>
      grinCode.contains(opName)
    }
    usedOps.isEmpty match {
      case true  => ""
      case false =>
        val declarations = usedOps.map(_._2).mkString("\n  ")
        s"ffi pure\n  $declarations"
    }
  }

  /** Generate GRIN external declarations to replace the built-in prelude. Used
    * with --no-prelude flag. Key difference from GRIN's default prelude:
    * effectful functions return T_Int64 instead of T_Unit, avoiding LLVM's
    * "void type only allowed for function results" error when Unit values
    * appear in constructor fields.
    */
  def generatePrelude(grinCode: String): String = {
    def filterUsed(ops: List[(String, String)]): List[String] =
      ops
        .filter { case (name, _) =>
          grinCode.contains(name) && !grinCode.contains(s"$name ::")
        }
        .map(_._2)
    val sections = List(
      ("ffi effectful", filterUsed(GrinPrelude.ffiEffectful)),
      ("ffi pure", filterUsed(GrinPrelude.ffiPure)),
      ("primop pure", filterUsed(GrinPrelude.primopPure))
    ).filter(_._2.nonEmpty).map { case (header, decls) =>
      s"$header\n  ${decls.mkString("\n  ")}"
    }
    sections.mkString("\n\n")
  }

  // Generate apply functions one per (arity, paramType, returnType) bucket.
  // Closures whose parameter type is unresolved (a bare type-variable
  // placeholder produced by type erasure) are duplicated into every
  // concrete-param sibling group that shares the same (arity, returnType).
  // This keeps the backend's heap-points-to type environments monomorphic
  // (no mixed-type fields) while still dispatching the erased-type lambdas
  // from whichever concrete call site they actually reach.
  def buildApply(
      partialFun: List[PartialFunValue]
  )(implicit env: Env): ContextState[String] =
    partialFun match {
      case Nil => "".pure[ContextState]
      case pfs => buildGroupedApply(pfs)
    }

  case class ApplyKey(remaining: Int, paramType: String, returnType: String)

  def buildGroupedApply(
      partialFuns: List[PartialFunValue]
  )(implicit env: Env): ContextState[String] = {
    val entries: List[(ApplyKey, PartialFunValue)] =
      partialFuns.flatMap(pf => pfApplyKeys(pf).map(_ -> pf))

    val concretesByRR: Map[(Int, String), List[String]] = entries
      .collect {
        case (k, _) if !isUnresolvedParamType(k.paramType) =>
          ((k.remaining, k.returnType), k.paramType)
      }
      .groupBy(_._1)
      .map { case (k, v) => (k, v.map(_._2).distinct) }

    val expanded: List[(ApplyKey, PartialFunValue)] = entries.flatMap {
      case (k, pf) if !isUnresolvedParamType(k.paramType) => List(k -> pf)
      case (k, pf)                                        =>
        concretesByRR.getOrElse((k.remaining, k.returnType), Nil) match {
          case Nil       => List(k -> pf)
          case concretes => concretes.map(pt => k.copy(paramType = pt) -> pf)
        }
    }

    expanded
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).distinctBy(_.f))
      .toList
      .sortBy { case (k, _) => (k.remaining, k.paramType, k.returnType) }
      .traverse { case (k, pfs) => buildApplyForGroup(k, pfs) }
      .map(_.mkString("\n\n"))
  }

  // Per-level ApplyKey list for a PartialFunValue. Pulls raw param/return
  // types from the already-populated ArityFact so grouping matches what
  // applyFnForClosure picks at call sites.
  def pfApplyKeys(pf: PartialFunValue)(implicit env: Env): List[ApplyKey] = {
    val fact = env.arityFactsMap.get(pf.f)
    val returnType =
      fact
        .map(_.returnTypeKey)
        .filter(_.nonEmpty)
        .getOrElse(getPartialFunReturnType(pf))
    (1 to pf.arity).toList.map { remaining =>
      val level = pf.arity - remaining
      val paramType = fact
        .flatMap(_.paramTypeKeys.lift(level))
        .filter(_.nonEmpty)
        .getOrElse(getPartialFunParamType(pf, level))
      ApplyKey(remaining, paramType, returnType)
    }
  }

  // A param type is "unresolved" if it's a bare type-variable placeholder
  // produced by the key encoder (any indexed type-variable name, or the
  // existential-variable fallback). These shouldn't split apply groups
  // since they'd accept any concrete type at runtime.
  def isUnresolvedParamType(paramType: String): Boolean =
    paramType.isEmpty ||
      paramType == "unknown" ||
      paramType == "TypeEVar" ||
      paramType.matches("T\\d+")

  // Choose the apply-function name to emit for a call on `closureName`
  // at `remaining` args-to-go. The name must match a dispatch table
  // produced by buildGroupedApply: we pick the param type that actually
  // keys a table in the same (arity, returnType) bucket.
  //
  // When the closure is known (fact present): use its raw param type, but
  // if that's unresolved prefer a concrete sibling (buildGroupedApply
  // duplicated the closure's dispatch clause into every concrete group).
  // When the closure is unknown (empty name): use any sibling's param
  // type from the same (arity, returnType) bucket, falling back to the
  // supplied typeKey only if there are no siblings.
  def applyFnForClosure(
      closureName: String,
      remaining: Int,
      typeKeyFallback: String
  )(implicit env: Env): String = {
    val fact = env.arityFactsMap.get(closureName)
    val returnType = fact
      .map(_.returnTypeKey)
      .filter(_.nonEmpty)
      .orElse(Some(extractReturnType(typeKeyFallback)).filter(_.nonEmpty))
      .getOrElse("unknown")
    val paramType = fact match {
      case Some(af) =>
        val level = af.totalParams - remaining
        val raw = af.paramTypeKeys
          .lift(level)
          .filter(_.nonEmpty)
          .getOrElse("unknown")
        isUnresolvedParamType(raw) match {
          case false => raw
          case true  => pickConcreteParam(remaining, returnType).getOrElse(raw)
        }
      case None =>
        siblingParam(remaining, returnType).getOrElse {
          val p = extractParamTypeAt(typeKeyFallback, 0)
          p.isEmpty match { case true => "unknown"; case false => p }
        }
    }
    applyFnName(remaining, paramType, returnType)
  }

  // Param type to use when emitting an apply call that targets the
  // (remaining, returnType) dispatch bucket: prefer a concrete sibling
  // (buildGroupedApply duplicates into every concrete group), falling back
  // to any sibling's raw param otherwise. `None` if no sibling exists.
  def siblingParam(remaining: Int, returnType: String)(implicit
      env: Env
  ): Option[String] = {
    val params = env.arityFactsMap.values.view
      .filter(af =>
        af.totalParams >= remaining && af.returnTypeKey == returnType
      )
      .flatMap(af => af.paramTypeKeys.lift(af.totalParams - remaining))
      .filter(_.nonEmpty)
      .toList
      .distinct
    params.filterNot(isUnresolvedParamType).headOption.orElse(params.headOption)
  }

  def pickConcreteParam(remaining: Int, returnType: String)(implicit
      env: Env
  ): Option[String] =
    siblingParam(remaining, returnType).filterNot(isUnresolvedParamType)

  def getPartialFunReturnType(pf: PartialFunValue): String =
    extractFromPF(pf, extractReturnType)

  def getPartialFunParamType(pf: PartialFunValue, level: Int): String =
    extractFromPF(pf, extractParamTypeAt(_, level))

  // Apply an extractor to a PartialFunValue's resolvedType (if any) then
  // fall back to its raw typeKey; both empty results collapse to "unknown".
  def extractFromPF(pf: PartialFunValue, f: String => String): String =
    pf.resolvedType
      .map(typeToKey andThen f)
      .filter(_.nonEmpty)
      .orElse(Some(f(pf.typeKey)).filter(_.nonEmpty))
      .getOrElse("unknown")

  /** Resolve TypeVar type constructors (in TypeApp t1 position) to TypeId using
    * adjusted De Bruijn indices to account for context growth since the TypeVar
    * was created.
    */
  def resolveTypeConstructors(ty: Type): ContextState[Type] = ty match {
    case TypeApp(info, tv @ TypeVar(tvInfo, idx, n), arg) =>
      for {
        currentCtxLen <- State.inspect { (ctx: Context) => ctx._1.length }
        adjustedIdx = currentCtxLen - n + idx
        nameOpt <- State.inspect { (ctx: Context) =>
          (adjustedIdx >= 0 && adjustedIdx < ctx._1.length) match {
            case true  => indexToName(ctx, adjustedIdx)
            case false => None
          }
        }
        resolvedArg <- resolveTypeConstructors(arg)
      } yield nameOpt match {
        case Some(name) if name.headOption.exists(_.isUpper) =>
          TypeApp(info, TypeId(tvInfo, name), resolvedArg)
        case _ => TypeApp(info, tv, resolvedArg)
      }
    case TypeApp(info, t1, t2) =>
      for {
        r1 <- resolveTypeConstructors(t1)
        r2 <- resolveTypeConstructors(t2)
      } yield TypeApp(info, r1, r2)
    case TypeArrow(info, t1, t2) =>
      for {
        r1 <- resolveTypeConstructors(t1)
        r2 <- resolveTypeConstructors(t2)
      } yield TypeArrow(info, r1, r2)
    case _ => ty.pure[ContextState]
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

  // Emit a single unified apply function for a dispatch group.
  def buildApplyForGroup(
      key: ApplyKey,
      closures: List[PartialFunValue]
  ): ContextState[String] = {
    val applyName = applyFnName(key.remaining, key.paramType, key.returnType)
    for {
      ptagParam <- addTempVariable()
      argParam <- addTempVariable()
      clauses <- closures.traverse(pf =>
        buildUnifiedClause(pf, key.remaining, argParam)
      )
    } yield s"$applyName $ptagParam $argParam =\n case $ptagParam of\n${clauses.mkString("\n")}"
  }

  // Build the dispatch function name. Param type is required so that
  // closures with the same return type but different input types don't
  // collide in the HPT type environment.
  def applyFnName(
      remaining: Int,
      paramType: String,
      returnType: String
  ): String =
    s"apply${remaining}_${sanitizeTypeName(orUnknown(paramType))}_to_${sanitizeTypeName(orUnknown(returnType))}"

  def orUnknown(s: String): String = s.isEmpty match {
    case true  => "unknown"
    case false => s
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
    // Pass closure types from the Bind to GRIN generation.
    // Also pass resolved closure types from insts as fallback for unannotated closures.
    val closureTypesFallback = binding.insts
      .filter(_.r == core.Instantiations.Resolution.Closure)
      .flatMap(cInst =>
        cInst.tys.headOption.collect {
          case ty: TypeArrow
              if !MonoSpecialize
                .getTypeContextLength(ty)
                .isDefined =>
            cInst.i -> ty
        }
      )
      .toMap
    implicit val localEnv: Env = env.copy(
      closureTypesFromBind = binding.closureTypes,
      closureTypesFallback = closureTypesFallback
    )
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

  def foldExpr[T](e: Expr)(collect: PartialFunction[Expr, List[T]])(
      recurse: Expr => List[T]
  ): List[T] =
    collect.applyOrElse(
      e,
      (_: Expr) =>
        e match {
          case BindExpr(_, _, exprs, _) => exprs.flatMap(recurse)
          case DoExpr(e, _)             => recurse(e)
          case CaseClause(_, e, _)      => recurse(e)
          case PureExpr(e, _)           => recurse(e)
          case Abs(_, e)                => recurse(e)
          case CaseExpr(e, cases, _)    => recurse(e) :++ cases.flatMap(recurse)
          case MultiLineExpr(exprs, r)  => exprs.flatMap(recurse) :++ recurse(r)
          case _                        => Nil
        }
    )

  def extractPartialFun(e: Expr): List[PartialFunValue] =
    foldExpr[PartialFunValue](e) {
      case p: PartialFunValue                   => List(p)
      case ClosureValue(_, _, _, binding, _, _) =>
        extractPartialFun(binding.expr)
    }(extractPartialFun)

  def lambdaLift(e: Expr): List[LambdaBinding] =
    foldExpr[LambdaBinding](e) { case ClosureValue(_, _, _, binding, _, _) =>
      binding :: lambdaLift(binding.expr)
    }(lambdaLift)

  def nameBind(name: String): String = name match {
    case b if b.startsWith(Desugar.MethodNamePrefix) =>
      methodToName(b)
    case b if b.startsWith(Desugar.RecordConstrPrefix) =>
      recordConstrToName(b)
    case TypeChecker.MainFunction                   => MainFunction
    case n if GrinReservedFunctionNames.contains(n) => s"${n}_"
    case n                                          => n
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
      case c: TermClosure       =>
        addTempVariable("c").flatMap(name => toClosureValue(name, name, c))
      case TermAbs(_, variable, variableType, body, _) =>
        for {
          variable1 <- includeFunctionSuffix(variable, variableType)
          variable2 <- Context.addBinding(variable1, VarBind(variableType))
          b <- pureToExpr(body)
        } yield Abs(toParamVariable(variable2), b)
      case TermApp(_, TermFold(_, ty), tag: TermTag) =>
        // Constructor application with TermFold.
        // For constructors whose fields contain function types, use double
        // indirection: store each closure-valued field on the heap first, then
        // store the constructor with the heap pointer. This ensures:
        // 1. Heap-points-to analysis can track closure types through fields
        // 2. The LLVM backend avoids illegal partial-application tags in
        //    struct fields
        // All other constructors use direct store (heap allocation).
        for {
          tyD <- typeShiftOnContextDiff(ty)
          typeName <- getNameFromType(tyD)
          hasFuncFields <- hasFieldsWithFunctionTypeS(tag)
          // Use tag.typ for type arg suffix - it preserves concrete types from monomorphization
          // while TermFold's ty has broken TypeVar references after specialization
          tagTyD <- typeShiftOnContextDiff(tag.typ)
          typeArgSuffix <- getTypeArgSuffix(tagTyD)
          // Handle empty constructors (TermUnit should not generate parameters)
          result <- tag.t match {
            case TermUnit(_) =>
              // Empty constructor like Nil - no parameters
              val constr = show"store (${cTag(tag.i)}${typeArgSuffix})"
              State.pure(MultiLineExpr(List(), AppExpr(constr)))
            case TermRecord(_, fields) =>
              for {
                params <- fields.traverse { case (_, term) =>
                  for {
                    e <- toExpr(term)
                    (preps, param) <- prepParameters(e)
                  } yield (preps, param)
                }
                prepExprs = params.flatMap(_._1)
                parameters = params.map(_._2).mkString(" ")
                // For function-typed fields: store each field on heap first
                result <- hasFuncFields match {
                  case true =>
                    params
                      .map(_._2)
                      .traverse { param =>
                        addTempVariable().map { storeVar =>
                          (
                            BindExpr(
                              s"$storeVar <- store $param",
                              storeVar,
                              List()
                            ),
                            storeVar
                          )
                        }
                      }
                      .map { storeResults =>
                        val storeExprs = storeResults.map(_._1)
                        val storeVars =
                          storeResults.map(_._2).mkString(" ")
                        val constr =
                          show"store (${cTag(tag.i)}${typeArgSuffix} $storeVars)"
                        MultiLineExpr(
                          prepExprs ++ storeExprs,
                          AppExpr(constr)
                        )
                      }
                  case false =>
                    val constr =
                      show"store (${cTag(tag.i)}${typeArgSuffix} $parameters)"
                    State.pure(MultiLineExpr(prepExprs, AppExpr(constr)))
                }
              } yield result
            case _ =>
              // Constructor with non-record body
              for {
                tagExpr <- pureToExpr(tag.t)
                (prepExprs, parameters) <- prepParameters(tagExpr)
                // For function-typed fields: store the closure on heap first,
                // then store the constructor with the heap pointer
                result <- hasFuncFields match {
                  case true =>
                    addTempVariable().map { storeVar =>
                      val storeExpr = BindExpr(
                        s"$storeVar <- store $parameters",
                        storeVar,
                        List()
                      )
                      val constr =
                        show"store (${cTag(tag.i)}${typeArgSuffix} $storeVar)"
                      MultiLineExpr(
                        prepExprs :+ storeExpr,
                        AppExpr(constr)
                      )
                    }
                  case false =>
                    val constr =
                      show"store (${cTag(tag.i)}${typeArgSuffix} $parameters)"
                    State.pure(MultiLineExpr(prepExprs, AppExpr(constr)))
                }
              } yield result
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
                  val closureName = closureFromTag(tag)
                  val capturedFields =
                    env.arityFactsMap.get(closureName) match {
                      case Some(af) => af.totalParams - arity + af.capturedVars
                      case None     => -1
                    }
                  val closureApplyFn =
                    applyFnForClosure(closureName, arity, tKey)
                  val applyGroupSize = env.arityFactsMap.values.count { af =>
                    af.totalParams >= arity &&
                    applyFnForClosure(
                      af.functionName,
                      arity,
                      ""
                    ) == closureApplyFn
                  }
                  (capturedFields == 0 && arity == 1 && applyGroupSize > 1) match {
                    case true =>
                      State.pure(
                        AppExpr(
                          s"$closureName $parameter",
                          arity - 1,
                          1,
                          nextPtag(tag, arity),
                          List(closureName)
                        )
                      )
                    case false =>
                      State.pure(
                        makeApplyCall(tag, result.show, parameter, arity, tKey)
                      )
                  }
                case None =>
                  lookupClosures(tKey, arity) match {
                    case cs if cs.nonEmpty =>
                      makeDispatch(f, cs, arity, parameter, tKey)
                    case _ =>
                      State.pure(applyFnCall(f, parameter, arity, tKey))
                  }
              }
            case FunctionValue(f, arity, _, tKey)
                if tKey.nonEmpty && !env.arityFactsMap.contains(f) &&
                  !isKnownFunction(f) =>
              // Closure variable from pattern match (VarBind with tKey set).
              // Not a known top-level function, must route through apply.
              State.pure(applyFnCall(f, parameter, arity, tKey))
            case _ =>
              State.pure(AppExpr(show"$result $parameter".strip()))
          }
        } yield MultiLineExpr(prepExprs, app)
      case TermLet(_, variable, t1, t2)
          if isClosureTerm(t1) && !isVarUsed(0, t2) =>
        // Dead closure let-binding: variable is not used in continuation.
        // Skip generating code for the value (avoids invalid GRIN from
        // unused generic closures with unresolved operator types).
        pureToExpr(core.Shifting.termShift(-1, t2))
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
              variableWithSuffix <- includeFunctionSuffix(
                variable,
                variableType
              )
              fVar <- Context.addBinding(
                variableWithSuffix,
                VarBind(variableType)
              )
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
          typeArgSuffix <- getTypeArgSuffix(tyT1D)
          p <- patterns.zipWithIndex.traverse { case ((p, e), idx) =>
            Context.run(
              toCaseClause(
                p,
                e,
                exprType,
                isLastPattern = idx == patterns.length - 1,
                typeArgSuffix = typeArgSuffix
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
              // Prefer the env-level lookup so forward references resolve — a
              // trait-default specialization rendered before the impl it
              // depends on won't yet have the impl in the ContextState.
              envClassName = env.typeInstances
                .getOrElse(typeName, Set.empty)
                .find(cls =>
                  env.typeInstanceMethods
                    .getOrElse((typeName, cls), Set.empty)
                    .contains(m)
                )
              className <- envClassName match {
                case Some(cls) => State.pure(Some(cls))
                case None      =>
                  for {
                    instances <- getTypeInstances(typeName)
                    r <- instances.findM(c =>
                      getAllTypeClassMethods(c.name).map(_.exists(_ == m))
                    )
                  } yield r.map(_.name)
              }
              f = className.fold(Desugar.toMethodID(m, typeName))(
                Desugar.toTypeInstanceMethodID(m, typeName, _)
              )
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
      case TermTApp(_, tv @ TermVar(_, idx, _), tyArg) =>
        // Type application over a nullary data constructor: if the inner
        // term is a constructor reference (uppercase TermVar), emit the
        // type-suffixed store form to match the producer site's naming
        // (`store (C<ctor><typeArgs>)` from the Fold path at
        // TermApp(TermFold(_, ty), TermTag(...))).
        // Without this, the reference emits the unsuffixed constructor name
        // while the definition stores the suffixed one, and the backend
        // rejects with `illegal code`.
        for {
          optName <- State.inspect((ctx: Context) => indexToName(ctx, idx))
          result <- optName match {
            case Some(name) if core.Instantiations.isDataConstrName(name) =>
              typeArgToString(tyArg).map(suffix =>
                MultiLineExpr(
                  List(),
                  AppExpr(s"store (${cTag(name)}${suffix})")
                ): Expr
              )
            case _ => toExpr(tv)
          }
        } yield result
      case TermTApp(_, inner, _)        => toExpr(inner)
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
              for {
                isFunc <- isFunctionType(ty)
                result <- isFunc match {
                  case false => Value(v).pure[ContextState]
                  case true  =>
                    resolveTypeConstructors(ty).map { resolvedTy =>
                      val arity = getFunctionArity(ty)
                      val tKey = typeToKey(resolvedTy)
                      val inferredPtag =
                        lookupSingleClosure(tKey, arity).map(pTag(arity, _))
                      FunctionValue(v, arity, inferredPtag, tKey)
                    }
                }
              } yield result
            case (v, TermAbbBind(c: TermClosure, Some(ty))) =>
              // When referencing a TermClosure binding (from TermFix in TermLet),
              // extract free variables and create ClosureValue for partial application
              freeVars(c).map { fvs =>
                val filtered = fvs.filter(_._1 != v)
                val freeVarNames = filtered.map(_._1)
                val isFuncFlags = filtered.map(_._3)
                val arity = getClosureArity(c)
                freeVarNames match {
                  case Nil =>
                    FunctionValue(v, arity)
                  case _ =>
                    // Dummy lambda is safe: ClosureValue only used to extract name/freeVars in TermApp (not lifted)
                    ClosureValue(
                      v,
                      arity,
                      freeVarNames,
                      LambdaBinding(v, DoExpr(Value(""), 0)),
                      freeVarIsFunction = isFuncFlags
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

  // Helper: Build a plain `applyN_<paramType>_to_<retType> f x` AppExpr
  // when no specific closure tag is available (unresolved FunctionValue
  // paths). Passes an empty closure name so applyFnForClosure falls back
  // to parsing tKey directly, and then routes through the usual concrete-
  // sibling preference.
  def applyFnCall(
      f: String,
      parameter: String,
      arity: Int,
      tKey: String
  )(implicit env: Env): AppExpr =
    AppExpr(
      s"${applyFnForClosure("", arity, tKey)} $f $parameter",
      arity - 1,
      1,
      None
    )

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

    // Use closure name to look up typeKey, which contains the accurate type info
    // This ensures we use the same typeKey that was used to generate the apply function
    val effectiveTKey = closureToTypeKey.getOrElse(closureName, tKey)
    val applyFn = applyFnForClosure(closureName, arity, effectiveTKey)
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
    val clauseInfos = closures.map { cn =>
      val tag = pTag(arity, cn)
      // Determine number of fields in the P-tag pattern from ArityFact.
      // Fields = capturedVars + (totalParams - current_arity).
      // Use fixed wildcard names to avoid incrementing the global variable counter.
      val numFields = env.arityFactsMap
        .get(cn)
        .map(af => af.capturedVars + (af.totalParams - arity))
        .getOrElse(0)
      val fieldStr = (0 until numFields).map(i => s" _${f}_${cn}_$i").mkString
      val pattern = s"($tag$fieldStr)"
      val closureTypeKey = closureToTypeKey.getOrElse(cn, tKey)
      val applyFn = applyFnForClosure(cn, arity, closureTypeKey)
      (cn, tag, pattern, applyFn)
    }
    // When all closures dispatch to the same apply function, skip the do-case dispatch
    // and generate a direct apply call. This avoids GRIN's SparseCaseOptimisation from
    // simplifying the case away and leaving an invalid "do { apply f p }" structure.
    val uniqueApplyFns = clauseInfos.map(_._4).distinct
    uniqueApplyFns match {
      case List(singleApplyFn) =>
        // All closures use the same apply function - call it directly
        val call = s"$singleApplyFn $f $param"
        val firstCn = clauseInfos.head._1
        State.pure(
          AppExpr(call, remainingArity, 1, None, clauseInfos.map(_._1))
        )
      case _ =>
        val clauses = clauseInfos.map { case (cn, tag, pattern, applyFn) =>
          val call = s"$applyFn $f $param"
          CaseClause(
            pattern,
            AppExpr(call, remainingArity, 1, nextPtag(tag, arity), List(cn)),
            1
          )
        }
        State.pure(CaseExpr(Value(f), clauses))
    }
  }

  // Helper: Get closures by arity (derived from arityFactsMap)
  def getClosuresByArity(arity: Int)(implicit env: Env): List[String] =
    env.arityFactsMap.collect {
      case (name, fact) if fact.totalParams == arity => name
    }.toList

  // Helper: Lookup closures with fallback (no specialized variant expansion)
  def lookupClosures(tKey: String, arity: Int)(implicit
      env: Env
  ): List[String] =
    // Check if tKey is a comma-separated list of closures (from DoExpr dispatch extraction)
    tKey.contains(",") match {
      case true  => tKey.split(",").toList
      case false =>
        env.closureMap.getOrElse(tKey, getClosuresByArity(arity))
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
      implicit
      outerSubstFunc: String => String = identity,
      env: Env = Env.empty
  ): ContextState[Expr] = for {
    substVarsRawAll <- freeVars(c).map(_.filter(_._1 != matchName))
    substVarsRaw = substVarsRawAll.distinctBy(_._1)
    freeVarNames = substVarsRaw.map(_._1).map(outerSubstFunc)
    tempVars = substVarsRaw.map(_._2)
    isFuncFlags = substVarsRaw.map(_._3)
    // For function-typed captured vars, create separate body names for fetch results
    bodyNames <- substVarsRaw.traverse { case (_, temp, isFunc) =>
      isFunc match {
        case true  => addTempVariable(temp)
        case false => State.pure[Context, String](temp)
      }
    }
    substFunc = (variable: String) => {
      matchName == variable match {
        case false =>
          substVarsRaw
            .zip(bodyNames)
            .find(_._1._1 == variable)
            .map(_._2)
            .getOrElse(variable)
        case true => s"$outputName ${tempVars.mkString(" ")}"
      }
    }
    expr <- Context.run(toClosureAbs(c)(substFunc, env))
    // Inject fetch bindings for function-typed captured vars inside the Abs chain
    fetchBinds = substVarsRaw.zip(bodyNames).collect {
      case ((_, ptrName, true), bodyName) =>
        BindExpr(s"$bodyName <- fetch $ptrName", bodyName, List(Value(ptrName)))
    }
    bodyWithFetches = injectBindingsIntoAbs(expr, fetchBinds)
    closure = tempVars.foldRight(bodyWithFetches) { (v, term) => Abs(v, term) }
    // Compute arity from closure structure instead of type-checking
    // (avoids De Bruijn index issues when closure has stale indices from type-checking phase)
    arity = getClosureArity(c)
    // Compute typeKey from closure type - try multiple sources:
    // 1. First try closureTypesFromBind (from monomorphization) - has accurate full arrow types
    // 2. Then try type-checking the closure
    // 3. Finally fall back to structure extraction
    closureTypeFromBind = c match {
      case TermClosure(_, variable, _, _) =>
        env.closureTypesFromBind.get(variable)
      case _ => None
    }
    closureTypeOpt <- closureTypeFromBind match {
      case Some(_) => State.pure(closureTypeFromBind)
      case None    =>
        toContextStateOption(
          TypeChecker.pureInfer(c)(checking = false).map(_._1)
        ).flatMap {
          case Some(ty) => State.pure(Some(ty))
          case None     =>
            getClosureTypeWithFallback(c).map {
              case Some(ty) => Some(ty)
              case None     =>
                // Last resort: check closure types from insts
                c match {
                  case TermClosure(_, variable, _, _) =>
                    env.closureTypesFallback.get(variable)
                  case _ => None
                }
            }
        }
    }
    resolvedTypeOpt <- closureTypeOpt match {
      case Some(ty) => resolveTypeConstructors(ty).map(Some(_))
      case None     => State.pure(None)
    }
    tKey = resolvedTypeOpt.map(typeToKey).getOrElse("")
  } yield ClosureValue(
    outputName,
    arity,
    freeVarNames,
    LambdaBinding(outputName, closure),
    tKey,
    isFuncFlags
  )

  // Inject bind expressions at the innermost level of an Abs chain.
  // Traverses past all Abs wrappers (function parameters) and prepends
  // the bindings before the body expression.
  def injectBindingsIntoAbs(expr: Expr, binds: List[BindExpr]): Expr =
    binds.isEmpty match {
      case true  => expr
      case false =>
        expr match {
          case Abs(v, inner) => Abs(v, injectBindingsIntoAbs(inner, binds))
          case other         => MultiLineExpr(binds, other)
        }
    }

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
      isLastPattern: Boolean = false,
      typeArgSuffix: String = ""
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
        // For constructors whose fields contain function types, the fields
        // are stored via double indirection (heap pointer). Generate temp
        // names for the pattern and add fetch bindings to retrieve the
        // actual closure values. `containsFunctionType` unwraps recursive
        // type wrappers so recursive variants carrying closures are also
        // handled.
        variantHasFuncFields = containsFunctionType(matchExprType)
        (patternVars, fetchBindings) <- variantHasFuncFields match {
          case true =>
            bindVariables
              .traverse { varName =>
                addTempVariable().map { ptrName =>
                  (
                    ptrName,
                    BindExpr(s"$varName <- fetch $ptrName", varName, List())
                  )
                }
              }
              .map(pairs => (pairs.map(_._1), pairs.map(_._2)))
          case false =>
            (bindVariables, List.empty[BindExpr]).pure[ContextState]
        }
        cpat = s"(${cTag(node)}${typeArgSuffix} ${patternVars.mkString(" ")})"
        caseExpr <- pureToExpr(e)
        (prepExprs, parameter) <- prepParameters(caseExpr)
      } yield CaseClause(
        cpat,
        MultiLineExpr(
          fetchBindings ++ prepExprs,
          PureExpr(Value(parameter))
        )
      )
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
      case c @ ClosureValue(f, arity, freeVars, _, tKey, isFuncFlags) =>
        // Store function-typed captured vars on the heap before embedding in P-tag fields.
        // HPT cannot track node values nested inside other node fields, but it can track
        // heap locations. This prevents "Dead/unused type" errors in GRIN's LLVM codegen.
        val flags = isFuncFlags.isEmpty match {
          case true  => freeVars.map(_ => false)
          case false => isFuncFlags
        }
        for {
          storedVars <- freeVars.zip(flags).traverse { case (v, isFunc) =>
            isFunc match {
              case true =>
                addTempVariable().map(stored =>
                  (
                    stored,
                    List(
                      BindExpr(s"$stored <- store $v", stored, List(Value(v)))
                    )
                  )
                )
              case false =>
                State.pure[Context, (String, List[BindExpr])]((v, List.empty))
            }
          }
          (modifiedFreeVars, storeBindLists) = storedVars.unzip
          storeBinds = storeBindLists.flatten
          p <- addTempVariable()
          result <- prepParameters(
            BindExpr(
              s"$p <- pure (${pTag(arity, f)} ${modifiedFreeVars.mkString(" ")})",
              p,
              List(c, PartialFunValue(f, arity, freeVars.length, tKey))
            )
          )
        } yield (storeBinds ++ result._1, result._2)
      case c: CaseExpr =>
        addTempVariable().flatMap(p =>
          c.expr match {
            case MultiLineExpr(preps, result) =>
              val pureCase = CaseExpr(result, c.cases, c.indent)
              prepParameters(
                BindExpr(show"$p <- $pureCase", p, List(pureCase))
              ).map { case (binds, v) => (preps ++ binds, v) }
            case _ =>
              prepParameters(
                BindExpr(show"$p <- $c", p, List(c))
              )
          }
        )
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
      val dispatchInfo = d.expr match {
        case CaseExpr(_, clauses, _) =>
          val clauseInfo = clauses.flatMap {
            case CaseClause(pattern, AppExpr(repr, arity, _, _, closures), _)
                if arity > 0 =>
              val ptagPattern = """\(P(\d+)(\w+)""".r
              ptagPattern.findFirstMatchIn(pattern).map { m =>
                val closureName = m.group(2)
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
      dispatchInfo match {
        case Some((arity, closures)) if arity > 0 =>
          // Result is a partial application - use FunctionValue with '' suffix
          addTempVariable().map(p => {
            val varName = toPartialFunVariable(p)
            val tKey = closures.mkString(",")
            (
              List(BindExpr(show"$varName <- $d", varName, List(d))),
              FunctionValue(varName, arity, None, tKey)
            )
          })
        case _ =>
          // Final application (arity 0) - treat like AppExpr with arity 0.
          // Don't allocate a temp variable to keep counter in sync with pass 1.
          State.pure(List.empty[BindExpr], d: Expr)
      }
    case c: CaseExpr =>
      c.expr match {
        case MultiLineExpr(preps, result) =>
          val pureCase = CaseExpr(result, c.cases, c.indent)
          addTempVariable().flatMap(p =>
            getResult(BindExpr(show"$p <- $pureCase", p, List(pureCase)))
              .map { case (binds, v) => (preps ++ binds, v) }
          )
        case _ =>
          addTempVariable().flatMap(p =>
            getResult(BindExpr(show"$p <- $c", p, List(c)))
          )
      }
  }

  def toParamVariable(v: String): String = v match {
    case WildcardName                                            => ""
    case s if s.startsWith(Desugar.RecursiveFunctionParamPrefix) => ""
    case _                                                       => v
  }

  /** Distinguish a pattern-matched closure variable (which lives on the heap
    * and must go through `apply<N>_<retType>`) from a top-level / user-named
    * function (which can be called directly). `arityFactsMap` only tracks
    * closures, so top-level functions appear as `FunctionValue` with a
    * non-empty `tKey` but no entry — yet they must NOT route through apply.
    *
    * Temp/closure variables are produced by `pickFreshName` with a single
    * letter prefix + digits (e.g. `p5`, `f4`, `c12`, `_8`); user-defined
    * function names break that shape.
    *
    * TODO: replace this name-shape check with a binding-type lookup once
    * `Context.getBinding` is plumbed through to the use site.
    */
  def isKnownFunction(name: String): Boolean =
    name.length > 2 && name.exists(_.isLetter) &&
      !name.matches("^[a-z_]\\d+$")

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
      case PrimOp(ty, op)                      => mapPrimitiveToGrin(ty, op)
      case v if BuiltInPrimopNames.contains(v) => BuiltInPrimopNames(v)
      case v if v.startsWith(Desugar.RecursiveFunctionParamPrefix) =>
        val stripped = v.stripPrefix(Desugar.RecursiveFunctionParamPrefix)
        GrinReservedFunctionNames.contains(stripped) match {
          case true  => s"${stripped}_"
          case false => stripped
        }
      case v if v.startsWith(Desugar.MethodNamePrefix)   => methodToName(v)
      case v if v.startsWith(Desugar.RecordConstrPrefix) =>
        recordConstrToName(v)
      case v if GrinReservedFunctionNames.contains(v) => s"${v}_"
      case s                                          => s
    })

  def methodToName(n: String): String =
    s"${n.filter(_.isLetterOrDigit)}'"

  def recordConstrToName(n: String): String =
    n.stripPrefix(Desugar.RecordConstrPrefix)
}
