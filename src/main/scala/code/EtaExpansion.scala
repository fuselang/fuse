package code

import core.Bindings.*
import core.Terms.*
import core.Types.*
import core.Shifting
import core.Desugar
import parser.Info.*
import parser.Info.ShowInfo.ShowInfoOps

/** Eta-expansion pass: rewrites partial applications (currying) of
  * known-arity functions into explicit closure chains, so GRIN codegen only
  * ever emits saturated direct calls.
  *
  * A spine `f(a₁, …, a_k)` whose head resolves to a function of `n > k`
  * parameters becomes
  *
  * {{{
  *   let ea0 = e₁ in …                 // one let per NON-VALUE arg,
  *                                     // evaluated once, in order
  *   closure eta0 … eta{m-1} =>        // m = n - k missing params
  *     f r₁ … r_k eta0 … eta{m-1}      // saturated call
  * }}}
  *
  * In let-RHS position the closure chain is wrapped in `TermFix` (so Grin's
  * `TermFix(TermClosure)` let path lifts it to a named function) and the
  * arg-lets are hoisted outside the enclosing `TermLet`.
  *
  * Runs after `Monomorphization.replace` (all instantiations already
  * applied); generic heads (`TermTAbs`/`TermTApp`) are left untouched.
  *
  * Invariants the pass establishes: no under-applied known-head spine
  * remains in the output (codegen's direct-call precondition); terms that
  * are already saturated are rebuilt unchanged (zero churn for existing
  * programs); argument evaluation order and multiplicity are preserved
  * (non-value args are bound exactly once, at partial-application time).
  *
  * Known limitations (out of scope, pre-existing behavior kept): bare
  * function references as arguments (`hof(sum)`, zero applied args),
  * under-applied recursive self-calls (`^`-binder heads), and generic
  * functions not specialized by monomorphization.
  */
object EtaExpansion {

  /** Result of rewriting one application spine. */
  sealed trait SpineResult
  /** Under-applied spine: `argLets` (name, rhs) to wrap outermost, then the
    * closure chain. */
  case class Expanded(argLets: List[(String, Term)], closureChain: Term)
      extends SpineResult
  /** Saturated or ineligible spine, rebuilt unchanged. */
  case class Plain(term: Term) extends SpineResult

  def run(binds: List[Bind]): List[Bind] =
    binds
      .foldLeft(List.empty[Bind]) { case (processedRev, bind) =>
        val newBind = bind.b match {
          case TermAbbBind(term, ty) =>
            bind.copy(b =
              TermAbbBind(rewriteTerm(List.empty, processedRev, term), ty)
            )
          case _ => bind
        }
        newBind :: processedRev
      }
      .reverse

  /** Structural rewrite. `locals` has one entry per enclosing local binder
    * (innermost first): `Some(paramTypes)` when the binder is let-bound to a
    * closure chain of known arity, `None` otherwise. `globals` are the
    * preceding top-level binds, innermost (most recent) first. */
  def rewriteTerm(
      locals: List[Option[List[Option[Type]]]],
      globals: List[Bind],
      term: Term
  ): Term =
    term match {
      case app: TermApp =>
        rewriteSpine(locals, globals, app, List.empty) match {
          case Expanded(argLets, closureChain) =>
            wrapArgLets(argLets, closureChain)
          case Plain(t) => t
        }
      case TermLet(info, x, t1, t2) =>
        val (argLets, rhs) = rewriteLetRhs(locals, globals, x, t1)
        val tracking = closureParamsOf(locals, globals, rhs)
        val t2Rewritten = rewriteTerm(tracking :: locals, globals, t2)
        val t2Shifted = argLets.length match {
          case 0 => t2Rewritten
          case j => Shifting.termShiftAbove(j, 1, t2Rewritten)
        }
        wrapArgLets(argLets, TermLet(info, x, rhs, t2Shifted))
      case TermAbs(info, i, ty, e, r) =>
        TermAbs(info, i, ty, rewriteTerm(None :: locals, globals, e), r)
      case TermClosure(info, i, ty, e) =>
        TermClosure(info, i, ty, rewriteTerm(None :: locals, globals, e))
      case TermTAbs(info, i, cls, e) =>
        TermTAbs(info, i, cls, rewriteTerm(None :: locals, globals, e))
      case TermFix(info, t) =>
        TermFix(info, rewriteTerm(locals, globals, t))
      case TermMatch(info, t, cases) =>
        TermMatch(
          info,
          rewriteTerm(locals, globals, t),
          cases.map((p, e) =>
            (
              p,
              rewriteTerm(
                List.fill(patternArity(p))(None) ::: locals,
                globals,
                e
              )
            )
          )
        )
      case TermProj(info, t, i) =>
        TermProj(info, rewriteTerm(locals, globals, t), i)
      case TermMethodProj(info, t, i) =>
        TermMethodProj(info, rewriteTerm(locals, globals, t), i)
      case TermRecord(info, v) =>
        TermRecord(info, v.map((i, t) => (i, rewriteTerm(locals, globals, t))))
      case TermTag(info, i, t, ty) =>
        TermTag(info, i, rewriteTerm(locals, globals, t), ty)
      case TermAscribe(info, t, ty) =>
        TermAscribe(info, rewriteTerm(locals, globals, t), ty)
      case TermTApp(info, t, ty) =>
        TermTApp(info, rewriteTerm(locals, globals, t), ty)
      case t => t
    }

  /** Rewrite a let RHS: an under-applied spine becomes a fix-wrapped
    * closure chain with its arg-lets returned for hoisting outside the
    * enclosing let. The chain is prefixed with a `^`-named self binder
    * (never referenced — the chain shifts by 1 to cross it) to mirror the
    * desugared recursive let-lambda shape `TermFix(TermClosure(^x, None,
    * …))`: Grin's fix let-path renders the `^` binder as a zero-width
    * param, and the unannotated outer closure makes its type recovery fall
    * through to full inference instead of the annotation-only extractor
    * (whose Unit return-type placeholder would poison the let type). */
  def rewriteLetRhs(
      locals: List[Option[List[Option[Type]]]],
      globals: List[Bind],
      letVar: String,
      t1: Term
  ): (List[(String, Term)], Term) =
    t1 match {
      case app: TermApp =>
        rewriteSpine(locals, globals, app, List.empty) match {
          case Expanded(argLets, closureChain) =>
            val selfChain = TermClosure(
              app.info,
              Desugar.toRecAbsId(letVar),
              None,
              Shifting.termShift(1, closureChain)
            )
            (argLets, TermFix(app.info, selfChain))
          case Plain(t) => (List.empty, t)
        }
      case _ => (List.empty, rewriteTerm(locals, globals, t1))
    }

  /** Walk down a spine collecting (application info, rewritten arg) pairs,
    * innermost arg first in `args` order. */
  def rewriteSpine(
      locals: List[Option[List[Option[Type]]]],
      globals: List[Bind],
      term: Term,
      args: List[(Info, Term)]
  ): SpineResult =
    term match {
      case TermApp(info, f, a) =>
        rewriteSpine(
          locals,
          globals,
          f,
          (info, rewriteTerm(locals, globals, a)) :: args
        )
      case head @ TermVar(_, idx, _) =>
        headParams(locals, globals, idx) match {
          case Some(params)
              if args.nonEmpty && args.length < params.length =>
            expand(head, params, args, locals.length + globals.length)
          case _ => Plain(rebuildSpine(head, args))
        }
      case head =>
        Plain(rebuildSpine(rewriteTerm(locals, globals, head), args))
    }

  /** Rebuild a spine with its original application infos. */
  def rebuildSpine(head: Term, args: List[(Info, Term)]): Term =
    args.foldLeft(head) { case (acc, (info, arg)) => TermApp(info, acc, arg) }

  /** The core rewrite. `head` applied to `k = args.length`
    * args with `n = params.length > k`: `j` arg-lets for the non-value args,
    * `m = n - k` closure binders, saturated call in the body. `ctxLen` is
    * the context length at the spine site (for generated `TermVar`s). */
  def expand(
      head: TermVar,
      params: List[Option[Type]],
      args: List[(Info, Term)],
      ctxLen: Int
  ): Expanded = {
    val k = args.length
    val n = params.length
    val m = n - k
    val j = args.count((_, a) => !isValueTerm(a))
    val info = head.info

    // Arg references as seen from the closure body (under j lets + m
    // closure binders): non-value arg #p (0-based, left to right) points at
    // its let binder; value args are inlined, shifted past the new binders.
    val argRefs = args
      .foldLeft((List.empty[(Info, Term)], 0)) {
        case ((refs, p), (appInfo, a)) =>
          isValueTerm(a) match {
            case true =>
              (refs :+ (appInfo, Shifting.termShift(j + m, a)), p)
            case false =>
              val ref = TermVar(a.info, m + (j - 1 - p), ctxLen + j + m)
              (refs :+ (appInfo, ref), p + 1)
          }
      }
      ._1
    val etaParamRefs = (0 until m).toList.map(i =>
      (info, TermVar(info, m - 1 - i, ctxLen + j + m))
    )
    val body =
      rebuildSpine(Shifting.termShift(j + m, head), argRefs ::: etaParamRefs)

    // m closure binders, outermost first; annotate only index-free
    // primitive types (others would need def-site → use-site index
    // adjustment; Grin's type inference recovers them).
    val closureChain = params
      .drop(k)
      .zipWithIndex
      .foldRight(body) { case ((ty, i), acc) =>
        TermClosure(info, s"eta$i", ty.flatMap(primitiveType), acc)
      }

    // One let per non-value arg, outermost first; arg #p sits under the p
    // previous arg-let binders.
    val argLets = args
      .filter((_, a) => !isValueTerm(a))
      .zipWithIndex
      .map { case ((_, a), p) => (s"etaArg$p", Shifting.termShift(p, a)) }

    Expanded(argLets, closureChain)
  }

  def wrapArgLets(argLets: List[(String, Term)], inner: Term): Term =
    argLets.foldRight(inner) { case ((name, rhs), acc) =>
      TermLet(rhs.info, name, rhs, acc)
    }

  /** Known parameter list of a spine head, if any: a local tracked as a
    * closure chain, or a preceding top-level non-generic function bind. */
  def headParams(
      locals: List[Option[List[Option[Type]]]],
      globals: List[Bind],
      idx: Int
  ): Option[List[Option[Type]]] =
    idx < locals.length match {
      case true  => locals(idx)
      case false =>
        globals.lift(idx - locals.length).flatMap(_.b match {
          case TermAbbBind(t, _) => funcParams(t)
          case _                 => None
        })
    }

  /** Parameter types of a top-level function bind, from its TERM shape
    * (never from its type: arrow-depth counting would over-count functions
    * that return functions, e.g. `make_adder : i32 -> i32 -> i32` with one
    * real parameter). Desugared shape: `TermFix(TermAbs(^name, funcTy,
    * <param abs chain>))`. Generic functions (any `TermTAbs` in the chain
    * position) yield `None`. */
  def funcParams(t: Term): Option[List[Option[Type]]] =
    t match {
      case TermFix(_, TermAbs(_, name, _, body, _))
          if name.startsWith(Desugar.RecursiveFunctionParamPrefix) =>
        someIfNonEmpty(absChainParams(body))
      case abs: TermAbs =>
        someIfNonEmpty(absChainParams(abs))
      case _ => None
    }

  def someIfNonEmpty(
      params: List[Option[Type]]
  ): Option[List[Option[Type]]] =
    params match {
      case Nil => None
      case ps  => Some(ps)
    }

  def absChainParams(t: Term): List[Option[Type]] =
    t match {
      case TermAbs(_, _, ty, e, _) => primitiveType(ty) :: absChainParams(e)
      case _                       => List.empty
    }

  /** Closure-chain parameter tracking for a rewritten let RHS. Skips the
    * `^`-prefixed recursive binder of desugared recursive let-lambdas;
    * resolves aliases of tracked vars. */
  def closureParamsOf(
      locals: List[Option[List[Option[Type]]]],
      globals: List[Bind],
      rhs: Term
  ): Option[List[Option[Type]]] =
    rhs match {
      case TermFix(_, c: TermClosure)
          if c.i.startsWith(Desugar.RecursiveFunctionParamPrefix) =>
        closChainParams(c.e)
      case TermFix(_, c: TermClosure)  => closChainParams(c)
      case c: TermClosure              => closChainParams(c)
      case TermLet(_, _, _, body)      =>
        closureParamsOf(None :: locals, globals, body)
      case TermVar(_, idx, _)          => headParams(locals, globals, idx)
      case _                           => None
    }

  def closChainParams(t: Term): Option[List[Option[Type]]] =
    t match {
      case c: TermClosure =>
        Some(collectClosParams(c).map(_.flatMap(primitiveType)))
      case _ => None
    }

  def collectClosParams(t: Term): List[Option[Type]] =
    t match {
      case TermClosure(_, _, ty, e) => ty :: collectClosParams(e)
      case _                        => List.empty
    }

  /** Value terms: evaluating them is pure and total, so
    * they may be inlined into the closure body; anything else must be
    * let-bound at partial-application time (evaluated exactly once).
    * Closure literals count as values — constructing one is pure, each arg
    * occurs exactly once in the expanded body, and let-binding one would
    * produce a bare-`TermClosure` let RHS, a shape Grin's let path cannot
    * type (its annotation-only extractor fabricates a `Unit` return). */
  def isValueTerm(t: Term): Boolean =
    t match {
      case _: TermVar | _: TermInt | _: TermFloat | _: TermString |
          _: TermTrue | _: TermFalse | _: TermUnit | _: TermClosure =>
        true
      case _ => false
    }

  /** Index-free primitive types are safe to copy from def site to use site;
    * anything with De Bruijn content is dropped (Grin re-infers). */
  def primitiveType(ty: Type): Option[Type] =
    ty match {
      case _: TypeInt | _: TypeString | _: TypeBool | _: TypeFloat |
          _: TypeUnit =>
        Some(ty)
      case _ => None
    }
}
