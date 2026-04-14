package core

import cats.Monad
import cats.implicits.*
import core.Terms.*
import core.Types.*
import parser.Info.Info

object TermFold {

  /** Algebra for folding over the Term ADT. Each method corresponds to one Term
    * variant. The `depth` parameter tracks binder depth (incremented under
    * TermAbs, TermTAbs, TermClosure, TermLet body).
    *
    * Leaf terms (literals, TermBuiltin, TermClassMethod, TermFold) are handled
    * by `onLeaf`.
    */
  trait TermAlgebra[F[_], A] {
    def onVar(info: Info, idx: Int, ctxLen: Int, depth: Int): F[A]
    def onAbs(
        info: Info,
        name: String,
        ty: Type,
        body: A,
        retTy: Option[Type],
        depth: Int
    ): F[A]
    def onClosure(
        info: Info,
        name: String,
        ty: Option[Type],
        body: A,
        depth: Int
    ): F[A]
    def onApp(info: Info, f: A, arg: A, depth: Int): F[A]
    def onFix(info: Info, t: A, depth: Int): F[A]
    def onMatch(
        info: Info,
        scrutinee: A,
        cases: List[(Pattern, A)],
        depth: Int
    ): F[A]
    def onLet(info: Info, name: String, t1: A, t2: A, depth: Int): F[A]
    def onProj(info: Info, t: A, label: String, depth: Int): F[A]
    def onMethodProj(info: Info, t: A, method: String, depth: Int): F[A]
    def onAssocProj(info: Info, ty: Type, method: String, depth: Int): F[A]
    def onRecord(info: Info, fields: List[(String, A)], depth: Int): F[A]
    def onTag(info: Info, label: String, t: A, ty: Type, depth: Int): F[A]
    def onAscribe(info: Info, t: A, ty: Type, depth: Int): F[A]
    def onTAbs(
        info: Info,
        name: String,
        cls: List[TypeClass],
        body: A,
        depth: Int
    ): F[A]
    def onTApp(info: Info, t: A, ty: Type, depth: Int): F[A]
    def onLeaf(term: Term, depth: Int): F[A]
  }

  /** Fold a Term into a value of type A using the provided algebra. */
  def foldTerm[F[_]: Monad, A](
      algebra: TermAlgebra[F, A],
      depth: Int,
      term: Term
  ): F[A] = {
    def go(d: Int, t: Term): F[A] = t match {
      case TermVar(info, idx, ctxLen) =>
        algebra.onVar(info, idx, ctxLen, d)
      case TermAbs(info, name, ty, body, retTy) =>
        go(d + 1, body).flatMap(b => algebra.onAbs(info, name, ty, b, retTy, d))
      case TermClosure(info, name, ty, body) =>
        go(d + 1, body).flatMap(b => algebra.onClosure(info, name, ty, b, d))
      case TermApp(info, f, arg) =>
        (go(d, f), go(d, arg)).flatMapN((fv, av) =>
          algebra.onApp(info, fv, av, d)
        )
      case TermFix(info, t) =>
        go(d, t).flatMap(v => algebra.onFix(info, v, d))
      case TermMatch(info, scrutinee, cases) =>
        for {
          sv <- go(d, scrutinee)
          cv <- cases.traverse { case (pattern, body) =>
            go(d + patternArity(pattern), body).map(bv => (pattern, bv))
          }
          result <- algebra.onMatch(info, sv, cv, d)
        } yield result
      case TermLet(info, name, t1, t2) =>
        (go(d, t1), go(d + 1, t2)).flatMapN((v1, v2) =>
          algebra.onLet(info, name, v1, v2, d)
        )
      case TermProj(info, t, label) =>
        go(d, t).flatMap(v => algebra.onProj(info, v, label, d))
      case TermMethodProj(info, t, method) =>
        go(d, t).flatMap(v => algebra.onMethodProj(info, v, method, d))
      case TermAssocProj(info, ty, method) =>
        algebra.onAssocProj(info, ty, method, d)
      case TermRecord(info, fields) =>
        fields
          .traverse { case (label, t) =>
            go(d, t).map(v => (label, v))
          }
          .flatMap(fv => algebra.onRecord(info, fv, d))
      case TermTag(info, label, t, ty) =>
        go(d, t).flatMap(v => algebra.onTag(info, label, v, ty, d))
      case TermAscribe(info, t, ty) =>
        go(d, t).flatMap(v => algebra.onAscribe(info, v, ty, d))
      case TermTAbs(info, name, cls, body) =>
        go(d + 1, body).flatMap(b => algebra.onTAbs(info, name, cls, b, d))
      case TermTApp(info, t, ty) =>
        go(d, t).flatMap(v => algebra.onTApp(info, v, ty, d))
      case leaf =>
        algebra.onLeaf(leaf, d)
    }
    go(depth, term)
  }

  /** Convenience: fold starting at depth 0. */
  def fold[F[_]: Monad, A](algebra: TermAlgebra[F, A], term: Term): F[A] =
    foldTerm(algebra, 0, term)

  /** A TermAlgebra that reconstructs the term (identity transform). Override
    * specific methods to implement targeted rewrites without manual recursion.
    */
  trait TermTransform[F[_]] extends TermAlgebra[F, Term] {
    given monad: Monad[F] = compiletime.deferred

    def onVar(info: Info, idx: Int, ctxLen: Int, depth: Int): F[Term] =
      (TermVar(info, idx, ctxLen): Term).pure[F]
    def onAbs(
        info: Info,
        name: String,
        ty: Type,
        body: Term,
        retTy: Option[Type],
        depth: Int
    ): F[Term] =
      (TermAbs(info, name, ty, body, retTy): Term).pure[F]
    def onClosure(
        info: Info,
        name: String,
        ty: Option[Type],
        body: Term,
        depth: Int
    ): F[Term] =
      (TermClosure(info, name, ty, body): Term).pure[F]
    def onApp(info: Info, f: Term, arg: Term, depth: Int): F[Term] =
      (TermApp(info, f, arg): Term).pure[F]
    def onFix(info: Info, t: Term, depth: Int): F[Term] =
      (TermFix(info, t): Term).pure[F]
    def onMatch(
        info: Info,
        scrutinee: Term,
        cases: List[(Pattern, Term)],
        depth: Int
    ): F[Term] =
      (TermMatch(info, scrutinee, cases): Term).pure[F]
    def onLet(
        info: Info,
        name: String,
        t1: Term,
        t2: Term,
        depth: Int
    ): F[Term] =
      (TermLet(info, name, t1, t2): Term).pure[F]
    def onProj(info: Info, t: Term, label: String, depth: Int): F[Term] =
      (TermProj(info, t, label): Term).pure[F]
    def onMethodProj(
        info: Info,
        t: Term,
        method: String,
        depth: Int
    ): F[Term] =
      (TermMethodProj(info, t, method): Term).pure[F]
    def onAssocProj(info: Info, ty: Type, method: String, depth: Int): F[Term] =
      (TermAssocProj(info, ty, method): Term).pure[F]
    def onRecord(
        info: Info,
        fields: List[(String, Term)],
        depth: Int
    ): F[Term] =
      (TermRecord(info, fields): Term).pure[F]
    def onTag(
        info: Info,
        label: String,
        t: Term,
        ty: Type,
        depth: Int
    ): F[Term] =
      (TermTag(info, label, t, ty): Term).pure[F]
    def onAscribe(info: Info, t: Term, ty: Type, depth: Int): F[Term] =
      (TermAscribe(info, t, ty): Term).pure[F]
    def onTAbs(
        info: Info,
        name: String,
        cls: List[TypeClass],
        body: Term,
        depth: Int
    ): F[Term] =
      (TermTAbs(info, name, cls, body): Term).pure[F]
    def onTApp(info: Info, t: Term, ty: Type, depth: Int): F[Term] =
      (TermTApp(info, t, ty): Term).pure[F]
    def onLeaf(term: Term, depth: Int): F[Term] =
      term.pure[F]
  }

  /** Check if a term contains a TermAssocProj call to a specific method. */
  def containsAssocProj(term: Term, methodName: String): Boolean = {
    import cats.Id
    val cleanName = methodName.startsWith(
      core.Desugar.MethodNamePrefix
    ) match {
      case true =>
        methodName
          .drop(core.Desugar.MethodNamePrefix.length)
          .takeWhile(_ != '#')
      case false => methodName
    }
    val algebra = new TermAlgebra[Id, Boolean] {
      def onVar(info: Info, idx: Int, ctxLen: Int, depth: Int): Boolean = false
      def onAbs(
          info: Info,
          name: String,
          ty: Type,
          body: Boolean,
          retTy: Option[Type],
          depth: Int
      ): Boolean = body
      def onClosure(
          info: Info,
          name: String,
          ty: Option[Type],
          body: Boolean,
          depth: Int
      ): Boolean = body
      def onApp(info: Info, f: Boolean, arg: Boolean, depth: Int): Boolean =
        f || arg
      def onFix(info: Info, t: Boolean, depth: Int): Boolean = t
      def onMatch(
          info: Info,
          scrutinee: Boolean,
          cases: List[(Pattern, Boolean)],
          depth: Int
      ): Boolean =
        scrutinee || cases.exists(_._2)
      def onLet(
          info: Info,
          name: String,
          t1: Boolean,
          t2: Boolean,
          depth: Int
      ): Boolean = t1 || t2
      def onProj(info: Info, t: Boolean, label: String, depth: Int): Boolean = t
      def onMethodProj(
          info: Info,
          t: Boolean,
          method: String,
          depth: Int
      ): Boolean = t
      def onAssocProj(
          info: Info,
          ty: Type,
          method: String,
          depth: Int
      ): Boolean =
        method == cleanName
      def onRecord(
          info: Info,
          fields: List[(String, Boolean)],
          depth: Int
      ): Boolean =
        fields.exists(_._2)
      def onTag(
          info: Info,
          label: String,
          t: Boolean,
          ty: Type,
          depth: Int
      ): Boolean = t
      def onAscribe(
          info: Info,
          t: Boolean,
          ty: Type,
          depth: Int
      ): Boolean = t
      def onTAbs(
          info: Info,
          name: String,
          cls: List[TypeClass],
          body: Boolean,
          depth: Int
      ): Boolean = body
      def onTApp(
          info: Info,
          t: Boolean,
          ty: Type,
          depth: Int
      ): Boolean = t
      def onLeaf(term: Term, depth: Int): Boolean = false
    }
    fold[Id, Boolean](algebra, term)
  }

  /** Collect TermVars that refer to bindings outside the current term (idx >=
    * local binder depth). Pattern-bound and lambda-bound vars are excluded.
    *
    * Callers that look up the collected TermVars' names in the Context rely on
    * this depth filter — a local-param TermVar would resolve to whatever
    * binding happens to sit at the ctx position of its local depth, not to a
    * bind.
    */
  def collectBindTermVars(term: Term): List[TermVar] = {
    import cats.Id
    val algebra = new TermAlgebra[Id, List[TermVar]] {
      def onVar(
          info: Info,
          idx: Int,
          ctxLen: Int,
          depth: Int
      ): List[TermVar] =
        (idx >= depth) match {
          case true  => List(TermVar(info, idx, ctxLen))
          case false => Nil
        }
      def onAbs(
          info: Info,
          name: String,
          ty: Type,
          body: List[TermVar],
          retTy: Option[Type],
          depth: Int
      ): List[TermVar] = body
      def onClosure(
          info: Info,
          name: String,
          ty: Option[Type],
          body: List[TermVar],
          depth: Int
      ): List[TermVar] = body
      def onApp(
          info: Info,
          f: List[TermVar],
          arg: List[TermVar],
          depth: Int
      ): List[TermVar] = f ++ arg
      def onFix(
          info: Info,
          t: List[TermVar],
          depth: Int
      ): List[TermVar] = t
      def onMatch(
          info: Info,
          scrutinee: List[TermVar],
          cases: List[(Pattern, List[TermVar])],
          depth: Int
      ): List[TermVar] =
        scrutinee ++ cases.flatMap(_._2)
      def onLet(
          info: Info,
          name: String,
          t1: List[TermVar],
          t2: List[TermVar],
          depth: Int
      ): List[TermVar] = t1 ++ t2
      def onProj(
          info: Info,
          t: List[TermVar],
          label: String,
          depth: Int
      ): List[TermVar] = t
      def onMethodProj(
          info: Info,
          t: List[TermVar],
          method: String,
          depth: Int
      ): List[TermVar] = t
      def onAssocProj(
          info: Info,
          ty: Type,
          method: String,
          depth: Int
      ): List[TermVar] = Nil
      def onRecord(
          info: Info,
          fields: List[(String, List[TermVar])],
          depth: Int
      ): List[TermVar] = fields.flatMap(_._2)
      def onTag(
          info: Info,
          label: String,
          t: List[TermVar],
          ty: Type,
          depth: Int
      ): List[TermVar] = t
      def onAscribe(
          info: Info,
          t: List[TermVar],
          ty: Type,
          depth: Int
      ): List[TermVar] = t
      def onTAbs(
          info: Info,
          name: String,
          cls: List[TypeClass],
          body: List[TermVar],
          depth: Int
      ): List[TermVar] = body
      def onTApp(
          info: Info,
          t: List[TermVar],
          ty: Type,
          depth: Int
      ): List[TermVar] = t
      def onLeaf(term: Term, depth: Int): List[TermVar] = Nil
    }
    fold[Id, List[TermVar]](algebra, term)
  }

  /** True if `term` contains a TermVar with both the given Info and idx. */
  def findVarByInfoAndIdx(
      term: Term,
      targetInfo: Info,
      targetIdx: Int
  ): Boolean = {
    def search(t: Term): Boolean = t match {
      case TermVar(info, idx, _)      => info == targetInfo && idx == targetIdx
      case TermAbs(_, _, _, body, _)  => search(body)
      case TermTAbs(_, _, _, body)    => search(body)
      case TermClosure(_, _, _, body) => search(body)
      case TermLet(_, _, t1, t2)      => search(t1) || search(t2)
      case TermFix(_, body)           => search(body)
      case TermApp(_, f, arg)         => search(f) || search(arg)
      case TermTApp(_, t, _)          => search(t)
      case TermMatch(_, scrutinee, cases) =>
        search(scrutinee) || cases.exists { case (_, body) => search(body) }
      case TermProj(_, t, _)       => search(t)
      case TermMethodProj(_, t, _) => search(t)
      case TermRecord(_, fields)   => fields.exists { case (_, t) => search(t) }
      case TermTag(_, _, t, _)     => search(t)
      case TermAscribe(_, t, _)    => search(t)
      case _                       => false
    }
    search(term)
  }

  /** Find the raw De Bruijn index, context length, and depth of a TermVar
    * matching a given Info.
    */
  def findVarByInfo(
      term: Term,
      targetInfo: Info
  ): Option[(Int, Int, Int)] = {
    // Use a direct recursive search for short-circuit behavior
    def search(t: Term, c: Int): Option[(Int, Int, Int)] = t match {
      case TermVar(info, idx, n) =>
        (info == targetInfo) match {
          case true  => Some((idx, n, c))
          case false => None
        }
      case TermAbs(_, _, _, body, _)  => search(body, c + 1)
      case TermTAbs(_, _, _, body)    => search(body, c + 1)
      case TermClosure(_, _, _, body) => search(body, c + 1)
      case TermLet(_, _, t1, t2)      => search(t1, c).orElse(search(t2, c + 1))
      case TermFix(_, body)           => search(body, c)
      case TermApp(_, f, arg)         => search(f, c).orElse(search(arg, c))
      case TermTApp(_, t, _)          => search(t, c)
      case TermMatch(_, scrutinee, cases) =>
        search(scrutinee, c).orElse(
          cases.view.flatMap { case (p, body) =>
            search(body, c + patternArity(p))
          }.headOption
        )
      case TermProj(_, t, _)       => search(t, c)
      case TermMethodProj(_, t, _) => search(t, c)
      case TermRecord(_, fields)   =>
        fields.view.flatMap { case (_, t) => search(t, c) }.headOption
      case TermTag(_, _, t, _)  => search(t, c)
      case TermAscribe(_, t, _) => search(t, c)
      case _                    => None
    }
    search(term, 0)
  }

  /** Apply specialized closure types to closure parameters in a term. */
  def applyClosureTypes(
      term: Term,
      closureInsts: List[core.Instantiations.Instantiation]
  ): Term = {
    import cats.Id
    val algebra = new TermTransform[Id] {
      override given monad: Monad[Id] = cats.catsInstancesForId

      override def onClosure(
          info: Info,
          name: String,
          ty: Option[Type],
          body: Term,
          depth: Int
      ): Id[Term] =
        ty match {
          case None =>
            closureInsts.find(_.i == name) match {
              case Some(inst) if inst.tys.nonEmpty =>
                val paramType = inst.tys.head match {
                  case TypeArrow(_, param, _) => param
                  case t                      => t
                }
                TermClosure(info, name, Some(paramType), body)
              case _ =>
                TermClosure(info, name, None, body)
            }
          case _ => TermClosure(info, name, ty, body)
        }
    }
    fold[Id, Term](algebra, term)
  }
}
