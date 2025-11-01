package code

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
import core.BuiltIn

object GrinUtils {
  val dummyType = TypeUnit(UnknownInfo)

  def cTag(tag: String) = s"C$tag"

  def pTag(arity: Int, tag: String) = s"P$arity$tag"

  def isNodeValue(t: Term): ContextState[Boolean] = typeCheck(t)
    .flatMap(TypeChecker.isRecursiveType(_))

  def isFunctionType(ty: Type): ContextState[Boolean] = for {
    tyS <- TypeChecker.simplifyType(ty)
    value = getFunctionArity(tyS) match {
      case v if v > 0 => true
      case _          => false
    }
  } yield value

  /** Check if a type requires heap allocation (store/fetch semantics).
    * User-defined types (records, variants, recursive types) are heap-allocated.
    */
  def isHeapAllocatedType(ty: Type): Boolean = ty match {
    case _: TypeRec => true     // Recursive types
    case _: TypeVariant => true // Sum types
    case _: TypeRecord => true  // Product types
    case _ => false
  }

  def getFunctionArity(ty: Type): Int = ty match {
    case TypeAbs(_, _, a: TypeArrow)   => getFunctionArity(a)
    case TypeArrow(_, _, a: TypeArrow) => 1 + getFunctionArity(a)
    case TypeArrow(_, _, _)            => 1
    case _                             => 0
  }

  /** Normalize a function type to a string key for closure map lookup.
    * This allows distinguishing closures with same arity but different types.
    * e.g., "i32 -> str" vs "i32 -> Option[i32]"
    */
  def typeToKey(ty: Type): String = ty match {
    case TypeArrow(_, param, result) =>
      s"${typeToKey(param)}->${typeToKey(result)}"
    case TypeAbs(_, _, body) =>
      typeToKey(body)
    case TypeApp(_, constructor, arg) =>
      // For TypeApp, normalize to "Constructor[Arg]" format
      val constructorKey = typeToKey(constructor)
      val argKey = typeToKey(arg)
      s"$constructorKey[$argKey]"
    case TypeVar(_, idx, _) =>
      // TypeVars are problematic - just use a generic placeholder based on index
      // This will likely cause mismatches, but better than crashing
      s"T$idx"
    case TypeId(_, name) =>
      name
    case TypeInt(_) => "i32"
    case TypeFloat(_) => "f32"
    case TypeString(_) => "str"
    case TypeBool(_) => "bool"
    case TypeUnit(_) => "unit"
    case TypeRec(_, name, _, _) => name
    case TypeRecord(_, fields) =>
      fields.map { case (n, t) => s"$n:${typeToKey(t)}" }.mkString("{", ",", "}")
    case TypeVariant(_, variants) =>
      variants.map { case (n, t) => s"$n:${typeToKey(t)}" }.mkString("[", "|", "]")
    case TypeAll(_, name, _, _, body) =>
      s"forall $name.${typeToKey(body)}"
    case _ => ty.getClass.getSimpleName
  }

  /** Type-to-key that simplifies the type first to resolve TypeVars */
  def typeToKeySimplified(ty: Type): ContextState[String] = for {
    simplified <- TypeChecker.simplifyType(ty)
  } yield typeToKey(simplified)

  /** Get closure arity without type-checking (avoids De Bruijn index issues) */
  def getClosureArity(c: Term): Int = c match {
    case TermClosure(_, _, _, body) => 1 + getClosureArity(body)
    case _                           => 0
  }

  /** Extract closure type from parameter annotations only.
    * Returns a type built from parameter type annotations.
    * Uses Unit as placeholder for return type since we only need parameter types for typeKey.
    * For closures without annotations, returns None to rely on arity-based fallback.
    */
  def getClosureType(c: Term): ContextState[Option[Type]] = c match {
    case TermFix(_, body) =>
      // Recursive closure wrapped in fixpoint
      getClosureType(body)
    case TermClosure(_, _, Some(paramType), body) =>
      // Extract inner closure type (if body is also a closure)
      getClosureType(body).map {
        case Some(bodyType) =>
          // Inner closure - combine types
          Some(TypeArrow(UnknownInfo, paramType, bodyType))
        case None =>
          // Innermost closure - use Unit as placeholder for return type
          Some(TypeArrow(UnknownInfo, paramType, TypeUnit(UnknownInfo)))
      }
    case TermClosure(_, _, None, _) =>
      // No type annotation - return None and rely on arity-based fallback
      State.pure(None)
    case _ =>
      // Not a closure
      State.pure(None)
  }

  def freeVars(term: Term): ContextState[List[(String, String)]] =
    Context.run(extractFreeVars(term))

  def extractFreeVars(term: Term): ContextState[List[(String, String)]] =
    term match {
      case TermVar(info, idx, ctxLength) =>
        toContextState(Context.getBinding(info, idx))
          .flatMap(_ match {
            case VarBind(_) =>
              getNameFromIndex(idx).flatMap(v =>
                addTempVariable(v).map { p => List((v, p)) }
              )
            // TODO: Capture constructors/functions in closures to avoid De Bruijn index issues
            // Currently TermAbbBind captures cause wrong constructors due to index corruption
            case _ =>
              State.pure(Nil)
          })
      case TermClosure(_, variable, _, e) =>
        for {
          _ <- Context.addName(variable)
          v <- freeVars(e)
        } yield v
      case TermMatch(_, t, c) =>
        for {
          v1 <- freeVars(t)
          v2 <- c.traverse { case (p, e) =>
            p match {
              case PatternNode(_, node, vars) =>
                for {
                  _ <- vars.traverse(Context.addName(_))
                  v <- freeVars(e)
                } yield v
              case _ => freeVars(e)
            }
          }
        } yield v1 :++ v2.flatten
      case TermLet(_, variable, t1, t2) =>
        for {
          v1 <- freeVars(t1)
          _ <- Context.addName(variable)
          v2 <- freeVars(t2)
        } yield v1 :++ v2
      case TermApp(_, t1, t2) =>
        for {
          v1 <- freeVars(t1)
          v2 <- freeVars(t2)
        } yield v1 :++ v2
      case TermProj(_, t, _)       => freeVars(t)
      case TermMethodProj(_, t, _) => freeVars(t)
      case TermAscribe(_, t, _)    => freeVars(t)
      case TermTAbs(_, _, _, t)    => freeVars(t)
      case TermTApp(_, t, _)       => freeVars(t)
      case _                       => State.pure(Nil)
    }

  def addTempVariable(name: String = "p"): ContextState[String] =
    pickFreshName(name)

  // Retrieve a name of the passed `ty` param type.
  def getNameFromType(ty: Type): ContextState[String] = ty match {
    // For TypeApp like Option[A], extract the root type constructor
    case TypeApp(_, ty1, _) => getNameFromType(ty1)
    // For TypeVar, get the name from context
    case typeVar: TypeVar => getNameFromIndex(typeVar.index)
    // For TypeId (resolved type constructor), return the name directly
    case TypeId(_, name) => State.pure(name)
    // For other types, try to find root TypeVar
    case _ =>
      TypeChecker.findRootTypeVar(ty)
        .map(typeVar => getNameFromIndex(typeVar.index))
        .getOrElse(getTypeFallbackName(ty))
  }

  private def getTypeFallbackName(ty: Type): ContextState[String] = ty match {
    case TypeArrow(_, _, _) => State.pure("Function")
    case TypeAll(_, _, _, _, _) => State.pure("Generic")
    case TypeId(_, name) => State.pure(name)
    case _ => State.pure(s"UnknownType_${ty.getClass.getSimpleName}")
  }

  def getNameFromIndex(idx: Int): ContextState[String] =
    State.inspect { ctx =>
      Context.indexToName(ctx, idx).getOrElse(s"UnknownIndex_$idx")
    }

  def typeCheck(term: Term): ContextState[Type] =
    toContextState(TypeChecker.pureInfer(term)(checking = false).map(_._1))

  def toContextState[T](stateEither: StateEither[T]): ContextState[T] =
    stateEither.value.map(v =>
      v match {
        case Right(v) => v
        case Left(e) => throw new RuntimeException(e)
      }
    )

  def toContextStateOption[T](
      stateEither: StateEither[T]
  ): ContextState[Option[T]] =
    stateEither.value.map(v =>
      v match {
        case Right(v) => Some(v)
        case Left(e)  => None
      }
    )

  case class PrimOp(t: String, op: String)

  /** Get type of specialized terms without re-inferring */
  def getSpecializedTermType(term: Term): ContextState[Option[Type]] = term match {
    case TermApp(_, fun, _) => getSpecializedTermType(fun).map(_.flatMap {
      case TypeArrow(_, _, res) => Some(res)
      case _ => None
    })
    case TermMethodProj(_, _, m) if fuse.SpecializedMethodUtils.isSpecializedMethod(m) =>
      for {
        idx <- State.inspect[Context, Option[Int]](nameToIndex(_, m))
        result <- idx.fold(State.pure[Context, Option[Type]](None))(i =>
          toContextStateOption(getBinding(parser.Info.UnknownInfo, i)).map(_.flatMap {
            case TermAbbBind(_, Some(ty)) => Some(ty)
            case _ => None
          })
        )
      } yield result
    case TermVar(_, idx, _) =>
      // Handle specialized data constructors (e.g., Cons#i32, Nil#i32)
      for {
        nameOpt <- State.inspect[Context, Option[String]](indexToName(_, idx))
        result <- nameOpt match {
          case Some(name) if name.contains("#") =>
            toContextStateOption(getBinding(parser.Info.UnknownInfo, idx)).map(_.flatMap {
              case TermAbbBind(_, Some(ty)) => Some(ty)
              case _ => None
            })
          case _ => State.pure(None)
        }
      } yield result
    case _ => State.pure(None)
  }

  object PrimOp:
    val BuiltInOps = BuiltIn.Ops.map(_.operator)
    def unapply(s: String): Option[PrimOp] =
      val pattern = """!(.*)#(\w+)#(\w+)""".r
      s match {
        case pattern(op, ty, cls) if BuiltInOps.contains(op) =>
          Some(PrimOp(ty, cls.toLowerCase))
        case _ => None
      }
}
