package core

import cats.data.EitherT
import cats.data.OptionT
import cats.data.State
import cats.data.StateT
import cats.implicits.*
import core.Bindings.*
import core.Terms.*
import core.Types.*
import parser.Info.*
import scala.annotation.tailrec

import scala.util.*

import Shifting.*
import core.Instantiations.Instantiation
import core.Desugar.RecordConstrPrefix

object Context {
  type Note = (String, Binding)
  type Context = Tuple2[List[Note], Int]

  type Error = String
  type ContextState[A] = State[Context, A]
  type StateEither[A] = EitherT[ContextState, Error, A]
  type StateOption[A] = OptionT[ContextState, A]

  val emptyContext = (List[(String, Binding)](), 0)
  val WildcardName = "_"

  def addName(n: String): ContextState[String] = addBinding(n, NameBind)

  def getNotes(ctx: Context, withMarks: Boolean = false): LazyList[Note] = {
    val (l, c) = ctx
    withMarks match {
      case false =>
        l.to(LazyList).filter {
          case (_, _: Mark) => false
          case (_, _)       => true
        }
      case true => l.to(LazyList)
    }
  }

  def addBinding(n: String, b: Binding): ContextState[String] = {
    def incrVarBindings(name: String, numOfVarBindings: Int) = b match {
      case _: VarBind if n.startsWith(Desugar.RecursiveFunctionParamPrefix) =>
        (n, numOfVarBindings)
      case _: VarBind =>
        (s"$n$numOfVarBindings", numOfVarBindings + 1)
      case _ => (n, numOfVarBindings)
    }
    State(ctx => {
      val (notes, currVarBindings) = ctx
      val (name, numOfVarBindings) = incrVarBindings(n, currVarBindings)
      val ctx1 = ((name, b) :: notes, numOfVarBindings)
      (ctx1, name)
    })
  }

  def pickFreshName(x: String): ContextState[String] =
    State(ctx => {
      val c = ctx.map(_ + 1)
      (c, s"$x${c._2}")
    })

  def indexToName(
      c: Context,
      x: Int,
      withMarks: Boolean = false
  ): Option[String] =
    getNotes(c, withMarks).lift(x).map { case ((i, _)) => i }

  def nameToIndex(
      c: Context,
      x: String,
      withMarks: Boolean = false
  ): Option[Int] =
    getNotes(c, withMarks).indexWhere { case ((i, _)) => i == x } match {
      case v if v >= 0 => Some(v)
      case _           => None
    }

  def run[T](state: ContextState[T]): ContextState[T] =
    State { ctx =>
      {
        try {
          val (ctx1, t) = state.run(ctx).value
          ((ctx._1, ctx._2 + (ctx1._2 - ctx._2)), t)
        } catch {
          case e: NoSuchElementException =>
            throw new RuntimeException(s"Context state computation failed - ${e.getMessage} - Context has ${ctx._1.length} bindings", e)
        }
      }
    }

  def runE[A](state: StateEither[A]): StateEither[A] =
    EitherT { run(state.value) }

  def getType(info: Info, idx: Int): StateEither[Type] =
    getBinding(info, idx).flatMap[Error, Type](_ match {
      case VarBind(ty)              => EitherT.rightT(ty)
      case TermAbbBind(_, Some(ty)) => EitherT.rightT(ty)
      case TermAbbBind(_, None) =>
        TypeError.format(NoTypeForVariableTypeError(info, idx))
      case TypeAbbBind(ty, _) => EitherT.rightT(ty)
      case _ =>
        TypeError.format(WrongBindingForVariableTypeError(info, idx))
    })

  def getBinding(
      info: Info,
      idx: Int,
      withMarks: Boolean = false
  ): StateEither[Binding] =
    EitherT
      .liftF(State.inspect { (ctx: Context) =>
        getNotes(ctx, withMarks).lift(idx)
      })
      .flatMap(_ match {
        case Some((_, b)) => bindingShift(idx + 1, b).pure[StateEither]
        case _            => TypeError.format(BindingNotFoundTypeError(info))
      })

  /** Check if a variable at the given index is a closure parameter.
    *
    * Closure parameters are:
    * - Bound with VarBind (not TermAbbBind or other types)
    * - Not recursive function parameters (which start with "^")
    *
    * This is used during instantiation collection to skip creating
    * instantiations for runtime closure parameters, which should not
    * be monomorphized.
    */
  def isClosureParameter(ctx: Context, idx: Int): Boolean = {
    getNotes(ctx, withMarks = false).lift(idx).exists {
      case (name, VarBind(_)) =>
        // VarBind indicates a variable binding (like closure parameters)
        // Exclude recursive function parameters (start with "^")
        !name.startsWith(Desugar.RecursiveFunctionParamPrefix)
      case _ => false
    }
  }

  def getAlgebraicDataTypeVar(
      info: Info,
      node: String
  ): ContextState[Option[TypeVar]] =
    findAlgebraicDataType(info, node).map(
      _.map(v => TypeVar(info, v._2, v._3))
    )

  def getAlgebraicDataTypeName(
      info: Info,
      node: String
  ): ContextState[Option[String]] =
    findAlgebraicDataType(info, node.stripPrefix(RecordConstrPrefix))
      .map(_.map(_._1._1))

  /** Finds algebraic data type name using the name from one of its nodes.
    *
    * For a record type the node name will be equal to ADT name, while for sum
    * type the node name will be equal to of its enumarations.
    */
  def findAlgebraicDataType(
      info: Info,
      node: String
  ): ContextState[Option[(Note, Int, Int)]] =
    State.inspect { (ctx: Context) =>
      val notes = getNotes(ctx).toList
      notes.zipWithIndex
        .find {
          case ((i, TypeAbbBind(_, _)), idx) if i == node =>
            true
          case ((_, TypeAbbBind(ty, _)), idx) if isSumType(ty, node) =>
            true
          case _ =>
            false
        }
        .map((n, idx) => (n, idx, notes.length))
    }

  def instantantionShiftOnContextDiff(
      inst: Instantiation
  ): ContextState[Instantiation] =
    inst.tys
      .traverse(typeShiftOnContextDiff(_))
      .map(
        Instantiation(inst.i, inst.term, _, inst.cls, inst.r)
      )

  /** Shifts type based on the difference between TypeVar length and current
    * length of the context.
    *
    * Required to do so when an existential variables gets resolved _deep_ in
    * the context but doesn't get shifted once _out_ of it. Usually that happens
    * in closures.
    */
  def typeShiftOnContextDiff(ty: Type): ContextState[Type] = ty match {
    case TypeVar(_, _, varContextLength) =>
      State.inspect { ctx =>
        val d = varContextLength - getNotes(ctx).toList.length
        typeShift(-d, ty)
      }
    case TypeApp(info, ty1, ty2) =>
      for {
        ty1S <- typeShiftOnContextDiff(ty1)
        ty2S <- typeShiftOnContextDiff(ty2)
      } yield TypeApp(info, ty1S, ty2S)
    case TypeArrow(info, ty1, ty2) =>
      for {
        ty1S <- typeShiftOnContextDiff(ty1)
        ty2S <- typeShiftOnContextDiff(ty2)
      } yield TypeArrow(info, ty1S, ty2S)
    case _ => ty.pure
  }

  @tailrec
  def isSumType(t: Type, node: String): Boolean = t match {
    case TypeAbs(_, _, ty) => isSumType(ty, node)
    case TypeRec(_, _, _, TypeVariant(_, fields))
        if fields.exists(_._1 == node) =>
      true
    case _ => false
  }

  /** Retrieves types that implement a given type class. */
  def getClassIntances(cls: TypeClass): ContextState[List[Type]] =
    State
      .inspect { (ctx: Context) =>
        getNotes(ctx).collect {
          case (_, TypeClassInstanceBind(className, ty, _))
              if cls.name == className =>
            ty
        }.toList
      }
      .flatMap(_.traverse(typeShiftOnContextDiff(_)))

  /** Retrieves type classes that implement given type. */
  def getTypeInstances(typeName: String): ContextState[List[TypeClass]] =
    State.inspect { (ctx: Context) =>
      getNotes(ctx).foldLeft(List[TypeClass]()) {
        case (acc, (binding, TypeClassInstanceBind(cls, tc: TypeVar, _)))
            if Desugar.toTypeInstanceBindID(cls, typeName) == binding =>
          TypeClass(tc.info, cls) :: acc
        case (acc, _) => acc
      }
    }

  def getTypeMethod(
      info: Info,
      ty: Type,
      rootTypeVar: TypeVar,
      method: String
  ): StateEither[Type] = for {
    optionName <- EitherT.liftF(
      State.inspect { (ctx: Context) =>
        indexToName(ctx, rootTypeVar.index)
      }
    )
    typeName <- optionName match {
      case Some(name) => name.pure[StateEither]
      case None       => TypeError.format(NotFoundTypeError(info))
    }
    methodOptionIdx <- EitherT.liftF(
      State.inspect { (ctx: Context) =>
        {
          val methodID = Desugar.toMethodID(method, typeName)
          val recAbsID = Desugar.toRecAbsId(methodID)
          getNotes(ctx).indexWhere { case ((i, _)) =>
            i.startsWith(methodID) || i == recAbsID
          } match {
            case v if v >= 0 => Some(v)
            case _           => None
          }
        }
      }
    )
    methodType <- methodOptionIdx match {
      case Some(idx) => getType(info, idx)
      case None =>
        for {
          cls <- EitherT.liftF(getTypeInstances(typeName))
          tys <- cls.traverse(
            getTypeClassMethodType(_, method)
              .map(Some(_))
              .leftFlatMap(_ => EitherT.rightT(None))
          )
          methodTypes = tys.collect { case Some(v) => v }
          ty <- methodTypes match {
            case h :: Nil => h.pure[StateEither]
            case Nil =>
              TypeError.format(
                MethodNotFoundTypeError(info, ty, method)
              )
            case _ =>
              TypeError.format(MultipleTypeClassMethodsFound(info, cls, method))
          }
        } yield ty
    }
  } yield methodType

  def getTypeClassMethodType(
      cls: TypeClass,
      method: String
  ): StateEither[Type] = for {
    methodOptionIdx <- EitherT.liftF(
      State.inspect { (ctx: Context) =>
        nameToIndex(ctx, Desugar.toMethodID(method, cls.name))
      }
    )
    methodType <- methodOptionIdx match {
      case Some(idx) => getType(cls.info, idx)
      case None =>
        TypeError.format(
          MethodNotFoundTypeError(cls.info, cls, method)
        )
    }
  } yield methodType

  def getTypeClassMethods(cls: String): ContextState[List[String]] =
    State.inspect { (ctx: Context) =>
      getNotes(ctx).collect {
        case (
              Desugar.MethodPattern(method, c),
              TermAbbBind(_: TermClassMethod, _)
            ) if cls == c =>
          method
      }.toList
    }

  // region: existential variables

  def addEVar(
      x: String,
      info: Info,
      mark: Mark,
      cls: List[TypeClass] = List()
  ): ContextState[TypeEVar] = for {
    freshName <- pickFreshName(x)
    eA <- addBinding(freshName, mark)
  } yield TypeEVar(info, freshName, cls)

  def addMark(x: String): ContextState[String] = for {
    freshName <- pickFreshName(x)
    m <- addBinding(freshName, TypeEMarkBind)
  } yield m

  def freshEVar(
      x: String,
      info: Info,
      cls: List[TypeClass]
  ): ContextState[TypeEVar] =
    pickFreshName(x).map(TypeEVar(info, _, cls))

  def containsFreeEVar(eV: TypeEVar): ContextState[Boolean] =
    State.inspect { ctx =>
      getNotes(ctx, withMarks = true).find {
        case (v, TypeEFreeBind) if v == eV.name => true
        case _                                  => false
      }.isDefined
    }

  def insertEArrow(
      eA: TypeEVar
  ): StateEither[Tuple3[TypeEVar, TypeEVar, TypeESolutionBind]] = for {
    (idx, cls) <- getFreeEVarIndex(eA)
    ty1 <- EitherT.liftF(freshEVar("a1", eA.info, cls))
    ty2 <- EitherT.liftF(freshEVar("a2", eA.info, cls))
    sol = TypeESolutionBind(TypeArrow(eA.info, ty1, ty2))
    notes = List(
      (eA.name, sol),
      (ty1.name, TypeEFreeBind),
      (ty2.name, TypeEFreeBind)
    )
    s <- EitherT.liftF(State.modify { (ctx: Context) =>
      {
        val (postNotes, preNotes) =
          getNotes(ctx, withMarks = true).toList.splitAt(idx)
        (postNotes ::: notes ::: preNotes.tail, ctx._2)
      }
    })
  } yield (ty1, ty2, sol)

  def insertEApp(
      eA: TypeEVar
  ): StateEither[Tuple3[TypeEVar, TypeEVar, TypeESolutionBind]] = for {
    (idx, cls) <- getFreeEVarIndex(eA)
    ty1 <- EitherT.liftF(freshEVar("a1", eA.info, cls))
    ty2 <- EitherT.liftF(freshEVar("a2", eA.info, cls))
    sol = TypeESolutionBind(TypeApp(eA.info, ty1, ty2))
    notes = List(
      (eA.name, sol),
      (ty1.name, TypeEFreeBind),
      (ty2.name, TypeEFreeBind)
    )
    s <- EitherT.liftF(State.modify { (ctx: Context) =>
      {
        val (postNotes, preNotes) =
          getNotes(ctx, withMarks = true).toList.splitAt(idx)
        (postNotes ::: notes ::: preNotes.tail, ctx._2)
      }
    })
  } yield (ty1, ty2, sol)

  /** Replaces existential variable at specified index with existential type
    * solution binding.
    *
    * @return
    *   modifies the input state and returns the existential type binding
    *   passed.
    */
  def replaceEVar(
      idx: Integer,
      eA: TypeEVar,
      m: TypeESolutionBind
  ): ContextState[TypeESolutionBind] =
    State
      .modify { (ctx: Context) =>
        (
          getNotes(ctx, withMarks = true).toList.updated(idx, (eA.name, m)),
          ctx._2
        )
      }
      .map(_ => m)

  def getFreeEVarIndex(
      eA: TypeEVar
  ): StateEither[Tuple2[Int, List[TypeClass]]] = for {
    idx <- EitherT.liftF(State.inspect { (ctx: Context) =>
      getNotes(ctx, withMarks = true).indexWhere {
        case (v, TypeEFreeBind) if v == eA.name => true
        case _                                  => false
      } match {
        case v if v >= 0 => Some(v)
        case _           => None
      }
    })
    r <- idx match {
      case Some(v) => v.pure[StateEither]
      case None    => TypeError.format(ExistentialVariableNotFoundTypeError(eA))
    }
  } yield (r, eA.cls)

  /** Looks up the solution for `ev` in `ctx`. */
  def solution(
      eV: TypeEVar,
      shift: Boolean = true
  ): ContextState[Option[(Type, List[TypeClass])]] =
    State.inspect { ctx =>
      val notes = getNotes(ctx, withMarks = true)
      val sol = notes.collectFirst {
        case (v, TypeESolutionBind(ty, cls)) if v == eV.name => (ty, cls)
      }
      // NOTE: We are shifting the existential variable solution by the number
      // of "regular" (non-mark) bindings untill its place in the context. This
      // is required because we are not using indexes for the evars, therefore
      // if any regular binding is inserted afterwards the index in the
      // solution type is incorrect – cause we haven't shifted it. So we
      // "manually" gotta do that, count the bindings and shift.
      sol.map { case (ty, c) =>
        if (shift) (typeShift(countBindingsTillEVar(notes, eV), ty), c)
        else (ty, c)
      }
    }

  def countBindingsTillEVar(notes: LazyList[Note], eV: TypeEVar): Int =
    notes
      .takeWhile { case (v, _) => v != eV.name }
      .filter {
        case (_, _: Mark) => false
        case _            => true
      }
      .size

  def isWellFormedWithPeel(ty: Type, eA: TypeEVar): ContextState[Boolean] =
    Context.run {
      for {
        _ <- peel(eA.name)
        r <- isWellFormed(ty)
      } yield r
    }

  def isWellFormed(ty: Type): ContextState[Boolean] = ty match {
    case eA: TypeEVar =>
      for {
        hasEVar <- containsFreeEVar(eA)
        r <- hasEVar match {
          case true  => solution(eA).map(_.isDefined)
          case false => false.pure[ContextState]
        }
      } yield r
    case TypeArrow(_, ty1, ty2) =>
      for {
        r1 <- isWellFormed(ty1)
        r2 <- isWellFormed(ty2)
      } yield r1 && r2
    case TypeApp(_, ty1, ty2) =>
      for {
        r1 <- isWellFormed(ty1)
        r2 <- isWellFormed(ty2)
      } yield r1 && r2
    case TypeAll(_, _, _, _, ty) => isWellFormed(ty)
    case TypeRecord(_, l) =>
      l.traverse { case (_, ty) => isWellFormed(ty) }.map(_.reduce(_ && _))
    case TypeVariant(_, l) =>
      l.traverse { case (_, ty) => isWellFormed(ty) }.map(_.reduce(_ && _))
    case TypeRec(_, _, _, ty) => isWellFormed(ty)
    case _                    => true.pure[ContextState]
  }

  def containsEVarWithPeel(
      eA: TypeEVar,
      peelToEVar: Type
  ): ContextState[Boolean] = peelToEVar match {
    case TypeEVar(_, name, _) =>
      Context.run {
        for {
          _ <- peel(name, true)
          r <- containsFreeEVar(eA)
        } yield r
      }
    case _ => false.pure
  }

  /** Peels off the end of a context up to and including `note`. */
  def peel(name: String, emptyOnNotFound: Boolean = false): ContextState[Unit] =
    State.modify { (ctx: Context) =>
      nameToIndex(ctx, name, withMarks = true) match {
        case Some(_) =>
          val notes = getNotes(ctx, withMarks = true).dropWhile {
            case (v, _) if v == name => false
            case _                   => true
          }.toList
          (
            notes match {
              case h :: t => t
              case Nil    => Nil
            },
            ctx._2
          )
        case None => if (emptyOnNotFound) emptyContext else ctx
      }
    }

  // region end

  def print(withMarks: Boolean = true): ContextState[Unit] = {
    println()
    State.inspect { ctx =>
      getNotes(ctx, withMarks).zipWithIndex.foreach(e =>
        val repr =
          Representation.bindingToType(e._1._2).value.runA(ctx).value.merge
        println(s"${e._2}|${e._1._1}: $repr")
      )
    }
  }
}
