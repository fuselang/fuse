package fuse

import munit.*
import scala.concurrent.duration.Duration
import cats.effect.{IO, Resource}
import java.nio.file.{Files, Path, Paths}
import scala.sys.process.*
import cats.effect.ExitCode

abstract class CompilerTests extends FunSuite {
  import CompilerTests.*
  override val munitTimeout = Duration(10, "m")

  /** Asserts fuse code is type checked. */
  def fuse(code: String, expected: Output = CheckOutput(None)) =
    expected match {
      case CheckOutput(s)                             => assertCheck(code, s)
      case BuildOutput(s)                             => assertBuild(code, s)
      case ExecutableOutput(stdout, exitCode, stdlib) =>
        assertExecutable(code, stdout, exitCode, stdlib)
    }

  def assertCheck(code: String, expectedError: Option[String]) =
    (check(code), expectedError) match {
      case (t, None)                    => assert(t.isRight, s"\n${t.merge}")
      case (t, Some(error)) if t.isLeft =>
        assert(t.merge.contains(error), s"\n${error} not in:\n${t.merge}")
      case (_, Some(error)) =>
        assert(false, s"\ncheck passed, error not thrown: '${error}'")
    }

  def assertBuild(code: String, expectedGrinCode: String) =
    build(code) match {
      case Right(grinCode) =>
        assertNoDiff(
          grinCode,
          expectedGrinCode,
          "expected GRIN code isn't equal"
        )
      case Left(error) =>
        assert(false, s"\nfailed to compile due to error: '${error}'")
    }

  def assertExecutable(
      code: String,
      expectedStdout: String,
      expectedExitCode: Int = 0,
      includeStdlib: Boolean = false
  ) = {
    import cats.effect.unsafe.implicits.global

    execute(code, includeStdlib).unsafeRunSync() match {
      case Right(result) =>
        assertEquals(
          result.exitCode,
          expectedExitCode,
          s"Exit code mismatch. stderr: ${result.stderr}"
        )
        assertEquals(result.stdout, expectedStdout, "Output mismatch")
      case Left(error) =>
        fail(s"Execution failed: $error")
    }
  }

}

class CompilerCheckTests extends CompilerTests {
  import CompilerTests.*

  test("check integer addition") {
    fuse("""
fun main() -> i32
    2 + 2
        """)
  }
  test("check integer addition of multiple values") {
    fuse("""
fun main() -> i32
    2 + 2 + 5 + 7
        """)
  }
  test("check integer and string addition") {
    fuse(
      """
fun main() -> i32
    2 + "2"
        """,
      CheckOutput(Some("expected type of `i32`, found `str`"))
    )
  }
  test("check float addition") {
    fuse("""
fun main() -> f32
    2.0 + 2.2
        """)
  }
  test("check string addition") {
    fuse("""
fun main() -> i32
    let s = "1" + "2"
    0
        """)
  }
  test("check unit functions") {
    fuse("""
fun greetings() -> Unit
  _print("Hello World")

fun main() -> Unit
  greetings()
        """)
  }
  test("check invalid record type addition") {
    fuse(
      """
type Point:
  x: i32
  y: i32

fun main() -> i32
    let s = Point(0, 0) + Point(1, 1)
    0
        """,
      CheckOutput(
        Some("expected one of types `{str, f32, i32}`, found `Point`")
      )
    )
  }
  test("check generic option") {
    fuse("""
type Option[A]:
    None
    Some(A)

impl Option[A]:
    fun is_some(self) -> bool
        match self:
            Some(v) => true
            _ => false

    fun is_none(self) -> bool
        match self:
            Some(v) => false
            _ => true

    fun map[B](self, f: A -> B) -> Option[B]
        match self:
            Some(v) => Some(f(v))
            _ => None

    fun get_or_else(self, default: A) -> A
        match self:
            Some(a) => a
            None => default

    fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
        match self:
            Some(a) => f(a)
            None => None

    fun filter(self, f: A -> bool) -> Option[A]
        match self:
            Some(a) => {
                match f(a):
                    true => self
                    _ => None
            }
            _ => None

    fun to_str(v: i32) -> Option[str]
        let s = Some(v)
        s.map(a => int_to_str(a))

fun main() -> i32
    let o = Some(5)
    let o1 = o.flat_map(t => Some(t + 1))
    let l = Option::to_str(5)
    match l:
        Some(v) => 0
        None => 1
        """)
  }
  test("check generic list") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun foldRight[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::foldRight(xs, z, f))
            Nil => z

    fun map[B](self, f: A -> B) -> List[B]
        List::foldRight(self, Nil[B], (h, t) => Cons(f(h), t))

    fun map_2[B](self, f: A -> B) -> List[B]
        let iter = (l: List[A], acc: List[B]) => {
            match l:
                Cons(h, t) => iter(t, Cons(f(h), acc))
                Nil => acc
        }
        iter(self, Nil[B])

fun main() -> Unit
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map_2(v => v + 1)
    ()
        """
    )
  }
  test("check generic list with map_2 non-recursive") {
    fuse("""
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun simple_map[B](self, f: A -> B) -> List[B]
        match self:
            Cons(h, t) => Cons(f(h), Nil[B])
            Nil => Nil[B]

fun main() -> Unit
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.simple_map(v => v + 1)
    ()
        """)
  }
  test("check generic list with unannotated iter map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun map_2[B](self, f: A -> B) -> List[B]
        let iter = (l, acc) => {
            match l:
                Cons(h, t) => iter(t, Cons(f(h), acc))
                Nil => acc
        }
        iter(self, Nil[B])

fun main() -> Unit
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map_2(v => v + 1)
    ()
        """,
      CheckOutput(
        Some("explicit type annotation required for variable")
      )
    )
  }
  test("check lambda calc") {
    fuse("""
type List[T]:
    Cons(h: T, t: List[T])
    Nil

type Option[T]:
    None
    Some(T)

impl Option[T]:
    fun is_some(self) -> bool
        match self:
            Some(v) => true
            _ => false

    fun is_none(self) -> bool
        match self:
            Some(v) => false
            _ => true

type Tuple[A, B](A, B)

type Type:
    TypeVar(index: i32, length: i32)
    TypeId(i: str)
    TypeArrow(t1: Type, t2: Type)
    TypeRecord(fields: List[Tuple[str, Type]])
    TypeVariant(values: List[Tuple[str, Type]])
    TypeUnit
    TypeBool
    TypeInt
    TypeString
    TypeFloat

type Term:
    TermTrue
    TermFalse
    TermInt(i: i32)
    TermFloat(f: f32)
    TermString(s: str)
    TermUnit

fun println(s: str) -> Unit
    _print(s)
    _print("\n")
    ()

fun type_of(t: Term) -> Option[Type]
    match t:
        TermTrue => Some(TypeBool)
        TermFalse => Some(TypeBool)
        TermInt(i) => Some(TypeInt)
        TermFloat(f) => Some(TypeFloat)
        TermString(s) => Some(TypeString)
        TermUnit => Some(TypeUnit)
        _ => None

fun is_type() -> bool
    let ty = type_of(TermTrue)
    ty.is_some()

fun main() -> i32
    0
        """)
  }
  test("check simple sum type") {
    fuse("""
type Animal:
  Dog
  Cat

fun value(a: Animal) -> i32
  match a:
      Dog => 0
      Cat => 1

fun main() -> i32
    value(Dog)
        """)
  }
  test("check function on record with primitive types") {
    fuse("""
type StateInt:
  run: i32 -> i32

fun value(a: StateInt) -> i32
  (a.run)(2)
  
fun main() -> i32
  value(StateInt(a => a + 1))
        """)
  }
  test("check function on record with sum type") {
    fuse("""
type Option[A]:
    None
    Some(A)

impl Option[A]:
    fun is_some(self) -> bool
        match self:
            Some(v) => true
            _ => false

type StateInt:
  run: i32 -> Option[i32]

fun value(a: StateInt) -> bool
  let o = (a.run)(2)
  o.is_some()
  
fun main() -> i32
  let s = StateInt(a => Some(a))
  match value(s):
    true => 1
    false => 0
        """)
  }
  test("check function on record with tuple type") {
    fuse("""
type Tuple[A, B](A, B)

type State[S, A]:
  run: S -> Tuple[A, S]

fun value(a: State[i32, i32]) -> i32
  let t = (a.run)(1)
  t.1 + t.2
  
fun main() -> i32
  value(State(a => Tuple(a + 1, a + 2)))
        """)
  }
  test("check inline lambda type inference") {
    fuse("""
fun main() -> i32
    let value = (a) => a + 1
    value(1)
        """)
  }
  test("check inline lambda type inference with two variables") {
    fuse("""
fun main() -> i32
    let value = (a, b) => a + b + 2
    value(1, 2)
        """)
  }
  test("check inline lambda type inference with two variables #2") {
    fuse("""
fun main() -> i32
    let value = (a, b) => a + 2 + b
    value(1, 2)
        """)
  }
  test("check inline lambda type inference with unknown param") {
    fuse("""
fun main() -> i32
    let id = o => o
    id(1)
        """)
  }
  test("check inline lambda type inference invalid param") {
    fuse(
      """
fun main() -> i32
    let value = (a) => a + 1
    value("2")
        """,
      CheckOutput(Some("expected type of `i32`, found `str`"))
    )
  }
  test("check inline lambda type inference with two variables invalid param") {
    fuse(
      """
fun main() -> i32
    let value = (a, b) => a + b + 2
    value("1", 2)
        """,
      CheckOutput(Some("expected type of `i32`, found `str`"))
    )
  }
  test("check inline lambda type inference with invalid record type addition") {
    fuse(
      """
type Point:
  x: i32
  y: i32

fun main() -> i32
    let value = (a) => Point(2, 3) + a
    value(Point(1, 2))
    0
        """,
      CheckOutput(
        Some("expected one of types `{str, f32, i32}`, found `Point`")
      )
    )
  }
  test(
    "check inline lambda type inference with invalid record type addition #2"
  ) {
    fuse(
      """
type Point:
  x: i32
  y: i32

fun main() -> i32
    let value = (a) => a + Point(2, 3) 
    value(Point(1, 2))
    0
        """,
      CheckOutput(
        Some("expected one of types `{str, f32, i32}`, found `Point`")
      )
    )
  }
  test("check closure inference with match statement for primitive type") {
    fuse("""
fun main() -> i32
    let value = (b) => {
        match b:
            true => 1
            false => 0
    }
    value(true)
        """)
  }
  test("check closure inference with match statement for sum type") {
    fuse("""
type Animal:
  Dog
  Cat

fun main() -> i32
    let value = (a) => {
        match a:
            Dog => 0
            Cat => 1
    }
    value(Dog)
        """)
  }
  test("check closure inference with match statement for product type") {
    fuse("""
type Point:
  x: i32
  y: i32

fun main() -> i32
    let value = (a) => {
        match a:
            Point(x, y) => x + y
    }
    value(Point(2, 3))
        """)
  }
  test("check closure inference with match statement for option type") {
    fuse("""
type Option[T]:
    None
    Some(T)

fun main() -> i32
    # closure type should be Option[i32] -> i32
    # inferred by the first's case expression type -> i32
    let value = (a) => {
        match a:
            None => 0
            Some(v) => v
    }
    value(Some(5))
        """)
  }
  test("check closure inference with match statement for invalid arg") {
    fuse(
      """
type Option[T]:
    None
    Some(T)

fun main() -> i32
    let value = (a) => {
        match a:
            Some(v) => v
            None => 0
    }
    value(123)
        """,
      CheckOutput(Some("expected type of `Option[i32]`, found `i32`"))
    )
  }
  test("check closure inference with match statement for invalid generic arg") {
    fuse(
      """
type Option[T]:
    None
    Some(T)

fun main() -> i32
    let value = (a) => {
        match a:
            None => 0
            Some(v) => v
    }
    value(Some("123"))
        """,
      CheckOutput(Some("expected type of `Option[i32]`, found `Option[str]`"))
    )
  }
  test("check closure inference with match statement for nested option type") {
    fuse(
      """
type Animal[A]:
    Cat(A)
    Dog

type Option[T]:
    None
    Some(T)

fun main() -> i32
    let value = (a) => {
        match a:
            Cat(v) => v
            Dog => None[str]
    }
    let v = value(Cat(Some("123")))
    match v:
      Some(_) => 1
      None => 0
        """
    )
  }
  test(
    "check closure inference with match statement for nested option type invalid arg"
  ) {
    fuse(
      """
type Animal[A]:
    Cat(A)
    Dog

type Option[T]:
    None
    Some(T)

fun main() -> i32
    let value = (a) => {
        match a:
            Cat(v) => v
            Dog => None[str]
    }
    let v = value(Cat(Some("123")))
    let n = value(Cat(Some(123)))
        """,
      CheckOutput(
        Some(
          "expected type of `Animal[Option[str]]`, found `Animal[Option[i32]]`"
        )
      )
    )
  }
  test("check closure inference with match statement for list type") {
    fuse("""
type List[A]:
    Cons(h: A, t: List[A])
    Nil

fun main() -> i32
    let value = (a) => {
        match a:
            Nil => 0
            Cons(v, _) => v
    }
    value(Cons(1, Nil))
        """)
  }
  test("check recursive closure inference with match statement for list type") {
    fuse("""
type List[A]:
    Cons(h: A, t: List[A])
    Nil

fun main() -> i32
    let f = (a: i32) => int_to_str(a)
    let iter = (a, acc) => {
        match a:
            Nil => acc
            Cons(h, t) => Cons(f(h), iter(t, acc))
    }
    let v = iter(Cons(1, Nil), Nil[str])
    match v:
      Cons(_, _) => 0
      Nil => 1
        """)
  }
  test(
    "check recursive closure inference with match statement for list type annotations required"
  ) {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

fun main() -> i32
    let iter = (a, acc) => {
        match a:
            Nil => acc
            Cons(h, t) => Cons(h, iter(t, acc))
    }
    let v = iter(Cons(1, Nil), Nil[str]())
    match v:
      Cons(_, _) => 0
      Nil => 1
        """,
      CheckOutput(
        Some(
          "can't determine the type of variable, type annotation is required"
        )
      )
    )
  }
  test("check inference with match statement for either type") {
    fuse("""
type Either[A, B]:
    Right(A)
    Left(B)

fun main() -> i32
    let value = (a) => {
        match a:
            Right(t) => t
            Left(b) => b
    }
    match value(Right("123")):
      "123" => 0
      _ => 1
        """)
  }
  test("check inference with match statement for either type invalid arg") {
    fuse(
      """
type Either[A, B]:
    Right(A)
    Left(B)

fun main() -> i32
    let value = (a) => {
        match a:
            Right(t) => t
            Left(b) => b
    }
    let r = value(Right("123"))
    let l = value(Left(123))
        """,
      CheckOutput(Some("expected type of `Either[str][str]`"))
    )
  }
  test("check simple trait") {
    fuse("""
trait Summary:
  fun summarize(self) -> str;

type Tweet:
  username: str
  content: str

impl Summary for Tweet:
  fun summarize(self) -> str
    self.username + ": " + self.content

fun notify[T: Summary](s: T) -> Unit
  _print("Breaking news! " + s.summarize())

fun main() -> i32
    let tweet = Tweet("elon", "work!")
    _print(tweet.summarize())
    notify(tweet)
    0
        """)

  }
  test("check addition type bounds") {
    fuse("""
fun add[T: Add](a: T, b: T) -> T
  a + b

fun main() -> i32
  add(2, 3)
        """)

  }
  test("check invalid addition type bounds") {
    fuse(
      """
fun add[T: Add](a: T, b: T) -> T
  a + b

fun main() -> i32
  add(true, 3)
        """,
      CheckOutput(
        Some("expected one of types `{str, f32, i32}`, found `bool` type")
      )
    )

  }
  test("check invalid type bound for a trait method") {
    fuse(
      """
trait Summary:
  fun summarize() -> str;

fun notify[T: Add](s: T) -> Unit
  _print("Breaking news! " + s.summarize())

fun main() -> i32
    0
        """,
      CheckOutput(Some("`summarize` method not found in `Add` type"))
    )

  }
  test("check invalid type bound for a trait method with type instance") {
    fuse(
      """
trait Summary:
  fun summarize(self) -> str;

type Tweet:
  username: str
  content: str

impl Summary for Tweet:
  fun summarize(self) -> str
    self.username + ": " + self.content

fun notify[T: Summary](s: T) -> Unit
  _print("Breaking news! " + s.summarize())

fun main() -> i32
    notify(5)
    0
        """,
      CheckOutput(Some("expected one of types `{Tweet}`, found `i32` type"))
    )

  }
  test("check invalid type b i32) ounds for a type param") {
    fuse(
      """
trait Summary:
  fun summarize() -> str;

trait Collection:
  fun summarize() -> str;

fun notify[T: Summary + Collection](s: T) -> Unit
  _print("Breaking news! " + s.summarize())

fun main() -> i32
    0
        """,
      CheckOutput(
        Some(
          "multiple `summarize` method implementations found for `{Summary, Collection}` bounds"
        )
      )
    )

  }
  test("check invalid type for function with type bounds") {
    fuse(
      """
trait Summary:
  fun summarize(self) -> str;

type Tweet:
  username: str
  content: str

fun notify[T: Summary](s: T) -> Unit
  _print("Breaking news! " + s.summarize())

fun main() -> i32
    let tweet = Tweet("elon", "work!")
    notify(tweet)
    0
        """,
      CheckOutput(
        Some(
          "expected type that implements `{Summary}` traits, found `Tweet` type"
        )
      )
    )

  }
  test("check invalid type method for a trait impl") {
    fuse(
      """
trait Summary:
  fun summarize() -> str;

type Tweet:
  username: str
  content: str

impl Summary for Tweet:
  fun summarize() -> i32
    3

fun main() -> i32
    0
        """,
      CheckOutput(Some(", found `Unit -> i32` for `summarize`"))
    )

  }
  test("check wrong type method for a trait impl") {
    fuse(
      """
trait Summary:
  fun summarize(self) -> str;

type Tweet:
  username: str
  content: str

impl Summary for Tweet:
  fun make(self) -> str
    self.username + self.content

  fun summarize(self) -> str
    self.username + ": " + self.content

fun main() -> i32
    0
        """,
      CheckOutput(Some("`make` method not found in `Summary` type"))
    )

  }
  test("check missing type method for a trait impl") {
    fuse(
      """
trait Summary:
  fun summarize(self) -> str;
  fun title(self) -> str;

type Tweet:
  username: str
  content: str

impl Summary for Tweet:
  fun summarize(self) -> str
    self.username + ": " + self.content

fun main() -> i32
    0
        """,
      CheckOutput(Some("`{title}` methods not implemented for `Tweet` type"))
    )

  }
  test("check generic trait functor implementation") {
    fuse("""
trait Functor[A]:
  fun map[B](self, f: A -> B) -> Self[B];
  # This how the desugared type should look like.
  # fun map[Self: Functor, A, B](self: Self[B], f: A -> B) -> Self[B];

type Option[T]:
  Some(T)
  None

impl Functor for Option[A]:
  fun map[B](self, f: A -> B) -> Option[B]
    match self:
      Some(v) => Some(f(v))
      _ => None

fun fmap[A, B, F: Functor](f: A -> B, c: F[A]) -> F[B]
    c.map(f)

fun main() -> i32
    let o = Some(5)
    fmap(a => a + 1, o)
    0
        """)
  }
  test("check generic trait monad implementation") {
    fuse("""
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];
  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

fun main() -> i32
    let o = Some(5)
    o.flat_map(t => Some(t + 1))
    0
        """)

  }
  test("check generic trait functor implementation wrong method type") {
    fuse(
      """
trait Functor[A]:
  fun map[B](self, f: A -> B) -> Self[B];
  # This how the desugared type should look like.
  # fun map[Self: Functor, A, B](self: Self[B], f: A -> B) -> Self[B];

type Option[T]:
  Some(T)
  None

impl Functor for Option[A]:
  fun map[B](self, f: A -> B) -> Option[A]
    match self:
      Some(v) => Some(v)
      _ => None

fun main() -> i32
    let o = Some(5)
    o.map(a => a + 1)
    0
        """,
      CheckOutput(
        Some(
          "expected `[Self::* -> *]: Functor, [A::*], [B::*] => Self[A] -> A -> B -> Self[B]`, found `[A::*], [B::*] => Option[A] -> A -> B -> Option[A]` for `map`"
        )
      )
    )

  }
  test("check generic trait monad with default implementation") {
    fuse("""
trait Monad[A]:
  fun unit[T](a: T) -> Self[T];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[T](a: T) -> Option[T]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

fun main() -> i32
    let o = Some(5)
    o.map(a => a + 1)
    0
        """)

  }
  test("check generic trait monad for state") {
    fuse("""
trait Monad[A]:
  fun unit[B](a: B) -> Self[B];
  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

type Tuple[A, B](A, B)

type State[S, A]:
  run: S -> Tuple[A, S]

impl State[S, A]:
  fun get[S]() -> State[S, S] 
    State((s: S) => Tuple(s, s))

  fun exec(self, s: S) -> Tuple[A, S]
    (self.run)(s)

  fun value(self, s: S) -> A
    match self.exec(s):
      Tuple(v, _) => v

impl Monad for State[S, A]:
  fun unit[S, A](a: A) -> State[S, A]
    let f = (s: S) => Tuple(a, s) 
    State(f)

  fun flat_map[B](self, f: A -> State[S, B]) -> State[S, B]
    let r = s => {
      let v = (self.run)(s)
      f(v.1).exec(v.2)
    }
    State(r)

fun main() -> i32
  let s = State::unit(5)
  let s1 = s.flat_map(i => State(a => Tuple(a + i, i)))
  s1.value(2)
        """)
  }
  test("check generic traits monad + show with default implementation") {
    fuse("""
trait Monad[A]:
  fun unit[B](a: B) -> Self[B];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    let f = a => Self::unit(f(a))
    self.flat_map(f)

trait Show[A]:
  fun show() -> str;

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

impl Show for Option[A]:
  fun show() -> str
    "Option[T]"

fun main() -> i32
    let o = Some(5)
    o.map(a => a + 1)
    0
        """)
  }
  test("check invalid type classes used for type param for different kinds") {
    fuse(
      """
trait Summary:
  fun summarize() -> str;

trait Show[A]:
  fun show() -> str;

fun to_str[T: Summary + Show](s: T) -> str
  s.show()

fun main() -> i32
    0
        """,
      CheckOutput(Some("`{Summary, Show}` type classes have different kinds"))
    )

  }
  test("check invalid type classes used for type param for same methods") {
    fuse(
      """
trait Summary:
  fun summarize() -> str;

trait ShortSummary:
  fun summarize() -> str;

fun to_str[T: Summary + ShortSummary](s: T) -> str
  s.summarize()

fun main() -> i32
    0
        """,
      CheckOutput(
        Some(
          "multiple `summarize` method implementations found for `{Summary, ShortSummary}` bounds"
        )
      )
    )

  }
  test("check invalid type class used for type param when doesn't exist") {
    fuse(
      """
trait Summary:
  fun summarize() -> str;

fun to_str[T: Show](s: T) -> str
  s.summarize()

fun main() -> i32
    0
        """,
      CheckOutput(Some("`Show` type class not found"))
    )

  }
  test("check do expr") {
    fuse("""
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

fun main() -> i32
  let x = Some(1)
  let y = Some(2)
  let z = Some(3)
  let d = {
    do:
      i <- x
      j <- y
      k <- z
      i + j + k
  }
  match d:
    Some(v) => v
    _ => 0
        """)

  }
  test("check do expr invalid do expr") {
    fuse(
      """
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

fun main() -> i32
  let x = Some(1)
  let y = Some(2)
  let z = Some(3)
  let d = {
    do:
      i <- x
  }
  match d:
    Some(v) => v
    _ => 0
        """,
      CheckOutput(Some("yield expression not found"))
    )

  }
  test("check do expr yield expr not found") {
    fuse(
      """
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

fun main() -> i32
  let x = Some(1)
  let y = Some(2)
  let z = Some(3)
  let d = {
    do:
      i <- x
      j <- y
      k <- z
  }
  match d:
    Some(v) => v
    _ => 0
        """,
      CheckOutput(Some("yield expression not found"))
    )

  }
  test("check do expr missing assign expr expected") {
    fuse(
      """
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

fun main() -> i32
  let x = Some(1)
  let y = Some(2)
  let z = Some(3)
  let d = {
    do:
      i <- x
      j <- y
      i + j
      k <- z
      i + j + k
  }
  match d:
    Some(v) => v
    _ => 0
        """,
      CheckOutput(Some("assignment expression expected"))
    )

  }
  test("check do expr invalid primitive type") {
    fuse(
      """
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

fun main() -> i32
  let x = Some(1)
  let y = Some(2)
  let z = Some(3)
  let d = {
    do:
      i <- x
      g <- 7
      j <- y
      k <- z
      i + j + k + g
  }
  match d:
    Some(v) => v
    _ => 0
        """,
      CheckOutput(Some("`i32` isn't a data type"))
    )

  }
  test("check do expr invalid data type") {
    fuse(
      """
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

type Option[T]:
  Some(T)
  None

type Either[A, B]:
    Right(A)
    Left(B)

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

impl Monad for Either[A, B]:
  fun unit[A](a: A) -> Either[A, A]
    Right[A, A](a)

  fun flat_map[C](self, f: A -> Either[C, B]) -> Either[C, B]
    match self:
      Right(v) => f(v)
      Left(l) => Left(l)

fun main() -> i32
  let x = Some(1)
  let y = Some(2)
  let z = Some(3)
  let g = Right(5)
  let d = {
    do:
      i <- x
      t <- g
      j <- y
      k <- z
      i + j + k + t
  }
  match d:
    Some(v) => v
    _ => 0
        """,
      CheckOutput(
        Some(
          "expected `[Self::* -> *]: Monad, [A::*], [B::*] => Self[A] -> A -> Self[B] -> Self[B]`, found `[A::*], [B::*], [C::*] => Either[A][B] -> A -> Either[C][B] -> Either[C][B]` for `flat_map`"
        )
      )
    )

  }
  test("check do expr invalid primitive type in data type") {
    fuse(
      """
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

fun main() -> i32
  let x = Some(1)
  let y = Some("2")
  let z = Some(3)
  let d = {
    do:
      i <- x
      j <- y
      k <- z
      i + j + k
  }
  match d:
    Some(v) => v
    _ => 0
        """,
      CheckOutput(
        Some(
          "expected type of `i32`, found `str`"
        )
      )
    )

  }
  test("check io type") {
    fuse("""
fun greetings() -> IO[Unit]
  do:
    h <- print("Hello")
    s <- print(" ")
    v <- print("World!")
    ()

fun main() -> Unit
  greetings().exec()
        """)

  }
  test("check generic list map with unit return type") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun map[B](self, f: A -> B) -> List[B]
        match self:
            Cons(h, t) => Cons(f(h), t.map(f))
            Nil => Nil[B]

fun main() -> i32
    let l = Cons(1, Nil)
    l.map(x => _print(int_to_str(x)))
    0
        """,
      CheckOutput(None)
    )
  }
  test("check generic list with fold_right using map and sum") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    let s = List::sum(l1)
    _print(int_to_str(s))
    0
        """,
      CheckOutput(None)
    )
  }
  test("check generic list with fold_right using flat_map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun append[A](l1: List[A], l2: List[A]) -> List[A]
        List::fold_right(l1, l2, (h, t) => Cons(h, t))

    fun flat_map[B](self, f: A -> List[B]) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => List::append(f(h), t))

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

fun main() -> i32
    let l = Cons(1, Cons(2, Cons(3, Nil)))
    let l1 = l.flat_map(v => Cons(v, Cons(v * 10, Nil)))
    let s = List::sum(l1)
    _print(int_to_str(s))
    0
        """,
      CheckOutput(None)
    )
  }
  test("check generic list with fold_right using chained map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    l1.map(r => _print(int_to_str(r)))
    0
        """,
      CheckOutput(None)
    )
  }

  test("check generic list with append and product using fold_left") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun fold_left[A, B](l: List[A], acc: B, f: (B, A) -> B) -> B
        match l:
            Cons(h, t) => List::fold_left(t, f(acc, h), f)
            Nil => acc

    fun append[A](l1: List[A], l2: List[A]) -> List[A]
        List::fold_right(l1, l2, (h, t) => Cons(h, t))

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

    fun product(l: List[i32]) -> i32
        List::fold_left(l, 1, (acc, b) => acc * b)

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    let l2 = Cons(7, Nil)
    let l3 = List::append(l1, l2)
    let s = List::sum(l3)
    let p = List::product(l3)
    _print(int_to_str(s + p))
    0
        """,
      CheckOutput(None)
    )
  }

  test("build generic list with append called from main") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun append[A](l1: List[A], l2: List[A]) -> List[A]
        List::fold_right(l1, l2, (h, t) => Cons(h, t))

fun main() -> i32
    let l1 = Cons(1, Nil)
    let l2 = Cons(2, Nil)
    let l3 = List::append(l1, l2)
    0
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CConsi32 h0 t1)

Nil'i32 =  store (CNili32)

foldrightListi32Listi32' as4 z5 f''6 =
 p9 <- fetch as4
 case p9 of
  (CConsi32 x9 xs'10) ->
   p12'' <- apply2_i32_to_List_i32 f''6 x9
   p13 <- foldrightListi32Listi32' xs'10 z5 f''6
   p14 <- apply1_List_i32_to_List_i32 p12'' p13
   pure p14
  #default ->
   pure z5

appendListi32' l114 l215 =
 p20 <- pure (P2c17 )
 foldrightListi32Listi32' l114 l215 p20

c17 h17 t18 =
 Cons'i32 h17 t18

grinMain _20 =
 p22 <- Nil'i32
 l122 <-  Cons'i32 1 p22
 p24 <- Nil'i32
 l224 <-  Cons'i32 2 p24
 l326 <-  appendListi32' l122 l224
 pure 0

apply1_List_i32_to_List_i32 p28 p29 =
 case p28 of
  (P1c17 p30) ->
   c17 p30 p29

apply2_i32_to_List_i32 p31 p32 =
 case p31 of
  (P2c17) ->
   pure (P1c17 p32)""")
    )
  }

  test("build generic list with append and product using fold_left") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun fold_left[A, B](l: List[A], acc: B, f: (B, A) -> B) -> B
        match l:
            Cons(h, t) => List::fold_left(t, f(acc, h), f)
            Nil => acc

    fun append[A](l1: List[A], l2: List[A]) -> List[A]
        List::fold_right(l1, l2, (h, t) => Cons(h, t))

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

    fun product(l: List[i32]) -> i32
        List::fold_left(l, 1, (acc, b) => acc * b)

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    let l2 = Cons(7, Nil)
    let l3 = List::append(l1, l2)
    let s = List::sum(l3)
    let p = List::product(l3)
    _print(int_to_str(s + p))
    0
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CConsi32 h0 t1)

Nil'i32 =  store (CNili32)

foldrightListi32i32' as4 z5 f''6 =
 p9 <- fetch as4
 case p9 of
  (CConsi32 x9 xs'10) ->
   p12'' <- apply2_i32_to_i32 f''6 x9
   p13 <- foldrightListi32i32' xs'10 z5 f''6
   p14 <- apply1_i32_to_i32 p12'' p13
   pure p14
  #default ->
   pure z5

foldrightListi32Listi32' as14 z15 f''16 =
 p19 <- fetch as14
 case p19 of
  (CConsi32 x19 xs'20) ->
   p22'' <- apply2_i32_to_List_i32 f''16 x19
   p23 <- foldrightListi32Listi32' xs'20 z15 f''16
   p24 <- apply1_List_i32_to_List_i32 p22'' p23
   pure p24
  #default ->
   pure z15

foldleftListi32i32' l24 acc25 f''26 =
 p29 <- fetch l24
 case p29 of
  (CConsi32 h29 t'30) ->
   p32'' <- apply2_i32_to_i32 f''26 acc25
   p33 <- apply1_i32_to_i32 p32'' h29
   p34 <- foldleftListi32i32' t'30 p33 f''26
   pure p34
  #default ->
   pure acc25

appendListi32' l134 l235 =
 p40 <- pure (P2c37 )
 foldrightListi32Listi32' l134 l235 p40

c37 h37 t38 =
 Cons'i32 h37 t38

mapListi32i32' self40 f''41 =
 p43 <- Nil'i32
 p50 <- store f''41
 p51 <- pure (P2c44 p50)
 foldrightListi32Listi32' self40 p43 p51

c44 f''4145 h46 t47 =
 f''414546 <- fetch f''4145
 p49 <- c80 h46
 Cons'i32 p49 t47

sumList' l51 =
 p63 <- pure (P2c53 )
 foldrightListi32i32' l51 0 p63

c53 acc53 b54 =
 _prim_int_add acc53 b54

productList' l63 =
 p75 <- pure (P2c65 )
 foldleftListi32i32' l63 1 p75

c65 acc65 b66 =
 _prim_int_mul acc65 b66

grinMain _75 =
 p77 <- Nil'i32
 p78 <- Cons'i32 3 p77
 l78 <-  Cons'i32 2 p78
 p86 <- pure (P1c80 )
 l186 <-  mapListi32i32' l78 p86
 p88 <- Nil'i32
 l288 <-  Cons'i32 7 p88
 l390 <-  appendListi32' l186 l288
 s92 <-  sumList' l390
 p94 <-  productList' l390
 p96 <- _prim_int_add s92 p94
 p97 <- _prim_int_str p96
 _98 <-  _prim_string_print p97
 pure 0

c80 v80 =
 _prim_int_add v80 1

apply1_List_i32_to_List_i32 p100 p101 =
 case p100 of
  (P1c37 p102) ->
   c37 p102 p101
  (P1c44 p103 p104) ->
   c44 p103 p104 p101

apply1_i32_to_i32 p105 p106 =
 case p105 of
  (P1c53 p107) ->
   c53 p107 p106
  (P1c65 p108) ->
   c65 p108 p106
  (P1c80) ->
   c80 p106

apply2_i32_to_List_i32 p109 p110 =
 case p109 of
  (P2c37) ->
   pure (P1c37 p110)
  (P2c44 p111) ->
   pure (P1c44 p111 p110)

apply2_i32_to_i32 p112 p113 =
 case p112 of
  (P2c53) ->
   pure (P1c53 p113)
  (P2c65) ->
   pure (P1c65 p113)""")
    )
  }

  test("check list filter with comparison operator") {
    fuse("""
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun filter[A](self, f: A -> bool) -> List[A]
        List::fold_right(self, Nil[A], (h, t) => {
            match f(h):
                true => Cons(h, t)
                false => t
        })

fun main() -> i32
    let l = Cons(1, Cons(2, Cons(3, Nil)))
    let l2 = l.filter(e => e > 1)
    0
        """)
  }

  test("check do expr build output") {
    fuse("""
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];
  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];
  fun map[B](self, f: A -> B) -> Self[B];

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)
  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None
  fun map[B](self, f: A -> B) -> Option[B]
    match self:
      Some(v) => Some(f(v))
      _ => None

fun main() -> i32
  let x = Some(1)
  let y = Some(2)
  let result = x.flat_map(i => y.map(j => i + j))
  match result:
    Some(v) => {
      _print(int_to_str(v))
      0
    }
    _ => 1
        """)
  }

  test("check multi-param lambda unused") {
    fuse("""
fun main() -> i32
    let f = a => a + 1
    let g = (x, y) => x + y
    _print(int_to_str(f(5)))
    0
        """)
  }

  test("check method call chaining") {
    fuse("""
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

    fun filter[A](self, f: A -> bool) -> List[A]
        List::fold_right(self, Nil[A], (h, t) => {
            match f(h):
                true => Cons(h, t)
                false => t
        })

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let result = l.map(v => v + 1).filter(e => e > 3)
    let s = List::sum(result)
    _print(int_to_str(s))
    0
        """)
  }

  test("check monad trait flat_map with nested map") {
    fuse("""
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];
  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];
  fun map[B](self, f: A -> B) -> Self[B];

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)
  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None
  fun map[B](self, f: A -> B) -> Option[B]
    match self:
      Some(v) => Some(f(v))
      _ => None

fun main() -> i32
  let x = Some(1)
  let y = Some(2)
  let result = x.flat_map(i => y.map(j => i + j))
  match result:
    Some(v) => v
    _ => 0
    """)
  }

}

class CompilerBuildTests extends CompilerTests {
  import CompilerTests.*

  test("build unit function") {
    fuse(
      """
fun greetings() -> Unit
  _print("Hello World")

fun main() -> Unit
  greetings()
    """,
      BuildOutput("""
greetings _0 =
 _prim_string_print #"Hello World"

grinMain _1 =
 greetings 0""")
    )
  }
  test("build simple sum type") {
    fuse(
      """
type Animal:
  Dog
  Cat

fun value(a: Animal) -> i32
  match a:
      Dog => 0
      Cat => 1

fun main() -> i32
    value(Dog)
        """,
      BuildOutput("""
Dog =  store (CDog)

Cat =  store (CCat)

value a0 =
 p3 <- fetch a0
 case p3 of
  (CDog ) ->
   pure 0
  #default ->
   pure 1

grinMain _3 =
 p5 <- Dog
 value p5""")
    )
  }
  test("build integer addition") {
    fuse(
      """
fun main() -> i32
    2 + 2
    """,
      BuildOutput("""
grinMain _0 =
 _prim_int_add 2 2""")
    )
  }
  test("build float addition") {
    fuse(
      """
fun main() -> f32
    2.0 + 2.0
    """,
      BuildOutput("""
grinMain _0 =
 _prim_float_add 2.0 2.0""")
    )
  }
  test("build string addition") {
    fuse(
      """
fun main() -> str
    "Hello" + "World"
    """,
      BuildOutput("""
grinMain _0 =
 _prim_string_concat #"Hello" #"World"""")
    )
  }
  test("build integer subtraction") {
    fuse(
      """
fun main() -> i32
    2 - 2
    """,
      BuildOutput("""
grinMain _0 =
 _prim_int_sub 2 2""")
    )
  }
  test("build float multiplication") {
    fuse(
      """
fun main() -> f32
    2.0 * 2.0
    """,
      BuildOutput("""
grinMain _0 =
 _prim_float_mul 2.0 2.0""")
    )
  }
  test("build float division") {
    fuse(
      """
fun main() -> f32
    2.0 / 2.0
    """,
      BuildOutput("""
grinMain _0 =
 _prim_float_div 2.0 2.0""")
    )
  }
  test("build int modulo") {
    fuse(
      """
fun main() -> i32
    10 % 2
    """,
      BuildOutput("""
ffi pure
  _prim_int_mod :: T_Int64 -> T_Int64 -> T_Int64

grinMain _0 =
 _prim_int_mod 10 2""")
    )
  }
  test("build arithmetic expression") {
    fuse(
      """
fun main() -> i32
    2 + 3 * 6
    """,
      BuildOutput("""
grinMain _0 =
 p2 <- _prim_int_mul 3 6
 _prim_int_add 2 p2""")
    )
  }
  test("build int equal") {
    fuse(
      """
fun main() -> bool
    10 == 10
    """,
      BuildOutput("""
grinMain _0 =
 _prim_int_eq 10 10""")
    )
  }
  test("build str not equal") {
    fuse(
      """
fun main() -> bool
    "Hello" != "World"
    """,
      BuildOutput("""
ffi pure
  _prim_string_ne :: T_String -> T_String -> T_Bool

grinMain _0 =
 _prim_string_ne #"Hello" #"World"""")
    )
  }
  test("build and") {
    fuse(
      """
fun main() -> bool
    1 != 2 && 3 == 4
    """,
      BuildOutput("""
ffi pure
  _prim_bool_and :: T_Bool -> T_Bool -> T_Bool

grinMain _0 =
 p2 <- _prim_int_ne 1 2
 p3 <- _prim_int_eq 3 4
 _prim_bool_and p2 p3""")
    )
  }
  test("build or") {
    fuse(
      """
fun main() -> bool
    1 != 2 || 3 == 4
    """,
      BuildOutput("""
ffi pure
  _prim_bool_or :: T_Bool -> T_Bool -> T_Bool

grinMain _0 =
 p2 <- _prim_int_ne 1 2
 p3 <- _prim_int_eq 3 4
 _prim_bool_or p2 p3""")
    )
  }
  test("build inline lambda with type annotation") {
    fuse(
      """
fun main() -> i32
    let value = (a: i32) => a + 1
    value(1)
        """,
      BuildOutput("""
grinMain _0 =
 value2 1

value2 a2 =
 _prim_int_add a2 1
        """)
    )
  }
  test("build inline lambda with two variables with type annotations") {
    fuse(
      """
fun main() -> i32
    let value = (a: i32, b: i32) => a + b + 2
    value(1, 2)
        """,
      BuildOutput("""
grinMain _0 =
 value2 1 2

value2 a2 b3 =
 p5 <- _prim_int_add a2 b3
 _prim_int_add p5 2
        """)
    )
  }
  test("build inline lambda") {
    fuse(
      """
fun main() -> i32
    let value = (a) => a + 1
    value(1)
        """,
      BuildOutput("""
grinMain _0 =
 value2 1

value2 a2 =
 _prim_int_add a2 1
        """)
    )
  }
  test("build inline lambda with two variables") {
    fuse(
      """
fun main() -> i32
    let value = (a, b) => a + b + 2
    value(1, 2)
        """,
      BuildOutput("""
grinMain _0 =
 value2 1 2

value2 a2 b3 =
 p5 <- _prim_int_add a2 b3
 _prim_int_add p5 2
        """)
    )
  }
  test("build generic function") {
    fuse(
      """
fun identity[T](v: T) -> T
    v

fun main() -> Unit
    let s = identity("Hello World")
    _print(s)
        """,
      BuildOutput("""
identity'str v0 =
 pure v0

grinMain _1 =
 s2 <-  identity'str #"Hello World"
 _prim_string_print s2
        """)
    )
  }
  test("build generic function with multiple instantiations") {
    fuse(
      """
fun identity[T](v: T) -> T
    v

fun main() -> i32
    let s = identity("Hello World")
    let i = identity(1)
    _print(s)
    i
        """,
      BuildOutput("""
identity'str v0 =
 pure v0

identity'i32 v1 =
 pure v1

grinMain _2 =
 s3 <-  identity'str #"Hello World"
 i4 <-  identity'i32 1
 _6 <-  _prim_string_print s3
 pure i4
        """)
    )
  }
  test("build generic function that calls different generic function") {
    fuse(
      """
fun identity[T](v: T) -> T
    v

fun id[A](a: A) -> A
  identity(a)

fun main() -> i32
    let s = id("Hello World")
    let i = id(5)
    let f = id(99.9)
    _print(s)
    i
        """,
      BuildOutput("""
identity'str v0 =
 pure v0

identity'i32 v1 =
 pure v1

identity'f32 v2 =
 pure v2

id'str a3 =
 identity'str a3

id'i32 a4 =
 identity'i32 a4

id'f32 a5 =
 identity'f32 a5

grinMain _6 =
 s7 <-  id'str #"Hello World"
 i8 <-  id'i32 5
 f9 <-  id'f32 99.9
 _11 <-  _prim_string_print s7
 pure i8
        """)
    )
  }
  test("build function with params using generics") {
    fuse(
      """
fun summarize(a: str, b: str) -> str
  a + b

fun main() -> i32
    let s = summarize("Hello", "World")
    _print(s)
    0
        """,
      BuildOutput("""
summarize a0 b1 =
 _prim_string_concat a0 b1

grinMain _2 =
 s4 <-  summarize #"Hello" #"World"
 _6 <-  _prim_string_print s4
 pure 0
        """)
    )
  }
  test("build multiple functions using generics") {
    fuse(
      """
fun summarize(a: str, b: str) -> str
  a + ": " + b

fun addition[T: Add](a: T, b: T) -> T
  a + b
  
fun main() -> i32
    let s = summarize("Hello", "World")
    let a = addition(2, 3)
    _print(s)
    a
        """,
      BuildOutput("""
summarize a0 b1 =
 p3 <- _prim_string_concat a0 #": "
 _prim_string_concat p3 b1

additioni32Add' a3 b4 =
 _prim_int_add a3 b4

grinMain _5 =
 s7 <-  summarize #"Hello" #"World"
 a8 <-  additioni32Add' 2 3
 _10 <-  _prim_string_print s7
 pure a8
        """)
    )
  }
  test("build simple trait") {
    fuse(
      """
trait Summary:
  fun summarize(self) -> str;

type Tweet:
  username: str
  content: str

impl Summary for Tweet:
  fun summarize(self) -> str
    self.username + ": " + self.content

fun notify[T: Summary](s: T) -> Unit
  _print("Breaking news! " + s.summarize())

fun main() -> i32
    let tweet = Tweet("elon", "work!")
    _print(tweet.summarize())
    notify(tweet)
    0
        """,
      BuildOutput("""
Tweet username0 content1 =
 store (CTweet username0 content1)

summarizeTweetSummary' self4 =
 p7 <- fetch self4
 p10 <- do
   case p7 of
    (CTweet p8 p9) ->
     pure p8
 p11 <- _prim_string_concat p10 #": "
 p13 <- fetch self4
 p16 <- do
   case p13 of
    (CTweet p14 p15) ->
     pure p15
 _prim_string_concat p11 p16

notifyTweetSummary' s16 =
 p19 <- summarizeTweetSummary' s16
 p20 <- _prim_string_concat #"Breaking news! " p19
 _prim_string_print p20

grinMain _20 =
 tweet22 <-  Tweet #"elon" #"work!"
 p25 <- summarizeTweetSummary' tweet22
 _27 <-  _prim_string_print p25
 _28 <-  notifyTweetSummary' tweet22
 pure 0
        """)
    )
  }
  test("build simple trait with no-op generic func") {
    fuse(
      """
trait Summary:
  fun summarize(self) -> str;

type Tweet:
  username: str
  content: str

impl Summary for Tweet:
  fun summarize(self) -> str
    self.username + ": " + self.content

fun notify[T: Summary](s: T) -> str
  "no news!"

fun main() -> i32
    let tweet = Tweet("elon", "work!")
    _print(tweet.summarize())
    notify(tweet)
    0
        """,
      BuildOutput("""
Tweet username0 content1 =
 store (CTweet username0 content1)

summarizeTweetSummary' self4 =
 p7 <- fetch self4
 p10 <- do
   case p7 of
    (CTweet p8 p9) ->
     pure p8
 p11 <- _prim_string_concat p10 #": "
 p13 <- fetch self4
 p16 <- do
   case p13 of
    (CTweet p14 p15) ->
     pure p15
 _prim_string_concat p11 p16

notifyTweetSummary' s16 =
 pure #"no news!"

grinMain _17 =
 tweet19 <-  Tweet #"elon" #"work!"
 p22 <- summarizeTweetSummary' tweet19
 _24 <-  _prim_string_print p22
 _25 <-  notifyTweetSummary' tweet19
 pure 0
        """)
    )
  }
  test("build addition type bounds") {
    fuse(
      """
fun plus[T: Add](a: T, b: T) -> T
  a + b

fun main() -> i32
  plus(2, 3)
        """,
      BuildOutput("""
plusi32Add' a0 b1 =
 _prim_int_add a0 b1

grinMain _2 =
 plusi32Add' 2 3
       """)
    )
  }
  test("build generic record type") {
    fuse(
      """
type X[T]:
  v: T

fun main() -> i32
  let x = X(1)
  x.v
        """,
      BuildOutput("""
X'i32 v0 =
 store (CX v0)

grinMain _2 =
 x3 <-  X'i32 1
 p6 <- fetch x3
 p8 <- do
   case p6 of
    (CX p7) ->
     pure p7
 pure p8""")
    )
  }
  test("build generic point type") {
    fuse(
      """
type Point[T, V]:
  x: T
  y: V

fun main() -> i32
  let p = Point(1, "2")
  p.x
        """,
      BuildOutput("""
Point'i32'str x0 y1 =
 store (CPoint x0 y1)

grinMain _4 =
 p5 <-  Point'i32'str 1 #"2"
 p8 <- fetch p5
 p11 <- do
   case p8 of
    (CPoint p9 p10) ->
     pure p9
 pure p11
    """)
    )
  }
  test("build generic tuple type") {
    fuse(
      """
type Tuple[A, B](A, B)

fun main() -> i32
  let t = Tuple(1, "2")
  t.1
        """,
      BuildOutput("""
Tuple'i32'str t10 t21 =
 store (CTuple t10 t21)

grinMain _4 =
 t5 <-  Tuple'i32'str 1 #"2"
 p8 <- fetch t5
 p11 <- do
   case p8 of
    (CTuple p9 p10) ->
     pure p9
 pure p11
        """)
    )
  }
  test("build sum type") {
    fuse(
      """
type OptionI32:
    None
    Some(i32)

fun main() -> i32
    let o = Some(5)
    match o:
        Some(v) => v
        None => 1
        """,
      BuildOutput("""
None =  store (CNone)

Some t10 =
 store (CSome t10)

grinMain _2 =
 o4 <-  Some 5
 p7 <- fetch o4
 case p7 of
  (CSome v7) ->
   pure v7
  #default ->
   pure 1""")
    )
  }
  test("build generic sum type") {
    fuse(
      """
type Option[A]:
    None
    Some(A)

fun main() -> i32
    let o = Some(5)
    match o:
        Some(v) => v
        None => 1
        """,
      BuildOutput("""
None'i32 =  store (CNonei32)

Some'i32 t10 =
 store (CSomei32 t10)

grinMain _2 =
 o3 <-  Some'i32 5
 p6 <- fetch o3
 case p6 of
  (CSomei32 v6) ->
   pure v6
  #default ->
   pure 1
        """)
    )
  }
  test("build generic sum type with map function") {
    fuse(
      """
type Option[A]:
    None
    Some(A)

impl Option[A]:
  fun map[B](self, f: A -> B) -> Option[B]
    match self:
      Some(v) => Some(f(v))
      _ => None

fun main() -> i32
    let o = Some(5)
    o.map(a => a + 1)
    0
        """,
      BuildOutput("""None'i32 =  store (CNonei32)

Some'i32 t10 =
 store (CSomei32 t10)

mapOptioni32i32' self2 f''3 =
 p6 <- fetch self2
 case p6 of
  (CSomei32 v6) ->
   p8 <- apply1_i32_to_i32 f''3 v6
   p9 <- Some'i32 p8
   pure p9
  #default ->
   p10 <- None'i32
   pure p10

grinMain _10 =
 o11 <-  Some'i32 5
 p19 <- pure (P1c13 )
 _19 <-  mapOptioni32i32' o11 p19
 pure 0

c13 a13 =
 _prim_int_add a13 1

apply1_i32_to_i32 p21 p22 =
 case p21 of
  (P1c13) ->
   c13 p22
""")
    )
  }
  test("build generic trait functor implementation") {
    fuse(
      """
trait Functor[A]:
  fun map[B](self, f: A -> B) -> Self[B];

type Option[T]:
  Some(T)
  None

impl Functor for Option[A]:
  fun map[B](self, f: A -> B) -> Option[B]
    match self:
      Some(v) => Some(f(v))
      _ => None

fun fmap[A, B, F: Functor](f: A -> B, c: F[A]) -> F[B]
    c.map(f)

fun main() -> i32
    let o = Some(5)
    fmap(a => a + 1, o)
    0
        """,
      BuildOutput("""Some'i32 t10 =
 store (CSomei32 t10)

None'i32 =  store (CNonei32)

mapOptionFunctori32i32' self2 f''3 =
 p6 <- fetch self2
 case p6 of
  (CSomei32 v6) ->
   p8 <- apply1_i32_to_i32 f''3 v6
   p9 <- Some'i32 p8
   pure p9
  #default ->
   p10 <- None'i32
   pure p10

fmap'i32'i32'Option f''10 c11 =
 mapOptionFunctori32i32' c11 f''10

grinMain _12 =
 o13 <-  Some'i32 5
 p21 <- pure (P1c15 )
 _21 <-  fmap'i32'i32'Option p21 o13
 pure 0

c15 a15 =
 _prim_int_add a15 1

apply1_i32_to_i32 p23 p24 =
 case p23 of
  (P1c15) ->
   c15 p24
""")
    )
  }
  test("build minimal generic option") {
    fuse(
      """
type Option[A]:
    None
    Some(A)

impl Option[A]:
    fun map[B](self, f: A -> B) -> Option[B]
        match self:
            Some(v) => Some(f(v))
            _ => None

    fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
        match self:
            Some(a) => f(a)
            None => None

fun main() -> i32
    0
        """,
      BuildOutput("""grinMain _0 =
 pure 0
""")
    )
  }
  test("build generic option with map") {
    fuse(
      """
type Option[A]:
    None
    Some(A)

impl Option[A]:
    fun map[B](self, f: A -> B) -> Option[B]
        match self:
            Some(v) => Some(f(v))
            _ => None

fun main() -> i32
    let o = Some(5)
    let o1 = o.map(a => a + 1)
    match o1:
        Some(v) => v
        None => 0
        """,
      BuildOutput("""None'i32 =  store (CNonei32)

Some'i32 t10 =
 store (CSomei32 t10)

mapOptioni32i32' self2 f''3 =
 p6 <- fetch self2
 case p6 of
  (CSomei32 v6) ->
   p8 <- apply1_i32_to_i32 f''3 v6
   p9 <- Some'i32 p8
   pure p9
  #default ->
   p10 <- None'i32
   pure p10

grinMain _10 =
 o11 <-  Some'i32 5
 p19 <- pure (P1c13 )
 o119 <-  mapOptioni32i32' o11 p19
 p22 <- fetch o119
 case p22 of
  (CSomei32 v22) ->
   pure v22
  #default ->
   pure 0

c13 a13 =
 _prim_int_add a13 1

apply1_i32_to_i32 p24 p25 =
 case p24 of
  (P1c13) ->
   c13 p25
""")
    )
  }
  test("build generic option") {
    fuse(
      """
type Option[A]:
    None
    Some(A)

impl Option[A]:
    fun is_some(self) -> bool
        match self:
            Some(v) => true
            _ => false

    fun is_none(self) -> bool
        match self:
            Some(v) => false
            _ => true

    fun map[B](self, f: A -> B) -> Option[B]
        match self:
            Some(v) => Some(f(v))
            _ => None

    fun get_or_else(self, default: A) -> A
        match self:
            Some(a) => a
            None => default

    fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
        match self:
            Some(a) => f(a)
            None => None

    fun filter(self, f: A -> bool) -> Option[A]
        match self:
            Some(a) => {
                match f(a):
                    true => self
                    _ => None
            }
            _ => None

    fun to_str(v: i32) -> Option[str]
        let s = Some(v)
        s.map(a => int_to_str(a))

fun main() -> i32
    let o = Some(5)
    let o1 = o.flat_map(t => Some(t + 1))
    let l = Option::to_str(5)
    match l:
        Some(v) => 0
        None => 1
        """,
      BuildOutput("""None'i32 =  store (CNonei32)

Some'i32 t10 =
 store (CSomei32 t10)

Some'str t12 =
 store (CSomestr t12)

mapOptioni32str' self4 f''5 =
 p8 <- fetch self4
 case p8 of
  (CSomei32 v8) ->
   p10 <- apply1_i32_to_str f''5 v8
   p11 <- Some'str p10
   pure p11
  #default ->
   p12 <- None'i32
   pure p12

flatmapOptioni32i32' self12 f''13 =
 p16 <- fetch self12
 case p16 of
  (CSomei32 a16) ->
   p18 <- apply1_i32_to_Option_i32 f''13 a16
   pure p18
  #default ->
   p19 <- None'i32
   pure p19

tostrOption' v19 =
 s20 <-  Some'i32 v19
 p28 <- pure (P1c22 )
 mapOptioni32str' s20 p28

c22 a22 =
 _prim_int_str a22

grinMain _28 =
 o29 <-  Some'i32 5
 p38 <- pure (P1c31 )
 o138 <-  flatmapOptioni32i32' o29 p38
 l40 <-  tostrOption' 5
 p43 <- fetch l40
 case p43 of
  (CSomestr v43) ->
   pure 0
  #default ->
   pure 1

c31 t31 =
 p33 <- _prim_int_add t31 1
 Some'i32 p33

apply1_i32_to_Option_i32 p45 p46 =
 case p45 of
  (P1c31) ->
   c31 p46

apply1_i32_to_str p47 p48 =
 case p47 of
  (P1c22) ->
   c22 p48
""")
    )
  }
  test("build function on record with tuple type") {
    fuse(
      """
type Tuple[A, B](A, B)

type State[S, A]:
  run: S -> Tuple[A, S]

fun value(a: State[i32, i32]) -> i32
  let t = (a.run)(1)
  t.1 + t.2
  
fun main() -> i32
  value(State(a => Tuple(a + 1, a + 2)))
        """,
      BuildOutput("""Tuple'i32'i32 t10 t21 =
 store (CTuple t10 t21)

State'i32'i32 run''4 =
 store (CState run''4)

value a6 =
 p9 <- fetch a6
 p11'' <- do
   case p9 of
    (CState p10) ->
     pure p10
 t13 <-  apply1_i32_to_Tuple_i32__i32 p11'' 1
 p16 <- fetch t13
 p19 <- do
   case p16 of
    (CTuple p17 p18) ->
     pure p17
 p21 <- fetch t13
 p24 <- do
   case p21 of
    (CTuple p22 p23) ->
     pure p23
 _prim_int_add p19 p24

grinMain _24 =
 p34 <- pure (P1c26 )
 p35 <- State'i32'i32 p34
 value p35

c26 a26 =
 p28 <- _prim_int_add a26 1
 p29 <- _prim_int_add a26 2
 Tuple'i32'i32 p28 p29

apply1_i32_to_Tuple_i32__i32 p36 p37 =
 case p36 of
  (P1c26) ->
   c26 p37
""")
    )
  }
  test("build generic list with fold right") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun foldRight[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::foldRight(xs, z, f))
            Nil => z

    fun map[B](self, f: A -> B) -> List[B]
        List::foldRight(self, Nil[B], (h, t) => Cons(f(h), t))

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    match l1:
        Cons(h, t) => {
            _print(int_to_str(h))
            0
        }
        Nil => 1
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CConsi32 h0 t1)

Nil'i32 =  store (CNili32)

foldRightListi32Listi32' as4 z5 f''6 =
 p9 <- fetch as4
 case p9 of
  (CConsi32 x9 xs'10) ->
   p12'' <- apply2_i32_to_List_i32 f''6 x9
   p13 <- foldRightListi32Listi32' xs'10 z5 f''6
   p14 <- apply1_List_i32_to_List_i32 p12'' p13
   pure p14
  #default ->
   pure z5

mapListi32i32' self14 f''15 =
 p17 <- Nil'i32
 p24 <- store f''15
 p25 <- pure (P2c18 p24)
 foldRightListi32Listi32' self14 p17 p25

c18 f''1519 h20 t21 =
 f''151920 <- fetch f''1519
 p23 <- apply1_i32_to_i32 f''151920 h20
 Cons'i32 p23 t21

grinMain _25 =
 p27 <- Nil'i32
 p28 <- Cons'i32 3 p27
 l28 <-  Cons'i32 2 p28
 p36 <- pure (P1c30 )
 l136 <-  mapListi32i32' l28 p36
 p39 <- fetch l136
 case p39 of
  (CConsi32 h39 t'40) ->
   p42 <- _prim_int_str h39
   _43 <-  _prim_string_print p42
   pure 0
  #default ->
   pure 1

c30 v30 =
 _prim_int_add v30 1

apply1_List_i32_to_List_i32 p45 p46 =
 case p45 of
  (P1c18 p47 p48) ->
   c18 p47 p48 p46

apply1_i32_to_i32 p49 p50 =
 case p49 of
  (P1c30) ->
   c30 p50

apply2_i32_to_List_i32 p51 p52 =
 case p51 of
  (P2c18 p53) ->
   pure (P1c18 p53 p52)""")
    )
  }
  test("build generic list with simple map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun simple_map[B](self, f: A -> B) -> List[B]
        match self:
            Cons(h, t) => Cons(f(h), Nil[B])
            Nil => Nil[B]

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.simple_map(v => v + 1)
    0
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CConsi32 h0 t1)

Nil'i32 =  store (CNili32)

simplemapListi32i32' self4 f''5 =
 p8 <- fetch self4
 case p8 of
  (CConsi32 h8 t'9) ->
   p11 <- apply1_i32_to_i32 f''5 h8
   p12 <- Nil'i32
   p13 <- Cons'i32 p11 p12
   pure p13
  #default ->
   p14 <- Nil'i32
   pure p14

grinMain _14 =
 p16 <- Nil'i32
 p17 <- Cons'i32 3 p16
 l17 <-  Cons'i32 2 p17
 p25 <- pure (P1c19 )
 l125 <-  simplemapListi32i32' l17 p25
 pure 0

c19 v19 =
 _prim_int_add v19 1

apply1_i32_to_i32 p27 p28 =
 case p27 of
  (P1c19) ->
   c19 p28
""")
    )
  }

  test("build generic list with varied parameter names") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun map_with_mapper[B](self, mapper: A -> B) -> List[B]
        match self:
            Cons(h, t) => Cons(mapper(h), Nil[B])
            Nil => Nil[B]

    fun map_with_transform[B](self, transform: A -> B) -> List[B]
        match self:
            Cons(h, t) => Cons(transform(h), Nil[B])
            Nil => Nil[B]

    fun map_with_callback[B](self, callback: A -> B) -> List[B]
        match self:
            Cons(h, t) => Cons(callback(h), Nil[B])
            Nil => Nil[B]

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map_with_mapper(x => x + 1)
    let l2 = l.map_with_transform(y => y * 2)
    let l3 = l.map_with_callback(z => z + 3)
    0
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CConsi32 h0 t1)

Nil'i32 =  store (CNili32)

mapwithmapperListi32i32' self4 mapper''5 =
 p8 <- fetch self4
 case p8 of
  (CConsi32 h8 t'9) ->
   p11 <- apply1_i32_to_i32 mapper''5 h8
   p12 <- Nil'i32
   p13 <- Cons'i32 p11 p12
   pure p13
  #default ->
   p14 <- Nil'i32
   pure p14

mapwithtransformListi32i32' self14 transform''15 =
 p18 <- fetch self14
 case p18 of
  (CConsi32 h18 t'19) ->
   p21 <- apply1_i32_to_i32 transform''15 h18
   p22 <- Nil'i32
   p23 <- Cons'i32 p21 p22
   pure p23
  #default ->
   p24 <- Nil'i32
   pure p24

mapwithcallbackListi32i32' self24 callback''25 =
 p28 <- fetch self24
 case p28 of
  (CConsi32 h28 t'29) ->
   p31 <- apply1_i32_to_i32 callback''25 h28
   p32 <- Nil'i32
   p33 <- Cons'i32 p31 p32
   pure p33
  #default ->
   p34 <- Nil'i32
   pure p34

grinMain _34 =
 p36 <- Nil'i32
 p37 <- Cons'i32 3 p36
 l37 <-  Cons'i32 2 p37
 p45 <- pure (P1c39 )
 l145 <-  mapwithmapperListi32i32' l37 p45
 p53 <- pure (P1c47 )
 l253 <-  mapwithtransformListi32i32' l37 p53
 p61 <- pure (P1c55 )
 l361 <-  mapwithcallbackListi32i32' l37 p61
 pure 0

c39 x39 =
 _prim_int_add x39 1

c47 y47 =
 _prim_int_mul y47 2

c55 z55 =
 _prim_int_add z55 3

apply1_i32_to_i32 p63 p64 =
 case p63 of
  (P1c39) ->
   c39 p64
  (P1c47) ->
   c47 p64
  (P1c55) ->
   c55 p64
""")
    )
  }
  test("build generic list with iter map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun map_2[B](self, f: A -> B) -> List[B]
        let iter = (l: List[A], acc: List[B]) => {
            match l:
                Cons(h, t) => iter(t, Cons(f(h), acc))
                Nil => acc
        }
        iter(self, Nil[B])

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map_2(v => v + 1)
    match l1:
        Cons(h, t) => {
            _print(int_to_str(h))
            0
        }
        Nil => 1
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CConsi32 h0 t1)

Nil'i32 =  store (CNili32)

map2Listi32i32' self4 f''5 =
 p20 <- Nil'i32
 iter7 f''5 self4 p20

iter7 f''58 l9 acc10 =
 f''589 <- fetch f''58
 p13 <- fetch l9
 case p13 of
  (CConsi32 h13 t'14) ->
   p16 <- apply1_i32_to_i32 f''589 h13
   p17 <- Cons'i32 p16 acc10
   p18 <- iter7 f''58 t'14 p17
   pure p18
  #default ->
   pure acc10

grinMain _20 =
 p22 <- Nil'i32
 p23 <- Cons'i32 3 p22
 l23 <-  Cons'i32 2 p23
 p31 <- pure (P1c25 )
 l131 <-  map2Listi32i32' l23 p31
 p34 <- fetch l131
 case p34 of
  (CConsi32 h34 t'35) ->
   p37 <- _prim_int_str h34
   _38 <-  _prim_string_print p37
   pure 0
  #default ->
   pure 1

c25 v25 =
 _prim_int_add v25 1

apply1_i32_to_i32 p40 p41 =
 case p40 of
  (P1c25) ->
   c25 p41
""")
    )
  }
  test("build generic list with fold right using chained map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    l1.map(r => _print(int_to_str(r)))
    0
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CConsi32 h0 t1)

Cons'Unit h4 t5 =
 store (CConsUnit h4 t5)

Nil'i32 =  store (CNili32)

foldrightListi32Listi32' as8 z9 f''10 =
 p13 <- fetch as8
 case p13 of
  (CConsi32 x13 xs'14) ->
   p16'' <- apply2_i32_to_List_i32 f''10 x13
   p17 <- foldrightListi32Listi32' xs'14 z9 f''10
   p18 <- apply1_List_i32_to_List_i32 p16'' p17
   pure p18
  #default ->
   pure z9

foldrightListi32ListUnit' as18 z19 f''20 =
 p23 <- fetch as18
 case p23 of
  (CConsi32 x23 xs'24) ->
   p26'' <- apply2_i32_to_List_unit f''20 x23
   p27 <- foldrightListi32ListUnit' xs'24 z19 f''20
   p28 <- apply1_List_unit_to_List_unit p26'' p27
   pure p28
  #default ->
   pure z19

mapListi32i32' self28 f''29 =
 p31 <- Nil'i32
 p38 <- store f''29
 p39 <- pure (P2c32 p38)
 foldrightListi32Listi32' self28 p31 p39

c32 f''2933 h34 t35 =
 f''293334 <- fetch f''2933
 p37 <- apply1_i32_to_i32 f''293334 h34
 Cons'i32 p37 t35

mapListi32Unit' self39 f''40 =
 p42 <- Nil'i32
 p49 <- store f''40
 p50 <- pure (P2c43 p49)
 foldrightListi32ListUnit' self39 p42 p50

c43 f''4044 h45 t46 =
 f''404445 <- fetch f''4044
 p48 <- apply1_i32_to_unit f''404445 h45
 Cons'Unit p48 t46

grinMain _50 =
 p52 <- Nil'i32
 p53 <- Cons'i32 3 p52
 l53 <-  Cons'i32 2 p53
 p61 <- pure (P1c55 )
 l161 <-  mapListi32i32' l53 p61
 p70 <- pure (P1c63 )
 _70 <-  mapListi32Unit' l161 p70
 pure 0

c55 v55 =
 _prim_int_add v55 1

c63 r63 =
 p65 <- _prim_int_str r63
 _prim_string_print p65

apply1_List_i32_to_List_i32 p72 p73 =
 case p72 of
  (P1c32 p74 p75) ->
   c32 p74 p75 p73

apply1_List_unit_to_List_unit p76 p77 =
 case p76 of
  (P1c43 p78 p79) ->
   c43 p78 p79 p77

apply1_i32_to_i32 p80 p81 =
 case p80 of
  (P1c55) ->
   c55 p81

apply1_i32_to_unit p82 p83 =
 case p82 of
  (P1c63) ->
   c63 p83

apply2_i32_to_List_i32 p84 p85 =
 case p84 of
  (P2c32 p86) ->
   pure (P1c32 p86 p85)

apply2_i32_to_List_unit p87 p88 =
 case p87 of
  (P2c43 p89) ->
   pure (P1c43 p89 p88)""")
    )
  }

  test("build generic list with fold right using flat_map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun append[A](l1: List[A], l2: List[A]) -> List[A]
        List::fold_right(l1, l2, (h, t) => Cons(h, t))

    fun flat_map[B](self, f: A -> List[B]) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => List::append(f(h), t))

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

fun main() -> i32
    let l = Cons(1, Cons(2, Cons(3, Nil)))
    let l1 = l.flat_map(v => Cons(v, Cons(v * 10, Nil)))
    let s = List::sum(l1)
    _print(int_to_str(s))
    0
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CConsi32 h0 t1)

Nil'i32 =  store (CNili32)

foldrightListi32i32' as4 z5 f''6 =
 p9 <- fetch as4
 case p9 of
  (CConsi32 x9 xs'10) ->
   p12'' <- apply2_i32_to_i32 f''6 x9
   p13 <- foldrightListi32i32' xs'10 z5 f''6
   p14 <- apply1_i32_to_i32 p12'' p13
   pure p14
  #default ->
   pure z5

foldrightListi32Listi32' as14 z15 f''16 =
 p19 <- fetch as14
 case p19 of
  (CConsi32 x19 xs'20) ->
   p22'' <- apply2_i32_to_List_i32 f''16 x19
   p23 <- foldrightListi32Listi32' xs'20 z15 f''16
   p24 <- apply1_List_i32_to_List_i32 p22'' p23
   pure p24
  #default ->
   pure z15

appendListi32' l124 l225 =
 p30 <- pure (P2c27 )
 foldrightListi32Listi32' l124 l225 p30

c27 h27 t28 =
 Cons'i32 h27 t28

flatmapListi32i32' self30 f''31 =
 p33 <- Nil'i32
 p40 <- store f''31
 p41 <- pure (P2c34 p40)
 foldrightListi32Listi32' self30 p33 p41

c34 f''3135 h36 t37 =
 f''313536 <- fetch f''3135
 p39 <- apply1_i32_to_List_i32 f''313536 h36
 appendListi32' p39 t37

sumList' l41 =
 p53 <- pure (P2c43 )
 foldrightListi32i32' l41 0 p53

c43 acc43 b44 =
 _prim_int_add acc43 b44

grinMain _53 =
 p55 <- Nil'i32
 p56 <- Cons'i32 3 p55
 p57 <- Cons'i32 2 p56
 l57 <-  Cons'i32 1 p57
 p68 <- pure (P1c59 )
 l168 <-  flatmapListi32i32' l57 p68
 s70 <-  sumList' l168
 p72 <- _prim_int_str s70
 _73 <-  _prim_string_print p72
 pure 0

c59 v59 =
 p61 <- _prim_int_mul v59 10
 p62 <- Nil'i32
 p63 <- Cons'i32 p61 p62
 Cons'i32 v59 p63

apply1_List_i32_to_List_i32 p75 p76 =
 case p75 of
  (P1c27 p77) ->
   c27 p77 p76
  (P1c34 p78 p79) ->
   c34 p78 p79 p76

apply1_i32_to_List_i32 p80 p81 =
 case p80 of
  (P1c59) ->
   c59 p81

apply1_i32_to_i32 p82 p83 =
 case p82 of
  (P1c43 p84) ->
   c43 p84 p83

apply2_i32_to_List_i32 p85 p86 =
 case p85 of
  (P2c27) ->
   pure (P1c27 p86)
  (P2c34 p87) ->
   pure (P1c34 p87 p86)

apply2_i32_to_i32 p88 p89 =
 case p88 of
  (P2c43) ->
   pure (P1c43 p89)""")
    )
  }
  test("build list filter with comparison") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

    fun filter[A](self, f: A -> bool) -> List[A]
        List::fold_right(self, Nil[A], (h, t) => {
            match f(h):
                true => Cons(h, t)
                false => t
        })

fun main() -> i32
    let l = Cons(1, Cons(2, Cons(3, Nil)))
    let l2 = l.filter(e => e > 1)
    let s = List::sum(l2)
    _print(int_to_str(s))
    0
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CConsi32 h0 t1)

Nil'i32 =  store (CNili32)

foldrightListi32i32' as4 z5 f''6 =
 p9 <- fetch as4
 case p9 of
  (CConsi32 x9 xs'10) ->
   p12'' <- apply2_i32_to_i32 f''6 x9
   p13 <- foldrightListi32i32' xs'10 z5 f''6
   p14 <- apply1_i32_to_i32 p12'' p13
   pure p14
  #default ->
   pure z5

foldrightListi32Listi32' as14 z15 f''16 =
 p19 <- fetch as14
 case p19 of
  (CConsi32 x19 xs'20) ->
   p22'' <- apply2_i32_to_List_i32 f''16 x19
   p23 <- foldrightListi32Listi32' xs'20 z15 f''16
   p24 <- apply1_List_i32_to_List_i32 p22'' p23
   pure p24
  #default ->
   pure z15

sumList' l24 =
 p36 <- pure (P2c26 )
 foldrightListi32i32' l24 0 p36

c26 acc26 b27 =
 _prim_int_add acc26 b27

filterListi32' self36 f''37 =
 p39 <- Nil'i32
 p48 <- store f''37
 p49 <- pure (P2c40 p48)
 foldrightListi32Listi32' self36 p39 p49

c40 f''3741 h42 t43 =
 f''374142 <- fetch f''3741
 p46 <- apply1_i32_to_bool f''374142 h42
 case p46 of
  #True ->
   p47 <- Cons'i32 h42 t43
   pure p47
  #False ->
   pure t43

grinMain _49 =
 p51 <- Nil'i32
 p52 <- Cons'i32 3 p51
 p53 <- Cons'i32 2 p52
 l53 <-  Cons'i32 1 p53
 p61 <- pure (P1c55 )
 l261 <-  filterListi32' l53 p61
 s63 <-  sumList' l261
 p65 <- _prim_int_str s63
 _66 <-  _prim_string_print p65
 pure 0

c55 e55 =
 _prim_int_gt e55 1

apply1_List_i32_to_List_i32 p68 p69 =
 case p68 of
  (P1c40 p70 p71) ->
   c40 p70 p71 p69

apply1_i32_to_bool p72 p73 =
 case p72 of
  (P1c55) ->
   c55 p73

apply1_i32_to_i32 p74 p75 =
 case p74 of
  (P1c26 p76) ->
   c26 p76 p75

apply2_i32_to_List_i32 p77 p78 =
 case p77 of
  (P2c40 p79) ->
   pure (P1c40 p79 p78)

apply2_i32_to_i32 p80 p81 =
 case p80 of
  (P2c26) ->
   pure (P1c26 p81)""")
    )
  }

  test("build monad trait flat_map with nested map") {
    fuse(
      """
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];
  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];
  fun map[B](self, f: A -> B) -> Self[B];

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)
  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None
  fun map[B](self, f: A -> B) -> Option[B]
    match self:
      Some(v) => Some(f(v))
      _ => None

fun main() -> i32
  let x = Some(1)
  let y = Some(2)
  let result = x.flat_map(i => y.map(j => i + j))
  match result:
    Some(v) => {
      _print(int_to_str(v))
      0
    }
    _ => 1
    """,
      BuildOutput("""
Some'i32 t10 =
 store (CSomei32 t10)

None'i32 =  store (CNonei32)

flatmapOptionMonadi32i32' self2 f''3 =
 p6 <- fetch self2
 case p6 of
  (CSomei32 v6) ->
   p8 <- apply1_i32_to_Option_i32 f''3 v6
   pure p8
  #default ->
   p9 <- None'i32
   pure p9

mapOptionMonadi32i32' self9 f''10 =
 p13 <- fetch self9
 case p13 of
  (CSomei32 v13) ->
   p15 <- apply1_i32_to_i32 f''10 v13
   p16 <- Some'i32 p15
   pure p16
  #default ->
   p17 <- None'i32
   pure p17

grinMain _17 =
 x18 <-  Some'i32 1
 y19 <-  Some'i32 2
 p40 <- pure (P1c21 y19)
 result40 <-  flatmapOptionMonadi32i32' x18 p40
 p43 <- fetch result40
 case p43 of
  (CSomei32 v43) ->
   p45 <- _prim_int_str v43
   _46 <-  _prim_string_print p45
   pure 0
  #default ->
   pure 1

c21 y1922 i23 =
 p32 <- pure (P1c25 i23)
 mapOptionMonadi32i32' y1922 p32

c25 i2326 j26 =
 _prim_int_add i2326 j26

apply1_i32_to_Option_i32 p48 p49 =
 case p48 of
  (P1c21 p50) ->
   c21 p50 p49

apply1_i32_to_i32 p51 p52 =
 case p51 of
  (P1c25 p53) ->
   c25 p53 p52""")
    )
  }

  test("build nested generic ADT List[Opt[i32]]") {
    fuse(
      """
type List[A]:
  Cons(h: A, t: List[A])
  Nil

type Opt[A]:
  MySome(A)
  MyNone

fun sum_somes(l: List[Opt[i32]]) -> i32
  match l:
    Cons(h, t) => {
      match h:
        MySome(v) => v + sum_somes(t)
        MyNone => sum_somes(t)
    }
    Nil => 0

fun main() -> i32
  let l = Cons(MySome(1), Cons(MyNone[i32], Cons(MySome(3), Nil)))
  _print(int_to_str(sum_somes(l)))
  0
      """,
      BuildOutput("""Cons'Opti32 h0 t1 =
 store (CConsOpti32 h0 t1)

Nil'Opti32 =  store (CNilOpti32)

MySome'i32 t14 =
 store (CMySomei32 t14)

MyNone'i32 =  store (CMyNonei32)

sum_somes l6 =
 p9 <- fetch l6
 case p9 of
  (CConsOpti32 h'9 t'10) ->
   p13 <- fetch h'9
   p18 <-  case p13 of
     (CMySomei32 v13) ->
      p15 <- sum_somes t'10
      p16 <- _prim_int_add v13 p15
      pure p16
     #default ->
      p17 <- sum_somes t'10
      pure p17
   pure p18
  #default ->
   pure 0

grinMain _18 =
 p20 <- MySome'i32 1
 p21 <- MyNone'i32
 p22 <- MySome'i32 3
 p23 <- Nil'Opti32
 p24 <- Cons'Opti32 p22 p23
 p25 <- Cons'Opti32 p21 p24
 l25 <-  Cons'Opti32 p20 p25
 p27 <- sum_somes l25
 p28 <- _prim_int_str p27
 _29 <-  _prim_string_print p28
 pure 0""")
    )
  }

  test("build closure returned from function") {
    fuse(
      """
fun make_adder(n: i32) -> i32 -> i32
  x => x + n

fun main() -> i32
  let add5 = make_adder(5)
  _print(int_to_str(add5(10)))
  0
      """,
      BuildOutput("""make_adder n0 =
 pure (P1c2 n0)

c2 n03 x3 =
 _prim_int_add x3 n03

grinMain _8 =
 add5''10 <-  make_adder 5
 p12 <- apply1_i32_to_i32 add5''10 10
 p13 <- _prim_int_str p12
 _14 <-  _prim_string_print p13
 pure 0""")
    )
  }

  test("build io do notation with heterogeneous continuation param types") {
    // Regression: the sequenced effect expression produces two
    // continuations with the same return type but different parameter
    // types. Before the fix, both were grouped into a single dispatch
    // function, producing a backend node whose field held a mix of
    // concrete primitive types and tripping the heap-points-to analysis
    // with an "illegal node item type" error.
    fuse(
      """
type IO[A]:
  MkIO(() -> A)

impl IO[A]:
  fun exec(self) -> A
    match self:
      MkIO(f) => f(())

trait Monad[A]:
  fun unit[A](a: A) -> Self[A];
  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];
  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

impl Monad for IO[A]:
  fun flat_map[B](self, f: A -> IO[B]) -> IO[B]
    MkIO(_ => {
      let a = self.exec()
      let b = f(a)
      b.exec()
    })
  fun unit[A](a: A) -> IO[A]
    MkIO(_ => a)

fun main() -> i32
  let program = {
    do:
      s <- MkIO(_ => "hi")
      _ <- MkIO(_ => _print(s))
      0
  }
  program.exec()
      """,
      BuildOutput("""MkIO'str t1''0 =
 p3 <- store t1''0
 store (CMkIOstr p3)

MkIO'Unit t1''3 =
 p6 <- store t1''3
 store (CMkIOUnit p6)

MkIO'i32 t1''6 =
 p9 <- store t1''6
 store (CMkIOi32 p9)

execIOi32' self9 =
 p12 <- fetch self9
 case p12 of
  (CMkIOi32 p14) ->
   f12 <- fetch p14
   p15 <- apply1_unit_to_i32 f12 0
   pure p15

execIOstr' self15 =
 p18 <- fetch self15
 case p18 of
  (CMkIOstr p20) ->
   f18 <- fetch p20
   p21 <- apply1_TypeEVar_to_str f18 0
   pure p21

execIOUnit' self21 =
 p24 <- fetch self21
 case p24 of
  (CMkIOUnit p26) ->
   f24 <- fetch p26
   p27 <- apply1_unit_to_unit f24 0
   pure p27

mapMonadIOUniti32' self27 f''28 =
 p35 <- store f''28
 p36 <- pure (P1c30 p35)
 flatmapIOMonadUniti32' self27 p36

c30 f''2831 a32 =
 f''283132 <- fetch f''2831
 p34 <- apply1_unit_to_i32 f''283132 a32
 unitIOMonadi32' p34

flatmapIOMonadstri32' self36 f''37 =
 p48 <- store f''37
 p49 <- pure (P1c39 self36 p48)
 MkIO'i32 p49

c39 self3640 f''3742 _43 =
 f''374243 <- fetch f''3742
 a44 <-  execIOstr' self3640
 b46 <-  apply1_str_to_IO_i32 f''374243 a44
 execIOi32' b46

flatmapIOMonadUniti32' self49 f''50 =
 p61 <- store f''50
 p62 <- pure (P1c52 self49 p61)
 MkIO'i32 p62

c52 self4953 f''5055 _56 =
 f''505556 <- fetch f''5055
 a57 <-  execIOUnit' self4953
 b59 <-  apply1_unit_to_IO_i32 f''505556 a57
 execIOi32' b59

unitIOMonadi32' a62 =
 p67 <- pure (P1c64 a62)
 MkIO'i32 p67

c64 a6265 _65 =
 pure a6265

grinMain _67 =
 p75 <- pure (P1c69 )
 p76 <- MkIO'str p75
 p105 <- pure (P1c77 )
 program105 <-  flatmapIOMonadstri32' p76 p105
 execIOi32' program105

c69 _69 =
 pure #"hi"

c77 s77 =
 p86 <- pure (P1c79 s77)
 p87 <- MkIO'Unit p86
 p94 <- pure (P1c88 )
 mapMonadIOUniti32' p87 p94

c79 s7780 _80 =
 _prim_string_print s7780

c88 _88 =
 pure 0

apply1_TypeEVar_to_str p107 p108 =
 case p107 of
  (P1c69) ->
   c69 p108

apply1_str_to_IO_i32 p109 p110 =
 case p109 of
  (P1c77) ->
   c77 p110

apply1_unit_to_IO_i32 p111 p112 =
 case p111 of
  (P1c30 p113) ->
   c30 p113 p112

apply1_unit_to_i32 p114 p115 =
 case p114 of
  (P1c39 p116 p117) ->
   c39 p116 p117 p115
  (P1c52 p118 p119) ->
   c52 p118 p119 p115
  (P1c64 p120) ->
   c64 p120 p115
  (P1c79 p121) ->
   c79 p121 p115
  (P1c88) ->
   c88 p115""")
    )
  }

  test("build user-defined generic option with free function") {
    fuse(
      """
type MyOpt[A]:
  MySome(A)
  MyNone

fun or_else(o: MyOpt[i32], d: i32) -> i32
  match o:
    MySome(v) => v
    MyNone => d

fun main() -> i32
  _print(int_to_str(or_else(MySome(7), 0)))
  0
      """,
      BuildOutput("""MySome'i32 t10 =
 store (CMySomei32 t10)

MyNone'i32 =  store (CMyNonei32)

or_else o2 d3 =
 p6 <- fetch o2
 case p6 of
  (CMySomei32 v6) ->
   pure v6
  #default ->
   pure d3

grinMain _7 =
 p9 <- MySome'i32 7
 p10 <- or_else p9 0
 p11 <- _prim_int_str p10
 _12 <-  _prim_string_print p11
 pure 0""")
    )
  }

  test("build qualified nullary constructor") {
    fuse(
      """
type MyOpt[A]:
  MySome(A)
  MyNone

fun describe(o: MyOpt[i32]) -> str
  match o:
    MySome(v) => "some"
    MyNone => "none"

fun main() -> i32
  let o = MyNone[i32]
  _print(describe(o))
  0
      """,
      BuildOutput("""describe o0 =
 p3 <- fetch o0
 case p3 of
  (CMySomei32 v3) ->
   pure #"some"
  #default ->
   pure #"none"

grinMain _4 =
 o7 <-  store (CMyNonei32)
 p9 <- describe o7
 _10 <-  _prim_string_print p9
 pure 0""")
    )
  }

  test("build trait default method") {
    fuse(
      """
trait Greeter:
  fun hello(self) -> str;
  fun twice(self) -> str
    self.hello() + "-" + self.hello()

type Speaker:
  lang: str

impl Greeter for Speaker:
  fun hello(self) -> str
    "hello"

fun main() -> i32
  let s = Speaker("en")
  _print(s.twice())
  0
      """,
      BuildOutput("""twiceGreeterSpeakerSpeakerGreeter' self0 =
 p3 <- helloSpeakerGreeter' self0
 p4 <- _prim_string_concat p3 #"-"
 p6 <- helloSpeakerGreeter' self0
 _prim_string_concat p4 p6

Speaker lang6 =
 store (CSpeaker lang6)

helloSpeakerGreeter' self8 =
 pure #"hello"

grinMain _9 =
 s11 <-  Speaker #"en"
 p13 <- twiceGreeterSpeakerSpeakerGreeter' s11
 _15 <-  _prim_string_print p13
 pure 0""")
    )
  }

}

class CompilerExecTests extends CompilerTests {
  import CompilerTests.*

  test("execute generic phantom in return") {
    fuse(
      """
type MyOpt[A]:
  MySome(A)
  MyNone

fun empty[A]() -> MyOpt[A]
  MyNone[A]

fun main() -> i32
  let o: MyOpt[i32] = empty[i32]()
  match o:
    MySome(v) => v
    MyNone => {
      _print("0")
      0
    }
      """,
      ExecutableOutput("0")
    )
  }

  test("execute interpreter with env lookup") {
    fuse(
      """
type List[A]:
  Cons(h: A, t: List[A])
  Nil

type Entry:
  name: str
  value: i32

fun lookup(env: List[Entry], key: str) -> i32
  match env:
    Cons(h, t) => {
      match h.name == key:
        true => h.value
        false => lookup(t, key)
    }
    Nil => 0 - 1

fun main() -> i32
  let env = Cons(Entry("x", 5), Cons(Entry("y", 9), Nil))
  _print(int_to_str(lookup(env, "x")))
  0
      """,
      ExecutableOutput("5")
    )
  }

  test("execute generic list with simple map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun simple_map[B](self, f: A -> B) -> List[B]
        match self:
            Cons(h, t) => Cons(f(h), Nil[B])
            Nil => Nil[B]

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.simple_map(v => v + 1)
    match l1:
        Cons(h, t) => {
            _print(int_to_str(h))
            0
        }
        Nil => 1
      """,
      ExecutableOutput("3")
    )
  }
  test("execute generic option with map") {
    fuse(
      """
type Option[A]:
    None
    Some(A)

impl Option[A]:
    fun map[B](self, f: A -> B) -> Option[B]
        match self:
            Some(v) => Some(f(v))
            _ => None

fun main() -> i32
    let o = Some(5)
    let o1 = o.map(a => a + 1)
    match o1:
        Some(v) => {
            _print(int_to_str(v))
            0
        }
        None => 1
      """,
      ExecutableOutput("6")
    )
  }
  test("execute generic list with iter map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun map_2[B](self, f: A -> B) -> List[B]
        let iter = (l: List[A], acc: List[B]) => {
            match l:
                Cons(h, t) => iter(t, Cons(f(h), acc))
                Nil => acc
        }
        iter(self, Nil[B])

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map_2(v => v + 1)
    match l1:
        Cons(h, t) => {
            _print(int_to_str(h))
            0
        }
        Nil => 1
      """,
      ExecutableOutput("4")
    )
  }
  test("execute integer addition") {
    fuse(
      """
fun main() -> i32
    let result = 2 + 2
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("4")
    )
  }
  test("execute string addition") {
    fuse(
      """
fun main() -> i32
    let result = "Hello" + "World"
    _print(result)
    0
        """,
      ExecutableOutput("HelloWorld")
    )
  }
  test("execute integer subtraction") {
    fuse(
      """
fun main() -> i32
    let result = 2 - 2
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("0")
    )
  }
  test("execute int modulo") {
    fuse(
      """
fun main() -> i32
    let result = 10 % 2
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("0")
    )
  }
  test("execute arithmetic expression") {
    fuse(
      """
fun main() -> i32
    let result = 2 + 3 * 6
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("20")
    )
  }
  test("execute int equal") {
    fuse(
      """
fun main() -> i32
    let result = 10 == 10
    let output = {
        match result:
            true => 1
            false => 0
    }
    _print(int_to_str(output))
    0
        """,
      ExecutableOutput("1")
    )
  }
  test("execute str not equal") {
    fuse(
      """
fun main() -> i32
    let result = "Hello" != "World"
    let output = {
        match result:
            true => 1
            false => 0
    }
    _print(int_to_str(output))
    0
        """,
      ExecutableOutput("1")
    )
  }
  test("execute and") {
    fuse(
      """
fun main() -> i32
    let result = 1 != 2 && 3 == 4
    let output = {
        match result:
            true => 1
            false => 0
    }
    _print(int_to_str(output))
    0
        """,
      ExecutableOutput("0")
    )
  }
  test("execute or") {
    fuse(
      """
fun main() -> i32
    let result = 1 != 2 || 3 == 4
    let output = {
        match result:
            true => 1
            false => 0
    }
    _print(int_to_str(output))
    0
        """,
      ExecutableOutput("1")
    )
  }
  test("execute unit function") {
    fuse(
      """
fun greetings() -> Unit
  _print("Hello World")

fun main() -> i32
  greetings()
  0
        """,
      ExecutableOutput("Hello World")
    )
  }
  test("execute inline lambda with type annotation") {
    fuse(
      """
fun main() -> i32
    let value = (a: i32) => a + 1
    let result = value(1)
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("2")
    )
  }
  test("execute inline lambda with two variables with type annotations") {
    fuse(
      """
fun main() -> i32
    let value = (a: i32, b: i32) => a + b + 2
    let result = value(1, 2)
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("5")
    )
  }
  test("execute inline lambda") {
    fuse(
      """
fun main() -> i32
    let value = (a) => a + 1
    let result = value(1)
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("2")
    )
  }
  test("execute inline lambda with two variables") {
    fuse(
      """
fun main() -> i32
    let value = (a, b) => a + b + 2
    let result = value(1, 2)
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("5")
    )
  }
  test("execute generic function") {
    fuse(
      """
fun identity[T](v: T) -> T
    v

fun main() -> i32
    let s = identity("Hello World")
    _print(s)
    0
        """,
      ExecutableOutput("Hello World")
    )
  }
  test("execute generic function with multiple instantiations") {
    fuse(
      """
fun identity[T](v: T) -> T
    v

fun main() -> i32
    let s = identity("Hello World")
    let i = identity(1)
    _print(s + "\n" + int_to_str(i))
    0
        """,
      ExecutableOutput("Hello World\n1")
    )
  }
  test("execute generic function that calls different generic function") {
    fuse(
      """
fun identity[T](v: T) -> T
    v

fun id[A](a: A) -> A
  identity(a)

fun main() -> i32
    let s = id("Hello World")
    let i = id(5)
    _print(s + "\n" + int_to_str(i))
    0
        """,
      ExecutableOutput("Hello World\n5")
    )
  }
  test("execute function with params using generics") {
    fuse(
      """
fun summarize(a: str, b: str) -> str
  a + b

fun main() -> i32
    let s = summarize("Hello", "World")
    _print(s)
    0
        """,
      ExecutableOutput("HelloWorld")
    )
  }
  test("execute multiple functions using generics") {
    fuse(
      """
fun summarize(a: str, b: str) -> str
  a + ": " + b

fun addition[T: Add](a: T, b: T) -> T
  a + b

fun main() -> i32
    let s = summarize("Hello", "World")
    let a = addition(2, 3)
    _print(s + "\n" + int_to_str(a))
    0
        """,
      ExecutableOutput("Hello: World\n5")
    )
  }
  test("execute addition type bounds") {
    fuse(
      """
fun plus[T: Add](a: T, b: T) -> T
  a + b

fun main() -> i32
  let result = plus(2, 3)
  _print(int_to_str(result))
  0
        """,
      ExecutableOutput("5")
    )
  }
  test("execute simple trait") {
    fuse(
      """
trait Summary:
  fun summarize(self) -> str;

type Tweet:
  username: str
  content: str

impl Summary for Tweet:
  fun summarize(self) -> str
    self.username + ": " + self.content

fun notify[T: Summary](s: T) -> Unit
  _print("Breaking news! " + s.summarize())

fun main() -> i32
    let tweet = Tweet("elon", "work!")
    _print(tweet.summarize() + "\n")
    notify(tweet)
    0
        """,
      ExecutableOutput("elon: work!\nBreaking news! elon: work!")
    )
  }
  test("execute simple trait with no-op generic func") {
    fuse(
      """
trait Summary:
  fun summarize(self) -> str;

type Tweet:
  username: str
  content: str

impl Summary for Tweet:
  fun summarize(self) -> str
    self.username + ": " + self.content

fun notify[T: Summary](s: T) -> str
  "no news!"

fun main() -> i32
    let tweet = Tweet("elon", "work!")
    _print(tweet.summarize())
    notify(tweet)
    0
        """,
      ExecutableOutput("elon: work!")
    )
  }
  test("execute generic record type") {
    fuse(
      """
type X[T]:
  v: T

fun main() -> i32
  let x = X(1)
  _print(int_to_str(x.v))
  0
        """,
      ExecutableOutput("1")
    )
  }
  test("execute generic point type") {
    fuse(
      """
type Point[T, V]:
  x: T
  y: V

fun main() -> i32
  let p = Point(1, "2")
  _print(int_to_str(p.x))
  0
        """,
      ExecutableOutput("1")
    )
  }
  test("execute generic tuple type") {
    fuse(
      """
type Tuple[A, B](A, B)

fun main() -> i32
  let t = Tuple(1, "2")
  _print(int_to_str(t.1))
  0
        """,
      ExecutableOutput("1")
    )
  }
  test("execute simple sum type") {
    fuse(
      """
type Animal:
  Dog
  Cat

fun value(a: Animal) -> i32
  match a:
      Dog => 0
      Cat => 1

fun main() -> i32
    let result = value(Dog)
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("0")
    )
  }
  test("execute sum type") {
    fuse(
      """
type OptionI32:
    None
    Some(i32)

fun main() -> i32
    let o = Some(5)
    let result = {
        match o:
            Some(v) => v
            None => 1
    }
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("5")
    )
  }
  test("execute generic sum type") {
    fuse(
      """
type Option[A]:
    None
    Some(A)

fun main() -> i32
    let o = Some(5)
    let result = {
        match o:
            Some(v) => v
            None => 1
    }
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("5")
    )
  }
  test("execute generic sum type with map function") {
    fuse(
      """
type Option[A]:
    None
    Some(A)

impl Option[A]:
  fun map[B](self, f: A -> B) -> Option[B]
    match self:
      Some(v) => Some(f(v))
      _ => None

fun main() -> i32
    let o = Some(5)
    let o1 = o.map(a => a + 1)
    _print("0")
    0
        """,
      ExecutableOutput("0")
    )
  }
  test("execute generic trait functor implementation") {
    fuse(
      """
trait Functor[A]:
  fun map[B](self, f: A -> B) -> Self[B];

type Option[T]:
  Some(T)
  None

impl Functor for Option[A]:
  fun map[B](self, f: A -> B) -> Option[B]
    match self:
      Some(v) => Some(f(v))
      _ => None

fun fmap[A, B, F: Functor](f: A -> B, c: F[A]) -> F[B]
    c.map(f)

fun main() -> i32
    let o = Some(5)
    fmap(a => a + 1, o)
    _print("ok")
    0
        """,
      ExecutableOutput("ok")
    )
  }
  test("execute minimal generic option") {
    fuse(
      """
type Option[A]:
    None
    Some(A)

impl Option[A]:
    fun map[B](self, f: A -> B) -> Option[B]
        match self:
            Some(v) => Some(f(v))
            _ => None

    fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
        match self:
            Some(a) => f(a)
            None => None

fun main() -> i32
    _print("0")
    0
        """,
      ExecutableOutput("0")
    )
  }
  test("execute generic option") {
    fuse(
      """
type Option[A]:
    None
    Some(A)

impl Option[A]:
    fun map[B](self, f: A -> B) -> Option[B]
        match self:
            Some(v) => Some(f(v))
            _ => None

    fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
        match self:
            Some(a) => f(a)
            None => None

    fun to_str(v: i32) -> Option[str]
        let s = Some(v)
        s.map(a => int_to_str(a))

fun main() -> i32
    let o = Some(5)
    let o1 = o.flat_map(t => Some(t + 1))
    let l = Option::to_str(5)
    let result = {
        match l:
            Some(v) => 0
            None => 1
    }
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("0")
    )
  }
  test("execute function on record with tuple type") {
    fuse(
      """
type Tuple[A, B](A, B)

type State[S, A]:
  run: S -> Tuple[A, S]

fun value(a: State[i32, i32]) -> i32
  let t = (a.run)(1)
  t.1 + t.2

fun main() -> i32
  let result = value(State(a => Tuple(a + 1, a + 2)))
  _print(int_to_str(result))
  0
        """,
      ExecutableOutput("5")
    )
  }
  // This test verifies that specialized apply functions (apply_P1c18, apply_P2c18, apply_P1c28)
  // properly handle higher-order functions with different return types.
  // The solution: generate specialized apply functions for each P-tag instead of a generic apply.
  test("execute generic list with fold right") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun foldRight[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::foldRight(xs, z, f))
            Nil => z

    fun map[B](self, f: A -> B) -> List[B]
        List::foldRight(self, Nil[B], (h, t) => Cons(f(h), t))

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    let result = {
        match l1:
            Cons(h, t) => h
            Nil => 0
    }
    _print(int_to_str(result))
    0
        """,
      ExecutableOutput("3")
    )
  }
  test("execute generic list with varied parameter names") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun map_with_mapper[B](self, mapper: A -> B) -> List[B]
        match self:
            Cons(h, t) => Cons(mapper(h), Nil[B])
            Nil => Nil[B]

    fun map_with_transform[B](self, transform: A -> B) -> List[B]
        match self:
            Cons(h, t) => Cons(transform(h), Nil[B])
            Nil => Nil[B]

    fun map_with_callback[B](self, callback: A -> B) -> List[B]
        match self:
            Cons(h, t) => Cons(callback(h), Nil[B])
            Nil => Nil[B]

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map_with_mapper(x => x + 1)
    let l2 = l.map_with_transform(y => y * 2)
    let l3 = l.map_with_callback(z => z + 3)
    _print("0")
    0
        """,
      ExecutableOutput("0")
    )
  }
  test("execute generic list with fold_right using chained map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    l1.map(r => _print(int_to_str(r)))
    0
      """,
      ExecutableOutput("43")
    )
  }

  test("execute generic list with fold_right using map and sum") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    let s = List::sum(l1)
    _print(int_to_str(s))
    0
        """,
      ExecutableOutput("7")
    )
  }

  test("execute generic list with append map sum product") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun fold_left[A, B](l: List[A], acc: B, f: (B, A) -> B) -> B
        match l:
            Cons(h, t) => List::fold_left(t, f(acc, h), f)
            Nil => acc

    fun append[A](l1: List[A], l2: List[A]) -> List[A]
        List::fold_right(l1, l2, (h, t) => Cons(h, t))

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

    fun product(l: List[i32]) -> i32
        List::fold_left(l, 1, (acc, b) => acc * b)

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    let l2 = Cons(7, Nil)
    let l3 = List::append(l1, l2)
    let s = List::sum(l3)
    let p = List::product(l3)
    _print(int_to_str(s + p))
    0
        """,
      ExecutableOutput("98")
    )
  }

  test("execute list.fuse with all list operations") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun fold_left[A, B](l: List[A], acc: B, f: (B, A) -> B) -> B
        match l:
            Cons(h, t) => List::fold_left(t, f(acc, h), f)
            Nil => acc

    fun append[A](l1: List[A], l2: List[A]) -> List[A]
        List::fold_right(l1, l2, (h, t) => Cons(h, t))

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

    fun flat_map[B](self, f: A -> List[B]) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => List::append(f(h), t))

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

    fun product(l: List[i32]) -> i32
        List::fold_left(l, 1, (acc, b) => acc * b)

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map(v => v + 1)
    let l2 = Cons(7, Nil)
    let l3 = List::append(l1, l2)
    let s = List::sum(l3)
    let p = List::product(l3)
    _print(int_to_str(s + p))
    0
        """,
      ExecutableOutput("98")
    )
  }

  test("execute generic list with fold_right using flat_map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun append[A](l1: List[A], l2: List[A]) -> List[A]
        List::fold_right(l1, l2, (h, t) => Cons(h, t))

    fun flat_map[B](self, f: A -> List[B]) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => List::append(f(h), t))

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

fun main() -> i32
    let l = Cons(1, Cons(2, Cons(3, Nil)))
    let l1 = l.flat_map(v => Cons(v, Cons(v * 10, Nil)))
    let s = List::sum(l1)
    _print(int_to_str(s))
    0
        """,
      ExecutableOutput("66")
    )
  }
  test("exec list filter with comparison") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

    fun filter[A](self, f: A -> bool) -> List[A]
        List::fold_right(self, Nil[A], (h, t) => {
            match f(h):
                true => Cons(h, t)
                false => t
        })

fun main() -> i32
    let l = Cons(1, Cons(2, Cons(3, Cons(4, Nil))))
    let l2 = l.filter(e => e > 2)
    let s = List::sum(l2)
    _print(int_to_str(s))
    0
        """,
      ExecutableOutput("7")
    )
  }
  test("execute nested match in case arm") {
    fuse(
      """
type Animal:
    Dog(age: i32)
    Cat

fun describe(a: Animal) -> i32
    match a:
        Dog(age) => {
            match age > 5:
                true => 1
                false => 0
        }
        Cat => 2

fun main() -> i32
    _print(int_to_str(describe(Dog(10))))
    0
      """,
      ExecutableOutput("1")
    )
  }
  test("execute untyped lambda calculus") {
    fuse(
      """
type Term:
    Var(index: i32, ctxlen: i32)
    Abs(hint: str, body: Term)
    App(t1: Term, t2: Term)

type MaybeTerm:
    Nothing
    Just(Term)

fun term_shift_above(d: i32, c: i32, t: Term) -> Term
    match t:
        Abs(hint, body) => Abs(hint, term_shift_above(d, c + 1, body))
        App(t1, t2) => App(term_shift_above(d, c, t1), term_shift_above(d, c, t2))
        Var(x, n) => {
            match x >= c:
                true => Var(x + d, n + d)
                false => Var(x, n + d)
        }

fun term_shift(d: i32, t: Term) -> Term
    term_shift_above(d, 0, t)

fun term_subst(j: i32, s: Term, t: Term) -> Term
    match t:
        Abs(hint, body) => Abs(hint, term_subst(j + 1, s, body))
        App(t1, t2) => App(term_subst(j, s, t1), term_subst(j, s, t2))
        Var(x, n) => {
            match x == j:
                true => term_shift(j, s)
                false => Var(x, n)
        }

fun term_subst_top(s: Term, t: Term) -> Term
    term_shift(0 - 1, term_subst(0, term_shift(1, s), t))

fun is_val(t: Term) -> bool
    match t:
        Abs(h, b) => true
        _ => false

fun eval1(t: Term) -> MaybeTerm
    match t:
        App(t1, t2) => {
            match t1:
                Abs(hint, body) => {
                    match is_val(t2):
                        true => Just(term_subst_top(t2, body))
                        false => {
                            match eval1(t2):
                                Just(t2p) => Just(App(t1, t2p))
                                Nothing => Nothing
                        }
                }
                _ => {
                    match eval1(t1):
                        Just(t1p) => Just(App(t1p, t2))
                        Nothing => Nothing
                }
        }
        _ => Nothing

fun eval(t: Term) -> Term
    match eval1(t):
        Just(tp) => eval(tp)
        Nothing => t

fun term_to_str(t: Term) -> str
    match t:
        Var(x, n) => int_to_str(x)
        Abs(hint, body) => "(\\" + hint + ". " + term_to_str(body) + ")"
        App(t1, t2) => "(" + term_to_str(t1) + " " + term_to_str(t2) + ")"

fun println(s: str) -> Unit
    _print(s + "\n")
    ()

fun main() -> i32
    let id = Abs("x", Var(0, 1))
    let app_id = App(Abs("x", Var(0, 1)), Abs("y", Var(0, 1)))
    println("id = " + term_to_str(id))
    println("(id id) = " + term_to_str(eval(app_id)))
    0
      """,
      ExecutableOutput("id = (\\x. 0)\n(id id) = (\\y. 0)")
    )
  }
  test("execute untyped lambda with eval") {
    fuse(
      """
type Option[T]:
    None
    Some(T)

type Term:
    Var(index: i32, ctxlen: i32)
    Abs(hint: str, body: Term)
    App(t1: Term, t2: Term)

fun term_shift_above(d: i32, c: i32, t: Term) -> Term
    match t:
        Abs(hint, body) => Abs(hint, term_shift_above(d, c + 1, body))
        App(t1, t2) => App(term_shift_above(d, c, t1), term_shift_above(d, c, t2))
        Var(x, n) => {
            match x >= c:
                true => Var(x + d, n + d)
                false => Var(x, n + d)
        }

fun term_shift(d: i32, t: Term) -> Term
    term_shift_above(d, 0, t)

fun term_subst(j: i32, s: Term, t: Term) -> Term
    match t:
        Abs(hint, body) => Abs(hint, term_subst(j + 1, s, body))
        App(t1, t2) => App(term_subst(j, s, t1), term_subst(j, s, t2))
        Var(x, n) => {
            match x == j:
                true => term_shift(j, s)
                false => Var(x, n)
        }

fun term_subst_top(s: Term, t: Term) -> Term
    term_shift(0 - 1, term_subst(0, term_shift(1, s), t))

fun is_val(t: Term) -> bool
    match t:
        Abs(h, b) => true
        _ => false

fun eval1(t: Term) -> Option[Term]
    match t:
        App(t1, t2) => {
            match t1:
                Abs(hint, body) => {
                    match is_val(t2):
                        true => Some(term_subst_top(t2, body))
                        false => {
                            match eval1(t2):
                                Some(t2p) => Some(App(t1, t2p))
                                None => None[Term]
                        }
                }
                _ => {
                    match eval1(t1):
                        Some(t1p) => Some(App(t1p, t2))
                        None => None[Term]
                }
        }
        _ => None[Term]

fun eval(t: Term) -> Term
    match eval1(t):
        Some(tp) => eval(tp)
        None => t

fun term_to_str(t: Term) -> str
    match t:
        Var(x, n) => int_to_str(x)
        Abs(hint, body) => "(\\" + hint + ". " + term_to_str(body) + ")"
        App(t1, t2) => "(" + term_to_str(t1) + " " + term_to_str(t2) + ")"

fun main() -> i32
    let app_id = App(Abs("x", Var(0, 1)), Abs("y", Var(0, 1)))
    _print(term_to_str(eval(app_id)))
    0
      """,
      ExecutableOutput("(\\y. 0)")
    )
  }
  test("execute untyped lambda subst only") {
    fuse(
      """
type Term:
    Var(index: i32, ctxlen: i32)
    Abs(hint: str, body: Term)
    App(t1: Term, t2: Term)

fun term_shift_above(d: i32, c: i32, t: Term) -> Term
    match t:
        Abs(hint, body) => Abs(hint, term_shift_above(d, c + 1, body))
        App(t1, t2) => App(term_shift_above(d, c, t1), term_shift_above(d, c, t2))
        Var(x, n) => {
            match x >= c:
                true => Var(x + d, n + d)
                false => Var(x, n + d)
        }

fun term_shift(d: i32, t: Term) -> Term
    term_shift_above(d, 0, t)

fun term_subst(j: i32, s: Term, t: Term) -> Term
    match t:
        Abs(hint, body) => Abs(hint, term_subst(j + 1, s, body))
        App(t1, t2) => App(term_subst(j, s, t1), term_subst(j, s, t2))
        Var(x, n) => {
            match x == j:
                true => term_shift(j, s)
                false => Var(x, n)
        }

fun term_subst_top(s: Term, t: Term) -> Term
    term_shift(0 - 1, term_subst(0, term_shift(1, s), t))

fun term_to_str(t: Term) -> str
    match t:
        Var(x, n) => int_to_str(x)
        Abs(hint, body) => "(\\" + hint + ". " + term_to_str(body) + ")"
        App(t1, t2) => "(" + term_to_str(t1) + " " + term_to_str(t2) + ")"

fun main() -> i32
    let body = Var(0, 1)
    let arg = Abs("y", Var(0, 1))
    let result = term_subst_top(arg, body)
    _print(term_to_str(result))
    0
      """,
      ExecutableOutput("(\\y. 0)")
    )
  }
  test("execute comparison operator in standalone function") {
    fuse(
      """
fun gte(a: i32, b: i32) -> i32
    match a >= b:
        true => 1
        false => 0

fun main() -> i32
    _print(int_to_str(gte(5, 3)))
    0
      """,
      ExecutableOutput("1")
    )
  }

  test("execute multi-param lambda unused") {
    fuse(
      """
fun main() -> i32
    let f = a => a + 1
    let g = (x, y) => x + y
    _print(int_to_str(f(5)))
    0
        """,
      ExecutableOutput("6")
    )
  }

  test("execute method call chaining") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun fold_right[A, B](as: List[A], z: B, f: (A, B) -> B) -> B
        match as:
            Cons(x, xs) => f(x, List::fold_right(xs, z, f))
            Nil => z

    fun map[B](self, f: A -> B) -> List[B]
        List::fold_right(self, Nil[B], (h, t) => Cons(f(h), t))

    fun filter[A](self, f: A -> bool) -> List[A]
        List::fold_right(self, Nil[A], (h, t) => {
            match f(h):
                true => Cons(h, t)
                false => t
        })

    fun sum(l: List[i32]) -> i32
        List::fold_right(l, 0, (acc, b) => acc + b)

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let result = l.map(v => v + 1).filter(e => e > 3)
    let s = List::sum(result)
    _print(int_to_str(s))
    0
        """,
      ExecutableOutput("4")
    )
  }

  test("execute do expr with option monad") {
    fuse(
      """
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];
  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];
  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

type Option[T]:
  Some(T)
  None

impl Monad for Option[A]:
  fun unit[A](a: A) -> Option[A]
    Some(a)
  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None

fun main() -> i32
  let x = Some(1)
  let y = Some(2)
  let z = Some(3)
  let result = {
    do:
      i <- x
      j <- y
      k <- z
      i + j + k
  }
  match result:
    Some(v) => {
      _print(int_to_str(v))
      0
    }
    _ => 1
        """,
      ExecutableOutput("6")
    )
  }
  test("execute io print") {
    fuse(
      """
fun main() -> i32
  let action = print("Hello IO!")
  action.exec()
  0
      """,
      ExecutableOutput("Hello IO!", includeStdlib = true)
    )
  }
  test("execute io unit and flat_map") {
    fuse(
      """
type IO[A]:
  MkIO(() -> A)

impl IO[A]:
  fun exec(self) -> A
    match self:
      MkIO(f) => f(())

trait Monad[A]:
  fun unit[A](a: A) -> Self[A];
  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];
  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

impl Monad for IO[A]:
  fun flat_map[B](self, f: A -> IO[B]) -> IO[B]
    MkIO(_ => {
      let a = self.exec()
      let b = f(a)
      b.exec()
    })
  fun unit[A](a: A) -> IO[A]
    MkIO(_ => a)

fun main() -> i32
  let action = MkIO(_ => _print("Hello"))
  let action2 = action.flat_map(_ => MkIO(_ => _print(" World!")))
  action2.exec()
  0
      """,
      ExecutableOutput("Hello World!")
    )
  }

  test("execute io do notation") {
    fuse(
      """
type IO[A]:
  MkIO(() -> A)

impl IO[A]:
  fun exec(self) -> A
    match self:
      MkIO(f) => f(())

trait Monad[A]:
  fun unit[A](a: A) -> Self[A];
  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];
  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

impl Monad for IO[A]:
  fun flat_map[B](self, f: A -> IO[B]) -> IO[B]
    MkIO(_ => {
      let a = self.exec()
      let b = f(a)
      b.exec()
    })
  fun unit[A](a: A) -> IO[A]
    MkIO(_ => a)

fun main() -> i32
  let result = {
    do:
      _ <- MkIO(_ => _print("Hello"))
      _ <- MkIO(_ => _print(" "))
      _ <- MkIO(_ => _print("World!"))
      ()
  }
  result.exec()
  0
      """,
      ExecutableOutput("Hello World!")
    )
  }

  test("execute io do notation with file read and write") {
    fuse(
      """
type IO[A]:
  MkIO(() -> A)

impl IO[A]:
  fun exec(self) -> A
    match self:
      MkIO(f) => f(())

trait Monad[A]:
  fun unit[A](a: A) -> Self[A];
  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];
  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

impl Monad for IO[A]:
  fun flat_map[B](self, f: A -> IO[B]) -> IO[B]
    MkIO(_ => {
      let a = self.exec()
      let b = f(a)
      b.exec()
    })
  fun unit[A](a: A) -> IO[A]
    MkIO(_ => a)

fun main() -> i32
  let program = {
    do:
      _ <- MkIO(_ => _file_write("/tmp/fuse_io_test.txt", "Hello from IO!"))
      _ <- MkIO(_ => {
        let content = _file_read("/tmp/fuse_io_test.txt")
        _print(content)
      })
      ()
  }
  program.exec()
  0
      """,
      ExecutableOutput("Hello from IO!")
    )
  }

  test("execute io do notation read write") {
    fuse(
      """
fun rw(file_name: str, content: str) -> IO[i32]
  do:
    _ <- write(file_name, content)
    v <- read(file_name)
    _ <- print(v)
    0

fun main() -> i32
  rw("/tmp/fuse_rw_test.txt", "Hello World!").exec()
      """,
      ExecutableOutput("Hello World!", includeStdlib = true)
    )
  }

  test("execute io flat_map with non-unit binding") {
    fuse(
      """
type IO[A]:
  MkIO(() -> A)

impl IO[A]:
  fun exec(self) -> A
    match self:
      MkIO(f) => f(())

  fun flat_map[B](self, f: A -> IO[B]) -> IO[B]
    MkIO(_ => {
      let a = self.exec()
      let b = f(a)
      b.exec()
    })

fun main() -> i32
  let action = MkIO(_ => 42)
  let result = action.flat_map(x => MkIO(_ => x + 1))
  _print(int_to_str(result.exec()))
  0
      """,
      ExecutableOutput("43")
    )
  }
}

object CompilerTests {
  import Fuse.*

  sealed trait Output
  case class CheckOutput(s: Option[String]) extends Output
  case class BuildOutput(s: String) extends Output
  case class ExecutableOutput(
      expectedStdout: String,
      expectedExitCode: Int = 0,
      includeStdlib: Boolean = false
  ) extends Output

  case class ExecutionResult(stdout: String, stderr: String, exitCode: Int)

  private val testTempDir: Path = {
    val dir = Paths.get("target/test-temp")
    if (!Files.exists(dir)) Files.createDirectories(dir)
    dir
  }

  private def createTempFuseFile(code: String): Resource[IO, Path] = {
    val acquire = IO {
      val tempFile =
        Files.createTempFile(testTempDir, "test-", s".$FuseFileExtension")
      Files.write(tempFile, code.trim.getBytes)
      tempFile
    }
    val release = (path: Path) =>
      IO {
        val baseName = path.toString.stripSuffix(s".$FuseFileExtension")
        Files.deleteIfExists(path)
        Files.deleteIfExists(Paths.get(baseName + s".$FuseGrinExtension"))
        Files.deleteIfExists(Paths.get(baseName + s".$FuseOutputExtension"))
      }.void
    Resource.make(acquire)(release)
  }

  private def executeProcess(exePath: Path): IO[ExecutionResult] = {
    IO.blocking {
      val stdout = new StringBuilder
      val stderr = new StringBuilder

      val logger = ProcessLogger(
        out => stdout.append(out).append("\n"),
        err => stderr.append(err).append("\n")
      )

      val exitCode = Process(exePath.toString).!(logger)

      ExecutionResult(
        stdout.toString.trim,
        stderr.toString.trim,
        exitCode
      )
    }
  }

  def execute(
      code: String,
      includeStdlib: Boolean = false
  ): IO[Either[String, ExecutionResult]] = {
    createTempFuseFile(code).use { fusePath =>
      for {
        buildExitCode <- Fuse.build(
          BuildFile(fusePath.toString, includeStdlib)
        )
        result <- buildExitCode match {
          case ExitCode.Success =>
            val outPath =
              Paths.get(
                fusePath.toString.stripSuffix(
                  s".$FuseFileExtension"
                ) + s".$FuseOutputExtension"
              )
            executeProcess(outPath).map(Right(_))
          case _ =>
            IO.pure(Left("compilation failed: fuse or grin error"))
        }
      } yield result
    }
  }

  /** Synchronously load the pre-parsed library module for the test helpers if
    * the command requests it. Mirrors `Compiler.run`'s load-then-compile flow
    * so tests can stay synchronous while `Compiler.compile` itself remains pure
    * (effects at the boundary).
    */
  def loadStdlibSync(command: Command) = {
    import cats.effect.unsafe.implicits.global
    command.includeStdlib match {
      case true  => Compiler.loadStdlib().unsafeRunSync().map(Some(_))
      case false => Right(None)
    }
  }

  def check(code: String, fileName: String = s"test.$FuseFileExtension") = {
    val command = CheckFile(fileName)
    loadStdlibSync(command).flatMap(stdlib =>
      Compiler.compile(command, code.trim, fileName, stdlib)
    )
  }

  def build(
      code: String,
      fileName: String = s"test.$FuseFileExtension",
      includeStdlib: Boolean = false
  ) = {
    val command = BuildFile(fileName, includeStdlib)
    loadStdlibSync(command).flatMap(stdlib =>
      Compiler.compile(command, code.trim, fileName, stdlib)
    )
  }
}
