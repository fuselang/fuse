package fuse

import munit.*
import scala.concurrent.duration.Duration
import cats.effect.{IO, Resource}
import java.nio.file.{Files, Path, Paths}
import scala.sys.process.*
import cats.effect.ExitCode

class CompilerTests extends FunSuite {
  import CompilerTests.*
  override val munitTimeout = Duration(10, "m")

  /** Asserts fuse code is type checked. */
  def fuse(code: String, expected: Output = CheckOutput(None)) =
    expected match {
      case CheckOutput(s) => assertCheck(code, s)
      case BuildOutput(s) => assertBuild(code, s)
      case ExecutableOutput(stdout, exitCode) =>
        assertExecutable(code, stdout, exitCode)
    }

  def assertCheck(code: String, expectedError: Option[String]) =
    (check(code), expectedError) match {
      case (t, None) => assert(t.isRight, s"\n${t.merge}")
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
      expectedExitCode: Int = 0
  ) = {
    import cats.effect.unsafe.implicits.global

    execute(code).unsafeRunSync() match {
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
  print("Hello World")

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
    print(s)
    print("\n")
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
  print("Breaking news! " + s.summarize())

fun main() -> i32
    let tweet = Tweet("elon", "work!")
    print(tweet.summarize())
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
  print("Breaking news! " + s.summarize())

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
  print("Breaking news! " + s.summarize())

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
  print("Breaking news! " + s.summarize())

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
  print("Breaking news! " + s.summarize())

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

fun main() -> i32
    let o = Some(5)
    o.map(a => a + 1)
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
          "expected type of `Right[{unknown}][{unknown}]`, found `Option[i32]`"
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
trait Monad[A]:
  fun unit[A](a: A) -> Self[A];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    self.flat_map(a => Self::unit(f(a)))

type IO[A]:
  run: () -> A

impl IO[A]:
  fun exec(self) -> A
    (self.run)()

impl Monad for IO[A]:
  fun flat_map[B](self, f: A -> IO[B]) -> IO[B]
    let r = _ => {
      let b = f(self.exec())
      b.exec()
    }
    IO(r)

  fun unit[A](a: A) -> IO[A]
    let r = _ => a
    IO[A](r)

fun printline(s: str) -> IO[Unit]
  IO(_ => print(s))

fun greetings() -> IO[Unit]
  do:
    h <- printline("Hello")
    s <- printline(" ")
    v <- printline("World!")
    ()

fun main() -> Unit
  greetings().exec()
        """)

  }
}

class CompilerBuildTests extends CompilerTests {
  import CompilerTests.*

  test("build unit function") {
    fuse(
      """
fun greetings() -> Unit
  print("Hello World")

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
  _prim_string_ne :: T_String -> T_String -> T_Int64

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
    print(s)
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
    print(s)
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
    print(s)
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
 identity'str a4

id'f32 a5 =
 identity'str a5

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
    print(s)
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
    print(s)
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
  print("Breaking news! " + s.summarize())

fun main() -> i32
    let tweet = Tweet("elon", "work!")
    print(tweet.summarize())
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
    print(tweet.summarize())
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
None'i32 =  store (CNone)

Some'i32 t10 =
 store (CSome t10)

grinMain _2 =
 o3 <-  Some'i32 5
 p6 <- fetch o3
 case p6 of
  (CSome v6) ->
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
      BuildOutput("""None'i32 =  store (CNone)

Some'i32 t10 =
 store (CSome t10)

mapOptioni32i32' self2 f''3 =
 p6 <- fetch self2
 case p6 of
  (CSome v6) ->
   p8 <- apply_P1c13 f''3 v6
   p9 <- Some'i32 p8
   pure p9
  #default ->
   p10 <- None'i32
   pure p10

grinMain _10 =
 o11 <-  Some'i32 5
 p15 <- pure (P1c13 )
 _15 <-  mapOptioni32i32' o11 p15
 pure 0

c13 a13 =
 _prim_int_add a13 1

apply_P1c13 p17 p18 =
 case p17 of
  (P1c13 ) ->
   c13  p18
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

fun main() -> i32
    let o = Some(5)
    o.map(a => a + 1)
    0
        """,
      BuildOutput("""
Some'i32 t10 =
 store (CSome t10)

None'i32 =  store (CNone)

mapOptionFunctori32i32' self2 f''3 =
 p6 <- fetch self2
 case p6 of
  (CSome v6) ->
   p8 <- apply_P1c13 f''3 v6
   p9 <- Some'i32 p8
   pure p9
  #default ->
   p10 <- None'i32
   pure p10

grinMain _10 =
 o11 <-  Some'i32 5
 p15 <- pure (P1c13 )
 _15 <-  mapOptionFunctori32i32' o11 p15
 pure 0

c13 a13 =
 _prim_int_add a13 1

apply_P1c13 p17 p18 =
 case p17 of
  (P1c13 ) ->
   c13  p18
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
      BuildOutput("""None'i32 =  store (CNone)

Some'i32 t10 =
 store (CSome t10)

mapOptioni32i32' self2 f''3 =
 p6 <- fetch self2
 case p6 of
  (CSome v6) ->
   p8 <- apply_P1c13 f''3 v6
   p9 <- Some'i32 p8
   pure p9
  #default ->
   p10 <- None'i32
   pure p10

grinMain _10 =
 o11 <-  Some'i32 5
 p15 <- pure (P1c13 )
 o115 <-  mapOptioni32i32' o11 p15
 p18 <- fetch o115
 case p18 of
  (CSome v18) ->
   pure v18
  #default ->
   pure 0

c13 a13 =
 _prim_int_add a13 1

apply_P1c13 p20 p21 =
 case p20 of
  (P1c13 ) ->
   c13  p21
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
      BuildOutput("""None'i32 =  store (CNone)

Some'i32 t10 =
 store (CSome t10)

mapOptionstri32' self2 f''3 =
 p6 <- fetch self2
 case p6 of
  (CSome v6) ->
   p8 <- do
     case f''3 of
      (P1c20) ->
       c20 v6
      (P1c25) ->
       c25 v6
   p9 <- Some'i32 p8
   pure p9
  #default ->
   p10 <- None'i32
   pure p10

flatmapOptioni32i32' self10 f''11 =
 p14 <- fetch self10
 case p14 of
  (CSome a14) ->
   p16 <- do
     case f''11 of
      (P1c20) ->
       c20 a14
      (P1c25) ->
       c25 a14
   pure p16
  #default ->
   p17 <- None'i32
   pure p17

tostrOption' v17 =
 s18 <-  Some'i32 v17
 p22 <- pure (P1c20 )
 mapOptionstri32' s18 p22

c20 a20 =
 _prim_int_str a20

grinMain _22 =
 o23 <-  Some'i32 5
 p28 <- pure (P1c25 )
 o128 <-  flatmapOptioni32i32' o23 p28
 l30 <-  tostrOption' 5
 p33 <- fetch l30
 case p33 of
  (CSome v33) ->
   pure 0
  #default ->
   pure 1

c25 t25 =
 p27 <- _prim_int_add t25 1
 Some'i32 p27

apply_P1c20 p35 p36 =
 case p35 of
  (P1c20 ) ->
   c20  p36

apply_P1c25 p37 p38 =
 case p37 of
  (P1c25 ) ->
   c25  p38
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
      BuildOutput("""
Tuple'i32'i32 t10 t21 =
 store (CTuple t10 t21)

State'i32'i32 run''4 =
 store (CState run''4)

value a6 =
 p9 <- fetch a6
 p11'' <- do
   case p9 of
    (CState p10) ->
     pure p10
 t13 <-  apply_P1c26 p11'' 1
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
 p30 <- pure (P1c26 )
 p31 <- State'i32'i32 p30
 value p31

c26 a26 =
 p28 <- _prim_int_add a26 1
 p29 <- _prim_int_add a26 2
 Tuple'i32'i32 p28 p29

apply_P1c26 p32 p33 =
 case p32 of
  (P1c26 ) ->
   c26  p33""")
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
            print(int_to_str(h))
            0
        }
        Nil => 1
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CCons h0 t1)

Nil'i32 =  store (CNil)

foldRightListi32i32' as4 z5 f''6 =
 p9 <- fetch as4
 case p9 of
  (CCons x9 xs'10) ->
   p12'' <- apply_P2c18 f''6 x9
   p13 <- foldRightListi32i32' xs'10 z5 f''6
   p14 <- apply_P1c18 p12'' p13
   pure p14
  #default ->
   pure z5

mapListi32i32' self14 f''15 =
 p17 <- Nil'i32
 p23 <- pure (P2c18 f''15)
 foldRightListi32i32' self14 p17 p23

c18 f''1519 h19 t20 =
 p22 <- apply_P1c28 f''1519 h19
 Cons'i32 p22 t20

grinMain _23 =
 p25 <- Nil'i32
 p26 <- Cons'i32 3 p25
 l26 <-  Cons'i32 2 p26
 p30 <- pure (P1c28 )
 l130 <-  mapListi32i32' l26 p30
 p33 <- fetch l130
 case p33 of
  (CCons h33 t'34) ->
   p36 <- _prim_int_str h33
   _37 <-  _prim_string_print p36
   pure 0
  #default ->
   pure 1

c28 v28 =
 _prim_int_add v28 1

apply_P1c18 p39 p40 =
 case p39 of
  (P1c18 p41 p42) ->
   c18 p41 p42 p40

apply_P2c18 p43 p44 =
 case p43 of
  (P2c18 p45) ->
   pure (P1c18 p45 p44)

apply_P1c28 p46 p47 =
 case p46 of
  (P1c28 ) ->
   c28  p47
""")
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
 store (CCons h0 t1)

Nil'i32 =  store (CNil)

simplemapListi32i32' self4 f''5 =
 p8 <- fetch self4
 case p8 of
  (CCons h8 t'9) ->
   p11 <- apply_P1c19 f''5 h8
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
 p21 <- pure (P1c19 )
 l121 <-  simplemapListi32i32' l17 p21
 pure 0

c19 v19 =
 _prim_int_add v19 1

apply_P1c19 p23 p24 =
 case p23 of
  (P1c19 ) ->
   c19  p24
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
 store (CCons h0 t1)

Nil'i32 =  store (CNil)

mapwithmapperListi32i32' self4 mapper''5 =
 p8 <- fetch self4
 case p8 of
  (CCons h8 t'9) ->
   p11 <- do
     case mapper''5 of
      (P1c39) ->
       c39 h8
      (P1c43) ->
       c43 h8
      (P1c47) ->
       c47 h8
   p12 <- Nil'i32
   p13 <- Cons'i32 p11 p12
   pure p13
  #default ->
   p14 <- Nil'i32
   pure p14

mapwithtransformListi32i32' self14 transform''15 =
 p18 <- fetch self14
 case p18 of
  (CCons h18 t'19) ->
   p21 <- do
     case transform''15 of
      (P1c39) ->
       c39 h18
      (P1c43) ->
       c43 h18
      (P1c47) ->
       c47 h18
   p22 <- Nil'i32
   p23 <- Cons'i32 p21 p22
   pure p23
  #default ->
   p24 <- Nil'i32
   pure p24

mapwithcallbackListi32i32' self24 callback''25 =
 p28 <- fetch self24
 case p28 of
  (CCons h28 t'29) ->
   p31 <- do
     case callback''25 of
      (P1c39) ->
       c39 h28
      (P1c43) ->
       c43 h28
      (P1c47) ->
       c47 h28
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
 p41 <- pure (P1c39 )
 l141 <-  mapwithmapperListi32i32' l37 p41
 p45 <- pure (P1c43 )
 l245 <-  mapwithtransformListi32i32' l37 p45
 p49 <- pure (P1c47 )
 l349 <-  mapwithcallbackListi32i32' l37 p49
 pure 0

c39 x39 =
 _prim_int_add x39 1

c43 y43 =
 _prim_int_mul y43 2

c47 z47 =
 _prim_int_add z47 3

apply_P1c39 p51 p52 =
 case p51 of
  (P1c39 ) ->
   c39  p52

apply_P1c43 p53 p54 =
 case p53 of
  (P1c43 ) ->
   c43  p54

apply_P1c47 p55 p56 =
 case p55 of
  (P1c47 ) ->
   c47  p56
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
            print(int_to_str(h))
            0
        }
        Nil => 1
        """,
      BuildOutput("""Cons'i32 h0 t1 =
 store (CCons h0 t1)

Nil'i32 =  store (CNil)

map2Listi32i32' self4 f''5 =
 p19 <- Nil'i32
 iter7 f''5 self4 p19

iter7 f''58 l8 acc9 =
 p12 <- fetch l8
 case p12 of
  (CCons h12 t'13) ->
   p15 <- apply_P1c24 f''58 h12
   p16 <- Cons'i32 p15 acc9
   p17 <- iter7 f''58 t'13 p16
   pure p17
  #default ->
   pure acc9

grinMain _19 =
 p21 <- Nil'i32
 p22 <- Cons'i32 3 p21
 l22 <-  Cons'i32 2 p22
 p26 <- pure (P1c24 )
 l126 <-  map2Listi32i32' l22 p26
 p29 <- fetch l126
 case p29 of
  (CCons h29 t'30) ->
   p32 <- _prim_int_str h29
   _33 <-  _prim_string_print p32
   pure 0
  #default ->
   pure 1

c24 v24 =
 _prim_int_add v24 1

apply_P1c24 p35 p36 =
 case p35 of
  (P1c24 ) ->
   c24  p36
""")
    )
  }
}

class CompilerExecTests extends CompilerTests {
  import CompilerTests.*

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
            print(int_to_str(h))
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
            print(int_to_str(v))
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
            print(int_to_str(h))
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
    print(int_to_str(result))
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
    print(result)
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
    print(int_to_str(result))
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
    print(int_to_str(result))
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
    print(int_to_str(result))
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
    print(int_to_str(output))
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
    print(int_to_str(output))
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
    print(int_to_str(output))
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
    print(int_to_str(output))
    0
        """,
      ExecutableOutput("1")
    )
  }
  test("execute unit function") {
    fuse(
      """
fun greetings() -> Unit
  print("Hello World")

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
    print(int_to_str(result))
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
    print(int_to_str(result))
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
    print(int_to_str(result))
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
    print(int_to_str(result))
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
    print(s)
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
    print(s + "\n" + int_to_str(i))
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
    print(s + "\n" + int_to_str(i))
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
    print(s)
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
    print(s + "\n" + int_to_str(a))
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
  print(int_to_str(result))
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
  print("Breaking news! " + s.summarize())

fun main() -> i32
    let tweet = Tweet("elon", "work!")
    print(tweet.summarize() + "\n")
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
    print(tweet.summarize())
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
  print(int_to_str(x.v))
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
  print(int_to_str(p.x))
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
  print(int_to_str(t.1))
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
    print(int_to_str(result))
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
    print(int_to_str(result))
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
    print(int_to_str(result))
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
    print("0")
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

fun main() -> i32
    let o = Some(5)
    let o1 = o.map(a => a + 1)
    print("0")
    0
        """,
      ExecutableOutput("0")
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
    print("0")
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
    print(int_to_str(result))
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
  print(int_to_str(result))
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
    print(int_to_str(result))
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
    print("0")
    0
        """,
      ExecutableOutput("0")
    )
  }
}

object CompilerTests {
  import Fuse.*

  sealed trait Output
  case class CheckOutput(s: Option[String]) extends Output
  case class BuildOutput(s: String) extends Output
  case class ExecutableOutput(expectedStdout: String, expectedExitCode: Int = 0)
      extends Output

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

  def execute(code: String): IO[Either[String, ExecutionResult]] = {
    createTempFuseFile(code).use { fusePath =>
      for {
        buildExitCode <- Fuse.build(BuildFile(fusePath.toString))
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

  def check(code: String, fileName: String = s"test.$FuseFileExtension") =
    Compiler.compile(CheckFile(fileName), code.trim, fileName)

  def build(code: String, fileName: String = s"test.$FuseFileExtension") =
    Compiler.compile(BuildFile(fileName), code.trim, fileName)
}
