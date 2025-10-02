package fuse

import munit.*
import scala.concurrent.duration.Duration

class CompilerTests extends FunSuite {
  import CompilerTests.*
  override val munitTimeout = Duration(10, "m")

  /** Asserts fuse code is type checked. */
  def fuse(code: String, expected: Output = CheckOutput(None)) =
    expected match {
      case CheckOutput(s) => assertCheck(code, s)
      case BuildOutput(s) => assertBuild(code, s)
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
    fuse("""
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
        let iter = (acc, l) => {
            match l:
                Cons(h, t) => Cons(f(h), iter(acc, t))
                Nil => acc
        }
        iter(Nil[B], self)

fun main() -> Unit
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map_2(v => v + 1)
    ()
        """)
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
Dog =  pure  (CDog)

Cat =  pure  (CCat)

value a0 =
 case a0 of
  (CDog ) ->
   pure 0
  (CCat ) ->
   pure 1

grinMain _2 =
 p4 <- Dog
 value p4""")
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
 _prim_i32_add 2 2""")
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
 _prim_f32_add 2.0 2.0""")
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
 _prim_str_add #"Hello" #"World"""")
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
 _prim_i32_sub 2 2""")
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
 _prim_f32_mul 2.0 2.0""")
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
 _prim_f32_div 2.0 2.0""")
    )
  }
  test("build int modulo") {
    fuse(
      """
fun main() -> i32
    10 % 2
    """,
      BuildOutput("""
grinMain _0 =
 _prim_i32_mod 10 2""")
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
 p2 <- _prim_i32_mul 3 6
 _prim_i32_add 2 p2""")
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
 _prim_i32_eq 10 10""")
    )
  }
  test("build str not equal") {
    fuse(
      """
fun main() -> bool
    "Hello" != "World"
    """,
      BuildOutput("""
grinMain _0 =
 _prim_str_noteq #"Hello" #"World"""")
    )
  }
  test("build and") {
    fuse(
      """
fun main() -> bool
    1 != 2 && 3 == 4
    """,
      BuildOutput("""
grinMain _0 =
 p2 <- _prim_i32_noteq 1 2
 p3 <- _prim_i32_eq 3 4
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
grinMain _0 =
 p2 <- _prim_i32_noteq 1 2
 p3 <- _prim_i32_eq 3 4
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
 _prim_i32_add a2 1
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
 p5 <- _prim_i32_add a2 b3
 _prim_i32_add p5 2
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
 _prim_i32_add a2 1
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
 p5 <- _prim_i32_add a2 b3
 _prim_i32_add p5 2
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
identity#str v0 =
 pure v0

grinMain _1 =
 s2 <-  identity#str #"Hello World"
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
identity#str v0 =
 pure v0

identity#i32 v1 =
 pure v1

grinMain _2 =
 s3 <-  identity#str #"Hello World"
 i4 <-  identity#i32 1
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
identity#str v0 =
 pure v0

identity#i32 v1 =
 pure v1

identity#f32 v2 =
 pure v2

id#str a3 =
 identity#str a3

id#i32 a4 =
 identity#str a4

id#f32 a5 =
 identity#str a5

grinMain _6 =
 s7 <-  id#str #"Hello World"
 i8 <-  id#i32 5
 f9 <-  id#f32 99.9
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
 _prim_str_add a0 b1

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
 p3 <- _prim_str_add a0 #": "
 _prim_str_add p3 b1

additioni32Add' a3 b4 =
 _prim_i32_add a3 b4

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
 pure (CTweet username0 content1)

summarizeTweetSummary' self4 =
 p9 <- do
   case self4 of
    (CTweet p7 p8) ->
     pure p7
 p10 <- _prim_str_add p9 #": "
 p14 <- do
   case self4 of
    (CTweet p12 p13) ->
     pure p13
 _prim_str_add p10 p14

notifyTweetSummary' s14 =
 p17 <- summarizeTweetSummary' s14
 p18 <- _prim_str_add #"Breaking news! " p17
 _prim_string_print p18

grinMain _18 =
 tweet20 <-  Tweet #"elon" #"work!"
 p23 <- summarizeTweetSummary' tweet20
 _25 <-  _prim_string_print p23
 _26 <-  notifyTweetSummary' tweet20
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
 pure (CTweet username0 content1)

summarizeTweetSummary' self4 =
 p9 <- do
   case self4 of
    (CTweet p7 p8) ->
     pure p7
 p10 <- _prim_str_add p9 #": "
 p14 <- do
   case self4 of
    (CTweet p12 p13) ->
     pure p13
 _prim_str_add p10 p14

notifyTweetSummary' s14 =
 pure #"no news!"

grinMain _15 =
 tweet17 <-  Tweet #"elon" #"work!"
 p20 <- summarizeTweetSummary' tweet17
 _22 <-  _prim_string_print p20
 _23 <-  notifyTweetSummary' tweet17
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
 _prim_i32_add a0 b1

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
X#i32 v0 =
 pure (CX v0)

grinMain _2 =
 x3 <-  X#i32 1
 p7 <- do
   case x3 of
    (CX p6) ->
     pure p6
 pure p7""")
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
Point#i32#str x0 y1 =
 pure (CPoint x0 y1)

grinMain _4 =
 p5 <-  Point#i32#str 1 #"2"
 p10 <- do
   case p5 of
    (CPoint p8 p9) ->
     pure p8
 pure p10
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
Tuple#i32#str t10 t21 =
 pure (CTuple t10 t21)

grinMain _4 =
 t5 <-  Tuple#i32#str 1 #"2"
 p10 <- do
   case t5 of
    (CTuple p8 p9) ->
     pure p8
 pure p10
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
None =  pure  (CNone)

Some t10 =
 pure  (CSome t10)

grinMain _2 =
 o4 <-  Some 5
 case o4 of
  (CSome v6) ->
   pure v6
  (CNone ) ->
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
None#i32 =  pure  (CNone)

Some#i32 t10 =
 pure  (CSome t10)

grinMain _2 =
 o3 <-  Some#i32 5
 case o3 of
  (CSome v5) ->
   pure v5
  (CNone ) ->
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
      BuildOutput("""None#i32 =  pure  (CNone)

Some#i32 t10 =
 pure  (CSome t10)

mapOptioni32i32' self2 f''3 =
 case self2 of
  (CSome v5) ->
   p7 <- apply f''3 v5
   p8 <- Some#i32 p7
   pure p8
  #default ->
   p9 <- None#i32
   pure p9

grinMain _9 =
 o10 <-  Some#i32 5
 p14 <- pure (P1c12 )
 _14 <-  mapOptioni32i32' o10 p14
 pure 0

c12 a12 =
 _prim_i32_add a12 1

apply p16 p17 =
 case p16 of
  (P1c12 ) ->
   c12  p17
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
Some#i32 t10 =
 pure  (CSome t10)

None#i32 =  pure  (CNone)

mapOptionFunctori32i32' self2 f''3 =
 case self2 of
  (CSome v5) ->
   p7 <- apply f''3 v5
   p8 <- Some#i32 p7
   pure p8
  #default ->
   p9 <- None#i32
   pure p9

grinMain _9 =
 o10 <-  Some#i32 5
 p14 <- pure (P1c12 )
 _14 <-  mapOptionFunctori32i32' o10 p14
 pure 0

c12 a12 =
 _prim_i32_add a12 1

apply p16 p17 =
 case p16 of
  (P1c12 ) ->
   c12  p17
        """)
    )
  }
  test("build minimal generic option") {
    fuse("""
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
    fuse("""
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
      BuildOutput("""None#i32 =  pure  (CNone)

Some#i32 t10 =
 pure  (CSome t10)

mapOptioni32i32' self2 f''3 =
 case self2 of
  (CSome v5) ->
   p7 <- apply f''3 v5
   p8 <- Some#i32 p7
   pure p8
  #default ->
   p9 <- None#i32
   pure p9

grinMain _9 =
 o10 <-  Some#i32 5
 p14 <- pure (P1c12 )
 o114 <-  mapOptioni32i32' o10 p14
 case o114 of
  (CSome v16) ->
   pure v16
  (CNone ) ->
   pure 0

c12 a12 =
 _prim_i32_add a12 1

apply p18 p19 =
 case p18 of
  (P1c12 ) ->
   c12  p19
""")
    )
  }
  test("build generic option") {
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
        """,
      BuildOutput("""None#i32 =  pure  (CNone)

Some#i32 t10 =
 pure  (CSome t10)

mapOptionstri32' self2 f''3 =
 case self2 of
  (CSome v5) ->
   p7 <- apply f''3 v5
   p8 <- Some#i32 p7
   pure p8
  #default ->
   p9 <- None#i32
   pure p9

flatmapOptioni32i32' self9 f''10 =
 case self9 of
  (CSome a12) ->
   p14 <- apply f''10 a12
   pure p14
  (CNone ) ->
   p15 <- None#i32
   pure p15

tostrOption' v15 =
 s16 <-  Some#i32 v15
 p20 <- pure (P1c18 )
 mapOptionstri32' s16 p20

c18 a18 =
 _prim_int_str a18

grinMain _20 =
 o21 <-  Some#i32 5
 p26 <- pure (P1c23 )
 o126 <-  flatmapOptioni32i32' o21 p26
 l28 <-  tostr' 5
 case l28 of
  (CSome v30) ->
   pure 0
  (CNone ) ->
   pure 1

c23 t23 =
 p25 <- _prim_i32_add t23 1
 Some#i32 p25

apply p32 p33 =
 case p32 of
  (P1c18 ) ->
   c18  p33
  (P1c23 ) ->
   c23  p33
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
Tuple#i32#i32 t10 t21 =
 pure (CTuple t10 t21)

State#i32#i32 run''4 =
 p7 <- fetch run''4
 pure (CState p7)

value a7 =
 p11 <- do
   case a7 of
    (CState p10) ->
     pure p10
 t13 <-  p11 1
 p18 <- do
   case t13 of
    (CTuple p16 p17) ->
     pure p16
 p22 <- do
   case t13 of
    (CTuple p20 p21) ->
     pure p21
 _prim_i32_add p18 p22

grinMain _22 =
 p28 <- pure (P1c24 )
 p29 <- State#i32#i32 p28
 value p29

c24 a24 =
 p26 <- _prim_i32_add a24 1
 p27 <- _prim_i32_add a24 2
 Tuple#i32#i32 p26 p27

apply p30 p31 =
 case p30 of
  (P1c24 ) ->
   c24  p31""")
    )
  }
  test("build generic list with fold right") {
    fuse("""
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
    0
        """,
      BuildOutput("""Cons#i32 h0 t1 =
 pure  (CCons h0 t1)

Nil#i32 =  pure  (CNil)

foldRightListi32i32' as4 z5 f''6 =
 case as4 of
  (CCons x8 xs'9) ->
   p12'' <- apply f''6 x8
   p11 <- fetch xs'9
   p13 <- foldRightListi32i32' p11 z5 f''6
   p14 <- apply p12'' p13
   pure p14
  (CNil ) ->
   pure z5

mapListi32i32' self14 f''15 =
 p17 <- Nil#i32
 p23 <- pure (P2c18 f''15)
 foldRightListi32i32' self14 p17 p23

c18 f''1519 h19 t20 =
 p22 <- apply f''1519 h19
 Cons#i32 p22 t20

grinMain _23 =
 p25 <- Nil#i32
 p26 <- Cons#i32 3 p25
 l26 <-  Cons#i32 2 p26
 p30 <- pure (P1c28 )
 l130 <-  mapListi32i32' l26 p30
 pure 0

c28 v28 =
 _prim_i32_add v28 1

apply p32 p33 =
 case p32 of
  (P2c18 p36) ->
   pure (P1c18 p36 p33)
  (P1c18 p34 p35) ->
   c18 p34 p35 p33
  (P1c28 ) ->
   c28  p33
""")
    )
  }
  test("build generic list with simple map") {
    fuse("""
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
      BuildOutput("""Cons#i32 h0 t1 =
 pure  (CCons h0 t1)

Nil#i32 =  pure  (CNil)

simplemapListi32i32' self4 f''5 =
 case self4 of
  (CCons h7 t'8) ->
   p10 <- apply f''5 h7
   p11 <- Nil#i32
   p12 <- Cons#i32 p10 p11
   pure p12
  (CNil ) ->
   p13 <- Nil#i32
   pure p13

grinMain _13 =
 p15 <- Nil#i32
 p16 <- Cons#i32 3 p15
 l16 <-  Cons#i32 2 p16
 p20 <- pure (P1c18 )
 l120 <-  simplemapListi32i32' l16 p20
 pure 0

c18 v18 =
 _prim_i32_add v18 1

apply p22 p23 =
 case p22 of
  (P1c18 ) ->
   c18  p23
""")
    )
  }

  test("build generic list with varied parameter names") {
    fuse("""
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
      BuildOutput("""Cons#i32 h0 t1 =
 pure  (CCons h0 t1)

Nil#i32 =  pure  (CNil)

mapwithmapperListi32i32' self4 mapper''5 =
 case self4 of
  (CCons h7 t'8) ->
   p10 <- apply mapper''5 h7
   p11 <- Nil#i32
   p12 <- Cons#i32 p10 p11
   pure p12
  (CNil ) ->
   p13 <- Nil#i32
   pure p13

mapwithtransformListi32i32' self13 transform''14 =
 case self13 of
  (CCons h16 t'17) ->
   p19 <- apply transform''14 h16
   p20 <- Nil#i32
   p21 <- Cons#i32 p19 p20
   pure p21
  (CNil ) ->
   p22 <- Nil#i32
   pure p22

mapwithcallbackListi32i32' self22 callback''23 =
 case self22 of
  (CCons h25 t'26) ->
   p28 <- apply callback''23 h25
   p29 <- Nil#i32
   p30 <- Cons#i32 p28 p29
   pure p30
  (CNil ) ->
   p31 <- Nil#i32
   pure p31

grinMain _31 =
 p33 <- Nil#i32
 p34 <- Cons#i32 3 p33
 l34 <-  Cons#i32 2 p34
 p38 <- pure (P1c36 )
 l138 <-  mapwithmapperListi32i32' l34 p38
 p42 <- pure (P1c40 )
 l242 <-  mapwithtransformListi32i32' l34 p42
 p46 <- pure (P1c44 )
 l346 <-  mapwithcallbackListi32i32' l34 p46
 pure 0

c36 x36 =
 _prim_i32_add x36 1

c40 y40 =
 _prim_i32_mul y40 2

c44 z44 =
 _prim_i32_add z44 3

apply p48 p49 =
 case p48 of
  (P1c36 ) ->
   c36  p49
  (P1c40 ) ->
   c40  p49
  (P1c44 ) ->
   c44  p49
""")
    )
  }

  // LIMITATION: Recursive closures with generic types
  // This test is disabled because recursive closures in generic contexts (like map_2[B])
  // create TermClosure nodes without type annotations. During monomorphization, we cannot
  // infer these types because:
  // 1. The specialized term still contains TypeVars pointing to context (e.g., List type constructor)
  // 2. Re-inferring types during monomorphization creates existential variables that remain unresolved
  // 3. GRIN generation runs in a fresh context, so cannot look up types from bindings
  //
  // Possible solutions (not yet implemented):
  // - Type-guided population: traverse specialized type and term in parallel to extract parameter types
  // - Reconstruct closures with types during type checking phase (before monomorphization)
  // - Extend GRIN generation to handle closure parameter type inference
  test("build generic list with iter map") {
    fuse(
      """
type List[A]:
    Cons(h: A, t: List[A])
    Nil

impl List[A]:
    fun map_2[B](self, f: A -> B) -> List[B]
        let iter = (acc, l) => {
            match l:
                Cons(h, t) => Cons(f(h), iter(acc, t))
                Nil => acc
        }
        iter(Nil[B], self)

fun main() -> i32
    let l = Cons(2, Cons(3, Nil))
    let l1 = l.map_2(v => v + 1)
    0
        """,
      BuildOutput("""Cons#i32 h0 t1 =
 pure  (CCons h0 t1)

Nil#i32 =  pure  (CNil)

map2Listi32i32' self4 f''5 =
 p19 <- Nil#i32
 iter7 f''5 p19 self4

iter7 f''58 acc8 l9 =
 case l9 of
  (CCons h11 t'12) ->
   p14 <- apply f''58 h11
   p15 <- fetch t'12
   p16 <- iter7 f''58 acc8 p15
   p17 <- Cons#i32 p14 p16
   pure p17
  (CNil ) ->
   pure acc8

grinMain _19 =
 p21 <- Nil#i32
 p22 <- Cons#i32 3 p21
 l22 <-  Cons#i32 2 p22
 p26 <- pure (P1c24 )
 l126 <-  map2Listi32i32' l22 p26
 pure 0

c24 v24 =
 _prim_i32_add v24 1

apply p28 p29 =
 case p28 of
  (P1c24 ) ->
   c24  p29
""")
    )
  }
}

object CompilerTests {
  sealed trait Output
  case class CheckOutput(s: Option[String]) extends Output
  case class BuildOutput(s: String) extends Output

  def check(code: String, fileName: String = "test.fuse") =
    Compiler.compile(CheckFile(fileName), code.trim, fileName)

  def build(code: String, fileName: String = "test.fuse") =
    Compiler.compile(BuildFile(fileName), code.trim, fileName)
}
