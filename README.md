**Fuse** is a statically typed functional language.

# Features

- Algebraic Data Types (ADT)
- Generics (Parametric Polymorphism)
- Type Methods
- Traits (Type Classes)
- Pattern Matching
- Uses [Grin](https://github.com/grin-compiler/grin) as compiler backend

# Installation

```bash
curl -fsSL https://fuselang.github.io/fuse/fuseup | sh
```

# Road-map

- [x] Parser
- [x] Type Checker for System F with ADT and Pattern Matching
- [x] Type Error Messages
- [x] Code Generation to LLVM (without generics)
- [x] Implement Type Inference for higher order types (Bidirectional)
- [x] Implement Type Classes
- [x] Implement `do` notation
- [x] Code Generation to LLVM with monomorphisation
- [ ] Add modules & imports

# Example with [Tree-Sitter](https://github.com/stevanmilic/tree-sitter-fuse)

![fuse](https://user-images.githubusercontent.com/6879030/231895964-3d6e447a-726c-4bfd-9b0a-7785a54d419d.png)

# ADTs (Algebraic Data Types)

## Sum type (Enumeration)

```
type bool:
    true
    false
```

## Record (Product type)

```
type Point:
    x: i32
    y: i32
```

## Tuple (Product type)

```
type Pair(i32, str)
```

## Generics (parametric polymorphism)

```
type Option[T]:
    None
    Some(T)

type Map[K, V]:
    key: K
    value: V

type Data[T](Option[Map[str, T]])

type Tuple[A, B](A, B)
```

# Type Methods

```
impl Option[T]:
    fun is_some(self) -> bool
        match self:
            Some(_) => True
            None => False

impl Point:
    fun distance(self, other: Point) -> f32
        let x_diff = self.x - other.x
        let y_diff = self.y - other.y
        math.sqrt(x_diff * x_diff - y_diff * y_diff)
```

# Traits (Type Classes)

```
trait Monad[A]:
  fun unit[T](a: T) -> Self[T];

  fun flat_map[B](self, f: A -> Self[B]) -> Self[B];

  fun map[B](self, f: A -> B) -> Self[B]
    let f = a => Self::unit(f(a))
    self.flat_map(f)

impl Monad for Option[A]:
  fun unit[T](a: T) -> Option[T]
    Some(a)

  fun flat_map[B](self, f: A -> Option[B]) -> Option[B]
    match self:
      Some(v) => f(v)
      _ => None
```

# Type Alias

```
type Ints = List[i32]
```

# Expressions

## Operators

```
1 + 1
# output: 2 (i32)

2 - 1
# output: 1 (i32)

"Hello " + " World"
# output: "Hello World" (str)

1 == 2
# output: false (bool)

# comparison and equality
less < than
less_than <= or_equal
greater > then
greaterThan >= orEqual
```

## Let expression

```
let x = 5
let y = x + 1

let m = Some(5)
```

## Match expression

```
let x = 2

match x:
    1 => "one"
    2 => "two"
    _ => "whatever"

let m = Some(5)
match m:
    Some(v) => v + 1
    None => 0
```

# Functions

```
fun sum(x: i32, y: i32) -> i32
    x + y

sum(5, 3)
```

## Recursive Functions

```
fun fib(n: i32, a: i32, b: i32) -> i32
    match n:
        0 => b
        _ => fib(n - 1, b, a + b)
```

## Lambda Functions

```
let f = a => a + 1

let g = (x, y) => x + y

let f_annotated: i32 = (a: i32) -> i32 => a + 1
```


## Do Notation

```
let x = Some(1)
let y = Some(2)
let z = Some(3)
do:
    i <- x
    j <- y
    k <- z
    i + j + k
```
