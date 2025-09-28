# Monomorphization Process in Fuse

## Overview

Monomorphization (`src/main/scala/code/Monomorphization.scala:28-40`) is the process of replacing generic (polymorphic) functions with specialized monomorphic versions for each concrete type they're used with. This enables compilation to LLVM by eliminating runtime type parameters.

## Key Components

### 1. Instantiation Collection (`core/Instantiations.scala:24-58`)

The **Instantiation** data structure tracks:
- Function identifier (`i: String`)
- Term usage (`term: Term`) 
- Concrete types (`tys: List[Type]`)
- Type classes (`cls: List[TypeClass]`)
- Resolved index (`r: Option[Int]`)

The **bindName()** method generates specialized function names using the pattern:
```
functionName#type1#type2#...#typeClass
```
Example: `map#i32#List` for `map` with `i32` and `List` types.

### 2. Main Monomorphization Algorithm (`Monomorphization.scala:41-60`)

The `replace()` function follows this recursive process:

1. **Collect instantiations** from all bindings
2. **Create specialized functions** for each instantiation
3. **Replace generic calls** with specialized versions  
4. **Recurse** until no more generic instantiations exist

### 3. Specialized Function Generation (`Monomorphization.scala:176-228`)

For each generic function with instantiations:

- **Type Substitution**: `specializeTerm()` substitutes type variables with concrete types
- **Context Management**: Handles variable shifting as new specialized bindings are added
- **Name Generation**: Creates unique names for specialized versions

### 4. Generic Type Handling

- **TypeAll**: Universal quantification (`∀ T. ...`) - represents generic types
- **TypeVar**: Type variables with De Bruijn indices for scope tracking
- **Primitive Type Filtering**: Only processes instantiations with primitive types to avoid infinite recursion

### 5. Integration with Grin Backend (`code/Grin.scala`)

- Monomorphic functions are converted to Grin intermediate representation
- Specialized functions become regular Grin functions without type parameters
- The pipeline: **Fuse AST → Monomorphization → Grin IR → LLVM**

## Process Flow

1. **Type Checker** collects instantiations during inference (`TypeChecker.scala:109`)
2. **Monomorphization** creates specialized versions of generic functions
3. **Grin Generator** produces monomorphic intermediate code
4. **LLVM Backend** compiles to machine code

## Example Transformation

```scala
// Generic function
fun map[T, U](f: T -> U, list: List[T]) -> List[U] = ...

// Usage
map(x => x + 1, [1, 2, 3])      // T=i32, U=i32
map(x => x.length, ["a", "b"])  // T=str, U=i32

// After monomorphization
map#i32#i32(f: i32 -> i32, list: List[i32]) -> List[i32] = ...
map#str#i32(f: str -> i32, list: List[str]) -> List[i32] = ...
```

## Implementation Details

### Type Substitution (`Monomorphization.scala:230-260`)

The `specializeTerm()` function handles type variable substitution:
- Removes type abstractions (`TermTAbs`)
- Substitutes concrete types for type variables
- Maintains proper variable indexing

### Context Management and De Bruijn Index Handling

The monomorphization process carefully manages the typing context through proper De Bruijn index handling:

- **Term Shifting**: Critical `termShiftAbove(-1, ctxlen, i.term)` operations adjust variable indices when moving terms between contexts
- **Context Length Tracking**: Maintains accurate context depths as specialized bindings are added
- **Index Preservation**: Ensures instantiated terms maintain correct variable references after specialization

#### Common De Bruijn Index Issues

**Problem**: Specialized method calls may reference wrong bindings due to index mismatches after monomorphization.

**Example**:
```scala
// Before monomorphization: TermVar(_, 5, 100) resolves to "+"
// After monomorphization: TermVar(_, 95, 102) incorrectly resolves to different binding
```

**Solution**: The Grin generation phase includes specialized method handling to avoid re-inferring types on terms with potentially incorrect indices:

```scala
// In Grin.scala TermLet case
variableType <- if (SpecializedMethodUtils.containsSpecializedMethod(t1)) {
  getSpecializedTermType(t1).flatMap {
    case Some(ty) => State.pure(ty)  // Use extracted type
    case None => typeCheck(t1)       // Fallback to inference
  }
} else {
  typeCheck(t1)  // Regular type checking
}
```

This prevents type errors when processing expressions containing specialized methods like `!map#Option#i32#i32`.

### Recursive Processing

The algorithm processes instantiations recursively because:
- Specialized functions may introduce new generic instantiations
- Complex types are reduced in subsequent iterations
- Ensures complete monomorphization
- Handles nested generic structures properly

## Troubleshooting Common Issues

### Type Errors in Grin Generation

**Symptoms**: Error messages like "expected type of `print[i32]`, found `int_to_str[i32]`" during Grin generation.

**Root Cause**: Usually indicates De Bruijn index mismatches where instantiated terms reference wrong bindings after monomorphization.

**Debugging Steps**:
1. Add debug statements to trace variable resolution across phases
2. Compare index→name mappings before and after monomorphization
3. Check for missing `termShiftAbove` calls in specialized binding creation
4. Verify context lengths match between instantiation collection and resolution

### Operator Monomorphization Issues

**Symptoms**: Operators like `+` not being replaced with primitives like `_prim_i32_add`.

**Root Cause**: Term shifting not properly adjusting indices during specialization.

**Solution**: Ensure `termShiftAbove(-1, ctxlen, i.term)` is called when creating specialized instantiations.

### Infinite Specialization Loops

**Symptoms**: Compilation never terminates, creating endless specialized functions.

**Root Cause**: Non-primitive types in instantiation filtering.

**Solution**: The `filteredBindInsts` logic ensures only primitive types are processed:
```scala
filteredBindInsts <- bindInsts
  .filter(_.tys.forall(_.isPrimitive))
```

## Benefits

This process eliminates runtime polymorphism while:
- Preserving type safety
- Supporting complex generic features like type classes
- Enabling efficient compilation to LLVM
- Maintaining support for algebraic data types

The monomorphization phase is crucial for Fuse's compilation strategy, bridging the gap between high-level polymorphic code and efficient machine code generation.