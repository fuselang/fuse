# Fuse Compiler Project Guide

## Monomorphization
The Fuse compiler implements monomorphization to convert generic functions into specialized versions. See `docs/monomorphization.md` for common issues and troubleshooting.

## Key Files
- **`src/main/scala/core/Instantiations.scala`**: Type checking and instantiation creation
- **`src/main/scala/code/Monomorphization.scala`**: Generic function specialization
- **`src/main/scala/code/Grin.scala`**: Code generation phase

## Test Command
```bash
bloop test fuse-test -- "fuse.CompilerCheckTests.<test-name>"
```

## Debugging De Bruijn Index Issues: A Tutorial

When working with monomorphization and generic functions, De Bruijn index mismatches can be tricky to debug. Here's a systematic approach:

### 1. Start with Strategic Debug Placement

Add debug statements at key transformation points to trace variable resolution:

```scala
// In Instantiations.scala - Track instantiation collection
case (termVar @ TermVar(info, idx, c), _, _) =>
  for {
    _ <- EitherT.liftF(State.pure(println(s"[DEBUG-INST] Creating TermVar instantiation: idx=$idx, context_len=$c")))
    optionName <- EitherT.liftF(State.inspect { (ctx: Context) =>
      val name = indexToName(ctx, idx)
      println(s"[DEBUG-INST] TermVar idx=$idx resolves to name='${name.getOrElse("NOT FOUND")}', ctx_len=${ctx._1.length}")
      name
    })
    // ... rest of logic
  } yield acc :+ Instantiation(name, termVar, tys, cls)
```

```scala
// In Monomorphization.scala - Track term replacement
def replaceInstantiations(bind: Bind): ContextState[Bind] =
  println(s"[DEBUG-MONO] Processing bind '${bind.i}' with ${bind.insts.length} instantiations")
  bind.insts.foreach(inst => println(s"[DEBUG-MONO]   - Inst: ${inst.i}, term=${inst.term}, r=${inst.r}"))
  // ... rest of function
```

```scala
// In Grin.scala - Track code generation phase
case TermVar(info, idx, ctxLen) =>
  for {
    _ <- State.pure(println(s"[DEBUG-GRIN] Processing TermVar with idx=$idx, ctxLen=$ctxLen"))
    variable <- toVariable(idx)
    _ <- State.pure(println(s"[DEBUG-GRIN] TermVar resolved to variable: $variable"))
    // ... rest of logic
  } yield expr
```

### 2. Trace Index Resolution Across Phases

Follow a specific variable through all transformation phases:

1. **Type Checking Phase**: Where is the instantiation captured?
2. **Monomorphization Phase**: How are indices adjusted during specialization?
3. **Grin Generation Phase**: Where does the final resolution fail?

Look for patterns like:
- Same index resolving to different variables in different phases
- Context length mismatches between phases
- Indices pointing to wrong bindings after transformations

### 3. Compare Original vs Specialized Terms

When indices are wrong, compare the original term structure with the specialized version:

```scala
// Before specialization: TermVar(_, 5, 100) -> "+"
// After specialization: TermVar(_, 95, 102) -> "!+#i32#Add"
// Problem: Index 95 in context 102 points to wrong binding
```

### 4. Check Term Shifting Operations

Focus on functions that modify De Bruijn indices:

```scala
// Look for missing or incorrect term shifting
termShiftAbove(-1, ctxlen, i.term)  // Critical for proper index adjustment
```

### 5. Isolate the Failing Component

When you find type errors in Grin generation, work backwards:
- Which exact term is causing the type mismatch?
- What binding does the failing index point to?
- Was this index correct before monomorphization?

### 6. Test Hypothesis with Targeted Fixes

Instead of broad changes, make surgical fixes:
- Fix term shifting in one specific location
- Add specialized method handling for one specific case
- Preserve correct indices during transformation

### 7. Clean Up Debug Statements

Once the issue is resolved, systematically remove debug statements:

```scala
// Remove all println debug statements
// Simplify complex debug for-comprehensions
// Keep the essential fix, remove exploration code
```

### Common Patterns to Watch For

- **Index Capture**: Instantiations captured with context-dependent indices
- **Term Shifting**: Missing `termShiftAbove` calls during specialization
- **Specialized Methods**: Need special type extraction to avoid re-inferring with wrong indices
- **Context Depth**: Different context lengths between phases causing index misalignment

### Key Insight

De Bruijn index bugs often manifest as type errors in later phases (like Grin generation) but originate from incorrect index management in earlier phases (instantiation collection or monomorphization).

- add to memory. always use `bloop test fuse-test -- "fuse.CompilerCheckTests.<test-name"` to run tests
- add to memory; always use match case expressions instead of `if` expressions in scala codebase
- add to memory; always use functional style instead of doing any mutations => pure functions
- add to memory; always write tests and iterate over results to write the correct code
- add to memory; always use debug statements to understand the issue before trying to resolve it
- add to memory; always try to make as least number of changes (files and lines) to find the correct solution.
- add to memory; always define functions in file below the function it's being used it
- add to memory. every instantiation collected during monomorphization phase must be applied; in case they're not applied an infinite recurion (stack overflow) might happen, do not apply band-aid fixes for such cases rather focus on root cause resolution of instantiation not being replaced.