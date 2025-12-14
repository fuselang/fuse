# Fuse Compiler Project Guide

## Monomorphization
The Fuse compiler implements monomorphization to convert generic functions into specialized versions. See `docs/monomorphization.md` for common issues and troubleshooting.

## Key Files
- **`src/main/scala/core/Instantiations.scala`**: Type checking and instantiation creation
- **`src/main/scala/code/Monomorphization.scala`**: Generic function specialization
- **`src/main/scala/code/Grin.scala`**: Code generation phase

## Test Command
```bash
nix-shell --run "bloop test fuse-test -- 'fuse.CompilerCheckTests.<test-name>'"
```

## GRIN Executable

The `grin` executable is available at `/root/grin/result/bin/grin` (added to PATH in `~/.zshrc`).

**Purpose**: GRIN (Graph Reduction Intermediate Notation) is a low-level intermediate representation and compiler for lazy functional languages. It's used as the compilation target for the Fuse compiler.

**IMPORTANT**: GRIN must ALWAYS be invoked through `nix-shell` to ensure proper environment setup and dependencies:

```bash
nix-shell --run "grin FILES... [OPTIONS]"
```

**Common Options**:
- `--optimize`: Run optimization passes
- `--llvm`: Generate LLVM IR
- `--eval`: Evaluate the GRIN program
- `--save-llvm ARG`: Save LLVM output to file
- `--save-grin ARG`: Save GRIN output to file
- `-l, --lint`: Lint the GRIN code
- `-t, --hpt`: Run Heap Points-To analysis
- `--print-grin OPT`: Print GRIN code
- `-C FILE`: Link C runtime files (required: `-C grin/runtime.c -C grin/prim_ops.c`)

**Example Usage**:
```bash
# Compile GRIN to executable
nix-shell --run "grin file.grin --optimize -o output.out -q -C grin/runtime.c -C grin/prim_ops.c"

# Lint GRIN code
nix-shell --run "grin file.grin --lint -C grin/runtime.c -C grin/prim_ops.c"
```

**Example Workflow**:
1. Fuse compiler generates GRIN code from Fuse source
2. GRIN compiler optimizes and analyzes the intermediate representation
3. GRIN compiler generates executable via LLVM backend

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

## Memory

- add to memory. always use `nix-shell -- run "bloop test fuse-test -- 'fuse.CompilerCheckTests.<test-name'"` to run tests
- add to memory; always use match case expressions instead of `if` expressions in scala codebase
- add to memory; always use functional style instead of doing any mutations => pure functions
- add to memory; always define helper (child) functions below the (parent) function they are used uin
- add to memory; always write tests and iterate over results to write the correct code
- add to memory; always use debug statements to understand the issue before trying to resolve it
- add to memory; always try to make as least number of changes (files and lines) to find the correct solution.
- add to memory; always define functions in file below the function it's being used it
- add to memory. every instantiation collected during monomorphization phase must be applied; in case they're not applied an infinite recurion (stack overflow) might happen, do not apply band-aid fixes for such cases rather focus on root cause resolution of instantiation not being replaced.
- add to memory. always try to make the test pass with its existing structure, instead of changing the test.
- add to memory. always use the assertion for the complete `BuildOutput` (grin code) for compiler build tests.
- add to memory. always incluide a debugging step in the planning phase when requested to solve an issue.
- add to memory. always ensure all tests are passing when making incremental changes by running the whole test suite.
- add to memory. `grin` executable is available at `/root/grin/result/bin/grin` (in PATH). It compiles GRIN intermediate representation to executables via LLVM. The Fuse compiler outputs GRIN code which is then processed by the grin executable.
- add to memory; always run test to evaluate grin code execution. do not use tmp fuse or grin files
- add to memory. **CRITICAL**: GRIN must ALWAYS be invoked through `nix-shell` from `/root/fuse`: `nix-shell --run "grin <file.grin> --optimize -o <output> -q -C grin/runtime.c -C grin/prim_ops.c"`. NEVER invoke grin directly without nix-shell wrapper.
- add to memory. C file paths for grin must be relative (grin/runtime.c, grin/prim_ops.c) not absolute. Wrong paths cause misleading T_Dead errors.
- add to memory; don't use /tmp folder for grin compilation for simpler execution
- add to memory; always add `-C grin/prim_ops.c` and `-C grin/runtime.c` when using `grin` cli command to ensure proper compilation
- add to memory. when invoking grin programmatically (e.g., in Fuse.scala), wrap the command with `nix-shell --run "..."` to ensure proper environment setup
