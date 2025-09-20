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

- add to memory. always use `bloop test fuse-test -- "fuse.CompilerCheckTests.<test-name"` to run tests
- add to memory; always use match case expressions instead of `if` expressions in scala codebase
- add to memory; always use functional style instead of doing any mutations => pure functions
- add to memory; always write tests and iterate over results to write the correct code
- add to memory; always use debug statements to understand the issue before trying to resolve it
- add to memory; always try to make as least number of changes (files and lines) to find the correct solution.
- add to memory; always define functions in file below the function it's being used it
