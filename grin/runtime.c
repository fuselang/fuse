#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

#ifdef USE_BOEHM_GC
#include <gc.h>
#else
extern int64_t _heap_ptr_;
#endif

int g_argc = 0;
char** g_argv = NULL;

/* Compiled Fuse programs guarantee `grinMain` yields the process exit code as
   a T_Int64: `main() -> i32` returns its own value, every other `main` return
   type is wrapped by the code generator so the result is discarded and 0 is
   returned in its place. */
int64_t grinMain();

void __runtime_error(int64_t c){
  exit(c);
}

int main(int argc, char** argv) {
  g_argc = argc;
  g_argv = argv;

#ifdef USE_BOEHM_GC
  GC_INIT();
#else
  int64_t* heap = malloc(100*1024*1024);
  _heap_ptr_ = (int64_t)heap;
#endif

  int64_t exit_code = grinMain();

#ifndef USE_BOEHM_GC
  free(heap);
#endif

  return (int)exit_code;
}
