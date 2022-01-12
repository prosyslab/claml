#include "clang/AST/DeclBase.h"

#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#define DEBUG 0

#ifdef DEBUG
#define LOG(s) fprintf(stderr, "%s @ %s:%d\n", s, __FILE__, __LINE__);
#else
#define LOG(s)
#endif

extern "C" {
value clang_to_string(const char *str);

value clang_to_qual_type(clang::QualType QT);

value clang_to_int64(llvm::APInt I);
}
