#include "clang/AST/DeclBase.h"

#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

extern "C" {
value clang_to_string(const char *str);

value clang_to_qual_type(clang::QualType QT);
}
