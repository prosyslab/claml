#include "utils.h"

#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

extern "C" {
value clang_to_string(const char *str) {
  value s = caml_copy_string(str);
  return s;
}

value clang_to_qual_type(clang::QualType QT) {
  CAMLparam0();
  CAMLlocal2(R, T);
  R = caml_alloc(2, Abstract_tag);
  T = caml_alloc(1, Abstract_tag);
  *((const clang::Type **)Data_abstract_val(T)) = QT.getTypePtr();
  Store_field(R, 0, T);
  Store_field(R, 1, Val_bool(QT.isConstQualified()));
  CAMLreturn(R);
}
}
