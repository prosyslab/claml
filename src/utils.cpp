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
  LOG(__FUNCTION__);
  R = caml_alloc(2, 0);
  T = caml_alloc(1, Abstract_tag);
  *((const clang::Type **)Data_abstract_val(T)) = QT.getTypePtr();
  LOG(__FUNCTION__);
  Store_field(R, 0, T);
  Store_field(R, 1, Val_bool(QT.isConstQualified()));
  LOG(__FUNCTION__);
  CAMLreturn(R);
}

value clang_to_int64(llvm::APInt I) {
  CAMLparam0();
  LOG(__FUNCTION__);
  unsigned int Bit = I.getBitWidth();
  if (I.isSignedIntN(Bit)) {
    CAMLreturn(caml_copy_int64(I.getSExtValue()));
  } else {
    CAMLreturn(caml_copy_int64(I.getZExtValue()));
  }
}
}
