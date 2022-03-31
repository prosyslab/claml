#include "clang/AST/Attr.h"
#include "clang/AST/DeclBase.h"

#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

extern "C" {
extern int debug;
}

#define LOG(s)                                                                 \
  if (debug) {                                                                 \
    fprintf(stderr, "%s @ %s:%d\n", s, __FILE__, __LINE__);                    \
    fflush(stderr);                                                            \
  }

#define WRAPPER_INT(fname, param_type, fun)                                    \
  value fname(value Param) {                                                   \
    CAMLparam1(Param);                                                         \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    CAMLreturn(Val_int(P->fun()));                                             \
  }

#define WRAPPER_INT64(fname, param_type, fun)                                  \
  value fname(value Param) {                                                   \
    CAMLparam1(Param);                                                         \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    llvm::APInt V = P->fun();                                                  \
    CAMLreturn(clang_to_int64(V));                                             \
  }

#define WRAPPER_STR(fname, param_type, fun)                                    \
  value fname(value Param) {                                                   \
    CAMLparam1(Param);                                                         \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    assert(P != NULL);                                                         \
    CAMLreturn(clang_to_string(P->fun()));                                     \
  }

#define WRAPPER_STRREF(fname, param_type, fun)                                 \
  value fname(value Param) {                                                   \
    CAMLparam1(Param);                                                         \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    CAMLreturn(clang_to_string(P->fun().data()));                              \
  }

#define WRAPPER_BOOL(fname, param_type, fun)                                   \
  value fname(value Param) {                                                   \
    CAMLparam1(Param);                                                         \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    CAMLreturn(Val_bool(P->fun()));                                            \
  }

#define WRAPPER_PTR(fname, param_type, return_type, fun)                       \
  value fname(value Param) {                                                   \
    CAMLparam1(Param);                                                         \
    CAMLlocal1(R);                                                             \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    R = caml_alloc(1, Abstract_tag);                                           \
    *((clang::return_type **)Data_abstract_val(R)) = P->fun();                 \
    CAMLreturn(R);                                                             \
  }

#define WRAPPER_PTR_OPTION(fname, param_type, return_type, fun)                \
  value fname(value Param) {                                                   \
    CAMLparam1(Param);                                                         \
    CAMLlocal1(R);                                                             \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    clang::return_type *Ret = P->fun();                                        \
    if (Ret) {                                                                 \
      R = caml_alloc(1, Abstract_tag);                                         \
      *((clang::return_type **)Data_abstract_val(R)) = Ret;                    \
      CAMLreturn(caml_alloc_some(R));                                          \
    } else {                                                                   \
      CAMLreturn(Val_none);                                                    \
    }                                                                          \
  }

#define WRAPPER_VOID(fname, param_type, fun)                                   \
  void fname(value Param) {                                                    \
    CAMLparam1(Param);                                                         \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    P->fun();                                                                  \
    CAMLreturn0;                                                               \
  }

#define WRAPPER_LIST_WITH_IDX(fname, param_type, elem_type, fun_size,          \
                              fun_access)                                      \
  value fname(value Param) {                                                   \
    CAMLparam1(Param);                                                         \
    CAMLlocal3(Hd, Tl, Tmp);                                                   \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    Tl = Val_int(0);                                                           \
    for (unsigned int i = P->fun_size(); i > 0; i--) {                         \
      Hd = caml_alloc(1, Abstract_tag);                                        \
      *((const clang::elem_type **)Data_abstract_val(Hd)) =                    \
          P->fun_access(i - 1);                                                \
      Tmp = caml_alloc(2, 0);                                                  \
      Store_field(Tmp, 0, Hd);                                                 \
      Store_field(Tmp, 1, Tl);                                                 \
      Tl = Tmp;                                                                \
    }                                                                          \
    CAMLreturn(Tl);                                                            \
  }

#define WRAPPER_LIST_WITH_REV_ITER(fname, param_type, elem_type, fun_rbegin,   \
                                   fun_rend)                                   \
  value fname(value Param) {                                                   \
    CAMLparam1(Param);                                                         \
    CAMLlocal4(Hd, Tl, AT, PT);                                                \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    Tl = Val_int(0);                                                           \
    for (auto i = P->fun_rbegin(); i != P->fun_rend(); i++) {                  \
      Hd = caml_alloc(1, Abstract_tag);                                        \
      *((const clang::elem_type **)Data_abstract_val(Hd)) = *i;                \
      value Tmp = caml_alloc(2, 0);                                            \
      Store_field(Tmp, 0, Hd);                                                 \
      Store_field(Tmp, 1, Tl);                                                 \
      Tl = Tmp;                                                                \
    }                                                                          \
    CAMLreturn(Tl);                                                            \
  }

#define WRAPPER_QUAL_TYPE(fname, param_type, fun)                              \
  value fname(value Param) {                                                   \
    CAMLparam1(Param);                                                         \
    LOG("" #fname "");                                                         \
    clang::param_type *P = *((clang::param_type **)Data_abstract_val(Param));  \
    CAMLreturn(clang_to_qual_type(P->fun()));                                  \
  }

extern "C" {
value clang_to_string(const char *str);

value clang_to_qual_type(clang::QualType QT);

value clang_to_int64(llvm::APInt I);
}
