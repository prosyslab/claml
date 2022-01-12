#include "clang/AST/DeclBase.h"
#include "clang/Frontend/ASTUnit.h"
#include "clang/Frontend/CompilerInstance.h"

#include "caml/alloc.h"
#include "caml/callback.h"
#include "caml/custom.h"
#include "caml/fail.h"
#include "caml/memory.h"
#include "caml/mlvalues.h"

#include "utils.h"

extern "C" {
value clang_type_ptr(value QT) {
  CAMLparam1(QT);
  CAMLlocal1(result);
  clang::QualType *qt = *((clang::QualType **)Data_abstract_val(QT));
  result = caml_alloc(1, Abstract_tag);
  *((const clang::Type **)Data_abstract_val(result)) = qt->getTypePtr();
  CAMLreturn((result));
}

value clang_builtin_type_kind(value T) {
  CAMLparam1(T);
  clang::Type *Ty = *((clang::Type **)Data_abstract_val(T));
  if (clang::BuiltinType *BT = llvm::dyn_cast<clang::BuiltinType>(Ty)) {
    CAMLreturn(Val_int(BT->getKind() - 99));
  } else {
    assert(false);
  }
}

value clang_qual_type_to_string(value QT) {
  CAMLparam1(QT);
  CAMLlocal1(result);
  clang::QualType *qt = *((clang::QualType **)Data_abstract_val(QT));
  CAMLreturn(clang_to_string(qt->getAsString().c_str()));
}

value clang_type_kind(value type) {
  CAMLparam1(type);
  clang::Type *Ty = *((clang::Type **)Data_abstract_val(type));
  CAMLreturn(Val_int(Ty->getTypeClass()));
}

value clang_type_get_kind_name(value Type) {
  CAMLparam1(Type);
  clang::Type *Ty = *((clang::Type **)Data_abstract_val(Type));
  CAMLreturn(clang_to_string(Ty->getTypeClassName()));
}

value clang_type_get_kind_enum(value Type) {
  CAMLparam1(Type);
  clang::Type *Ty = *((clang::Type **)Data_abstract_val(Type));
  CAMLreturn(Val_int(Ty->getTypeClass()));
}

value clang_function_type_get_return_type(value QT) {
  CAMLparam1(QT);
  clang::FunctionType *FT = *((clang::FunctionType **)Data_abstract_val(QT));
  CAMLreturn(clang_to_qual_type(FT->getReturnType()));
}

value clang_function_type_get_param_types(value T) {
  CAMLparam1(T);
  CAMLlocal4(Hd, Tl, AT, PT);
  clang::FunctionProtoType *FT =
      *((clang::FunctionProtoType **)Data_abstract_val(T));
  Tl = Val_int(0);
  for (unsigned int i = FT->getNumParams(); i > 0; i--) {
    Hd = clang_to_qual_type(FT->getParamType(i - 1));
    value Tmp = caml_alloc(2, Abstract_tag);
    Field(Tmp, 0) = Hd;
    Field(Tmp, 1) = Tl;
    Tl = Tmp;
  }
  CAMLreturn(Tl);
}

value clang_pointer_type_get_pointee_type(value PT) {
  CAMLparam1(PT);
  clang::PointerType *FT = *((clang::PointerType **)Data_abstract_val(PT));
  CAMLreturn(clang_to_qual_type(FT->getPointeeType()));
}

value clang_elaborated_type_desugar(value Type) {
  CAMLparam1(Type);
  clang::ElaboratedType *ET =
      *((clang::ElaboratedType **)Data_abstract_val(Type));
  CAMLreturn(clang_to_qual_type(ET->desugar()));
}

value clang_enum_type_get_decl(value Type) {
  CAMLparam1(Type);
  CAMLlocal1(R);
  clang::EnumType *T = *((clang::EnumType **)Data_abstract_val(Type));
  R = caml_alloc(1, Abstract_tag);
  *((clang::EnumDecl **)Data_abstract_val(R)) = T->getDecl();
  CAMLreturn(R);
}

value clang_record_type_get_decl(value Type) {
  CAMLparam1(Type);
  CAMLlocal1(R);
  clang::RecordType *T = *((clang::RecordType **)Data_abstract_val(Type));
  R = caml_alloc(1, Abstract_tag);
  *((clang::RecordDecl **)Data_abstract_val(R)) = T->getDecl();
  CAMLreturn(R);
}

value clang_typedef_type_get_decl(value Type) {
  CAMLparam1(Type);
  CAMLlocal1(R);
  clang::TypedefType *T = *((clang::TypedefType **)Data_abstract_val(Type));
  R = caml_alloc(1, Abstract_tag);
  *((clang::TypedefNameDecl **)Data_abstract_val(R)) = T->getDecl();
  CAMLreturn(R);
}

value clang_decayed_type_get_decayed_type(value Type) {
  CAMLparam1(Type);
  clang::DecayedType *T = *((clang::DecayedType **)Data_abstract_val(Type));
  CAMLreturn(clang_to_qual_type(T->getDecayedType()));
}

value clang_adjusted_type_get_original_type(value Type) {
  CAMLparam1(Type);
  clang::AdjustedType *T = *((clang::AdjustedType **)Data_abstract_val(Type));
  CAMLreturn(clang_to_qual_type(T->getOriginalType()));
}

value clang_array_type_get_element_type(value Type) {
  CAMLparam1(Type);
  CAMLlocal1(R);
  clang::ArrayType *T = *((clang::ArrayType **)Data_abstract_val(Type));
  CAMLreturn(clang_to_qual_type(T->getElementType()));
}

value clang_constant_array_type_get_size_expr(value Type) {
  CAMLparam1(Type);
  LOG("begin clang_constant_array_type_get_size_expr");
  CAMLlocal1(R);
  clang::ConstantArrayType *T =
      *((clang::ConstantArrayType **)Data_abstract_val(Type));
  R = caml_alloc(1, Abstract_tag);
  llvm::APInt V = T->getSize();
  LOG("end clang_constant_array_type_get_size_expr");
  CAMLreturn(clang_to_int64(V));
}

value clang_variable_array_type_get_size_expr(value Type) {
  CAMLparam1(Type);
  CAMLlocal1(R);
  clang::VariableArrayType *T =
      *((clang::VariableArrayType **)Data_abstract_val(Type));
  R = caml_alloc(1, Abstract_tag);
  *((clang::Expr **)Data_abstract_val(R)) = T->getSizeExpr();
  CAMLreturn(R);
}
}
