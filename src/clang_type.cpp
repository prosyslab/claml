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

value clang_builtin_type_get_kind(value T) {
  CAMLparam1(T);
  clang::Type *Ty = *((clang::Type **)Data_abstract_val(T));
  if (clang::BuiltinType *BT = llvm::dyn_cast<clang::BuiltinType>(Ty)) {
    CAMLreturn(Val_int(BT->getKind()));
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

WRAPPER_INT(clang_type_get_kind, Type, getTypeClass)

WRAPPER_STR(clang_type_get_kind_name, Type, getTypeClassName)

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

WRAPPER_QUAL_TYPE(clang_pointer_type_get_pointee_type, PointerType,
                  getPointeeType)

WRAPPER_QUAL_TYPE(clang_paren_type_desugar, ParenType, desugar)

WRAPPER_QUAL_TYPE(clang_elaborated_type_desugar, ElaboratedType, desugar)

WRAPPER_QUAL_TYPE(clang_elaborated_type_get_named_type, ElaboratedType,
                  getNamedType)

WRAPPER_PTR(clang_enum_type_get_decl, EnumType, EnumDecl, getDecl)

WRAPPER_PTR(clang_record_type_get_decl, RecordType, RecordDecl, getDecl)

WRAPPER_PTR(clang_type_of_expr_type_get_underlying_expr, TypeOfExprType, Expr,
            getUnderlyingExpr)

WRAPPER_PTR(clang_typedef_type_get_decl, TypedefType, TypedefNameDecl, getDecl)

WRAPPER_QUAL_TYPE(clang_decayed_type_get_decayed_type, DecayedType,
                  getDecayedType)

WRAPPER_QUAL_TYPE(clang_adjusted_type_get_original_type, AdjustedType,
                  getOriginalType)

WRAPPER_QUAL_TYPE(clang_array_type_get_element_type, ArrayType, getElementType)

WRAPPER_INT64(clang_constant_array_type_get_size_expr, ConstantArrayType,
              getSize)

WRAPPER_PTR(clang_variable_array_type_get_size_expr, VariableArrayType, Expr,
            getSizeExpr)
}
