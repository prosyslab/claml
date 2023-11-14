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
value clang_builtin_type_get_kind(value T) {
  CAMLparam1(T);
  LOG(__FUNCTION__);
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
  LOG(__FUNCTION__);
  clang::QualType *qt = *((clang::QualType **)Data_abstract_val(QT));
  CAMLreturn(clang_to_string(qt->getAsString().c_str()));
}

value clang_atomic_type_get_value_type(value T) {
  CAMLparam1(T);
  LOG(__FUNCTION__);
  clang::AtomicType *AT = *((clang::AtomicType **)Data_abstract_val(T));
  CAMLreturn(clang_to_qual_type(AT->getValueType()));
}

WRAPPER_BOOL(clang_qual_type_is_null, QualType, isNull)

WRAPPER_INT(clang_type_get_kind, Type, getTypeClass)

WRAPPER_STR(clang_type_get_kind_name, Type, getTypeClassName)

WRAPPER_INT(clang_type_get_kind_enum, Type, getTypeClass)

WRAPPER_QUAL_TYPE(clang_function_type_get_return_type, FunctionType,
                  getReturnType)

WRAPPER_BOOL(clang_function_proto_type_is_variadic, FunctionProtoType,
             isVariadic)

value clang_function_proto_type_get_param_types(value T) {
  CAMLparam1(T);
  CAMLlocal4(Hd, Tl, AT, PT);
  LOG(__FUNCTION__);
  clang::FunctionProtoType *FT =
      *((clang::FunctionProtoType **)Data_abstract_val(T));
  Tl = Val_int(0);
  for (unsigned int i = FT->getNumParams(); i > 0; i--) {
    Hd = clang_to_qual_type(FT->getParamType(i - 1));
    value Tmp = caml_alloc(2, 0);
    Store_field(Tmp, 0, Hd);
    Store_field(Tmp, 1, Tl);
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

WRAPPER_PTR(clang_enum_type_get_decl, Type, EnumType, EnumDecl, getDecl)

WRAPPER_PTR(clang_record_type_get_decl, Type, RecordType, RecordDecl, getDecl)

WRAPPER_PTR(clang_type_of_expr_type_get_underlying_expr, Type, TypeOfExprType,
            Expr, getUnderlyingExpr)

WRAPPER_QUAL_TYPE(clang_type_of_type_get_underlying_type, TypeOfType,
                  getUnderlyingType)

WRAPPER_PTR(clang_typedef_type_get_decl, Type, TypedefType, TypedefNameDecl,
            getDecl)

WRAPPER_QUAL_TYPE(clang_decayed_type_get_decayed_type, DecayedType,
                  getDecayedType)

WRAPPER_QUAL_TYPE(clang_adjusted_type_get_original_type, AdjustedType,
                  getOriginalType)

WRAPPER_QUAL_TYPE(clang_array_type_get_element_type, ArrayType, getElementType)

WRAPPER_INT64(clang_constant_array_type_get_size_expr, ConstantArrayType,
              getSize)

WRAPPER_PTR(clang_variable_array_type_get_size_expr, Type, VariableArrayType,
            Expr, getSizeExpr)
}
