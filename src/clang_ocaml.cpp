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
value clang_parse_file(value args) {
  CAMLparam1(args);
  CAMLlocal1(v);
  int num_args = Wosize_val(args);
  int i;
  char const **clang_args =
      (char const **)malloc(num_args * sizeof(const char *const));
  for (i = 0; i < num_args; i++) {
    clang_args[i] = String_val(Field(args, i));
  }
  clang::IntrusiveRefCntPtr<clang::DiagnosticsEngine> Diags =
      clang::CompilerInstance::createDiagnostics(
          new clang::DiagnosticOptions());
  clang::FileSystemOptions FileSystemOpts;
  llvm::SmallVector<const char *, 4> Args;
  Args.push_back("clang");
  Args.push_back(clang_args[0]);
  std::shared_ptr<clang::PCHContainerOperations> PCHContainerOps =
      std::make_shared<clang::PCHContainerOperations>();
  clang::ASTUnit *Unit(clang::ASTUnit::LoadFromCommandLine(
      Args.data(), Args.data() + Args.size(), PCHContainerOps, Diags, ""));
  clang::TranslationUnitDecl *TU =
      Unit->getASTContext().getTranslationUnitDecl();
  v = caml_alloc(0, Abstract_tag);
  *((clang::TranslationUnitDecl **)Data_abstract_val(v)) = TU;
  CAMLreturn(v);
}

void clang_dump_translation_unit(value TU) {
  CAMLparam1(TU);
  clang::TranslationUnitDecl *TTU =
      *((clang::TranslationUnitDecl **)Data_abstract_val(TU));
  TTU->dump();
  CAMLreturn0;
}

value clang_decls_begin(value TU) {
  CAMLparam1(TU);
  CAMLlocal1(v);
  clang::TranslationUnitDecl *TTU =
      *((clang::TranslationUnitDecl **)Data_abstract_val(TU));
  v = caml_alloc(1, Abstract_tag);
  clang::Decl *D = *(TTU->decls_begin());
  if (D) {
    *((clang::Decl **)Data_abstract_val(v)) = D;
    CAMLreturn(caml_alloc_some(v));
  } else {
    CAMLreturn(Val_none);
  }
}

value clang_decls_succ(value Decl) {
  CAMLparam1(Decl);
  CAMLlocal1(v);
  clang::Decl *TTU = *((clang::Decl **)Data_abstract_val(Decl));
  v = caml_alloc(1, Abstract_tag);
  clang::Decl *Next = TTU->getNextDeclInContext();
  if (Next) {
    *((clang::Decl **)Data_abstract_val(v)) = Next;
    CAMLreturn(caml_alloc_some(v));
  } else {
    CAMLreturn(Val_none);
  }
}

////////////////////////////////////////////////////////////////////////////////
// Begin Decl
////////////////////////////////////////////////////////////////////////////////

WRAPPER_INT(clang_decl_get_kind, Decl, getKind)
WRAPPER_STR(clang_decl_get_kind_name, Decl, getDeclKindName)
WRAPPER_VOID(clang_decl_dump, Decl, dump)
WRAPPER_BOOL(clang_decl_is_implicit, Decl, isImplicit)
value clang_decl_is_value_decl(value Decl) {
  CAMLparam1(Decl);
  clang::Decl *D = *((clang::Decl **)Data_abstract_val(Decl));
  if (clang::ValueDecl *VD = llvm::dyn_cast<clang::ValueDecl>(D)) {
    CAMLreturn(Val_int(1));
  } else {
    CAMLreturn(Val_int(0));
  }
}

WRAPPER_LIST_WITH_IDX(clang_function_decl_get_params, FunctionDecl, ParmVarDecl,
                      getNumParams, getParamDecl)
WRAPPER_QUAL_TYPE(clang_function_decl_return_type, FunctionDecl, getReturnType)
WRAPPER_BOOL(clang_function_decl_has_body, FunctionDecl, hasBody)
WRAPPER_BOOL(clang_function_decl_is_inline_specified, FunctionDecl,
             isInlineSpecified)
WRAPPER_PTR(clang_function_decl_get_body, FunctionDecl, Stmt, getBody)

WRAPPER_BOOL(clang_record_decl_is_anonymous, RecordDecl,
             isAnonymousStructOrUnion)
WRAPPER_BOOL(clang_record_decl_is_struct, RecordDecl, isStruct)
WRAPPER_BOOL(clang_record_decl_is_union, RecordDecl, isUnion)

value clang_record_decl_field_begin(value Decl) {
  CAMLparam1(Decl);
  CAMLlocal1(R);
  clang::RecordDecl *RD = *((clang::RecordDecl **)Data_abstract_val(Decl));
  if (RD->field_empty())
    CAMLreturn(Val_none);
  clang::FieldDecl *FD = *(RD->field_begin());
  if (FD) {
    R = caml_alloc(1, Abstract_tag);
    *((clang::FieldDecl **)Data_abstract_val(R)) = FD;
    CAMLreturn(caml_alloc_some(R));
  } else {
    CAMLreturn(Val_none);
  }
}

value clang_record_decl_field_list_internal(value Decl) {
  CAMLparam1(Decl);
  CAMLlocal3(Hd, Tl, R);
  clang::RecordDecl *RD = *((clang::RecordDecl **)Data_abstract_val(Decl));
  R = caml_alloc(1, Abstract_tag);
  Tl = Val_int(0);
  for (auto i = RD->field_begin(); i != RD->field_end(); i++) {
    Hd = caml_alloc(1, Abstract_tag);
    *((clang::FieldDecl **)Data_abstract_val(Hd)) = *i;
    value Tmp = caml_alloc(2, Abstract_tag);
    Field(Tmp, 0) = Hd;
    Field(Tmp, 1) = Tl;
    Tl = Tmp;
  }
  CAMLreturn(Tl);
}

value clang_decl_get_name(value Decl) {
  CAMLparam1(Decl);
  clang::Decl *D = *((clang::Decl **)Data_abstract_val(Decl));
  if (clang::NamedDecl *VD = llvm::dyn_cast<clang::NamedDecl>(D)) {
    CAMLreturn(clang_to_string(VD->getName().data()));
  } else {
    assert(false);
  }
}

value clang_decl_get_type(value Decl) {
  CAMLparam1(Decl);
  clang::Decl *D = *((clang::Decl **)Data_abstract_val(Decl));
  if (clang::ValueDecl *VD = llvm::dyn_cast<clang::ValueDecl>(D)) {
    CAMLreturn(clang_to_qual_type(VD->getType()));
  } else {
    assert(false);
  }
}

value clang_source_location(value decl) {
  CAMLparam1(decl);
  CAMLlocal1(result);
  clang::Decl *d = *((clang::Decl **)Data_abstract_val(decl));
  const clang::PresumedLoc &loc =
      d->getASTContext().getSourceManager().getPresumedLoc(d->getLocation());
  if (loc.isValid()) {
    result = caml_alloc(3, 0);
    Store_field(result, 0, clang_to_string(loc.getFilename()));
    Store_field(result, 1, Val_int(loc.getLine()));
    Store_field(result, 2, Val_int(loc.getColumn()));
    CAMLreturn(caml_alloc_some(result));
  } else {
    CAMLreturn(Val_none);
  }
}

value clang_decl_storage_class(value Decl) {
  CAMLparam1(Decl);
  clang::Decl *D = *((clang::Decl **)Data_abstract_val(Decl));
  if (clang::FunctionDecl *FD = llvm::dyn_cast<clang::FunctionDecl>(D)) {
    CAMLreturn(Val_int(FD->getStorageClass()));
  } else if (clang::VarDecl *VD = llvm::dyn_cast<clang::VarDecl>(D)) {
    CAMLreturn(Val_int(VD->getStorageClass()));
  } else {
    CAMLreturn(Val_int(clang::SC_None));
  }
}

WRAPPER_BOOL(clang_vardecl_has_init, VarDecl, hasInit)

value clang_vardecl_get_init(value VarDecl) {
  CAMLparam1(VarDecl);
  CAMLlocal1(R);
  clang::VarDecl *VD = *((clang::VarDecl **)Data_abstract_val(VarDecl));
  R = caml_alloc(1, Abstract_tag);
  if (VD->hasInit()) {
    *((clang::Expr **)Data_abstract_val(R)) = VD->getInit();
    CAMLreturn(caml_alloc_some(R));
  } else {
    CAMLreturn(Val_none);
  }
}

WRAPPER_PTR(clang_goto_stmt_get_label, GotoStmt, LabelDecl, getLabel)

WRAPPER_PTR(clang_if_stmt_get_cond, IfStmt, Expr, getCond)
WRAPPER_PTR(clang_if_stmt_get_then, IfStmt, Stmt, getThen)
WRAPPER_PTR(clang_if_stmt_get_else, IfStmt, Stmt, getElse)
WRAPPER_BOOL(clang_if_stmt_has_else_storage, IfStmt, hasElseStorage)

WRAPPER_STR(clang_label_stmt_get_name, LabelStmt, getName)
WRAPPER_PTR(clang_label_stmt_get_sub_stmt, LabelStmt, Stmt, getSubStmt)

WRAPPER_PTR(clang_while_stmt_get_cond, WhileStmt, Expr, getCond)
WRAPPER_PTR(clang_while_stmt_get_body, WhileStmt, Stmt, getBody)

WRAPPER_QUAL_TYPE(clang_typedef_decl_get_underlying_type, TypedefDecl,
                  getUnderlyingType)

value clang_enum_decl_get_enums(value T) {
  CAMLparam1(T);
  CAMLlocal4(Hd, Tl, AT, PT);
  clang::EnumDecl *D = *((clang::EnumDecl **)Data_abstract_val(T));
  Tl = Val_int(0);
  for (auto i = D->enumerator_begin(); i != D->enumerator_end(); i++) {
    Hd = caml_alloc(1, Abstract_tag);
    *((const clang::EnumConstantDecl **)Data_abstract_val(Hd)) = *i;
    value Tmp = caml_alloc(2, Abstract_tag);
    Field(Tmp, 0) = Hd;
    Field(Tmp, 1) = Tl;
    Tl = Tmp;
  }
  CAMLreturn(Tl);
}

////////////////////////////////////////////////////////////////////////////////
// Expr
////////////////////////////////////////////////////////////////////////////////

WRAPPER_INT(clang_stmt_get_kind, Stmt, getStmtClass)
WRAPPER_STR(clang_stmt_get_kind_name, Stmt, getStmtClassName)

value clang_integer_literal_to_int(value Expr) {
  CAMLparam1(Expr);
  clang::IntegerLiteral *E =
      *((clang::IntegerLiteral **)Data_abstract_val(Expr));
  llvm::APInt V = E->getValue();
  unsigned int Bit = V.getBitWidth();
  if (V.isSignedIntN(Bit)) {
    CAMLreturn(caml_copy_int64(V.getSExtValue()));
  } else {
    CAMLreturn(caml_copy_int64(V.getZExtValue()));
  }
}

WRAPPER_INT(clang_character_literal_get_kind, CharacterLiteral, getKind)

WRAPPER_STRREF(clang_string_literal_get_string, StringLiteral, getString)

WRAPPER_INT(clang_character_literal_get_value, CharacterLiteral, getValue)

value clang_floating_literal_to_float(value Expr) {
  CAMLparam1(Expr);
  clang::FloatingLiteral *E =
      *((clang::FloatingLiteral **)Data_abstract_val(Expr));
  llvm::APFloat V = E->getValue();
  CAMLreturn(caml_copy_double(V.convertToDouble()));
}

WRAPPER_PTR(clang_constant_expr_get_sub_expr, ConstantExpr, Expr, getSubExpr)

WRAPPER_PTR(clang_stmt_expr_get_sub_stmt, StmtExpr, CompoundStmt, getSubStmt)

WRAPPER_INT(clang_get_cast_kind, CastExpr, getCastKind)

WRAPPER_STR(clang_cast_get_kind_name, CastExpr, getCastKindName)

WRAPPER_PTR(clang_get_cast_sub_expr, CastExpr, Expr, getSubExpr)

WRAPPER_QUAL_TYPE(clang_expr_get_type, Expr, getType)

WRAPPER_PTR(clang_predefined_expr_get_function_name, PredefinedExpr,
            StringLiteral, getFunctionName)

WRAPPER_INT(clang_predefined_expr_get_ident_kind, PredefinedExpr, getIdentKind)

WRAPPER_LIST_WITH_REV_ITER(clang_compound_stmt_body_list, CompoundStmt, Stmt,
                           body_rbegin, body_rend)

WRAPPER_LIST_WITH_REV_ITER(clang_decl_stmt_decl_list, DeclStmt, Decl,
                           decl_rbegin, decl_rend)

value clang_return_stmt_get_ret_value(value Stmt) {
  CAMLparam1(Stmt);
  CAMLlocal1(R);
  clang::ReturnStmt *S = *((clang::ReturnStmt **)Data_abstract_val(Stmt));
  R = caml_alloc(1, Abstract_tag);
  clang::Expr *D = S->getRetValue();
  if (D) {
    *((clang::Stmt **)Data_abstract_val(R)) = D;
    CAMLreturn(caml_alloc_some(R));
  } else {
    CAMLreturn(Val_none);
  }
}

WRAPPER_INT(clang_binary_operator_kind, BinaryOperator, getOpcode)

WRAPPER_STRREF(clang_binary_operator_kind_name, BinaryOperator, getOpcodeStr)

WRAPPER_PTR(clang_binary_operator_get_lhs, BinaryOperator, Expr, getLHS)

WRAPPER_PTR(clang_binary_operator_get_rhs, BinaryOperator, Expr, getRHS)

WRAPPER_INT(clang_unary_operator_kind, UnaryOperator, getOpcode)

WRAPPER_PTR(clang_unary_operator_get_sub_expr, UnaryOperator, Expr, getSubExpr)

WRAPPER_PTR(clang_decl_ref_get_decl, DeclRefExpr, ValueDecl, getDecl)

WRAPPER_INT(clang_unary_expr_or_type_trait_expr_get_kind,
            UnaryExprOrTypeTraitExpr, getKind)

WRAPPER_BOOL(clang_unary_expr_or_type_trait_expr_is_argument_type,
             UnaryExprOrTypeTraitExpr, isArgumentType)

WRAPPER_PTR(clang_unary_expr_or_type_trait_expr_get_argument_expr,
            UnaryExprOrTypeTraitExpr, Expr, getArgumentExpr)

WRAPPER_QUAL_TYPE(clang_unary_expr_or_type_trait_expr_get_argument_type,
                  UnaryExprOrTypeTraitExpr, getArgumentType)

WRAPPER_PTR(clang_member_expr_get_base, MemberExpr, Expr, getBase)

WRAPPER_PTR(clang_member_expr_get_member_decl, MemberExpr, ValueDecl,
            getMemberDecl)

WRAPPER_BOOL(clang_member_expr_is_arrow, MemberExpr, isArrow)

WRAPPER_PTR(clang_opaque_value_expr_get_source_expr, OpaqueValueExpr, Expr,
            getSourceExpr)

WRAPPER_PTR(clang_paren_expr_get_sub_expr, ParenExpr, Expr, getSubExpr)

WRAPPER_PTR(clang_call_expr_get_callee, CallExpr, Expr, getCallee)

WRAPPER_LIST_WITH_IDX(clang_call_expr_get_args, CallExpr, Expr, getNumArgs,
                      getArg)

WRAPPER_PTR(clang_case_stmt_get_lhs, CaseStmt, Expr, getLHS)

WRAPPER_PTR(clang_case_stmt_get_rhs, CaseStmt, Expr, getRHS)

WRAPPER_PTR(clang_case_stmt_get_sub_stmt, CaseStmt, Stmt, getSubStmt)

WRAPPER_PTR(clang_default_stmt_get_sub_stmt, DefaultStmt, Stmt, getSubStmt)

WRAPPER_PTR(clang_switch_stmt_get_cond, SwitchStmt, Expr, getCond)

WRAPPER_PTR(clang_switch_stmt_get_body, SwitchStmt, Stmt, getBody)

value clang_attributed_stmt_get_attrs(value Param) {
  CAMLparam1(Param);
  CAMLlocal4(Hd, Tl, AT, PT);
  clang::AttributedStmt *P =
      *((clang::AttributedStmt **)Data_abstract_val(Param));
  Tl = Val_int(0);
  clang::ArrayRef<const clang::Attr *> Attrs = P->getAttrs();
  for (auto i = Attrs.rbegin(); i != Attrs.rend(); i++) {
    Hd = caml_alloc(1, Abstract_tag);
    *((const clang::Attr **)Data_abstract_val(Hd)) = *i;
    value Tmp = caml_alloc(2, Abstract_tag);
    Field(Tmp, 0) = Hd;
    Field(Tmp, 1) = Tl;
    Tl = Tmp;
  }
  CAMLreturn(Tl);
}

WRAPPER_PTR(clang_attributed_stmt_get_sub_stmt, AttributedStmt, Stmt,
            getSubStmt)

WRAPPER_PTR(clang_binary_conditional_operator_get_cond,
            BinaryConditionalOperator, Expr, getCond)

WRAPPER_PTR(clang_binary_conditional_operator_get_true_expr,
            BinaryConditionalOperator, Expr, getTrueExpr)

WRAPPER_PTR(clang_binary_conditional_operator_get_false_expr,
            BinaryConditionalOperator, Expr, getFalseExpr)

WRAPPER_PTR(clang_conditional_operator_get_cond, ConditionalOperator, Expr,
            getCond)

WRAPPER_PTR(clang_conditional_operator_get_true_expr, ConditionalOperator, Expr,
            getTrueExpr)

WRAPPER_PTR(clang_conditional_operator_get_false_expr, ConditionalOperator,
            Expr, getFalseExpr)

WRAPPER_PTR(clang_array_subscript_expr_get_base, ArraySubscriptExpr, Expr,
            getBase)

WRAPPER_PTR(clang_array_subscript_expr_get_idx, ArraySubscriptExpr, Expr,
            getIdx)

WRAPPER_PTR(clang_va_arg_expr_get_sub_expr, VAArgExpr, Expr, getSubExpr)

WRAPPER_LIST_WITH_IDX(clang_init_list_expr_get_inits, InitListExpr, Expr,
                      getNumInits, getInit)

WRAPPER_INT(clang_attr_get_kind, Attr, getKind)

WRAPPER_STR(clang_attr_get_spelling, Attr, getSpelling)
}
