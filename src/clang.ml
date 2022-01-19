module F = Format

let pp_semicolon fmt = F.fprintf fmt ";"

let pp_endline fmt = F.fprintf fmt "\n"

module rec Decl : Sig.DECL = struct
  type t

  type kind = DeclKind.t [@@deriving show]

  type source_location = { filename : string; line : int; column : int }

  type storage_class =
    | NoneSC
    | Extern
    | Static
    | PrivateExtern
    | Auto
    | Register

  external get_kind : t -> kind = "clang_decl_get_kind"

  external get_kind_name : t -> string = "clang_decl_get_kind_name"

  external get_kind_enum : t -> int = "clang_decl_get_kind"

  external storage_class : t -> storage_class = "clang_decl_storage_class"

  external dump : t -> unit = "clang_decl_dump"

  external source_location : t -> source_location option
    = "clang_source_location"

  external is_value_decl : t -> bool = "clang_decl_is_value_decl"

  external is_implicit : t -> bool = "clang_decl_is_implicit"

  let pp_storage_class fmt = function
    | NoneSC -> ()
    | Extern -> F.fprintf fmt "extern"
    | Static -> F.fprintf fmt "static"
    | PrivateExtern -> F.fprintf fmt "private extern"
    | Auto -> F.fprintf fmt "auto"
    | Register -> F.fprintf fmt "register"

  let pp_loc fmt decl =
    match source_location decl with
    | Some loc -> F.fprintf fmt "#line %d \"%s\"\n" loc.line loc.filename
    | None -> ()

  let pp fmt decl =
    ( match get_kind decl with
    | TypedefDecl | FunctionDecl | VarDecl | EnumDecl | RecordDecl ->
        pp_loc fmt decl
    | FieldDecl | EnumConstantDecl -> ()
    | _ when is_value_decl decl -> pp_loc fmt decl
    | _ -> () );
    ( match storage_class decl with
    | NoneSC -> ()
    | s -> F.fprintf fmt "%a " pp_storage_class s );
    match get_kind decl with
    | TypedefDecl -> TypedefDecl.pp fmt decl
    | FunctionDecl -> FunctionDecl.pp fmt decl
    | VarDecl -> VarDecl.pp fmt decl
    | EnumDecl ->
        EnumDecl.pp fmt decl;
        pp_semicolon fmt
    | RecordDecl ->
        RecordDecl.pp fmt decl;
        pp_semicolon fmt
    | FieldDecl -> FieldDecl.pp fmt decl
    | EnumConstantDecl -> EnumConstantDecl.pp fmt decl
    | _ when is_value_decl decl ->
        F.fprintf fmt "%a %s (%s, %d)" QualType.pp (ValueDecl.get_type decl)
          (NamedDecl.get_name decl) (get_kind_name decl) (get_kind_enum decl)
    | _ -> pp_kind fmt (get_kind decl)

  (* decl checker *)
  external native_kind_access_spec : unit -> t = "clang_decl_kind_AccessSpec"

  external native_kind_typedef : unit -> kind = "clang_decl_kind_Typedef"
end

and NamedDecl : (Sig.NAMED_DECL with type t = Decl.t) = struct
  include Decl

  external get_name : t -> string = "clang_decl_get_name"

  let pp fmt decl = F.fprintf fmt "%s" (get_name decl)
end

and ValueDecl :
  (Sig.VALUE_DECL
    with type t = Decl.t
     and type QualType.Type.t = QualType.Type.t
     and type QualType.t = QualType.t) = struct
  include NamedDecl
  module Decl = Decl
  module QualType = QualType

  external get_type : t -> QualType.t = "clang_decl_get_type"

  let pp fmt decl =
    F.fprintf fmt "%a %s" QualType.pp (get_type decl) (get_name decl)
end

and EnumConstantDecl : (Sig.NAMED_DECL with type t = Decl.t) = NamedDecl

and ParmVarDecl : (Sig.PARAM_VAR_DECL with type t = Decl.t) = struct
  include ValueDecl
end

and FunctionDecl : (Sig.FUNCTION_DECL with type t = Decl.t) = struct
  include ValueDecl

  external return_type : t -> QualType.t = "clang_function_decl_return_type"

  external get_params : t -> ParmVarDecl.t list
    = "clang_function_decl_get_params"

  external has_body : t -> bool = "clang_function_decl_has_body"

  external get_body : t -> Stmt.t = "clang_function_decl_get_body"

  let pp fmt fdecl =
    F.fprintf fmt "%a %s (" QualType.pp (return_type fdecl) (get_name fdecl);
    List.iter
      (fun param -> F.fprintf fmt "%a, " ParmVarDecl.pp param)
      (get_params fdecl);
    F.fprintf fmt ")";
    if has_body fdecl then F.fprintf fmt " %a" Stmt.pp (get_body fdecl)
    else F.fprintf fmt ";"
end

and VarDecl : (Sig.VAR_DECL with type t = Decl.t) = struct
  include ValueDecl

  external has_init : t -> bool = "clang_vardecl_has_init"

  external get_init : t -> Stmt.t option = "clang_vardecl_get_init"

  let pp fmt vdecl =
    match get_init vdecl with
    | Some e ->
        F.fprintf fmt "%a %s = %a;" QualType.pp (get_type vdecl)
          (get_name vdecl) Stmt.pp e
    | None ->
        F.fprintf fmt "%a %s;" QualType.pp (get_type vdecl) (get_name vdecl)
end

and TypedefDecl : (Sig.TYPEDEF_DECL with type t = Decl.t) = struct
  include NamedDecl

  external get_underlying_type : t -> QualType.t
    = "clang_typedef_decl_get_underlying_type"

  let pp fmt decl =
    if is_implicit decl then F.fprintf fmt "// implicit typedef"
    else
      F.fprintf fmt "typedef %a %s;" QualType.pp (get_underlying_type decl)
        (get_name decl)
end

and EnumDecl : (Sig.ENUM_DECL with type t = Decl.t) = struct
  include NamedDecl

  (* TODO: order *)
  external get_enums : Decl.t -> Decl.t list = "clang_enum_decl_get_enums"

  let rec pp_list fmt = function
    | [ h ] -> F.fprintf fmt "%a" Decl.pp h
    | h :: t ->
        F.fprintf fmt "%a, " Decl.pp h;
        pp_list fmt t
    | [] -> ()

  let pp fmt decl =
    F.fprintf fmt "enum { ";
    get_enums decl |> pp_list fmt;
    F.fprintf fmt " } %s" (get_name decl)
end

and RecordDecl : (Sig.RECORD_DECL with type t = Decl.t) = struct
  include NamedDecl
  module Decl = Decl

  external is_anonymous : t -> bool = "clang_record_decl_is_anonymous"

  external is_struct : t -> bool = "clang_record_decl_is_struct"

  external is_union : t -> bool = "clang_record_decl_is_union"

  external field_begin : t -> t option = "clang_record_decl_field_begin"

  external field_list_internal : t -> t list
    = "clang_record_decl_field_list_internal"

  let field_list decl = field_list_internal decl |> List.rev

  let iter_field f decl = List.iter f (field_list decl)

  let pp fmt decl =
    if is_struct decl then F.fprintf fmt "struct {\n"
    else F.fprintf fmt "union {\n";
    iter_field (fun field -> F.fprintf fmt "%a;\n" Decl.pp field) decl;
    F.fprintf fmt "}";
    if is_anonymous decl then () else F.fprintf fmt " %s" (get_name decl)
end

and FieldDecl : (Sig.FIELD_DECL with type t = Decl.t) = struct
  include ValueDecl
end

and LabelDecl : Sig.LABEL_DECL = struct
  include NamedDecl
end

and Stmt : Sig.STMT = struct
  type t

  type kind = StmtKind.t [@@deriving show]

  external get_kind : t -> kind = "clang_stmt_get_kind"

  external get_kind_enum : t -> int = "clang_stmt_get_kind"

  external get_kind_name : t -> string = "clang_stmt_get_kind_name"

  let pp fmt exp =
    match get_kind exp with
    | BreakStmt -> F.fprintf fmt "break;"
    | CompoundStmt -> CompoundStmt.pp fmt exp
    | DeclStmt -> DeclStmt.pp fmt exp
    | GotoStmt -> GotoStmt.pp fmt exp
    | IfStmt -> IfStmt.pp fmt exp
    | NullStmt -> F.fprintf fmt ";"
    | ReturnStmt -> ReturnStmt.pp fmt exp
    | CaseStmt -> CaseStmt.pp fmt exp
    | DefaultStmt -> DefaultStmt.pp fmt exp
    | SwitchStmt -> SwitchStmt.pp fmt exp
    | BinaryConditionalOperator -> BinaryConditionalOperator.pp fmt exp
    | ConditionalOperator -> ConditionalOperator.pp fmt exp
    | ArraySubscriptExpr -> ArraySubscriptExpr.pp fmt exp
    | BinaryOperator -> BinaryOperator.pp fmt exp
    | CallExpr -> CallExpr.pp fmt exp
    | CStyleCastExpr -> ExplicitCast.pp fmt exp
    | ImplicitCastExpr -> ImplicitCast.pp fmt exp
    | CharacterLiteral -> CharacterLiteral.pp fmt exp
    | DeclRefExpr -> DeclRefExpr.pp fmt exp
    | FloatingLiteral -> FloatingLiteral.pp fmt exp
    | ConstantExpr -> ConstantExpr.pp fmt exp
    | ImplicitValueInitExpr -> ImplicitValueInitExpr.pp fmt exp
    | InitListExpr -> InitListExpr.pp fmt exp
    | IntegerLiteral -> IntegerLiteral.pp fmt exp
    | MemberExpr -> MemberExpr.pp fmt exp
    | OpaqueValueExpr -> OpaqueValueExpr.pp fmt exp
    | ParenExpr -> ParenExpr.pp fmt exp
    | StmtExpr -> StmtExpr.pp fmt exp
    | StringLiteral -> StringLiteral.pp fmt exp
    | UnaryExprOrTypeTraitExpr -> UnaryExprOrTypeTraitExpr.pp fmt exp
    | UnaryOperator -> UnaryOperator.pp fmt exp
    | VAArgExpr -> VAArgExpr.pp fmt exp
    | LabelStmt -> LabelStmt.pp fmt exp
    | WhileStmt -> WhileStmt.pp fmt exp
    | k ->
        F.fprintf fmt "%a (%s, %d)" pp_kind k (get_kind_name exp)
          (get_kind_enum exp)
end

and Expr : (Sig.EXPR with type t = Stmt.t) = struct
  include Stmt
  module QualType = QualType

  external get_type : t -> QualType.t = "clang_expr_get_type"
end

and CompoundStmt : (Sig.COMPOUND_STMT with type t = Stmt.t) = struct
  type t = Stmt.t

  external body_begin : Stmt.t -> Stmt.t option
    = "clang_compound_stmt_body_begin"

  external body_succ : Stmt.t -> Stmt.t option = "clang_compound_stmt_body_succ"

  external body_list : Stmt.t -> Stmt.t list = "clang_compound_stmt_body_list"

  let pp fmt cs =
    F.fprintf fmt "{\n";
    List.iter
      (fun s ->
        F.fprintf fmt "%a" Stmt.pp s;
        pp_endline fmt)
      (body_list cs);
    F.fprintf fmt "}"
end

and DeclStmt : (Sig.DECL_STMT with type t = Stmt.t) = struct
  type t = Stmt.t

  external decl_list : Stmt.t -> Decl.t list = "clang_decl_stmt_decl_list"

  let rec pp_list fmt = function
    | [ h ] -> F.fprintf fmt "%a" Decl.pp h
    | h :: t ->
        F.fprintf fmt "%a\n" Decl.pp h;
        pp_list fmt t
    | [] -> ()

  let pp fmt cs = decl_list cs |> pp_list fmt
end

and GotoStmt : (Sig.GOTO_STMT with type t = Stmt.t) = struct
  type t = Stmt.t

  external get_label : Stmt.t -> LabelDecl.t = "clang_goto_stmt_get_label"

  let pp fmt s = F.fprintf fmt "goto %s;" (get_label s |> LabelDecl.get_name)
end

and ImplicitCast : (Sig.IMPLICIT_CAST with type t = Stmt.t) = struct
  include Expr

  type kind = ImplicitCastKind.t [@@deriving show]

  external sub_expr : Stmt.t -> Stmt.t = "clang_cast_sub_expr"

  external get_kind : Stmt.t -> kind = "clang_cast_kind"

  external get_kind_enum : Stmt.t -> int = "clang_cast_kind"

  let pp fmt e =
    match get_kind e with
    | LValueToRValue | NoOp | ArrayToPointerDecay | FunctionToPointerDecay
    | BuiltinFnToFnPtr ->
        Stmt.pp fmt (sub_expr e)
    | NullToPointer | IntegerToPointer | IntegralCast ->
        F.fprintf fmt "(%a) %a" QualType.pp (get_type e) Stmt.pp (sub_expr e)
    | k ->
        F.fprintf fmt "(%a) %a (%s, %d)" pp_kind k Stmt.pp (sub_expr e)
          (Stmt.get_kind_name e) (get_kind_enum e)
end

and CharacterLiteral : (Sig.CHARACTER_LITERAL with type t = Stmt.t) = struct
  include Expr

  type kind = CharacterKind.t

  external get_kind : t -> kind = "clang_character_literal_get_kind"

  external get_value : t -> int = "clang_character_literal_get_value"

  let pp fmt e = F.fprintf fmt "%d" (get_value e)
end

and StmtExpr : (Sig.STMT_EXPR with type t = Stmt.t) = struct
  include Expr

  external get_sub_stmt : t -> t = "clang_stmt_expr_get_sub_stmt"

  let pp fmt s = F.fprintf fmt "(%a)" Stmt.pp (get_sub_stmt s)
end

and StringLiteral : (Sig.STRING_LITERAL with type t = Stmt.t) = struct
  include Expr

  external get_string : t -> string = "clang_string_literal_get_string"

  let pp fmt e = F.fprintf fmt "\"%s\"" (get_string e |> String.escaped)
end

and ExplicitCast : (Sig.EXPLICIT_CAST with type t = Expr.t) = struct
  include Expr

  external sub_expr : Expr.t -> Expr.t = "clang_cast_sub_expr"

  let pp fmt e =
    F.fprintf fmt "(%a) %a" QualType.pp (get_type e) Expr.pp (sub_expr e)
end

and ImplicitValueInitExpr :
  (Sig.IMPLICIT_VALUE_INIT_EXPR with type t = Stmt.t) = struct
  include Expr

  let pp fmt e = ()
end

and InitListExpr : (Sig.INIT_LIST_EXPR with type t = Stmt.t) = struct
  include Expr

  external get_inits : t -> Expr.t list = "clang_init_list_expr_get_inits"

  let pp fmt e =
    F.fprintf fmt "{";
    List.iter (F.fprintf fmt "%a," Expr.pp) (get_inits e);
    F.fprintf fmt "}"
end

and IntegerLiteral : (Sig.INTEGER_LITERAL with type t = Stmt.t) = struct
  type t = Stmt.t

  external to_int : Stmt.t -> Int64.t = "clang_integer_literal_to_int"

  let pp fmt i = F.fprintf fmt "%s" (to_int i |> Int64.to_string)
end

and FloatingLiteral : (Sig.FLOATING_LITERAL with type t = Stmt.t) = struct
  type t = Stmt.t

  external to_float : Stmt.t -> float = "clang_floating_literal_to_float"

  let pp fmt i = F.fprintf fmt "%f" (to_float i)
end

and ConstantExpr : (Sig.CONSTANT_EXPR with type t = Stmt.t) = struct
  include Expr

  external get_sub_expr : t -> t = "clang_constant_expr_get_sub_expr"

  let pp fmt e = Expr.pp fmt (get_sub_expr e)
end

and ReturnStmt : (Sig.RETURN_STMT with type t = Stmt.t) = struct
  include Stmt

  external get_ret_value : t -> t option = "clang_return_stmt_get_ret_value"

  let pp fmt i =
    match get_ret_value i with
    | None -> F.fprintf fmt "return;"
    | Some e -> F.fprintf fmt "return %a;" Stmt.pp e
end

and CaseStmt : (Sig.CASE_STMT with type t = Stmt.t) = struct
  include Stmt
  module Stmt = Stmt
  module Expr = Expr

  external get_lhs : t -> t = "clang_case_stmt_get_lhs"

  external get_rhs : t -> t = "clang_case_stmt_get_rhs"

  external get_sub_stmt : t -> t = "clang_case_stmt_get_sub_stmt"

  let pp fmt s =
    F.fprintf fmt "case %a:%a" Expr.pp (get_lhs s) Stmt.pp (get_sub_stmt s)
end

and DefaultStmt : (Sig.DEFAULT_STMT with type t = Stmt.t) = struct
  include Stmt
  module Stmt = Stmt

  external get_sub_stmt : t -> t = "clang_default_stmt_get_sub_stmt"

  let pp fmt s = F.fprintf fmt "default:%a" Stmt.pp (get_sub_stmt s)
end

and SwitchStmt : (Sig.SWITCH_STMT with type t = Stmt.t) = struct
  include Stmt
  module Stmt = Stmt
  module Expr = Expr

  external get_cond : t -> Expr.t = "clang_switch_stmt_get_cond"

  external get_body : t -> Expr.t = "clang_switch_stmt_get_body"

  let pp fmt s =
    F.fprintf fmt "switch (%a)\n" Expr.pp (get_cond s);
    F.fprintf fmt "%a" Stmt.pp (get_body s)
end

and BinaryConditionalOperator :
  (Sig.BINARY_CONDITIONAL_OPERATOR with type t = Stmt.t) = struct
  include Expr

  external get_cond : t -> t = "clang_binary_conditional_operator_get_cond"

  external get_true_expr : t -> t
    = "clang_binary_conditional_operator_get_true_expr"

  external get_false_expr : t -> t
    = "clang_binary_conditional_operator_get_false_expr"

  let pp fmt e =
    F.fprintf fmt "%a ? : %a" Expr.pp (get_cond e) Expr.pp (get_false_expr e)
end

and ConditionalOperator : (Sig.CONDITIONAL_OPERATOR with type t = Stmt.t) =
struct
  include Expr

  external get_cond : t -> t = "clang_conditional_operator_get_cond"

  external get_true_expr : t -> t = "clang_conditional_operator_get_true_expr"

  external get_false_expr : t -> t = "clang_conditional_operator_get_false_expr"

  let pp fmt e =
    F.fprintf fmt "%a ? %a : %a" Expr.pp (get_cond e) Expr.pp (get_true_expr e)
      Expr.pp (get_false_expr e)
end

and ArraySubscriptExpr : (Sig.ARRAY_SUBSCRIPT_EXPR with type t = Stmt.t) =
struct
  type t = Stmt.t

  external get_base : t -> Stmt.t = "clang_array_subscript_expr_get_base"

  external get_idx : t -> Stmt.t = "clang_array_subscript_expr_get_idx"

  let pp fmt e = F.fprintf fmt "%a[%a]" Stmt.pp (get_base e) Stmt.pp (get_idx e)
end

and BinaryOperator : (Sig.BINARY_OPERATOR with type t = Stmt.t) = struct
  type t = Stmt.t

  type kind = BinaryOperatorKind.t [@@deriving show]

  external get_kind : t -> kind = "clang_binary_operator_kind"

  external get_kind_enum : t -> int = "clang_binary_operator_kind"

  external get_kind_name : t -> string = "clang_binary_operator_kind_name"

  external get_lhs : t -> t = "clang_binary_operator_get_lhs"

  external get_rhs : t -> t = "clang_binary_operator_get_rhs"

  let pp_kind fmt = function
    | BinaryOperatorKind.Add -> F.fprintf fmt "+"
    | LT -> F.fprintf fmt "<"
    | GT -> F.fprintf fmt ">"
    | LE -> F.fprintf fmt "<="
    | GE -> F.fprintf fmt ">="
    | EQ -> F.fprintf fmt "=="
    | NE -> F.fprintf fmt "!="
    | And -> F.fprintf fmt "&"
    | Xor -> F.fprintf fmt "^"
    | Or -> F.fprintf fmt "|"
    | LAnd -> F.fprintf fmt "&&"
    | LOr -> F.fprintf fmt "||"
    | Assign -> F.fprintf fmt "="
    | k -> pp_kind fmt k

  let pp fmt i =
    F.fprintf fmt "%a %a %a" Stmt.pp (get_lhs i) pp_kind (get_kind i) Stmt.pp
      (get_rhs i);
    match get_kind i with Assign -> pp_semicolon fmt | _ -> ()
end

and CallExpr : (Sig.CALL_EXPR with type t = Stmt.t) = struct
  type t = Stmt.t

  external get_callee : t -> Stmt.t = "clang_call_expr_get_callee"

  external get_args : t -> Stmt.t list = "clang_call_expr_get_args"

  let pp fmt t =
    F.fprintf fmt "%a(" Stmt.pp (get_callee t);
    List.iter (F.fprintf fmt "%a, " Stmt.pp) (get_args t);
    F.fprintf fmt ")"
end

and UnaryExprOrTypeTraitExpr :
  (Sig.UNARY_EXPR_OR_TYPE_TRAIT_EXPR with type t = Stmt.t) = struct
  type t = Stmt.t

  type kind =
    | SizeOf
    | AlignOf
    | PreferredAlignOf
    | VecStep
    | OpenMPRequiredSimdAlign
  [@@deriving show]

  external get_kind : t -> kind = "clang_unary_expr_or_type_trait_expr_get_kind"

  external is_argument_type : t -> bool
    = "clang_unary_expr_or_type_trait_expr_is_argument_type"

  external get_argument_expr : t -> t
    = "clang_unary_expr_or_type_trait_expr_get_argument_expr"

  external get_argument_type : t -> QualType.t
    = "clang_unary_expr_or_type_trait_expr_get_argument_type"

  let pp_kind fmt = function
    | SizeOf -> F.fprintf fmt "sizeof"
    | AlignOf -> F.fprintf fmt "alignof"
    | PreferredAlignOf -> F.fprintf fmt "__alignof__"
    | k -> pp_kind fmt k

  let pp fmt e =
    if is_argument_type e then
      F.fprintf fmt "%a (%a)" pp_kind (get_kind e) QualType.pp
        (get_argument_type e)
    else
      F.fprintf fmt "%a (%a)" pp_kind (get_kind e) Stmt.pp (get_argument_expr e)
end

and UnaryOperator : (Sig.UNARY_OPERATOR with type t = Stmt.t) = struct
  type t = Stmt.t

  type kind = UnaryOperatorKind.t [@@deriving show]

  external get_kind : t -> kind = "clang_unary_operator_kind"

  external get_sub_expr : t -> t = "clang_unary_operator_get_sub_expr"

  let pp fmt i =
    match get_kind i with
    | PostInc -> F.fprintf fmt "%a++;" Stmt.pp (get_sub_expr i)
    | PostDec -> F.fprintf fmt "%a--;" Stmt.pp (get_sub_expr i)
    | PreInc -> F.fprintf fmt "++%a;" Stmt.pp (get_sub_expr i)
    | PreDec -> F.fprintf fmt "--%a;" Stmt.pp (get_sub_expr i)
    | AddrOf -> F.fprintf fmt "&%a" Stmt.pp (get_sub_expr i)
    | Deref -> F.fprintf fmt "*%a" Stmt.pp (get_sub_expr i)
    | Plus -> F.fprintf fmt "+%a" Stmt.pp (get_sub_expr i)
    | Minus -> F.fprintf fmt "-%a" Stmt.pp (get_sub_expr i)
    | Not | LNot -> F.fprintf fmt "!%a" Stmt.pp (get_sub_expr i)
    | k -> pp_kind fmt k
end

and DeclRefExpr : (Sig.DECL_REF_EXPR with type t = Stmt.t) = struct
  type t = Stmt.t

  external get_decl : t -> NamedDecl.t = "clang_decl_ref_get_decl"

  let pp fmt d = F.fprintf fmt "%s" (get_decl d |> NamedDecl.get_name)
end

and IfStmt : (Sig.IF_STMT with type t = Stmt.t) = struct
  type t = Stmt.t

  external get_cond : t -> Stmt.t = "clang_if_stmt_get_cond"

  external get_then : t -> Stmt.t = "clang_if_stmt_get_then"

  external get_else : t -> Stmt.t = "clang_if_stmt_get_else"

  external has_else_storage : t -> bool = "clang_if_stmt_has_else_storage"

  let pp fmt d =
    F.fprintf fmt "if (%a) %a" Stmt.pp (get_cond d) Stmt.pp (get_then d);
    if has_else_storage d then F.fprintf fmt " else %a" Stmt.pp (get_else d)
    else ()
end

and VAArgExpr : (Sig.VA_ARG_EXPR with type t = Stmt.t) = struct
  include Expr

  external get_sub_expr : t -> t = "clang_va_arg_expr_get_sub_expr"

  let pp fmt s =
    F.fprintf fmt "__builtin_va_arg(%a, %a)" Stmt.pp (get_sub_expr s)
      QualType.pp (get_type s)
end

and LabelStmt : (Sig.LABEL_STMT with type t = Stmt.t) = struct
  type t = Stmt.t

  external get_name : t -> string = "clang_label_stmt_get_name"

  external get_sub_stmt : t -> Stmt.t = "clang_label_stmt_get_sub_stmt"

  let pp fmt s = F.fprintf fmt "%s:\n%a" (get_name s) Stmt.pp (get_sub_stmt s)
end

and WhileStmt : (Sig.WHILE_STMT with type t = Stmt.t) = struct
  type t = Stmt.t

  external get_cond : t -> Stmt.t = "clang_while_stmt_get_cond"

  external get_body : t -> Stmt.t = "clang_while_stmt_get_body"

  let pp fmt d =
    F.fprintf fmt "while (%a) %a" Stmt.pp (get_cond d) Stmt.pp (get_body d)
end

and MemberExpr : (Sig.MEMBER_EXPR with type t = Stmt.t) = struct
  type t = Stmt.t

  external get_base : t -> Stmt.t = "clang_member_expr_get_base"

  external get_member_decl : t -> NamedDecl.t
    = "clang_member_expr_get_member_decl"

  external is_arrow : t -> bool = "clang_member_expr_is_arrow"

  let pp fmt e =
    F.fprintf fmt "%a%s%s" Stmt.pp (get_base e)
      (if is_arrow e then "->" else ".")
      (get_member_decl e |> NamedDecl.get_name)
end

and OpaqueValueExpr : (Sig.OPAQUE_VALUE_EXPR with type t = Stmt.t) = struct
  include Expr
  module Expr = Expr

  external get_source_expr : t -> Expr.t
    = "clang_opaque_value_expr_get_source_expr"

  let pp fmt e = F.fprintf fmt "%a" Expr.pp (get_source_expr e)
end

and ParenExpr : (Sig.PAREN_EXPR with type t = Stmt.t) = struct
  include Expr
  module Expr = Expr

  external get_sub_expr : t -> Expr.t = "clang_paren_expr_get_sub_expr"

  let pp fmt e = F.fprintf fmt "%a" Expr.pp (get_sub_expr e)
end

and Type : Sig.TYPE = struct
  type t

  type kind = TypeKind.t [@@deriving show]

  external kind : t -> kind = "clang_type_kind"

  external get_kind_name : t -> string = "clang_type_get_kind_name"

  external get_kind_enum : t -> int = "clang_type_get_kind_enum"

  let pp fmt t =
    match kind t with
    | AdjustedType -> F.fprintf fmt "adjusted"
    | DecayedType -> F.fprintf fmt "%a" DecayedType.pp t
    | ConstantArrayType -> F.fprintf fmt "%a" ConstantArrayType.pp t
    | VariableArrayType -> F.fprintf fmt "%a" VariableArrayType.pp t
    | BuiltinType -> F.fprintf fmt "%a" BuiltinType.pp t
    | FunctionNoProtoType ->
        F.fprintf fmt "%a" QualType.pp (FunctionType.return_type t)
    | FunctionProtoType ->
        F.fprintf fmt "%a (" QualType.pp (FunctionType.return_type t);
        List.iter
          (fun at -> F.fprintf fmt "%a, " QualType.pp at)
          (FunctionType.param_types t);
        F.fprintf fmt ")"
    | PointerType -> F.fprintf fmt "%a" PointerType.pp t
    | ElaboratedType -> F.fprintf fmt "%a" ElaboratedType.pp t
    | EnumType -> F.fprintf fmt "%a" EnumType.pp t
    | RecordType -> F.fprintf fmt "%a" RecordType.pp t
    | TypeOfExprType -> F.fprintf fmt "%a" TypeOfExprType.pp t
    | TypedefType -> F.fprintf fmt "%a" TypedefType.pp t
    | k ->
        F.fprintf fmt "%a (%s, %d)" pp_kind k (get_kind_name t)
          (get_kind_enum t)
end

and AdjustedType : (Sig.ADJUSTED_TYPE with type t = Type.t) = struct
  type t = Type.t

  module QualType = QualType

  external get_original_type : t -> QualType.t
    = "clang_adjusted_type_get_original_type"

  let pp fmt t = QualType.pp fmt (get_original_type t)
end

and DecayedType : (Sig.DECAYED_TYPE with type t = Type.t) = struct
  include AdjustedType

  type t = Type.t

  external get_decayed_type : t -> QualType.t
    = "clang_decayed_type_get_decayed_type"

  let pp fmt t = QualType.pp fmt (get_original_type t)
end

and ArrayType : (Sig.ARRAY_TYPE with type t = Type.t) = struct
  type t = Type.t

  module QualType = QualType

  external get_element_type : t -> QualType.t
    = "clang_array_type_get_element_type"

  let pp fmt t = ()
end

and ConstantArrayType : (Sig.CONSTANT_ARRAY_TYPE with type t = Type.t) = struct
  include ArrayType

  external get_size : t -> Int64.t = "clang_constant_array_type_get_size_expr"

  let pp fmt t =
    F.fprintf fmt "%a [%d]" QualType.pp (get_element_type t)
      (get_size t |> Int64.to_int)
end

and VariableArrayType : (Sig.VARIABLE_ARRAY_TYPE with type t = Type.t) = struct
  include ArrayType
  module Expr = Expr

  external get_size_expr : t -> Stmt.t
    = "clang_variable_array_type_get_size_expr"

  let pp fmt t =
    F.fprintf fmt "%a [%a]" QualType.pp (get_element_type t) Stmt.pp
      (get_size_expr t)
end

and BuiltinType : (Sig.BUILTIN_TYPE with type t = Type.t) = struct
  type t = Type.t

  type kind = BuiltinTypeKind.t [@@deriving show]

  external kind : t -> kind = "clang_builtin_type_kind"

  let pp fmt t =
    match kind t with
    | Void -> F.fprintf fmt "void"
    | Bool -> F.fprintf fmt "bool"
    | Char_U -> F.fprintf fmt "unsigned char"
    | UChar -> F.fprintf fmt "unsigned char"
    | WChar_U -> F.fprintf fmt "wchar"
    | Char8 -> F.fprintf fmt "char8"
    | Char16 -> F.fprintf fmt "char16"
    | Char32 -> F.fprintf fmt "char32"
    | UShort -> F.fprintf fmt "unsigned short"
    | UInt -> F.fprintf fmt "unsigned int"
    | ULong -> F.fprintf fmt "unsigned long"
    | ULongLong -> F.fprintf fmt "unsigned long long"
    | UInt128 -> F.fprintf fmt "unsigned uint128"
    | Char_S -> F.fprintf fmt "char"
    | SChar -> F.fprintf fmt "signed char"
    | WChar_S -> F.fprintf fmt "signed wchar"
    | Short -> F.fprintf fmt "short"
    | Int -> F.fprintf fmt "int"
    | k -> pp_kind fmt k
end

and FunctionType :
  (Sig.FUNCTION_TYPE
    with type t = Type.t
     and type QualType.Type.t = QualType.Type.t
     and type QualType.t = QualType.t) = struct
  type t = Type.t

  module QualType = QualType

  external return_type : t -> QualType.t = "clang_function_type_get_return_type"

  external param_types : t -> QualType.t list
    = "clang_function_type_get_param_types"

  let pp fmt t = ()
end

and PointerType : (Sig.POINTER_TYPE with type t = Type.t) = struct
  type t = Type.t

  external get_pointee_type : t -> QualType.t
    = "clang_pointer_type_get_pointee_type"

  let pp fmt t = F.fprintf fmt "%a *" QualType.pp (get_pointee_type t)
end

and ElaboratedType : (Sig.ELABORATED_TYPE with type t = Type.t) = struct
  type t = Type.t

  external desugar : t -> QualType.t = "clang_elaborated_type_desugar"

  external get_named_type : t -> QualType.t
    = "clang_elaborated_type_get_named_type"

  let pp fmt t = F.fprintf fmt "%a" QualType.pp (desugar t)
end

and EnumType : (Sig.ENUM_TYPE with type t = Type.t) = struct
  include Type

  external get_decl : t -> EnumDecl.t = "clang_enum_type_get_decl"

  let pp fmt t = F.fprintf fmt "%a" EnumDecl.pp (get_decl t)
end

and RecordType : (Sig.RECORD_TYPE with type t = Type.t) = struct
  type t = Type.t

  external get_decl : t -> RecordDecl.t = "clang_record_type_get_decl"

  let pp fmt t =
    let decl = get_decl t in
    let name = NamedDecl.get_name decl in
    if RecordDecl.is_anonymous decl || name = "" then
      F.fprintf fmt "%a" RecordDecl.pp decl
    else F.fprintf fmt "%s" (NamedDecl.get_name decl)
end

and TypeOfExprType : (Sig.TYPE_OF_EXPR_TYPE with type t = Type.t) = struct
  include Type
  module Expr = Expr

  external get_underlying_expr : t -> Expr.t
    = "clang_type_of_expr_type_get_underlying_expr"

  let pp fmt t = F.fprintf fmt "__typeof__(%a)" Expr.pp (get_underlying_expr t)
end

and TypedefType : (Sig.TYPEDEF_TYPE with type t = Type.t) = struct
  type t = Type.t

  external get_decl : t -> TypedefDecl.t = "clang_typedef_type_get_decl"

  let pp fmt t = F.fprintf fmt "%s" (get_decl t |> TypedefDecl.get_name)
end

and QualType : (Sig.QUAL_TYPE with type Type.t = Type.t) = struct
  module Type = Type

  type t = { ty : Type.t; const : bool }

  external to_string : t -> string = "clang_qual_type_to_string"

  let pp fmt { ty; const } =
    if const then F.fprintf fmt "const ";
    F.fprintf fmt "%a" Type.pp ty
end

module TranslationUnit = struct
  type t

  external parse_file : string array -> t = "clang_parse_file"

  external dump_translation_unit : t -> unit = "clang_dump_translation_unit"

  external decls_begin : t -> Decl.t option = "clang_decls_begin"

  external decls_succ : Decl.t -> Decl.t option = "clang_decls_succ"

  let rec fold_left_decls_range f init i =
    match i with
    | None -> init
    | Some decl -> fold_left_decls_range f (f init decl) (decls_succ decl)

  let fold_left_decls f init tu = fold_left_decls_range f init (decls_begin tu)

  let rec iter_decls_range f i =
    match i with
    | None -> ()
    | Some decl ->
        f decl;
        iter_decls_range f (decls_succ decl)

  let iter_decls f tu = iter_decls_range f (decls_begin tu)

  let pp fmt tu = iter_decls (fun decl -> F.fprintf fmt "%a\n" Decl.pp decl) tu
end
