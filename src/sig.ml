module F = Format

module type SOURCE_LOCATION = sig
  type t = { filename : string; line : int; column : int }
end

module type NODE = sig
  type t

  val pp : F.formatter -> t -> unit
end

module type ATTR = sig
  include NODE

  type kind = AttrKind.t

  val get_kind : t -> kind

  val get_spelling : t -> string
end

module type DECL = sig
  type t

  type kind = DeclKind.t

  type storage_class =
    | NoneSC
    | Extern
    | Static
    | PrivateExtern
    | Auto
    | Register

  module SourceLocation : SOURCE_LOCATION

  module Attr : ATTR

  val get_kind : t -> kind

  val get_kind_name : t -> string

  val get_source_location : t -> SourceLocation.t option

  val get_storage_class : t -> storage_class

  val get_attrs : t -> Attr.t list

  val is_implicit : t -> bool

  val pp : F.formatter -> t -> unit

  val dump : t -> unit
end

module type TYPE = sig
  type t

  type kind = TypeKind.t

  val get_kind : t -> kind

  val get_kind_name : t -> string

  val pp : Format.formatter -> t -> unit
end

module type QUAL_TYPE = sig
  module Type : TYPE

  type t = { ty : Type.t; const : bool }

  val to_string : t -> string

  val pp : Format.formatter -> t -> unit
end

module type NAMED_DECL = sig
  include DECL

  val get_name : t -> string
end

module type VALUE_DECL = sig
  include NAMED_DECL

  module Decl : DECL

  module QualType : QUAL_TYPE

  val get_type : t -> QualType.t
end

module type PARAM_VAR_DECL = VALUE_DECL

module type STMT = sig
  include NODE

  type kind = StmtKind.t

  val get_kind : t -> kind

  val get_kind_name : t -> string
end

module type FUNCTION_DECL = sig
  include VALUE_DECL

  module ParmVarDecl : PARAM_VAR_DECL

  module Stmt : STMT

  val get_return_type : t -> QualType.t

  val get_params : t -> ParmVarDecl.t list

  val has_body : t -> bool

  val get_body : t -> Stmt.t

  val is_inline_specified : t -> bool

  val is_variadic : t -> bool
end

module type VAR_DECL = VALUE_DECL

module type FIELD_DECL = VALUE_DECL

module type LABEL_DECL = NAMED_DECL

module type TYPEDEF_DECL = NAMED_DECL

module type ENUM_DECL = NAMED_DECL

module type RECORD_DECL = sig
  include NAMED_DECL

  module Decl : DECL

  val is_anonymous : t -> bool

  val is_struct : t -> bool

  val is_union : t -> bool

  val field_begin : t -> t option

  val field_list_internal : t -> t list

  val field_list : t -> Decl.t list

  val iter_field : (Decl.t -> unit) -> t -> unit
end

module type TYPEDEC_DECL = NAMED_DECL

module type EXPR = sig
  include STMT

  module QualType : QUAL_TYPE

  val get_type : t -> QualType.t
end

module type COMPOUND_STMT = sig
  include STMT

  module Stmt : STMT

  val body_list : Stmt.t -> Stmt.t list
end

module type DECL_STMT = NODE

module type GOTO_STMT = NODE

module type IMPLICIT_CAST_EXPR = NODE

module type EXPLICIT_CAST_EXPR = NODE

module type IMPLICIT_VALUE_INIT_EXPR = NODE

module type INIT_LIST_EXPR = NODE

module type CHARACTER_LITERAL = sig
  include EXPR

  type kind

  val get_kind : t -> kind

  val get_value : t -> int
end

module type INTEGER_LITERAL = NODE

module type FLOATING_LITERAL = NODE

module type CONSTANT_EXPR = sig
  include EXPR

  val get_sub_expr : t -> t
end

module type STMT_EXPR = sig
  include EXPR

  val get_sub_stmt : t -> t
end

module type STRING_LITERAL = sig
  include EXPR

  val get_string : t -> string
end

module type PREDEFINED_EXPR = sig
  include EXPR

  module StringLiteral : STRING_LITERAL

  type kind

  val get_function_name : t -> StringLiteral.t
end

module type RETURN_STMT = NODE

module type BINARY_OPERATOR = sig
  include STMT

  type kind

  val get_kind : t -> kind

  val has_side_effect : t -> bool
end

module type UNARY_EXPR_OR_TYPE_TRAIT_EXPR = NODE

module type UNARY_OPERATOR = sig
  include STMT

  type kind

  val get_kind : t -> kind

  val has_side_effect : t -> bool
end

module type DECL_REF_EXPR = NODE

module type IF_STMT = sig
  include STMT

  module Stmt : STMT

  val get_cond : t -> Stmt.t

  val get_then : t -> Stmt.t

  val get_else : t -> Stmt.t

  val has_else_storage : t -> bool
end

module type LABEL_STMT = sig
  include STMT

  module Stmt : STMT

  val get_name : t -> string

  val get_sub_stmt : t -> Stmt.t
end

module type WHILE_STMT = NODE

module type MEMBER_EXPR = NODE

module type OPAQUE_VALUE_EXPR = sig
  include EXPR

  module Expr : EXPR

  val get_source_expr : t -> Expr.t
end

module type PAREN_EXPR = sig
  include EXPR

  module Expr : EXPR

  val get_sub_expr : t -> Expr.t
end

module type CALL_EXPR = NODE

module type CASE_STMT = sig
  include STMT

  module Stmt : STMT

  module Expr : EXPR

  val get_lhs : t -> Expr.t

  val get_rhs : t -> Expr.t

  val get_sub_stmt : t -> Stmt.t
end

module type DEFAULT_STMT = sig
  include STMT

  module Stmt : STMT

  val get_sub_stmt : t -> Stmt.t
end

module type SWITCH_STMT = sig
  include STMT

  module Stmt : STMT

  module Expr : EXPR

  val get_cond : t -> Expr.t

  val get_body : t -> Stmt.t
end

module type ATTRIBUTED_STMT = STMT

module type BINARY_CONDITIONAL_OPERATOR = sig
  include EXPR

  val get_cond : t -> t

  val get_true_expr : t -> t

  val get_false_expr : t -> t
end

module type CONDITIONAL_OPERATOR = sig
  include EXPR

  val get_cond : t -> t

  val get_true_expr : t -> t

  val get_false_expr : t -> t
end

module type ARRAY_SUBSCRIPT_EXPR = sig
  include NODE

  val get_base : t -> t

  val get_idx : t -> t
end

module type VA_ARG_EXPR = sig
  include EXPR

  val get_sub_expr : t -> t
end

(* Type *)

module type BUILTIN_TYPE = sig
  include NODE

  type kind = BuiltinTypeKind.t

  val get_kind : t -> kind
end

module type ADJUSTED_TYPE = sig
  include NODE

  module QualType : QUAL_TYPE

  val get_original_type : t -> QualType.t
end

module type DECAYED_TYPE = ADJUSTED_TYPE

module type ARRAY_TYPE = sig
  include NODE

  module QualType : QUAL_TYPE

  val get_element_type : t -> QualType.t
end

module type CONSTANT_ARRAY_TYPE = sig
  include ARRAY_TYPE

  val get_size : t -> Int64.t
end

module type VARIABLE_ARRAY_TYPE = sig
  include ARRAY_TYPE

  module Expr : EXPR

  val get_size_expr : t -> Expr.t
end

module type FUNCTION_TYPE = sig
  include NODE

  module QualType : QUAL_TYPE

  val return_type : t -> QualType.t

  val param_types : t -> QualType.t list
end

module type PAREN_TYPE = sig
  include NODE

  module QualType : QUAL_TYPE

  val desugar : t -> QualType.t
end

module type POINTER_TYPE = NODE

module type ELABORATED_TYPE = NODE

module type ENUM_TYPE = NODE

module type RECORD_TYPE = NODE

module type TYPE_OF_EXPR_TYPE = sig
  include TYPE

  module Expr : EXPR

  val get_underlying_expr : t -> Expr.t
end

module type TYPEDEF_TYPE = NODE
