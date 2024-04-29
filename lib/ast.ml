(** Abstract Syntax Tree emitted by the parser. *)

type identifier = string [@@deriving show]

module DataType = struct
  type simple_kind = Identifier | Location [@@deriving show]

  type t = { pos : Position.t; kind : kind }

  and kind =
    | Simple of simple_kind * identifier
    | Function of { inputs : t list; outputs : t list }
  [@@deriving show]
end

type binding_info = {
  name : identifier;
  parameters : identifier list;
  data_type : DataType.t;
}
[@@deriving show]
(** All information associated with a let binding except the bound expression
    itself (and the expression following 'in' in a let-in expression). *)

module Expr = struct
  type binary_operator =
    | And
    | Or
    | Equiv
    | NotEquiv
    | GreaterThan
    | LessThan
    | GreaterThanOrEqual
    | LessThanOrEqual
    | Add
    | Subtract
    | Multiply
    | Divide
  [@@deriving show]

  type unary_operator = Not | Negate [@@deriving show]

  type primary_kind =
    | NumberLiteral
    | StringLiteral
    | Identifier
    | Location
    | Constructor
  [@@deriving show]

  type t = { pos : Position.t; kind : kind } [@@deriving show]

  and kind =
    (* "let" IDENTIFIER { IDENTIFIER } ":" type "=" expr "in" expr *)
    | LetIn of { info : binding_info; bound_expr : t; in_expr : t }
    (* "match" expr "with" [ "|" ] match_arm { "|" match_arm } *)
    | Match of t * match_arm list
    (* "if" expr "then" expr "else" expr *)
    | IfThenElse of { condition_expr : t; then_expr : t; else_expr : t }
    (* expr binary_operator expr *)
    | BinOp of binary_operator * t * t
    (* unary_operator expr *)
    | UnaryOp of unary_operator * t
    (* expr expr { expr } *)
    | Application of t * t list
    (* "(" expr ")" *)
    | Grouping of t
    (* expr ";" expr *)
    | Chain of t * t
    (* NUMBER_LITERAL | STRING_LITERAL | IDENTIFIER | LOCATION_IDENTIFIER *)
    | Primary of primary_kind * string
  [@@deriving show]

  (* CAPITALISED_IDENTIFIER { IDENTIFIER } "->" expr *)
  and match_arm = {
    arm_pos : Position.t;
    constructor : identifier;
    parameters : identifier list;
    expr : t;
  }
  [@@deriving show]
end

(* CAPITALISED_IDENTFIER { type } *)
type data_arm = {
  arm_pos : Position.t;
  constructor : identifier;
  data_types : DataType.t list;
}
[@@deriving show]

type kind =
  (* "let" IDENTIFIER { IDENTIFIER } ":" type "=" expr *)
  | Let of binding_info * Expr.t
  (* "data" IDENTIFIER "=" [ "|" ] data_arm { "|" data_arm } *)
  | Data of identifier * data_arm list
[@@deriving show]

type t = { pos : Position.t; kind : kind } [@@deriving show]
(** A top-level definition. A program is comprised of one or more top-level
    definitions meaning a program's full AST is represented by a list of t. *)
