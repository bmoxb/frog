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
  recursive : bool;
}
[@@deriving show]
(** All information associated with a let binding except the bound expression
    itself (and the expression following 'in' in a let-in expression). *)

module Expr = struct
  type binary_operator =
    | Chain (* ; *)
    | MultiReturn (* , *)
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
    | LetIn of { info : binding_info; bound_expr : t; in_expr : t }
    | IfThenElse of { condition_expr : t; then_expr : t; else_expr : t }
    | Match of t * match_arm list
    | BinOp of binary_operator * t * t
    | UnaryOp of unary_operator * t
    | Application of t * t list
    | Primary of primary_kind * string
    | Grouping of t
  [@@deriving show]

  and match_arm = {
    arm_pos : Position.t;
    constructor : identifier;
    parameters : identifier list;
    expr : t;
  }
  [@@deriving show]
end

type data_arm = {
  arm_pos : Position.t;
  constructor : identifier;
  data_types : DataType.t list;
}
[@@deriving show]

type kind = Let of binding_info * Expr.t | Data of identifier * data_arm list
[@@deriving show]

type t = { pos : Position.t; kind : kind } [@@deriving show]
(** A top-level definition. A program is comprised of one or more top-level
    definitions meaning a program's full AST is represented by a list of t. *)
