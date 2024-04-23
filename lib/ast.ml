(** Abstract Syntax Tree emitted by the parser. *)

type identifier = string [@@deriving show]

module DataType = struct
  type simple_kind = Identifier | Location [@@deriving show]

  let token_kind_to_simple_kind = function
    | Token.Identifier -> Some Identifier
    | Token.LocationIdentifier -> Some Location
    | _ -> None

  type t = { pos : Position.t; kind : kind }

  and kind = Simple of simple_kind * identifier | Function of t list * t list
  [@@deriving show]
end

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

  let token_kind_to_binary_operator = function
    | Token.AndKeyword -> Some And
    | Token.OrKeyword -> Some Or
    | Token.Equiv -> Some Equiv
    | Token.NotEquiv -> Some NotEquiv
    | Token.GreaterThan -> Some GreaterThan
    | Token.LessThan -> Some LessThan
    | Token.GreaterThanOrEqual -> Some GreaterThanOrEqual
    | Token.LessThanOrEqual -> Some LessThanOrEqual
    | Token.Plus -> Some Add
    | Token.Minus -> Some Subtract
    | Token.Star -> Some Multiply
    | Token.Slash -> Some Divide
    | _ -> None

  type unary_operator = Not | Negate [@@deriving show]

  let token_kind_to_unary_operator = function
    | Token.NotKeyword -> Some Not
    | Token.Minus -> Some Negate
    | _ -> None

  type primary_kind =
    | NumberLiteral
    | StringLiteral
    | Identifier
    | Location
    | Constructor
  [@@deriving show]

  let token_kind_to_primary_kind = function
    | Token.NumberLiteral -> Some NumberLiteral
    | Token.StringLiteral -> Some StringLiteral
    | Token.Identifier -> Some Identifier
    | Token.LocationIdentifier -> Some Location
    | Token.CapitalisedIdentifier -> Some Constructor
    | _ -> None

  type t = { pos : Position.t; kind : kind } [@@deriving show]

  and kind =
    (* "let" IDENTIFIER { IDENTIFIER } ":" type "=" expr "in" expr *)
    | LetIn of identifier * identifier list * DataType.t * t * t
    (* "match" expr "with" [ "|" ] match_arm { "|" match_arm } *)
    | Match of t * match_arm list
    (* "if" expr "then" expr "else" expr *)
    | IfThenElse of t * t * t
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
  and match_arm = Position.t * identifier * identifier list * t
  [@@deriving show]
end

(* CAPITALISED_IDENTFIER { type } *)
type data_arm = Position.t * identifier * DataType.t list [@@deriving show]

type kind =
  (* "let" IDENTIFIER { IDENTIFIER } ":" type "=" expr *)
  | Let of identifier * identifier list * DataType.t * Expr.t
  (* "data" IDENTIFIER "=" [ "|" ] data_arm { "|" data_arm } *)
  | Data of identifier * data_arm list
[@@deriving show]

type t = { pos : Position.t; kind : kind } [@@deriving show]
(** A top-level definition. A program is comprised of one or more top-level
    definitions meaning a program's full AST is represented by a list of t. *)
