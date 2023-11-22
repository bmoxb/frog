type expr =
  | BinaryExpr of Token.t * expr * expr
  | UnaryExpr of Token.t * expr
  | Literal of Token.t
  | Group of expr
[@@deriving show]
