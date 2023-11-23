type expr =
  | BinaryExpr of Token.t * expr * expr
  | UnaryExpr of Token.t * expr
  | Group of expr
  | Literal of Token.t

(* Construct a human-readable representation of an expression that shows just
   token kinds in a simple tree structure. *)
val show_expr_simple : expr -> string
