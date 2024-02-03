type t

val init : string -> Token.t list -> t

val parse_expr : t -> t * (Ast.Expr.t, Err.t) result option
