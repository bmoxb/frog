type t

val init : Token.t list -> t

val parse_expr : t -> t * Ast.expr option
