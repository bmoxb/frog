(** Osaka's parser. Takes tokens as input and produces an AST as output. *)

type t

val init : string -> Token.t list -> t

val parse_expr : t -> t * (Ast.Expr.t, Err.t) result option
