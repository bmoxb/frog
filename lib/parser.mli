(** Osaka's parser. Takes tokens as input and produces an AST as output. *)

type t

val init : string -> Token.t list -> t
(** Initialise a parser with the given source code and a list of tokens
    (produced previously by the lexer) as input. *)

val parse_expr : t -> t * (Ast.Expr.t, Err.t) result option
