(** Osaka's parser. Takes tokens as input and produces an AST as output. *)

type t

val init : string -> Token.t list -> t
(** Initialise a parser with the given source code and a list of tokens
    (produced previously by the lexer) as input. *)

val next_ast : t -> t * (Ast.t, Err.t) result option
(* TODO: wrap the whole tuple in result option? *)
