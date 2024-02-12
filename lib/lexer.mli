(** Osaka's lexer. Takes source code as input and produces tokens as output. *)

type t

val init : string -> t
(** Initialise a parser with the given source code as input. *)

val token : t -> (t * Token.t) option
(** Get the next token. Will return none only when EOF is reached.
    @raise Err.Exception *)
