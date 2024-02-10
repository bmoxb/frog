(** Osaka's lexer. Takes source code as input and produces tokens as output. *)

type t

val init : string -> t
(** Initialise a parser with the given source code as input. *)

val next_token : t -> ((t * Token.t) option, Err.t) result
