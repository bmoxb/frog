(** Osaka's lexer. Takes source code as input and produces tokens as output. *)

type t

val init : string -> t

val next_token : t -> t * (Token.t, Err.t) result option
