(** Errors in an Osaka program (lexical, syntax, etc.) *)

type t

exception Exception of t

val raise_lexical_error : char -> Position.t -> 'a

val raise_syntax_error : Token.t -> string -> 'a

val raise_unexpected_eof : unit -> 'a

val display : filename:string -> source_code:string -> t -> string
(** Create a  human-readable description of an error that includes the
    offending line(s). *)
