(** Osaka's parser. Takes tokens as input and produces an AST as output. *)

type t

val init : string -> Token.t list -> t
(** Initialise a parser with the given source code and a list of tokens
    (produced previously by the lexer) as input. *)

val top_level : t -> (t * Ast.t) option
(** Parse one top-level definition. Will return none only when EOF is reached.
    @raise Err.Exception *)

val data_type : t -> (t * Ast.DataType.t) option
(** Parse a data type. Will return none if peeked token is not the start of a
    data type or if EOF.
    @raise Err.Exception *)

val pattern : t -> (t * Ast.Pattern.t) option
(** Parse a pattern. Will return none if peeked token is not the start of a
    pattern or if EOF.
    @raise Err.Exception *)

val expr : t -> t * Ast.Expr.t
(** Parse an expression. Will raise an exception if an expression could not be
    parsed (including due to EOF).
    @raise Err.Exception *)
