(** Osaka's parser. Takes tokens as input and produces an AST as output. *)

type t

val init : string -> Token.t list -> t
(** Initialise a parser with the given source code and a list of tokens
    (produced previously by the lexer) as input. *)

val top_level : t -> (t * Ast.t) option
(** Parse one top-level definition. Will return none only when EOF is reached.
    @raise Err.Exception *)

val data_type : t -> (t * Ast.DataType.t) option
(** Parse a data type.
    @raise Err.Exception *)

val pattern : t -> (t * Ast.Pattern.t) option
(** Parse a pattern.
    @raise Err.Exception *)

val expr : t -> (t * Ast.Expr.t) option
(** Paerse an expression.
    @raise Err.Exception *)
