(** Tokens that are emitted by the lexer and fed into the parser as part of
    compilation. *)

type kind =
  | OpenBracket
  | CloseBracket
  | OpenCurly
  | CloseCurly
  | Plus
  | Minus
  | Star
  | Slash
  | Equals
  | Colon
  | Semicolon
  | Dot
  | Comma
  | Exclamation
  | GreaterThan
  | LessThan
  | Arrow (* -> *)
  | NotEquiv (* != *)
  | Equiv (* == *)
  | GreaterThanOrEqual (* >= *)
  | LessThanOrEqual (* <= *)
  (* Identifiers / Keywords *)
  | Identifier
  | NotKeyword
  | AndKeyword
  | OrKeyword
  | IfKeyword
  | ThenKeyword
  | ElseKeyword
  | LetKeyword
  | InKeyword
  (* Literals *)
  | StringLiteral
  | NumberLiteral
[@@deriving show]

type t = { kind : kind; position : Position.t } [@@deriving show]
(** Represents a single token within a source file. *)

(** Match a lexeme to either a keyword token kind or, if not a known keyword, an
    identifier token kind. *)
let lookup_identifier_or_keyword lexeme =
  match lexeme with
  | "not" -> NotKeyword
  | "and" -> AndKeyword
  | "or" -> OrKeyword
  | "if" -> IfKeyword
  | "then" -> ThenKeyword
  | "else" -> ElseKeyword
  | "let" -> LetKeyword
  | "in" -> InKeyword
  | _ -> Identifier
