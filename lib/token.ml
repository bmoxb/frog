type invalid_kind = UnexpectedChar of char | UnexpectedEOF [@@deriving show]

type kind =
  | Invalid of invalid_kind
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
  | Identifier of string
  | NotKeyword
  | AndKeyword
  | OrKeyword
  | IfKeyword
  | ThenKeyword
  | ElseKeyword
  | LetKeyword
  | InKeyword
  (* Literals *)
  | StringLiteral of string
  | NumberLiteral of string
[@@deriving show]

(* Represents a single token of some token kind and at some position within a
   source file. *)
type t = { kind : kind; line_number : int; character_number : int }
[@@deriving show]

(* Match a lexeme to either a keyword token kind or, if not a known keyword, an
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
  | _ -> Identifier lexeme
