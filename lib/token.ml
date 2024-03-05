(** Tokens that are emitted by the lexer and fed into the parser as part of
    compilation. *)

type kind =
  | OpenBracket (* ( *)
  | CloseBracket (* ) *)
  | Plus (* + *)
  | Minus (* - *)
  | Arrow (* -> *)
  | Star (* * *)
  | Slash (* / *)
  | Equals (* = *)
  | Equiv (* == *)
  | Colon (* : *)
  | Semicolon (* ; *)
  | Comma (* , *)
  | Exclamation (* ! *)
  | NotEquiv (* != *)
  | GreaterThan (* > *)
  | GreaterThanOrEqual (* >= *)
  | LessThan (* < *)
  | LessThanOrEqual (* <= *)
  | Pipe (* | *)
  (* Literals *)
  | StringLiteral
  | NumberLiteral
  (* Identifiers / Keywords *)
  | Identifier (* begins with underscore or lowercase letter *)
  | CapitalisedIdentifier (* begins with uppercase letter *)
  | LocationIdentifier (* begins with '@' *)
  | NotKeyword
  | AndKeyword
  | OrKeyword
  | IfKeyword
  | ThenKeyword
  | ElseKeyword
  | LetKeyword
  | InKeyword
  | AliasKeyword
  | DataKeyword
  | MatchKeyword
  | WithKeyword
[@@deriving show]

type t = { kind : kind; pos : Position.t } [@@deriving show]
(** Represents a single token within a source file. *)

let lexeme source_code token = Position.substring source_code token.pos

(** Match a lexeme to either a keyword token kind or, if not a known keyword,
    an identifier (or capitalised identifier) token kind. *)
let lookup_identifier_or_keyword = function
  | "not" -> NotKeyword
  | "and" -> AndKeyword
  | "or" -> OrKeyword
  | "if" -> IfKeyword
  | "then" -> ThenKeyword
  | "else" -> ElseKeyword
  | "let" -> LetKeyword
  | "in" -> InKeyword
  | "alias" -> AliasKeyword
  | "data" -> DataKeyword
  | "match" -> MatchKeyword
  | "with" -> WithKeyword
  | lexeme -> (
      match lexeme.[0] with
      | 'A' .. 'Z' -> CapitalisedIdentifier
      | '@' -> LocationIdentifier
      | _ -> Identifier)
