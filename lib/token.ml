(** Tokens that are emitted by the lexer and fed into the parser as part of
    compilation. *)

type kind =
  | OpenBracket (* ( *)
  | CloseBracket (* ) *)
  | OpenCurly (* { *)
  | CloseCurly (* } *)
  | OpenSquare (* [ *)
  | CloseSquare (* ] *)
  | Plus (* + *)
  | Minus (* - *)
  | Arrow (* -> *)
  | Star (* * *)
  | Slash (* / *)
  | Equals (* = *)
  | Equiv (* == *)
  | Colon (* : *)
  | Semicolon (* ; *)
  | Dot (* . *)
  | Comma (* , *)
  | Exclamation (* ! *)
  | NotEquiv (* != *)
  | GreaterThan (* > *)
  | GreaterThanOrEqual (* >= *)
  | LessThan (* < *)
  | LessThanOrEqual (* <= *)
  | Pipe (* | *)
  | At (* @ *)
  (* Literals *)
  | StringLiteral
  | NumberLiteral
  (* Identifiers / Keywords *)
  | Identifier (* begins with underscore or lowercase letter *)
  | CapitalisedIdentifier (* begins with uppercase letter *)
  | NotKeyword
  | AndKeyword
  | OrKeyword
  | IfKeyword
  | ThenKeyword
  | ElseKeyword
  | LetKeyword
  | InKeyword
  | AliasKeyword (* TODO: test *)
  | DataKeyword (* TODO: test *)
  | MatchKeyword (* TODO: test *)
  | WithKeyword (* TODO: test *)
[@@deriving show]

type t = { kind : kind; position : Position.t } [@@deriving show]
(** Represents a single token within a source file. *)

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
  | lexeme ->
      let is_uppercase = function 'A' .. 'Z' -> true | _ -> false in
      if is_uppercase lexeme.[0] then CapitalisedIdentifier else Identifier
