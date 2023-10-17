type ttype =
  | OpenBracket
  | CloseBracket
  | OpenCurly
  | CloseCurly
  | Plus
  | Minus
  | Star
  | Slash
  | Equals
  | Semicolon
  | Dot
  | Comma
  | Arrow (* -> *)
  | Equiv (* == *)
  (* Identifiers / Keywords *)
  | Identifier of string
  | FnKeyword
  | ReturnKeyword
  | IfKeyword
  | ElseKeyword
  | WhileKeyword
  | StructKeyword
  | LinearKeyword
  | AffineKeyword
  (* Literals *)
  | StringLiteral of string
  | IntLiteral of int
  | FloatLiteral of float
[@@deriving show]

(* Represents a single token of some token type and at some position within a
   source file. *)
type t = { ttype : ttype; line_number : int; character_number : int }
[@@deriving show]

(* Match a lexeme to either a keyword token type or, if not a known keyword, an
   identifier token type. *)
let lookup_identifier_or_keyword lexeme =
  match lexeme with
  | "fn" -> FnKeyword
  | "return" -> ReturnKeyword
  | "if" -> IfKeyword
  | "else" -> ElseKeyword
  | "while" -> WhileKeyword
  | "struct" -> StructKeyword
  | "linear" -> LinearKeyword
  | "affine" -> AffineKeyword
  | _ -> Identifier lexeme
