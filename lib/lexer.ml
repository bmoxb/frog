type t = { source_code : string; position : Position.t }

let init source_code =
  { source_code; position = { start_offset = 0; end_offset = 0 } }

let is_identifier = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_whitespace = function ' ' | '\n' | '\t' -> true | _ -> false

(* Get the next character in the input stream without incrementing the current
   position in the stream. *)
let peek lexer =
  if lexer.position.end_offset < String.length lexer.source_code then
    Some lexer.source_code.[lexer.position.end_offset]
  else None

(* Increment the current position in the input stream. *)
let advance lexer =
  {
    lexer with
    position =
      { lexer.position with end_offset = lexer.position.end_offset + 1 };
  }

let conditionally_advance cond ~if_match ~otherwise lexer =
  match peek lexer with
  | Some peeked when cond peeked -> (advance lexer, Ok if_match)
  | _ -> (lexer, Ok otherwise)

let rec handle_string lexer =
  match peek lexer with
  | Some '"' -> (advance lexer, Ok Token.StringLiteral)
  | Some _ -> lexer |> advance |> handle_string
  | None -> (lexer, Error Err.UnexpectedEOF)

let rec handle_number ?(is_decimal = false) lexer =
  match peek lexer with
  | Some peeked when is_digit peeked ->
      lexer |> advance |> handle_number ~is_decimal
  | Some '.' when not is_decimal ->
      lexer |> advance |> handle_number ~is_decimal:true
  | _ -> (lexer, Ok Token.NumberLiteral)

let rec handle_identifier lexer =
  match peek lexer with
  | Some peeked when is_identifier peeked ->
      lexer |> advance |> handle_identifier
  | _ ->
      let lexeme = Position.substring lexer.source_code lexer.position in
      (lexer, Ok (Token.lookup_identifier_or_keyword lexeme))

let next_token_kind c lexer =
  let open Token in
  let lexer = advance lexer in
  match c with
  | '(' -> (lexer, Ok OpenBracket)
  | ')' -> (lexer, Ok CloseBracket)
  | '{' -> (lexer, Ok OpenCurly)
  | '}' -> (lexer, Ok CloseCurly)
  | '[' -> (lexer, Ok OpenSquare)
  | ']' -> (lexer, Ok CloseSquare)
  | '+' -> (lexer, Ok Plus)
  | '-' ->
      conditionally_advance
        (fun c -> c = '>')
        ~if_match:Arrow ~otherwise:Minus lexer
  | '*' -> (lexer, Ok Star)
  | '/' -> (lexer, Ok Slash)
  | '=' ->
      conditionally_advance
        (fun c -> c = '=')
        ~if_match:Equiv ~otherwise:Equals lexer
  | ':' -> (lexer, Ok Colon)
  | ';' -> (lexer, Ok Semicolon)
  | '.' -> (lexer, Ok Dot)
  | ',' -> (lexer, Ok Comma)
  | '!' ->
      conditionally_advance
        (fun c -> c = '=')
        ~if_match:NotEquiv ~otherwise:Exclamation lexer
  | '>' ->
      conditionally_advance
        (fun c -> c = '=')
        ~if_match:GreaterThanOrEqual ~otherwise:GreaterThan lexer
  | '<' ->
      conditionally_advance
        (fun c -> c = '=')
        ~if_match:LessThanOrEqual ~otherwise:LessThan lexer
  | '|' -> (lexer, Ok Pipe)
  | '"' -> handle_string lexer
  | c when is_digit c -> handle_number lexer
  | c when is_identifier c -> handle_identifier lexer
  | _ ->
      (lexer, Error (Err.Lexical { character = c; position = lexer.position }))

(* Advance until the first non-whitespace character is found (or EOF). *)
let rec skip_whitespace lexer =
  match peek lexer with
  | Some peeked when is_whitespace peeked -> lexer |> advance |> skip_whitespace
  | _ -> lexer

(* Set the lexer's internal start position to the end position value. *)
let reset_start_position lexer =
  let offset = lexer.position.end_offset in
  { lexer with position = { start_offset = offset; end_offset = offset } }

let next_token lexer =
  let lexer = lexer |> skip_whitespace |> reset_start_position in
  match peek lexer with
  | Some c ->
      let lexer, kind_result = next_token_kind c lexer in
      let token_result =
        Result.map
          (fun kind : Token.t -> { kind; position = lexer.position })
          kind_result
      in
      (lexer, Some token_result)
  | None -> (lexer, None)
