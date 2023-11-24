type t = {
  input : string;
  index : int;
  line_number : int;
  character_number : int;
}

let init input = { input; index = 0; line_number = 1; character_number = 0 }

let is_identifier = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_whitespace = function ' ' | '\n' | '\t' -> true | _ -> false

(* Buffers are used to construct lexeme strings efficiently for identifiers and
   literals one character at a time. This function constructs a new buffer of a
   sensible starting capacity for that purpose. *)
let new_buf () = Buffer.create 10

let char_to_buf c =
  let buf = new_buf () in
  Buffer.add_char buf c;
  buf

(* Get the next character in the input stream without incrementing the current
   position in the stream. *)
let peek lexer =
  if lexer.index < String.length lexer.input then Some lexer.input.[lexer.index]
  else None

(* Increment the current position in the input stream and update the current
   line and character numbers. *)
let advance lexer =
  match peek lexer with
  | Some peeked ->
      let line_number, character_number =
        match peeked with
        | '\n' -> (lexer.line_number + 1, 0)
        | '\t' -> (lexer.line_number, lexer.character_number + 4)
        | _ -> (lexer.line_number, lexer.character_number + 1)
      in
      { lexer with index = lexer.index + 1; line_number; character_number }
  | None -> lexer

let conditionally_advance cond ~if_match ~otherwise lexer =
  match peek lexer with
  | Some peeked when cond peeked -> (advance lexer, Ok if_match)
  | _ -> (lexer, Ok otherwise)

(* Advance until the first non-whitespace character is found (or EOF). *)
let rec skip_whitespace lexer =
  match peek lexer with
  | Some peeked when is_whitespace peeked -> lexer |> advance |> skip_whitespace
  | _ -> lexer

let rec handle_string lexeme_buf lexer =
  match peek lexer with
  | Some '"' ->
      let lexeme = Buffer.contents lexeme_buf in
      (advance lexer, Ok (Token.StringLiteral lexeme))
  | Some peeked ->
      Buffer.add_char lexeme_buf peeked;
      lexer |> advance |> handle_string lexeme_buf
  | None -> (lexer, Error Err.UnexpectedEOF)

let rec handle_number ?(is_decimal = false) lexeme_buf lexer =
  match peek lexer with
  | Some peeked when is_digit peeked ->
      Buffer.add_char lexeme_buf peeked;
      lexer |> advance |> handle_number ~is_decimal lexeme_buf
  | Some '.' when not is_decimal ->
      Buffer.add_char lexeme_buf '.';
      lexer |> advance |> handle_number ~is_decimal:true lexeme_buf
  | _ ->
      let lexeme = Buffer.contents lexeme_buf in
      (lexer, Ok (Token.NumberLiteral lexeme))

let rec handle_identifier lexeme_buf lexer =
  match peek lexer with
  | Some peeked when is_identifier peeked ->
      Buffer.add_char lexeme_buf peeked;
      lexer |> advance |> handle_identifier lexeme_buf
  | _ ->
      let lexeme = Buffer.contents lexeme_buf in
      (lexer, Ok (Token.lookup_identifier_or_keyword lexeme))

let make_token kind lexer : Token.t =
  {
    kind;
    line_number = lexer.line_number;
    character_number = lexer.character_number;
  }

let next_token lexer =
  let lexer = skip_whitespace lexer in
  match peek lexer with
  | None -> (lexer, None) (* EOF *)
  | Some c ->
      let open Token in
      let lexer = advance lexer in
      let lexer, token_kind_result =
        match c with
        | '(' -> (lexer, Ok OpenBracket)
        | ')' -> (lexer, Ok CloseBracket)
        | '{' -> (lexer, Ok OpenCurly)
        | '}' -> (lexer, Ok CloseCurly)
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
        | '"' -> handle_string (new_buf ()) lexer
        | c when is_digit c -> handle_number (char_to_buf c) lexer
        | c when is_identifier c -> handle_identifier (char_to_buf c) lexer
        | _ ->
            let lexical_error =
              Err.Lexical
                {
                  character = c;
                  line_number = lexer.line_number;
                  character_number = lexer.character_number;
                }
            in
            (lexer, Error lexical_error)
      in
      let token_result =
        match token_kind_result with
        | Ok token_kind -> Ok (make_token token_kind lexer)
        | Error err -> Error err
      in
      (lexer, Some token_result)
