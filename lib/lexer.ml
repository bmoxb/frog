type t = {
  input : string;
  index : int;
  line_number : int;
  character_number : int;
}

let init input = { input; index = 0; line_number = 1; character_number = 0 }

let rec next_token lexer =
  let lexer = skip_whitespace lexer in
  match peek lexer with
  | None -> (lexer, None) (* EOF *)
  | Some c ->
      let open Token in
      let lexer = advance lexer in
      let lexer, token_kind =
        match c with
        | '(' -> (lexer, OpenBracket)
        | ')' -> (lexer, CloseBracket)
        | '{' -> (lexer, OpenCurly)
        | '}' -> (lexer, CloseCurly)
        | '+' -> (lexer, Plus)
        | '-' ->
            conditionally_advance lexer
              (fun c -> c = '>')
              ~if_match:Arrow ~otherwise:Minus
        | '*' -> (lexer, Star)
        | '/' -> (lexer, Slash)
        | '=' ->
            conditionally_advance lexer
              (fun c -> c = '=')
              ~if_match:Equiv ~otherwise:Equals
        | ':' -> (lexer, Colon)
        | ';' -> (lexer, Semicolon)
        | '.' -> (lexer, Dot)
        | ',' -> (lexer, Comma)
        | '"' -> handle_string lexer ""
        | c when is_digit c ->
            handle_number lexer (String.make 1 c) ~is_decimal:false
        | c when is_identifier c -> handle_identifier lexer (String.make 1 c)
        | _ -> (lexer, Invalid (UnexpectedChar c))
      in
      (lexer, Some (make_token lexer token_kind))

and skip_whitespace lexer =
  match peek lexer with
  | Some peeked when is_whitespace peeked -> skip_whitespace (advance lexer)
  | _ -> lexer

and peek lexer =
  if lexer.index < String.length lexer.input then Some lexer.input.[lexer.index]
  else None

and advance lexer =
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

and conditionally_advance lexer cond ~if_match ~otherwise =
  match peek lexer with
  | Some peeked when cond peeked -> (advance lexer, if_match)
  | _ -> (lexer, otherwise)

and handle_identifier lexer lexeme =
  match peek lexer with
  | Some peeked when is_identifier peeked ->
      handle_identifier (advance lexer) (lexeme ^ String.make 1 peeked)
  | _ -> (lexer, Token.lookup_identifier_or_keyword lexeme)

and handle_number lexer lexeme ~is_decimal =
  match peek lexer with
  | Some peeked when is_digit peeked ->
      handle_number (advance lexer) (lexeme ^ String.make 1 peeked) ~is_decimal
  | Some '.' when not is_decimal ->
      handle_number (advance lexer) (lexeme ^ ".") ~is_decimal:true
  | _ -> (lexer, Token.NumberLiteral lexeme)

and handle_string lexer lexeme =
  match peek lexer with
  | Some '"' -> (advance lexer, Token.StringLiteral lexeme)
  | Some peeked -> handle_string (advance lexer) (lexeme ^ String.make 1 peeked)
  | None -> (lexer, Token.Invalid Token.UnexpectedEOF)

and make_token lexer kind : Token.t =
  {
    kind;
    line_number = lexer.line_number;
    character_number = lexer.character_number;
  }

and is_whitespace = function ' ' | '\n' | '\t' -> true | _ -> false

and is_identifier = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

and is_digit = function '0' .. '9' -> true | _ -> false
