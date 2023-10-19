type t = {
  input : string;
  index : int;
  line_number : int;
  character_number : int;
}

let init input = { input; index = 0; line_number = 1; character_number = 0 }

let rec next_token lexer =
  match peek lexer with
  | None -> (lexer, None) (* EOF *)
  | Some c ->
      let lexer = advance lexer in
      let open Token in
      let lexer, ttype =
        match c with
        | '(' -> (lexer, OpenBracket)
        | ')' -> (lexer, CloseBracket)
        | '{' -> (lexer, OpenCurly)
        | '}' -> (lexer, CloseCurly)
        | '+' -> (lexer, Plus)
        | '-' -> conditionally_advance lexer (fun c -> c = '>') Arrow Minus
        | '*' -> (lexer, Star)
        | '/' -> (lexer, Slash)
        | '=' -> conditionally_advance lexer (fun c -> c = '=') Equiv Equals
        | ':' -> (lexer, Colon)
        | ';' -> (lexer, Semicolon)
        | '.' -> (lexer, Dot)
        | ',' -> (lexer, Comma)
        | _ -> (lexer, Invalid c)
      in
      (lexer, Some (make_token lexer ttype))

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

and conditionally_advance lexer cond true_ttype false_ttype =
  match peek lexer with
  | Some peeked when cond peeked -> (advance lexer, true_ttype)
  | _ -> (lexer, false_ttype)

and make_token lexer ttype : Token.t =
  {
    ttype;
    line_number = lexer.line_number;
    character_number = lexer.character_number;
  }
