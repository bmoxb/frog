type lexical = { character : char; line_number : int; character_number : int }

type syntax = { token : Token.t; msg : string }

type t = UnexpectedEOF | Lexical of lexical | Syntax of syntax

let display err input_filename =
  let lineno, charno, msg =
    match err with
    | UnexpectedEOF -> (0, 0, "Unexpected EOF.")
    | Lexical { character; line_number; character_number } ->
        let msg =
          Printf.sprintf "Lexical error: Expected character '%c' in input."
            character
        in
        (line_number, character_number, msg)
    | Syntax { token; msg } ->
        (token.line_number, token.character_number, "Syntax error: " ^ msg)
  in
  Printf.sprintf "> %s:%d:%d\n%s" input_filename lineno charno msg
