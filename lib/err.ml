type lexical = { character : char; position : Position.t }

type syntax = { token : Token.t; msg : string }

type t = UnexpectedEOF | Lexical of lexical | Syntax of syntax

let display err filename source =
  let msg, position =
    match err with
    | UnexpectedEOF -> ("Unexpected EOF.", None)
    | Lexical { character; position } ->
        let msg =
          Printf.sprintf "Lexical error: Expected character '%c' in input."
            character
        in
        (msg, Some position)
    | Syntax { token; msg } -> ("Syntax error: " ^ msg, Some token.position)
  in
  let position_text =
    position
    |> Option.map (Position.determine_specific_position source)
    |> Option.map (fun (p : Position.specific) ->
           Printf.sprintf "%d:%d:" p.start_line_number p.start_character_number)
    |> Option.value ~default:""
  in
  Printf.sprintf "> %s:%s\n%s" filename position_text msg
