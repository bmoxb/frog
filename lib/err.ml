(** Errors in an Osaka program (lexical, syntax, etc.) *)

type lexical = { character : char; position : Position.t }

type syntax = { token : Token.t; msg : string }

type t = UnexpectedEOF | Lexical of lexical | Syntax of syntax

let bold_red_colour = "\027[1;31m"

let blue_colour = "\027[34m"

let reset_colour = "\027[0;0m"

let display_relevant_lines source_code (specific_position : Position.specific) =
  let lines =
    String.sub source_code specific_position.start_block_offset
      (specific_position.end_block_offset - specific_position.start_block_offset)
    |> String.split_on_char '\n'
  in
  let lines_text =
    lines
    |> List.mapi (fun index line ->
           Printf.sprintf "%s| %d%s %s" blue_colour
             (specific_position.start_line_number + index)
             reset_colour line)
    |> String.concat "\n"
  in
  let leftmost_character_number =
    if specific_position.start_line_number == specific_position.end_line_number
    then specific_position.start_character_number
    else 0
  in
  let arrows =
    blue_colour
    ^ String.make (leftmost_character_number + 4) ' '
    ^ String.make
        (specific_position.end_character_number - leftmost_character_number)
        '^'
    ^ reset_colour
  in
  lines_text ^ "\n" ^ arrows

let display filename source_code err =
  let open Printf in
  let kind, msg, (position : Position.t) =
    match err with
    | UnexpectedEOF ->
        let source_len = String.length source_code in
        ( "Error",
          "unexpected end of file.",
          { start_offset = source_len - 1; end_offset = source_len } )
    | Lexical { character; position } ->
        ( "Lexical error",
          sprintf "encountered unexpected character '%c'." character,
          position )
    | Syntax { token; msg } -> ("Syntax error", msg, token.position)
  in
  let specific_position =
    Position.determine_specific_position source_code position
  in
  let headline = sprintf "%s%s:%s %s\n" bold_red_colour kind reset_colour msg in
  let file_info =
    sprintf "%s| >%s %s:%d:%d\n" blue_colour reset_colour filename
      specific_position.start_line_number
      specific_position.start_character_number
  in
  let lines = display_relevant_lines source_code specific_position in
  headline ^ file_info ^ lines
