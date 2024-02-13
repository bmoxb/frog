type lexical = { character : char; pos : Position.t }

type syntax = { token : Token.t; msg : string }

type t = UnexpectedEOF | Lexical of lexical | Syntax of syntax

exception Exception of t

let raise_lexical_error character pos =
  raise_notrace (Exception (Lexical { character; pos }))

let raise_syntax_error token msg =
  raise_notrace (Exception (Syntax { token; msg }))

let raise_unexpected_eof () = raise_notrace (Exception UnexpectedEOF)

let bold_red_colour = "\027[1;31m"

let blue_colour = "\027[34m"

let reset_colour = "\027[0;0m"

let display_relevant_lines source_code (specific_pos : Position.specific) =
  let lines =
    String.sub source_code specific_pos.start_block_offset
      (specific_pos.end_block_offset - specific_pos.start_block_offset)
    |> String.split_on_char '\n'
  in
  let lines_text =
    lines
    |> List.mapi (fun index line ->
           Printf.sprintf "%s| %d%s %s" blue_colour
             (specific_pos.start_line_number + index)
             reset_colour line)
    |> String.concat "\n"
  in
  let leftmost_character_number =
    if specific_pos.start_line_number == specific_pos.end_line_number then
      specific_pos.start_character_number
    else 0
  in
  let arrows =
    blue_colour
    ^ String.make (leftmost_character_number + 4) ' '
    ^ String.make
        (specific_pos.end_character_number - leftmost_character_number)
        '^'
    ^ reset_colour
  in
  lines_text ^ "\n" ^ arrows

let display ~filename ~source_code err =
  let open Printf in
  let kind, msg, (pos : Position.t) =
    match err with
    | UnexpectedEOF ->
        let source_len = String.length source_code in
        ( "Error",
          "unexpected end of file.",
          { start = source_len - 1; finish = source_len } )
    | Lexical { character; pos } ->
        ( "Lexical error",
          sprintf "encountered unexpected character '%c'." character,
          pos )
    | Syntax { token; msg } -> ("Syntax error", msg, token.pos)
  in
  let specific_pos = Position.determine_specific_position source_code pos in
  let headline = sprintf "%s%s:%s %s\n" bold_red_colour kind reset_colour msg in
  let file_info =
    sprintf "%s| >%s %s:%d:%d\n" blue_colour reset_colour filename
      specific_pos.start_line_number specific_pos.start_character_number
  in
  let lines = display_relevant_lines source_code specific_pos in
  headline ^ file_info ^ lines
