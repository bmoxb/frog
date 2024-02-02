(* Represents a position in a source file as start and end byte offsets. This
   type may be stored with each token and AST node for the purpose of
   giving useful error messages later. *)
type t = { start_offset : int; end_offset : int } [@@deriving show]

(* Represents a human-readable position in a source file. *)
type specific = {
  start_line_number : int;
  end_line_number : int;
  start_character_number : int;
  end_character_number : int;
}

let determine_specific_position source offsets =
  let rec find_line_char target_index ~index ~line ~char =
    let new_line, new_char =
      match source.[index] with
      | '\n' -> (line + 1, 0)
      | '\t' -> (line, char + 4)
      | _ -> (line, char + 1)
    in
    if target_index == index then (new_line, new_char)
    else
      find_line_char target_index ~index:(index + 1) ~line:new_line
        ~char:new_char
  in
  let start_line_number, start_character_number =
    find_line_char offsets.start_offset ~index:0 ~line:1 ~char:0
  in
  let end_line_number, end_character_number =
    find_line_char offsets.end_offset ~index:offsets.start_offset
      ~line:start_line_number ~char:start_character_number
  in
  {
    start_line_number;
    end_line_number;
    start_character_number;
    end_character_number;
  }
