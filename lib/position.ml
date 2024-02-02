(* Represents a position in a source file as start and end byte offsets. This
   type may be stored with each token and AST node for the purpose of
   giving useful error messages later. *)
type t = { start_offset : int; end_offset : int } [@@deriving show]

(* Represents a human-readable position in a source file. *)
type specific = {
  start_line_number : int;
  start_character_number : int;
  end_line_number : int;
  end_character_number : int;
  start_block_offset : int;
  end_block_offset : int;
}
[@@deriving show]

let determine_specific_position source_code position =
  (* Find the line number and character number of the given offset. *)
  let rec find_line_char target_offset ~offset ~line ~char =
    if target_offset == offset then (line, char)
    else
      let new_line, new_char =
        match source_code.[offset] with
        | '\n' -> (line + 1, 0)
        | '\t' -> (line, char + 4)
        | _ -> (line, char + 1)
      in
      find_line_char target_offset ~offset:(offset + 1) ~line:new_line
        ~char:new_char
  in
  let rec find_line_start_or_end offset update_offset =
    if
      offset < 0
      || offset >= String.length source_code
      || source_code.[offset] == '\n'
    then offset
    else find_line_start_or_end (update_offset offset) update_offset
  in
  let start_line_number, start_character_number =
    find_line_char position.start_offset ~offset:0 ~line:1 ~char:0
  in
  let end_line_number, end_character_number =
    find_line_char position.end_offset ~offset:position.start_offset
      ~line:start_line_number ~char:start_character_number
  in
  {
    start_line_number;
    start_character_number;
    end_line_number;
    end_character_number;
    start_block_offset =
      find_line_start_or_end position.start_offset (fun n -> n - 1) + 1;
    end_block_offset = find_line_start_or_end position.end_offset (( + ) 1);
  }
