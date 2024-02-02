open Osaka
open OUnit2

let test_determine_specific_position name ~source ~position ~expected =
  name >:: fun _ ->
  let got = Position.determine_specific_position source position in
  assert_equal expected got ~printer:Position.show_specific

let tests =
  "position"
  >::: [
         test_determine_specific_position "single line" ~source:"abcdef"
           ~position:{ start_offset = 2; end_offset = 5 }
           ~expected:
             {
               start_line_number = 1;
               start_character_number = 2;
               end_line_number = 1;
               end_character_number = 5;
             };
         test_determine_specific_position "multiple lines"
           ~source:"line 1\nline 2\nline 3"
           ~position:{ start_offset = 7; end_offset = 20 }
           ~expected:
             {
               start_line_number = 2;
               start_character_number = 0;
               end_line_number = 3;
               end_character_number = 6;
             };
       ]

let () = run_test_tt_main tests
