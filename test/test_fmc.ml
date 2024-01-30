open Osaka
open OUnit2

(* These examples are taken from 'Example 3.2' in the FMC paper. *)

let example_1_displayed = "<x>.[x].[x]"

let example_1 =
  Fmc.Abstraction
    {
      location = None;
      variable = "x";
      term =
        Fmc.Application
          {
            arg = Fmc.Variable "x";
            location = None;
            func =
              Fmc.Application
                {
                  arg = Fmc.Variable "x";
                  location = None;
                  func = Fmc.Jump None;
                };
          };
    }

let example_2_displayed = "<x>.<y>"

let example_2 =
  Fmc.Abstraction
    {
      location = None;
      variable = "x";
      term =
        Fmc.Abstraction
          { location = None; variable = "y"; term = Fmc.Jump None };
    }

let test_display name expected_string input_term =
  name >:: fun _ ->
  assert_equal expected_string (Fmc.display input_term) ~printer:(fun s -> s)

let tests =
  "fmc"
  >::: [
         test_display "display example 1" example_1_displayed example_1;
         test_display "display example 2" example_2_displayed example_2;
       ]

let () = run_test_tt_main tests
