open Osaka.Fmc
open OUnit2

let example_1_displayed = "<x>.x"

let example_1 = Pop (Lambda, "x", Variable "x")

let example_2_displayed = "[10].[5].[2].+; -"

let example_2 =
  Choice
    ( Push
        ( Variable "10",
          Lambda,
          Push (Variable "5", Lambda, Push (Variable "2", Lambda, Variable "+"))
        ),
      Star,
      Variable "-" )

let test_display name expected_string input_term =
  name >:: fun _ ->
  assert_equal expected_string (display input_term) ~printer:(fun s -> s)

let tests =
  "fmc"
  >::: [
         test_display "display example 1" example_1_displayed example_1;
         test_display "display example 2" example_2_displayed example_2;
       ]

let () = run_test_tt_main tests
