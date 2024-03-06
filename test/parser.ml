(*open Osaka*)
open OUnit2

let tests = "parser" >::: [ ("testy" >:: fun _ -> assert_bool "uh oh" true) ]

let () = run_test_tt_main tests
