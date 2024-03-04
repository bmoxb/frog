open Osaka
open OUnit2

let test_translate_expr name source_code expected_terms =
  name >:: fun _ ->
  let tokens = Consume.get_tokens source_code in
  let _, expr = Parser.init source_code tokens |> Parser.expr in
  let terms = Translate.translate_expr expr |> Fmc.display in
  assert_equal expected_terms terms ~printer:(Printf.sprintf "\"%s\"")

let test_translate_exprs name_prefix =
  List.map (fun (source_code, expected_terms) ->
      test_translate_expr
        (name_prefix ^ ": " ^ source_code)
        source_code expected_terms)

let tests =
  "translate"
  >::: test_translate_exprs "primary"
         [ ("10", "10"); ("\"abc\"", "\"abc\""); ("foo", "foo") ]
       @ test_translate_exprs "arithmetic"
           [
             ("5 - 2", "[2].[5].-");
             ("10 / 2 - 4", "[4].[2].[10]./; -");
             ("30 - 15 / 5", "[5].[15]./; [30].-");
             ("3 * 3 - 6 / 2", "[2].[6]./; [3].[3].*; -");
           ]
       @ test_translate_exprs "application"
           [
             ("f 5", "[5].f");
             ("f (5 + 1)", "[1].[5].+; f");
             ("f 1 2", "[2].[1].f");
             ("f (1 + 2) 3", "[3].[1].[2].+; f");
           ]
       @ test_translate_exprs "let-in"
           [
             ("let foo : int = 10 in foo + 2", "[10]; <foo>.[2].[foo].+");
             ("let bar : int = 5 + 2 in bar", "[2].[5].+; <bar>.bar");
             ( "let f x : int -> int = x + 1 in f (5 + 2)",
               "[<x>.[1].[x].+].<f>.([2].[5].+; f)" );
           ]

let () = run_test_tt_main tests
