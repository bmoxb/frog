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
         [
           ("10", "[10]");
           ("\"abc\"", "[\"abc\"]");
           ("foo", "[foo]");
           ("@a", "a<x>.[x]");
         ]
       @ test_translate_exprs "binary arithmetic"
           [
             ("5 - 2", "[2].[5].-");
             ("10 / 2 - 4", "[4].[2].[10]./; -");
             ("30 - 15 / 5", "[5].[15]./; [30].-");
             ("3 * 3 - 6 / 2", "[2].[6]./; [3].[3].*; -");
           ]
       @ test_translate_exprs "unary arithmetic"
           [
             ("-5", "[5].[0].-");
             ("-x", "[x].[0].-");
             ("-(5 + x)", "[x].[5].+; [0].-");
           ]
       @ test_translate_exprs "application"
           [
             ("f 5", "[5].f");
             ("f (5 + 1)", "[1].[5].+; f");
             ("f 1 2", "[2].[1].f");
             ("f (1 + 2) 3", "[3].[2].[1].+; f");
             ("f 1 + 2", "[2].[1].f; +");
             ("f (g 10) (g (2 + x))", "[x].[2].+; g; [10].g; f");
           ]
       @ test_translate_exprs "location push"
           [
             ("@stdout \"hello\"", "[\"hello\"]stdout");
             ("@stdout 1 (2 + 3)", "[1]stdout.[3].[2].+; <x>.[x]stdout");
             ( "@location (10 + foo) (1 + 2)",
               "[foo].[10].+; <x>.[x]location.[2].[1].+; <x>.[x]location" );
           ]
       @ test_translate_exprs "location pop"
           [
             ("10 + @a", "a<x>.[x].[10].+");
             ("@a - @b", "b<x>.[x].a<x>.[x].-");
             ("@a - @b / @c", "c<x>.[x].b<x>.[x]./; a<x>.[x].-");
             ("f (1 + 2) @a", "a<x>.[x].[2].[1].+; f");
           ]

let () = run_test_tt_main tests