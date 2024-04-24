open Frog
open OUnit2

let test_translate_top_level name source_code expected_terms =
  name >:: fun _ ->
  let tokens = Consume.get_tokens source_code in
  let nodes = Consume.get_ast source_code tokens in
  let terms = Translate.translate nodes |> Fmc.display in
  assert_equal expected_terms terms ~printer:(Printf.sprintf "\"%s\"")

let test_translate_expr name source_code expected_terms =
  name >:: fun _ ->
  let tokens = Consume.get_tokens source_code in
  let _, expr = Parser.init source_code tokens |> Parser.expr in
  let terms = Translate.translate_expr expr |> Fmc.display in
  assert_equal expected_terms terms ~printer:(Printf.sprintf "\"%s\"")

let test_translate_pairs test_fun name =
  List.map (fun (source_code, expected_terms) ->
      test_fun (name ^ ": " ^ source_code) source_code expected_terms)

let test_translate_top_levels name =
  test_translate_pairs test_translate_top_level (name ^ " top-level")

let test_translate_exprs name =
  test_translate_pairs test_translate_expr (name ^ " expression")

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
       @ test_translate_exprs "data constructor"
           [
             ("None", "[None]");
             ("Some 10", "[10].<a>.([[a].Some])");
             ("Some (10 + x)", "[x].[10].+; <a>.([[a].Some])");
             ( "Cons 10 (Cons 20 Nil)",
               "[Nil].<a>.([20].<b>.([[b].[a].Cons])); \
                <a>.([10].<b>.([[b].[a].Cons]))" );
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
       @ test_translate_exprs "if-then-else"
           [
             ("if x then y else z", "x; True -> [y]; False -> [z]");
             ( "if @in == \"hello\" then 1 + 2 else 3 + 4",
               "[\"hello\"].in<x>.[x].==; <x>.x; True -> [2].[1].+; False -> \
                [4].[3].+" );
           ]
       @ test_translate_exprs "let-in binding constant"
           [
             ("let foo : int = 5 in foo", "[5].<foo>.([foo])");
             ( "let bar : int = 12 + 2 in bar - 6 / 2",
               "[2].[12].+; <bar>.([2].[6]./; [bar].-)" );
             ("let foo : string = @in in foo", "in<x>.[x].<foo>.([foo])");
           ]
       @ test_translate_exprs "let-in function"
           [
             ( "let identity x : int -> int = x in identity 5",
               "[<x>.([x])].<identity>.([5].identity)" );
             ( "let sub x y : int int -> int = x - y in sub (5 + 1) 2",
               "[<x>.(<y>.([y].[x].-))].<sub>.([2].[1].[5].+; sub)" );
           ]
       @ test_translate_exprs "match"
           [
             ( "match x with Some n -> n + 1 | None -> 0",
               "x; Some -> (<n>.([1].[n].+)); None -> ([0])" );
             ( "match range 5 with Cons num tail -> num | Nil -> 0",
               "[5].range; <x>.x; Cons -> (<tail>.(<num>.([num]))); Nil -> \
                ([0])" );
           ]
       @ test_translate_top_levels "main"
           [
             ("let main : int = 10", "10");
             ("let main : int = 10 + 2", "[2].[10].+; <x>.x");
           ]
       @ test_translate_top_levels "let binding constant"
           [
             ( "let a : int = 5 \n let main : @in -> int = @in + a",
               "[5].<a>.([a].in<x>.[x].+; <x>.x)" );
             ( "let a : int = 5 \n\
               \ let b : int = 10 \n\
               \ let main : @in -> int = a + b / @in",
               "[5].<a>.([10].<b>.(in<x>.[x].[b]./; [a].+; <x>.x))" );
           ]
       @ test_translate_top_levels "let function"
           [
             ( "let f a : int -> int = a + 2 * 2 \n\
               \ let main : @in -> int = f @in",
               "[<a>.([2].[2].*; [a].+)].<f>.(in<x>.[x].f; <x>.x)" );
           ]

let () = run_test_tt_main tests
