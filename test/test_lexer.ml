open Osaka
open OUnit2

let assert_valid_token expected_token output =
  match output with
  | Some (Ok token) -> assert_equal expected_token token ~printer:Token.show
  | Some (Error _) ->
      assert_failure "Function next_token gave unexpected error."
  | None -> assert_failure "Unexpected end of token stream."

let test_valid_token name input expected_token =
  name >:: fun _ ->
  let _, output = input |> Lexer.init |> Lexer.next_token in
  assert_valid_token expected_token output

let test_valid_tokens name_prefix input_kind_pairs =
  List.map
    (fun (input, expected_kind) ->
      let name = name_prefix ^ ": " ^ input in
      let expected_token : Token.t =
        {
          kind = expected_kind;
          line_number = 1;
          character_number = String.length input;
        }
      in
      test_valid_token name input expected_token)
    input_kind_pairs

let test_valid_simple_tokens = test_valid_tokens "simple token"

let test_valid_keyword_tokens = test_valid_tokens "keyword"

let test_valid_identifier_tokens inputs =
  test_valid_tokens "identifier"
    (List.map (fun s -> (s, Token.Identifier s)) inputs)

let test_valid_string_literal_tokens inputs =
  let input_to_pair s =
    let quotes_removed = String.sub s 1 (String.length s - 2) in
    (s, Token.StringLiteral quotes_removed)
  in
  test_valid_tokens "string literal" (List.map input_to_pair inputs)

let test_valid_number_literal_tokens inputs =
  test_valid_tokens "number literal"
    (List.map (fun s -> (s, Token.NumberLiteral s)) inputs)

let test_valid_token_stream name input expected_tokens =
  name >:: fun _ ->
  let rec assert_tokens lexer expected_tokens =
    match expected_tokens with
    | expected_token :: tail ->
        let lexer, output = Lexer.next_token lexer in
        assert_valid_token expected_token output;
        assert_tokens lexer tail
    | [] -> ()
  in
  assert_tokens (Lexer.init input) expected_tokens

let tests =
  "lexer"
  >::: test_valid_simple_tokens
         [
           ("(", Token.OpenBracket);
           (")", Token.CloseBracket);
           ("{", Token.OpenCurly);
           ("}", Token.CloseCurly);
           ("+", Token.Plus);
           ("-", Token.Minus);
           ("*", Token.Star);
           ("/", Token.Slash);
           ("=", Token.Equals);
           (":", Token.Colon);
           (";", Token.Semicolon);
           (".", Token.Dot);
           (",", Token.Comma);
           ("!", Token.Exclamation);
           (">", Token.GreaterThan);
           ("<", Token.LessThan);
           ("->", Token.Arrow);
           ("!=", Token.NotEquiv);
           ("==", Token.Equiv);
           (">=", Token.GreaterThanOrEqual);
           ("<=", Token.LessThanOrEqual);
         ]
       @ test_valid_keyword_tokens
           [
             ("not", NotKeyword);
             ("and", AndKeyword);
             ("or", OrKeyword);
             ("if", IfKeyword);
             ("then", ThenKeyword);
             ("else", ElseKeyword);
             ("let", LetKeyword);
             ("in", InKeyword);
           ]
       @ test_valid_identifier_tokens
           [ "x"; "_"; "identifier"; "noT"; "If"; "x0" ]
       @ test_valid_string_literal_tokens
           [ "\"\""; "\"Hello, world!\""; "\"123\"" ]
       @ test_valid_number_literal_tokens
           [ "0"; "10"; "123456789"; "0.1"; "1."; "135.790001000300050007" ]
       @ [
           test_valid_token_stream "repeated = signs" " ===== "
             [
               { kind = Token.Equiv; line_number = 1; character_number = 3 };
               { kind = Token.Equiv; line_number = 1; character_number = 5 };
               { kind = Token.Equals; line_number = 1; character_number = 6 };
             ];
           test_valid_token_stream "dot before number literal" ".5\n5."
             [
               { kind = Token.Dot; line_number = 1; character_number = 1 };
               {
                 kind = Token.NumberLiteral "5";
                 line_number = 1;
                 character_number = 2;
               };
               {
                 kind = Token.NumberLiteral "5.";
                 line_number = 2;
                 character_number = 2;
               };
             ];
           test_valid_token_stream "multiple string literals"
             "\"abc\"\"def\nghi\""
             [
               {
                 kind = Token.StringLiteral "abc";
                 line_number = 1;
                 character_number = 5;
               };
               {
                 kind = Token.StringLiteral "def\nghi";
                 line_number = 2;
                 character_number = 4;
               };
             ];
         ]

let () = run_test_tt_main tests
