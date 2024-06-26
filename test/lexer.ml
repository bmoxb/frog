open Frog
open OUnit2

let assert_valid_token expected_token = function
  | Some (lexer, token) ->
      assert_equal expected_token token ~printer:Token.show;
      lexer
  | None -> assert_failure "Unexpected end of token stream."

let test_valid_token name input expected_token =
  name >:: fun _ ->
  let output = input |> Lexer.init |> Lexer.token in
  let _ = assert_valid_token expected_token output in
  ()

let test_valid_tokens name_prefix =
  List.map (fun (input, expected_kind) ->
      let name = name_prefix ^ ": " ^ input in
      let expected_token : Token.t =
        {
          kind = expected_kind;
          pos = { start = 0; finish = String.length input };
        }
      in
      test_valid_token name input expected_token)

let test_valid_simple_tokens = test_valid_tokens "simple token"

let test_valid_keyword_tokens = test_valid_tokens "keyword"

let test_valid_identifier_tokens inputs =
  test_valid_tokens "identifier"
    (List.map (fun s -> (s, Token.Identifier)) inputs)

let test_valid_capitalised_identifier_tokens inputs =
  test_valid_tokens "capitalised identifier"
    (List.map (fun s -> (s, Token.CapitalisedIdentifier)) inputs)

let test_valid_location_identifier_tokens inputs =
  test_valid_tokens "location identifier"
    (List.map (fun s -> (s, Token.LocationIdentifier)) inputs)

let test_valid_string_literal_tokens inputs =
  let input_to_pair s = (s, Token.StringLiteral) in
  test_valid_tokens "string literal" (List.map input_to_pair inputs)

let test_valid_number_literal_tokens inputs =
  test_valid_tokens "number literal"
    (List.map (fun s -> (s, Token.NumberLiteral)) inputs)

let test_valid_token_stream name input expected_tokens =
  name >:: fun _ ->
  let rec assert_tokens expected_tokens lexer =
    match expected_tokens with
    | expected_token :: tail ->
        let output = Lexer.token lexer in
        let lexer = assert_valid_token expected_token output in
        assert_tokens tail lexer
    | [] -> ()
  in
  assert_tokens expected_tokens (Lexer.init input)

let tests =
  "lexer"
  >::: test_valid_simple_tokens
         [
           ("(", Token.OpenBracket);
           (")", Token.CloseBracket);
           ("+", Token.Plus);
           ("-", Token.Minus);
           ("*", Token.Star);
           ("/", Token.Slash);
           ("=", Token.Equals);
           (":", Token.Colon);
           (";", Token.Semicolon);
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
             ("data", DataKeyword);
             ("match", MatchKeyword);
             ("with", WithKeyword);
           ]
       @ test_valid_identifier_tokens
           [ "x"; "_"; "identifier"; "_identifier"; "noT"; "iF"; "x0" ]
       @ test_valid_capitalised_identifier_tokens
           [ "Foo"; "BAR"; "Else"; "X"; "A123" ]
       @ test_valid_location_identifier_tokens [ "@stdout"; "@stdin"; "@CAPS" ]
       @ test_valid_string_literal_tokens
           [ "\"\""; "\"Hello, world!\""; "\"123\"" ]
       @ test_valid_number_literal_tokens
           [
             "0"; "10"; "123456789"; "0.1"; "1."; ".1"; "135.790001000300050007";
           ]
       @ [
           test_valid_token_stream "repeated = signs" " ===== "
             [
               { kind = Token.Equiv; pos = { start = 1; finish = 3 } };
               { kind = Token.Equiv; pos = { start = 3; finish = 5 } };
               { kind = Token.Equals; pos = { start = 5; finish = 6 } };
             ];
           test_valid_token_stream "multiple string literals"
             "\"abc\"\"def\nghi\""
             [
               { kind = Token.StringLiteral; pos = { start = 0; finish = 5 } };
               { kind = Token.StringLiteral; pos = { start = 5; finish = 14 } };
             ];
         ]
       @ [
           test_valid_token "ignore comments" "\n#hello\n#world\n if"
             { kind = Token.IfKeyword; pos = { start = 16; finish = 18 } };
         ]

let () = run_test_tt_main tests
