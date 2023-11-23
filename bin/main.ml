open Osaka

let rec consume_tokens lexer =
  match Lexer.next_token lexer with
  | lexer, Some tok -> tok :: consume_tokens lexer
  | _ -> []

let rec repl () =
  let line = read_line () in
  let lexer = Lexer.init line in
  let tokens = consume_tokens lexer in
  print_endline "Tokens:";
  List.iter (fun token -> print_endline (Token.show token)) tokens;
  let parser = Parser.init tokens in
  let msg =
    match Parser.parse_expr parser with
    | _, Some expr -> Ast.show_expr_simple expr
    | _, None -> "invalid expr"
  in
  print_endline ("\nExpression:\n" ^ msg);
  repl ()

let () = repl ()
