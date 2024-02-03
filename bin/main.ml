open Osaka

let rec consume_tokens lexer line =
  match Lexer.next_token lexer with
  | lexer, Some (Ok tok) -> tok :: consume_tokens lexer line
  | _, Some (Error err) ->
      print_endline (Err.display "repl" line err);
      []
  | _ -> []

let rec repl () =
  let line = read_line () in
  print_endline "Tokens:";
  let lexer = Lexer.init line in
  let tokens = consume_tokens lexer line in
  List.iter (fun token -> print_endline (Token.show token)) tokens;
  print_endline "Expression:";
  let parser = Parser.init tokens in
  let msg =
    match Parser.parse_expr parser with
    | _, Some (Ok expr) -> Ast.Expr.show expr
    | _, Some (Error err) -> Err.display "repl" line err
    | _, None -> "invalid expr"
  in
  print_endline (msg ^ "\n");
  repl ()

let () = repl ()
