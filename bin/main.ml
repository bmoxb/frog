open Osaka

let rec consume_tokens lexer =
  match Lexer.next_token lexer with
  | lexer, Some tok -> tok :: consume_tokens lexer
  | _ -> []

let rec repl () =
  let line = read_line () in
  let lexer = Lexer.init line in
  let tokens = consume_tokens lexer in
  List.iter (fun token -> print_endline (Token.show token)) tokens;
  repl ()

let () = repl ()
