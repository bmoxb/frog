open Osaka

let get_tokens source_code =
  let rec consume lexer =
    match Lexer.next_token lexer with
    | lexer, Some result ->
        Result.bind result (fun token ->
            consume lexer |> Result.map (fun tokens -> token :: tokens))
    | _, None -> Ok []
  in
  consume (Lexer.init source_code)

let debug_tokens path tokens =
  if path <> "" then
    let text = tokens |> List.map Token.show |> String.concat "\n" in
    Out_channel.with_open_text path (fun chan ->
        Out_channel.output_string chan text)
  else ()

let get_exprs source_code tokens =
  let rec consume parser =
    match Parser.parse_expr parser with
    | parser, Some result ->
        Result.bind result (fun expr ->
            consume parser |> Result.map (fun exprs -> expr :: exprs))
    | _, None -> Ok []
  in
  consume (Parser.init source_code tokens)

let debug_ast path expr =
  if path <> "" then
    let text = expr |> Ast.Expr.to_tree |> Tree.to_dot in
    Out_channel.with_open_text path (fun chan ->
        Out_channel.output_string chan text)
  else ()

let run input_path debug_tokens_path debug_ast_path =
  let source_code =
    try In_channel.with_open_text input_path In_channel.input_all
    with _ ->
      Printf.printf "Failed to read input file: %s\n" input_path;
      exit 0
  in
  let tokens_result = get_tokens source_code in
  try
    let exprs_result =
      Result.bind tokens_result (fun tokens ->
          debug_tokens debug_tokens_path tokens;
          get_exprs source_code tokens)
    in
    match exprs_result with
    | Ok exprs ->
        debug_ast debug_ast_path (List.hd exprs);
        exprs |> List.iter (fun expr -> Ast.Expr.show expr |> print_endline)
    | Error err -> Err.display input_path source_code err |> print_endline
  with _ -> print_endline "Failed to write debug output files."
