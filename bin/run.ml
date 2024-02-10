open Osaka

let rec consume ~next obj =
  Result.bind (next obj) (function
    | Some (obj, head) ->
        consume ~next obj |> Result.map (fun tail -> head :: tail)
    | None -> Ok [])

let get_tokens source_code =
  consume ~next:Lexer.next_token (Lexer.init source_code)

let get_ast source_code tokens =
  consume ~next:Parser.next_ast (Parser.init source_code tokens)

let debug_tokens path tokens =
  if path <> "" then
    let text = tokens |> List.map Token.show |> String.concat "\n" in
    Out_channel.with_open_text path (fun chan ->
        Out_channel.output_string chan text)
  else ()

let debug_ast path expr =
  if path <> "" then
    let text = expr |> Ast.to_tree |> Tree.to_dot in
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
    let ast_result =
      Result.bind tokens_result (fun tokens ->
          debug_tokens debug_tokens_path tokens;
          get_ast source_code tokens)
    in
    match ast_result with
    | Ok ast ->
        debug_ast debug_ast_path (List.hd ast);
        ast |> List.iter (fun node -> Ast.show node |> print_endline)
    | Error err -> Err.display input_path source_code err |> print_endline
  with _ -> print_endline "Failed to write debug output files."
