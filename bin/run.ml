open Frog

let debug_tokens path tokens =
  if path <> "" then
    let text = tokens |> List.map Token.show |> String.concat "\n" in
    Out_channel.with_open_text path (fun chan ->
        Out_channel.output_string chan text)
  else ()

let debug_ast path ast =
  let top_level_to_edge top_level = Tree.edge (Ast.to_tree_vertex top_level) in
  if path <> "" then
    let tree =
      Tree.vertex
        ~edges:(List.map top_level_to_edge ast)
        "top-level definitions"
      |> Tree.init
    in
    Out_channel.with_open_text path (fun chan ->
        Out_channel.output_string chan (Tree.to_dot tree))
  else ()

let run input_path debug_tokens_path debug_ast_path =
  let source_code =
    try In_channel.with_open_text input_path In_channel.input_all
    with _ ->
      Printf.printf "Failed to read input file: %s\n" input_path;
      exit 0
  in
  try
    let tokens = Consume.get_tokens source_code in
    debug_tokens debug_tokens_path tokens;
    let ast = Consume.get_ast source_code tokens in
    debug_ast debug_ast_path ast;
    ast |> List.iter (fun node -> Ast.show node |> print_endline)
  with
  | Err.Exception err ->
      print_endline (Err.display ~filename:input_path ~source_code err)
  | Sys_error _ -> print_endline "Failed to write output files."
