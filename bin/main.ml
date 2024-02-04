let input_path = ref ""

let debug_tokens_path = ref ""

let debug_ast_path = ref ""

let speclist =
  [
    ( "--debug-tokens",
      Arg.Set_string debug_tokens_path,
      "Write token debug information to the given file." );
    ( "--debug-ast",
      Arg.Set_string debug_ast_path,
      "Write the AST in Graphviz DOT format to the given file." );
  ]

let anon_fun path = input_path := path

let usage_msg = "osaka [--debug-tokens <file>] [--debug-ast <file>] <input>"

let () =
  Arg.parse speclist anon_fun usage_msg;
  if !input_path = "" then print_endline "Please specify an input file."
  else Run.run !input_path !debug_tokens_path !debug_ast_path
