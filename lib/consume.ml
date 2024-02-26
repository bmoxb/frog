let rec consume ~next obj =
  next obj
  |> Option.map (fun (obj, head) -> head :: consume ~next obj)
  |> Option.value ~default:[]

let get_tokens source_code = consume ~next:Lexer.token (Lexer.init source_code)

let get_ast source_code tokens =
  consume ~next:Parser.top_level (Parser.init source_code tokens)
