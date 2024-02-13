type t = { source_code : string; pos : Position.t }

let init source_code = { source_code; pos = { start = 0; finish = 0 } }

let is_identifier = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_whitespace = function ' ' | '\n' | '\t' -> true | _ -> false

(* Get the next character in the input stream without incrementing the current
   position in the stream. *)
let peek lexer =
  if lexer.pos.finish < String.length lexer.source_code then
    Some lexer.source_code.[lexer.pos.finish]
  else None

(* Increment the current position in the input stream. *)
let advance lexer =
  { lexer with pos = { lexer.pos with finish = lexer.pos.finish + 1 } }

let conditionally_advance cond ~if_match ~otherwise lexer =
  match peek lexer with
  | Some peeked when cond peeked -> (advance lexer, if_match)
  | _ -> (lexer, otherwise)

let rec handle_string lexer =
  match peek lexer with
  | Some '"' -> (advance lexer, Token.StringLiteral)
  | Some _ -> lexer |> advance |> handle_string
  | None -> Err.raise_unexpected_eof ()

let rec handle_number ?(is_decimal = false) lexer =
  match peek lexer with
  | Some peeked when is_digit peeked ->
      lexer |> advance |> handle_number ~is_decimal
  | Some '.' when not is_decimal ->
      lexer |> advance |> handle_number ~is_decimal:true
  | _ -> (lexer, Token.NumberLiteral)

let rec handle_identifier lexer =
  match peek lexer with
  | Some peeked when is_identifier peeked ->
      lexer |> advance |> handle_identifier
  | _ ->
      let lexeme = Position.substring lexer.source_code lexer.pos in
      (lexer, Token.lookup_identifier_or_keyword lexeme)

let token_kind c lexer =
  let open Token in
  let lexer = advance lexer in
  match c with
  | '(' -> (lexer, OpenBracket)
  | ')' -> (lexer, CloseBracket)
  | '{' -> (lexer, OpenCurly)
  | '}' -> (lexer, CloseCurly)
  | '[' -> (lexer, OpenSquare)
  | ']' -> (lexer, CloseSquare)
  | '+' -> (lexer, Plus)
  | '-' ->
      lexer
      |> conditionally_advance (( = ) '>') ~if_match:Arrow ~otherwise:Minus
  | '*' -> (lexer, Star)
  | '/' -> (lexer, Slash)
  | '=' ->
      lexer
      |> conditionally_advance (( = ) '=') ~if_match:Equiv ~otherwise:Equals
  | ':' -> (lexer, Colon)
  | ';' -> (lexer, Semicolon)
  | '.' -> (lexer, Dot)
  | ',' -> (lexer, Comma)
  | '!' ->
      lexer
      |> conditionally_advance (( = ) '=') ~if_match:NotEquiv
           ~otherwise:Exclamation
  | '>' ->
      lexer
      |> conditionally_advance (( = ) '=') ~if_match:GreaterThanOrEqual
           ~otherwise:GreaterThan
  | '<' ->
      lexer
      |> conditionally_advance (( = ) '=') ~if_match:LessThanOrEqual
           ~otherwise:LessThan
  | '|' -> (lexer, Pipe)
  | '@' -> (lexer, At)
  | '"' -> handle_string lexer
  | c when is_digit c -> handle_number lexer
  | c when is_identifier c -> handle_identifier lexer
  | _ -> Err.raise_lexical_error c lexer.pos

(* Advance until the first non-whitespace character is found (or EOF). *)
let rec skip_whitespace lexer =
  match peek lexer with
  | Some peeked when is_whitespace peeked -> lexer |> advance |> skip_whitespace
  | _ -> lexer

(* Set the lexer's internal start position to the end position value. *)
let reset_start_position lexer =
  let offset = lexer.pos.finish in
  { lexer with pos = { start = offset; finish = offset } }

let token lexer =
  let lexer = lexer |> skip_whitespace |> reset_start_position in
  peek lexer
  |> Option.map (fun c ->
         let lexer, kind = token_kind c lexer in
         let token : Token.t = { kind; pos = lexer.pos } in
         (lexer, token))
