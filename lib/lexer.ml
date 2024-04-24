type t = { source_code : string; pos : Position.t }

let init source_code = { source_code; pos = { start = 0; finish = 0 } }

let is_identifier = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> true
  | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let is_whitespace = function ' ' | '\n' | '\t' -> true | _ -> false

(* Get the next character in the input stream without incrementing the current
   position in the stream. *)
let peek l =
  if l.pos.finish < String.length l.source_code then
    Some l.source_code.[l.pos.finish]
  else None

(* Increment the current position in the input stream. *)
let advance l = { l with pos = { l.pos with finish = l.pos.finish + 1 } }

(* Continually advance while the given condition remains true. *)
let rec advance_while should_advance l =
  match peek l with
  | Some c when should_advance c -> l |> advance |> advance_while should_advance
  | _ -> l

(* Advance if the condition is true and return the if_match token type.
   Otherwise, do not advance and return the otherwise token type. *)
let conditionally_advance cond ~if_match ~otherwise l =
  match peek l with
  | Some c when cond c -> (advance l, if_match)
  | _ -> (l, otherwise)

let rec handle_string l =
  match peek l with
  | Some '"' -> (advance l, Token.StringLiteral)
  | Some _ -> l |> advance |> handle_string
  | None -> Err.raise_unexpected_eof ()

let rec handle_number ?(is_decimal = false) l =
  match peek l with
  | Some c when is_digit c -> l |> advance |> handle_number ~is_decimal
  | Some '.' when not is_decimal ->
      l |> advance |> handle_number ~is_decimal:true
  | _ -> (l, Token.NumberLiteral)

let rec handle_identifier l =
  match peek l with
  | Some c when is_identifier c -> l |> advance |> handle_identifier
  | _ ->
      let lexeme = Position.substring l.source_code l.pos in
      (l, Token.lookup_identifier_or_keyword lexeme)

let token_kind c l =
  let open Token in
  let l = advance l in
  match c with
  | '(' -> (l, OpenBracket)
  | ')' -> (l, CloseBracket)
  | '+' -> (l, Plus)
  | '-' ->
      l |> conditionally_advance (( = ) '>') ~if_match:Arrow ~otherwise:Minus
  | '*' -> (l, Star)
  | '/' -> (l, Slash)
  | '=' ->
      l |> conditionally_advance (( = ) '=') ~if_match:Equiv ~otherwise:Equals
  | ':' -> (l, Colon)
  | ';' -> (l, Semicolon)
  | ',' -> (l, Comma)
  | '!' ->
      l
      |> conditionally_advance (( = ) '=') ~if_match:NotEquiv
           ~otherwise:Exclamation
  | '>' ->
      l
      |> conditionally_advance (( = ) '=') ~if_match:GreaterThanOrEqual
           ~otherwise:GreaterThan
  | '<' ->
      l
      |> conditionally_advance (( = ) '=') ~if_match:LessThanOrEqual
           ~otherwise:LessThan
  | '|' -> (l, Pipe)
  | '"' -> handle_string l
  | '@' -> handle_identifier l
  | c when is_digit c || c = '.' -> handle_number l
  | c when is_identifier c -> handle_identifier l
  | _ -> Err.raise_lexical_error c l.pos

(* Discard whitespace characters and comments (which start with a '#' and end
   with a newline. *)
let rec discard_characters l =
  match peek l with
  | Some '#' -> l |> advance_while (( <> ) '\n') |> discard_characters
  | Some c when is_whitespace c ->
      l |> advance_while is_whitespace |> discard_characters
  | _ -> l

(* Set the lexer's internal start position to the end position value. *)
let reset_start_position l =
  let offset = l.pos.finish in
  { l with pos = { start = offset; finish = offset } }

let token l =
  let l = l |> discard_characters |> reset_start_position in
  peek l
  |> Option.map (fun c ->
         let l, kind = token_kind c l in
         let token : Token.t = { kind; pos = l.pos } in
         (l, token))
