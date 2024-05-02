type t = { source_code : string; tokens : Token.t list }

let init source_code tokens = { source_code; tokens }

(* See the token kind of the next token without changing the current position
   in the token stream. *)
let peek_kind p =
  match p.tokens with token :: _ -> Some token.kind | [] -> None

(* Return the next token in the token stream and advance the current position
   by one. *)
let advance p =
  match p.tokens with
  | token :: tail -> ({ p with tokens = tail }, token)
  | [] -> Err.raise_unexpected_eof ()

(* Advance and discard the returned token. *)
let advance_and_discard p =
  let p, _ = advance p in
  p

(* If the next token has the given token kind, advance the current position.
   Otherwise, produce an error. *)
let expect_kind kind error_msg p =
  let p, token = advance p in
  if token.kind = kind then (p, token)
  else Err.raise_syntax_error token error_msg

(* Same as expect_kind but also returns the token's lexeme. *)
let expect_kind_with_lexeme kind error_msg p =
  let p, token = expect_kind kind error_msg p in
  let lexeme = Token.lexeme p.source_code token in
  (p, token, lexeme)

(* Call the given syntax rule function and if some unwrap and return the
   result. If none then just produce a syntax error. *)
let expect_or_syntax_error rule error_msg p =
  match rule p with
  | Some result -> result
  | None ->
      let _, token = advance p in
      Err.raise_syntax_error token error_msg

(* Parse using a rule any number (0 or more) times. Returns the last node (if
    any) along with the list of all nodes. *)
let any_number_of rule p =
  let rec traverse ~last p =
    match rule p with
    | Some (p, head) ->
        let p, last, tail = traverse ~last:(Some head) p in
        (p, last, head :: tail)
    | None -> (p, last, [])
  in
  traverse ~last:None p

(* If the next token is an identifier, consume it and return the lexeme.
   Otherwise, do nothing. *)
let parse_identifier p =
  Option.bind (peek_kind p) (function
    | Identifier ->
        let p, token = advance p in
        Some (p, Token.lexeme p.source_code token)
    | _ -> None)

(* Helper function for parsing match and data arms. *)
let parse_arms parse_arm p =
  (* Parse an arm with a pipe '|' token required (so for all arms after the
     first). *)
  let parse_arm_with_pipe p =
    Option.bind (peek_kind p) (function
      | Pipe -> Some (parse_arm (advance_and_discard p))
      | _ -> None)
  in
  (* The first pipe is optional. *)
  let p =
    match peek_kind p with Some Pipe -> advance_and_discard p | _ -> p
  in
  let p, head_arm = parse_arm p in
  let p, last_arm_opt, tail_arms = any_number_of parse_arm_with_pipe p in
  let last_arm = Option.value ~default:head_arm last_arm_opt in
  (p, last_arm, head_arm :: tail_arms)

let token_kind_to_simple_type_kind =
  let open Ast.DataType in
  function
  | Token.Identifier -> Some Identifier
  | Token.LocationIdentifier -> Some Location
  | _ -> None

(* type = simple_type, [ { simple_type }, "->", simple_type, { simple_type } ] *)
let rec data_type p =
  (* Either map a simple type into a function type (if possible) or return as
     is. *)
  let try_into_function_type (p, in_head) =
    let p, _, in_tail = any_number_of simple_type p in
    (* If there are no following simple types or a '->' token, then this must
       just be a single simple type rather than a function type. *)
    if List.is_empty in_tail && peek_kind p <> Some Arrow then (p, in_head)
    else
      let p, _ = expect_kind Arrow "Expected '->' in function type." p in
      let p, (out_head : Ast.DataType.t) = expect_simple_type p in
      let p, last_simple_type_opt, out_tail = any_number_of simple_type p in
      (* Set up a function data type AST node. *)
      let last_pos =
        (last_simple_type_opt |> Option.value ~default:out_head).pos
      in
      let inputs, outputs = (in_head :: in_tail, out_head :: out_tail) in
      let node : Ast.DataType.t =
        {
          pos = Position.merge in_head.pos last_pos;
          kind = Ast.DataType.Function { inputs; outputs };
        }
      in
      (p, node)
  in
  simple_type p |> Option.map try_into_function_type

and expect_data_type p = expect_or_syntax_error data_type "Expected a type." p

(* simple_type = IDENTIFIER | LOCATION_IDENTIFIER *)
and simple_type p =
  Option.bind (peek_kind p) token_kind_to_simple_type_kind
  |> Option.map (fun kind ->
         let p, token = advance p in
         let identifier = Token.lexeme p.source_code token in
         let node : Ast.DataType.t =
           { pos = token.pos; kind = Ast.DataType.Simple (kind, identifier) }
         in
         (p, node))

and expect_simple_type p =
  expect_or_syntax_error simple_type "Expected a simple type." p

let token_kind_to_binary_operator =
  let open Ast.Expr in
  function
  | Token.Semicolon -> Some Chain
  | Token.Comma -> Some MultiReturn
  | Token.AndKeyword -> Some And
  | Token.OrKeyword -> Some Or
  | Token.Equiv -> Some Equiv
  | Token.NotEquiv -> Some NotEquiv
  | Token.GreaterThan -> Some GreaterThan
  | Token.LessThan -> Some LessThan
  | Token.GreaterThanOrEqual -> Some GreaterThanOrEqual
  | Token.LessThanOrEqual -> Some LessThanOrEqual
  | Token.Plus -> Some Add
  | Token.Minus -> Some Subtract
  | Token.Star -> Some Multiply
  | Token.Slash -> Some Divide
  | _ -> None

let token_kind_to_unary_operator =
  let open Ast.Expr in
  function
  | Token.NotKeyword -> Some Not | Token.Minus -> Some Negate | _ -> None

let token_kind_to_primary_kind =
  let open Ast.Expr in
  function
  | Token.NumberLiteral -> Some NumberLiteral
  | Token.StringLiteral -> Some StringLiteral
  | Token.Identifier -> Some Identifier
  | Token.LocationIdentifier -> Some Location
  | Token.CapitalisedIdentifier -> Some Constructor
  | _ -> None

(* expr = let_in | if_then_else | match | chain_expr *)
let rec expr p : t * Ast.Expr.t =
  match peek_kind p with
  | Some LetKeyword -> let_in p
  | Some IfKeyword -> if_then_else p
  | Some MatchKeyword -> match_with p
  | _ -> chain_expr p

(* let_in = let, "in", expr *)
and let_in ?(last_rule = expr) p =
  let p, start, info, bound_expr = parse_let p in
  let p, _ = expect_kind InKeyword "Expected 'in' keyword." p in
  let p, in_expr = last_rule p in
  let node : Ast.Expr.t =
    {
      pos = { start; finish = in_expr.pos.finish };
      kind = Ast.Expr.LetIn { info; bound_expr; in_expr };
    }
  in
  (p, node)

(* if_then_else = "if", expr, "then", expr, "else", expr *)
and if_then_else ?(last_rule = expr) p =
  let p, if_token = advance p in
  let p, condition_expr = expr p in
  let p, _ =
    expect_kind ThenKeyword
      "Expected 'then' keyword after condition in if-then-else expression." p
  in
  let p, then_expr = expr p in
  let p, _ =
    expect_kind ElseKeyword
      "Expected an 'else' clause as part of an if-then-else expression." p
  in
  let p, else_expr = last_rule p in
  let node : Ast.Expr.t =
    {
      pos = Position.merge if_token.pos else_expr.pos;
      kind = Ast.Expr.IfThenElse { condition_expr; then_expr; else_expr };
    }
  in
  (p, node)

(* match = "match", expr, "with", [ "|" ], match_arm, { "|", match_arm } *)
and match_with p =
  let p, match_token = advance p in
  let p, condition_expr = expr p in
  let p, _ =
    expect_kind WithKeyword
      "Expected 'then' keyword after condition in match expression." p
  in
  let p, last_arm, arms = parse_arms match_arm p in
  let node : Ast.Expr.t =
    {
      pos = Position.merge match_token.pos last_arm.arm_pos;
      kind = Ast.Expr.Match (condition_expr, arms);
    }
  in
  (p, node)

(* match_arm = CAPITALISED_IDENTIFIER, { IDENTIFIER }, "->", arm_expr *)
and match_arm p : t * Ast.Expr.match_arm =
  let p, constructor_token, constructor =
    expect_kind_with_lexeme CapitalisedIdentifier
      "Expected a constructor to match on." p
  in
  let p, _, parameters = any_number_of parse_identifier p in
  let p, _ =
    expect_kind Arrow "Expected an arrow '->' token after pattern in match arm."
      p
  in
  let p, expr = arm_expr p in
  let arm_pos = Position.merge constructor_token.pos expr.pos in
  let arm : Ast.Expr.match_arm = { arm_pos; constructor; parameters; expr } in
  (p, arm)

(* arm_expr = let, "in", arm_expr
            | "if", expr, "then", expr, "else", arm_expr
            | chain_expr *)
and arm_expr p : t * Ast.Expr.t =
  match peek_kind p with
  | Some LetKeyword -> let_in ~last_rule:arm_expr p
  | Some IfKeyword -> if_then_else ~last_rule:arm_expr p
  | _ -> chain_expr p

(* chain_expr = multi_return, [ ";", chain_expr ] *)
and chain_expr p =
  left_associative_binary_expr multi_return (( = ) Ast.Expr.Chain) p

and multi_return p =
  left_associative_binary_expr logical_or (( = ) Ast.Expr.MultiReturn) p

(* logical_or = logical_and, { "or", logical_and } *)
and logical_or p =
  left_associative_binary_expr logical_and (( = ) Ast.Expr.Or) p

(* logical_and = equality, { "and", equality } *)
and logical_and p = left_associative_binary_expr equality (( = ) Ast.Expr.And) p

(* equality = comparison, { ( "==" | "!=" ), comparison } *)
and equality p =
  let open Ast.Expr in
  let is_wanted_op = function Equiv | NotEquiv -> true | _ -> false in
  left_associative_binary_expr comparison is_wanted_op p

(* comparison = term, { ( ">" | "<" | ">=" | "<=" ), term } *)
and comparison p =
  let open Ast.Expr in
  let is_wanted_op = function
    | GreaterThan | LessThan | GreaterThanOrEqual | LessThanOrEqual -> true
    | _ -> false
  in
  left_associative_binary_expr term is_wanted_op p

(* term = factor, { ( "+" | "-" ), factor } *)
and term p =
  let open Ast.Expr in
  let is_wanted_op = function Add | Subtract -> true | _ -> false in
  left_associative_binary_expr factor is_wanted_op p

(* factor = unary, { ( "*" | "/" ), unary } *)
and factor p =
  let open Ast.Expr in
  let is_wanted_op = function Multiply | Divide -> true | _ -> false in
  left_associative_binary_expr unary is_wanted_op p

(* unary = ( "not" | "-" ), unary | application *)
and unary p =
  match Option.bind (peek_kind p) token_kind_to_unary_operator with
  | Some op ->
      let p, op_token = advance p in
      let p, expr = unary p in
      let node : Ast.Expr.t =
        {
          pos = Position.merge op_token.pos expr.pos;
          kind = Ast.Expr.UnaryOp (op, expr);
        }
      in
      (p, node)
  | None -> application p

(* application = primary, { primary } *)
and application p =
  let p, (head_expr : Ast.Expr.t) = expect_primary p in
  let p, last_primary_opt, tail_exprs = any_number_of primary p in
  let pos =
    match last_primary_opt with
    | Some (last : Ast.Expr.t) -> Position.merge head_expr.pos last.pos
    | None -> head_expr.pos
  in
  if List.is_empty tail_exprs then (p, head_expr)
  else
    let node : Ast.Expr.t =
      { pos; kind = Ast.Expr.Application (head_expr, tail_exprs) }
    in
    (p, node)

(* primary = NUMBER_LITERAL | STRING_LITERAL | IDENTIFIER | LOCATION_IDENTIFIER
           | CAPITALISED_IDENTIFIER | grouping *)
and primary p =
  match Option.bind (peek_kind p) token_kind_to_primary_kind with
  | Some primary_kind ->
      let p, token = advance p in
      let literal = Token.lexeme p.source_code token in
      let node : Ast.Expr.t =
        { pos = token.pos; kind = Ast.Expr.Primary (primary_kind, literal) }
      in
      Some (p, node)
  | None -> grouping p

and expect_primary p =
  expect_or_syntax_error primary "Expected primary expression." p

(*  grouping = "(", expr, ")" *)
and grouping p =
  Option.bind (peek_kind p) (function
    | OpenBracket ->
        let p, open_token = advance p in
        let p, expr = expr p in
        let p, close_token =
          expect_kind CloseBracket "Expected closing ')' token." p
        in
        let node : Ast.Expr.t =
          {
            pos = Position.merge open_token.pos close_token.pos;
            kind = Ast.Expr.Grouping expr;
          }
        in
        Some (p, node)
    | _ -> None)

(* Helper function for parsing left associative binary expressions. *)
and left_associative_binary_expr child_expr is_wanted_op p =
  let p, left_expr = child_expr p in
  match Option.bind (peek_kind p) token_kind_to_binary_operator with
  | Some op when is_wanted_op op ->
      (* Consume the operator token. *)
      let p = advance_and_discard p in
      let p, right_expr =
        left_associative_binary_expr child_expr is_wanted_op p
      in
      let node : Ast.Expr.t =
        {
          pos = Position.merge left_expr.pos right_expr.pos;
          kind = Ast.Expr.BinOp (op, left_expr, right_expr);
        }
      in
      (p, node)
  | _ -> (p, left_expr)

(* Helper function for parsing a top-level let definition or the first part of
   a let-in expression. Returns the start position of the let keyword token
   (for position tracking), the binding info, and the bound expression. *)
and parse_let p =
  let p, let_token = advance p in
  let p, _, name =
    expect_kind_with_lexeme Identifier "Expected name to bind to." p
  in
  let p, _, parameters = any_number_of parse_identifier p in
  let p, _ =
    expect_kind Colon
      "Expected ':' token and a type for the binding being introduced." p
  in
  let p, data_type = expect_data_type p in
  let p, _ = expect_kind Equals "Expected '=' token." p in
  let p, bound_expr = expr p in
  let info : Ast.binding_info = { name; parameters; data_type } in
  (p, let_token.pos.start, info, bound_expr)

(* let = "let", pattern, { pattern }, ":", type, "=", expr *)
let let_binding p =
  let p, start, info, bound_expr = parse_let p in
  let node : Ast.t =
    {
      pos = { start; finish = bound_expr.pos.finish };
      kind = Ast.Let (info, bound_expr);
    }
  in
  (p, node)

(* data_arm = CAPITALISED_IDENTIFIER, { simple_type } *)
let data_arm p =
  let p, token, constructor =
    expect_kind_with_lexeme CapitalisedIdentifier
      "Expected a constructor identifier." p
  in
  let p, last_data_type_opt, data_types = any_number_of simple_type p in
  let arm_pos =
    match last_data_type_opt with
    | Some last -> Position.merge token.pos last.pos
    | None -> token.pos
  in
  let arm : Ast.data_arm = { arm_pos; constructor; data_types } in
  (p, arm)

(* data = "data", IDENTIFIER, "=", [ "|" ], data_arm, { "|", data_arm } *)
let data_definition p =
  let p, data_token = advance p in
  let p, _, identifier =
    expect_kind_with_lexeme Identifier
      "Expected an name for the data definition." p
  in
  let p, _ = expect_kind Equals "Expected '=' token." p in
  let p, last_arm, arms = parse_arms data_arm p in
  let node : Ast.t =
    {
      pos = Position.merge data_token.pos last_arm.arm_pos;
      kind = Ast.Data (identifier, arms);
    }
  in
  (p, node)

(* top_level = let | data *)
let top_level p =
  let match_kind = function
    | Token.LetKeyword -> let_binding p
    | Token.DataKeyword -> data_definition p
    | _ ->
        let _, unexpected_token = advance p in
        let lexeme = Token.lexeme p.source_code unexpected_token in
        let msg =
          Printf.sprintf "Expected a top-level definition but got token '%s'."
            lexeme
        in
        Err.raise_syntax_error unexpected_token msg
  in
  peek_kind p |> Option.map match_kind
