type t = { source_code : string; tokens : Token.t list }

let init source_code tokens = { source_code; tokens }

(* See the token kind of the next token without changing the current position
   in the token stream. *)
let peek_kind parser =
  match parser.tokens with token :: _ -> Some token.kind | [] -> None

(* Return the next token in the token stream and advance the current position
   by one. *)
let advance parser =
  match parser.tokens with
  | token :: tail -> ({ parser with tokens = tail }, token)
  | [] -> Err.raise_unexpected_eof ()

(* If the next token has the given token kind, advance the current position.
   Otherwise, produce an error. *)
let expect_kind kind error_msg parser =
  let parser, token = advance parser in
  if token.kind = kind then (parser, token)
  else Err.raise_syntax_error token error_msg

(* Call the given syntax rule function and if some unwrap and return the
   result. If none then just produce a syntax error. *)
let expect_or_syntax_error rule error_msg parser =
  match rule parser with
  | Some result -> result
  | None ->
      let _, token = advance parser in
      Err.raise_syntax_error token error_msg

(* Parse using a rule any number (0 or more) times. Returns the last node (if
    any) along with the list of all nodes. *)
let any_number_of rule parser =
  let rec traverse ~last parser =
    match rule parser with
    | Some (parser, head) ->
        let parser, last, tail = traverse ~last parser in
        (parser, last, head :: tail)
    | None -> (parser, last, [])
  in
  traverse ~last:None parser

(* Helper function for parsing match and data arms. *)
let parse_arms parse_arm parser =
  (* Parse an arm with a pipe '|' token required (so for all arms after the
     first). *)
  let parse_arm_with_pipe parser =
    Option.bind (peek_kind parser) (function
      | Pipe ->
          let parser, _ = advance parser in
          Some (parse_arm parser)
      | _ -> None)
  in
  (* The first pipe is optional. *)
  let parser =
    match peek_kind parser with
    | Some Token.Pipe ->
        let parser, _ = advance parser in
        parser
    | _ -> parser
  in
  let parser, head_arm = parse_arm parser in
  let parser, last_arm_opt, tail_arms =
    any_number_of parse_arm_with_pipe parser
  in
  (parser, Option.value ~default:head_arm last_arm_opt, head_arm :: tail_arms)

(* type = simple_type [ { simple_type } "->" simple_type { simple_type } ] *)
let rec data_type parser =
  (* Either map a simple type into a function type (if possible) or return as
     is. *)
  let maybe_into_function_type (parser, (head : Ast.DataType.t)) =
    let parser, _, tail = any_number_of simple_type parser in
    (* If there are no following simple types or a '->' token, then this must
       just be a single simple type rather than a function type. *)
    if List.is_empty tail && peek_kind parser <> Some Token.Arrow then
      (parser, head)
    else
      let parser, _ =
        expect_kind Token.Arrow "Expected '->' in function type." parser
      in
      let parser, (return_head : Ast.DataType.t) = expect_simple_type parser in
      let parser, last_return_opt, return_tail =
        any_number_of simple_type parser
      in
      let finish =
        (last_return_opt |> Option.value ~default:return_head).pos.finish
      in
      let node : Ast.DataType.t =
        {
          pos = { start = head.pos.start; finish };
          kind = Ast.DataType.Function (head :: tail, return_head :: return_tail);
        }
      in
      (parser, node)
  in
  simple_type parser |> Option.map maybe_into_function_type

and expect_data_type parser =
  expect_or_syntax_error data_type "Expected a type." parser

(* simple_type = IDENTIFIER | LOCATION_IDENTIFIER *)
and simple_type parser =
  Option.bind (peek_kind parser) Ast.DataType.token_kind_to_simple_kind
  |> Option.map (fun kind ->
         let parser, token = advance parser in
         let identifier = Token.lexeme parser.source_code token in
         let node : Ast.DataType.t =
           { pos = token.pos; kind = Ast.DataType.Simple (kind, identifier) }
         in
         (parser, node))

and expect_simple_type parser =
  expect_or_syntax_error simple_type "Expected a simple type." parser

(* pattern = identifier_pattern | type_constructor_pattern | literal_pattern *)
let rec pattern parser =
  Option.bind (peek_kind parser) (function
    | Identifier -> Some (identifier_pattern parser)
    | CapitalisedIdentifier -> Some (type_constructor_pattern parser)
    | NumberLiteral | StringLiteral -> Some (literal_pattern parser)
    | _ -> None)

(* identifier_pattern = IDENTIFER *)
and identifier_pattern parser =
  let parser, token = advance parser in
  let identifier = Token.lexeme parser.source_code token in
  let node : Ast.Pattern.t =
    { pos = token.pos; kind = Ast.Pattern.Identifier identifier }
  in
  (parser, node)

(* type_constructor_pattern = CAPITALISED_IDENTIFIER [ pattern ] *)
and type_constructor_pattern parser =
  let parser, token = advance parser in
  let identifier = Token.lexeme parser.source_code token in
  match pattern parser with
  | Some (parser, argument) ->
      let node : Ast.Pattern.t =
        {
          pos = { start = token.pos.start; finish = argument.pos.finish };
          kind = Ast.Pattern.TypeConstructor (identifier, Some argument);
        }
      in
      (parser, node)
  | None ->
      let node : Ast.Pattern.t =
        {
          pos = token.pos;
          kind = Ast.Pattern.TypeConstructor (identifier, None);
        }
      in
      (parser, node)

(* literal_pattern = NUMBER_LITERAL | STRING_LITERAL *)
and literal_pattern parser =
  let parser, token = advance parser in
  let literal = Token.lexeme parser.source_code token in
  (parser, { pos = token.pos; kind = Ast.Pattern.Literal literal })

let expect_pattern parser =
  expect_or_syntax_error pattern "Expected a pattern." parser

(* expr = core_expr [ ";" expr ] *)
let rec expr parser =
  let parser, (lhs : Ast.Expr.t) = core_expr parser in
  match peek_kind parser with
  | Some Token.Semicolon ->
      (* Consume and discard the semicolon. *)
      let parser, _ = advance parser in
      let parser, (rhs : Ast.Expr.t) = expr parser in
      let node : Ast.Expr.t =
        {
          pos = { start = lhs.pos.start; finish = rhs.pos.finish };
          kind = Ast.Expr.Chain (lhs, rhs);
        }
      in
      (parser, node)
  | _ -> (parser, lhs)

(* core_expr = let_in | match | if_then_else | logical_or *)
and core_expr parser =
  match peek_kind parser with
  | Some LetKeyword -> let_in parser
  | Some MatchKeyword -> match_with parser
  | Some IfKeyword -> if_then_else parser
  | _ -> logical_or parser

(* let_in = let "in" expr *)
and let_in parser =
  let parser, start, patterns, data_type, bound_expr = parse_let parser in
  let parser, _ = expect_kind InKeyword "Expected 'in' keyword." parser in
  let parser, body_expr = expr parser in
  let node : Ast.Expr.t =
    {
      pos = { start; finish = body_expr.pos.finish };
      kind = Ast.Expr.LetIn (patterns, data_type, bound_expr, body_expr);
    }
  in
  (parser, node)

(* match = "match" expr "with" [ "|" ] match_arm { "|" match_arm } *)
and match_with parser =
  let parser, match_token = advance parser in
  let parser, condition = expr parser in
  let parser, _ =
    expect_kind Token.WithKeyword
      "Expected 'then' keyword after condition in match expression." parser
  in
  let parser, last_arm, arms = parse_arms match_arm parser in
  let (last_arm_pos : Position.t), _, _ = last_arm in
  let node : Ast.Expr.t =
    {
      pos = { start = match_token.pos.start; finish = last_arm_pos.finish };
      kind = Ast.Expr.Match (condition, arms);
    }
  in
  (parser, node)

(* match_arm = pattern "->" expr *)
and match_arm parser =
  let parser, pattern = expect_pattern parser in
  let parser, _ =
    expect_kind Token.Arrow
      "Expected an arrow '->' token after pattern in match arm." parser
  in
  let parser, body = expr parser in
  let pos : Position.t =
    { start = pattern.pos.start; finish = body.pos.finish }
  in
  let arm = (pos, pattern, body) in
  (parser, arm)

(* if_then_else = "if" expr "then" expr "else" expr *)
and if_then_else parser =
  let parser, if_token = advance parser in
  let parser, condition = expr parser in
  let parser, _ =
    expect_kind Token.ThenKeyword
      "Expected 'then' keyword after condition in if-then-else expression."
      parser
  in
  let parser, then_expr = expr parser in
  let parser, _ =
    expect_kind Token.ElseKeyword
      "Expected an 'else' clause as part of an if-then-else expression." parser
  in
  let parser, else_expr = expr parser in
  let node : Ast.Expr.t =
    {
      pos = { start = if_token.pos.start; finish = else_expr.pos.finish };
      kind = Ast.Expr.IfThenElse (condition, then_expr, else_expr);
    }
  in
  (parser, node)

(* logical_or = logical_and { "or" logical_and } *)
and logical_or parser =
  left_associative_binary_expr parser logical_and (( = ) Ast.Expr.Or)

(* logical_and = equality { "and" equality } *)
and logical_and parser =
  left_associative_binary_expr parser equality (( = ) Ast.Expr.And)

(* equality = comparison { ( "==" | "!=" ) comparison } *)
and equality parser =
  let open Ast.Expr in
  let is_wanted_op = function Equiv | NotEquiv -> true | _ -> false in
  left_associative_binary_expr parser comparison is_wanted_op

(* comparison = term { ( ">" | "<" | ">=" | "<=" ) term } *)
and comparison parser =
  let open Ast.Expr in
  let is_wanted_op = function
    | GreaterThan | LessThan | GreaterThanOrEqual | LessThanOrEqual -> true
    | _ -> false
  in
  left_associative_binary_expr parser term is_wanted_op

(* term = factor { ( "+" | "-" ) factor } *)
and term parser =
  let open Ast.Expr in
  let is_wanted_op = function Add | Subtract -> true | _ -> false in
  left_associative_binary_expr parser factor is_wanted_op

(* factor = unary { ( "*" | "/" ) unary } *)
and factor parser =
  let open Ast.Expr in
  let is_wanted_op = function Multiply | Divide -> true | _ -> false in
  left_associative_binary_expr parser unary is_wanted_op

(* unary = ( "not" | "-" ) unary | application *)
and unary parser =
  match
    Option.bind (peek_kind parser) Ast.Expr.token_kind_to_unary_operator
  with
  | Some op ->
      let parser, op_token = advance parser in
      let parser, expr = unary parser in
      let node : Ast.Expr.t =
        {
          pos = { start = op_token.pos.start; finish = expr.pos.finish };
          kind = Ast.Expr.UnaryOp (op, expr);
        }
      in
      (parser, node)
  | None -> application parser

(* application = primary { primary } *)
and application parser =
  let parser, (head_expr : Ast.Expr.t) = expect_primary parser in
  let parser, last_expr_opt, tail_exprs = any_number_of primary parser in
  let finish = (last_expr_opt |> Option.value ~default:head_expr).pos.finish in
  if List.is_empty tail_exprs then (parser, head_expr)
  else
    let node : Ast.Expr.t =
      {
        pos = { start = head_expr.pos.start; finish };
        kind = Ast.Expr.Application (head_expr, tail_exprs);
      }
    in
    (parser, node)

(* primary = NUMBER_LITERAL | STRING_LITERAL | IDENTIFIER | LOCATION_IDENTIFIER
           | grouping *)
and primary parser =
  match Option.bind (peek_kind parser) Ast.Expr.token_kind_to_primary_kind with
  | Some primary_kind ->
      let parser, token = advance parser in
      let literal = Token.lexeme parser.source_code token in
      let node : Ast.Expr.t =
        { pos = token.pos; kind = Ast.Expr.Primary (primary_kind, literal) }
      in
      Some (parser, node)
  | None -> grouping parser

and expect_primary parser =
  expect_or_syntax_error primary "Expected primary expression." parser

(*  grouping = "(" expr ")" *)
and grouping parser =
  match peek_kind parser with
  | Some OpenBracket ->
      let parser, open_token = advance parser in
      let parser, expr = expr parser in
      let parser, close_token =
        expect_kind CloseBracket "Expected closing ')' token." parser
      in
      let node : Ast.Expr.t =
        {
          pos =
            { start = open_token.pos.start; finish = close_token.pos.finish };
          kind = Ast.Expr.Grouping expr;
        }
      in
      Some (parser, node)
  | _ -> None

(* Helper function for parsing left associative binary expressions. *)
and left_associative_binary_expr parser child_expr is_wanted_op =
  let parser, left_expr = child_expr parser in
  let peeked_kind = peek_kind parser in
  match Option.bind peeked_kind Ast.Expr.token_kind_to_binary_operator with
  | Some op when is_wanted_op op ->
      (* Consume the operator token. *)
      let parser, _ = advance parser in
      let parser, right_expr =
        left_associative_binary_expr parser child_expr is_wanted_op
      in
      let node : Ast.Expr.t =
        {
          pos = { start = left_expr.pos.start; finish = right_expr.pos.finish };
          kind = Ast.Expr.BinOp (op, left_expr, right_expr);
        }
      in
      (parser, node)
  | _ -> (parser, left_expr)

(* Helper function for parsing a top-level let definition or the first part of
   a let-in expression. *)
and parse_let parser =
  let parser, let_token = advance parser in
  let parser, head_pattern = expect_pattern parser in
  let parser, _, tail_patterns = any_number_of pattern parser in
  let parser, _ =
    expect_kind Token.Colon
      "Expected ':' token and a type for the binding being introduced." parser
  in
  let parser, data_type = expect_data_type parser in
  let parser, _ = expect_kind Token.Equals "Expected '=' token." parser in
  let parser, bound_expr = expr parser in
  ( parser,
    let_token.pos.start,
    head_pattern :: tail_patterns,
    data_type,
    bound_expr )

(* let = "let" pattern { pattern } ":" type "=" expr *)
let let_binding parser =
  let parser, start, patterns, data_type, bound_expr = parse_let parser in
  let node : Ast.t =
    {
      pos = { start; finish = bound_expr.pos.finish };
      kind = Ast.Let (patterns, data_type, bound_expr);
    }
  in
  (parser, node)

(* data_arm = CAPITALISED_IDENTIFIER [ type ] *)
let data_arm parser =
  let parser, token =
    expect_kind Token.CapitalisedIdentifier "Expected a constructor identifier."
      parser
  in
  let identifier = Token.lexeme parser.source_code token in
  match data_type parser with
  | Some (parser, data_type) ->
      let (pos : Position.t) =
        { start = token.pos.start; finish = data_type.pos.finish }
      in
      (parser, (pos, identifier, Some data_type))
  | None -> (parser, (token.pos, identifier, None))

(* data = "data" IDENTIFIER "=" [ "|" ] data_arm { "|" data_arm } *)
let data_definition parser =
  let parser, data_token = advance parser in
  let parser, identifier_token =
    expect_kind Token.Identifier
      "Expected an identifier name for the data definition." parser
  in
  let identifier = Token.lexeme parser.source_code identifier_token in
  let parser, _ = expect_kind Token.Equals "Expected '=' token." parser in
  let parser, last_arm, arms = parse_arms data_arm parser in
  let last_arm_pos, _, _ = last_arm in
  let node : Ast.t =
    {
      pos = { start = data_token.pos.start; finish = last_arm_pos.finish };
      kind = Ast.Data (identifier, arms);
    }
  in
  (parser, node)

(* top_level = let | alias | data *)
let top_level parser =
  let match_kind = function
    | Token.LetKeyword -> let_binding parser
    | Token.DataKeyword -> data_definition parser
    | _ ->
        let _, unexpected_token = advance parser in
        let lexeme = Token.lexeme parser.source_code unexpected_token in
        let msg =
          Printf.sprintf "Expected a top-level definition but got token '%s'."
            lexeme
        in
        Err.raise_syntax_error unexpected_token msg
  in
  peek_kind parser |> Option.map match_kind
