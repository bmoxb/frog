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

(* If the peeked token has the given kind, advance. Otherwise, do nothing. This
   is used to consume optional tokens. *)
let advance_if_kind kind parser =
  match peek_kind parser with
  | Some k when k = kind ->
      let parser, token = advance parser in
      (parser, Some token)
  | _ -> (parser, None)

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

(* Helper function for parsing match and data arms. *)
let parse_arms parse_arm parser =
  let rec additional_arms end_offset parser =
    match peek_kind parser with
    | Some Token.Pipe ->
        let parser, _ = advance parser in
        let parser, end_offset, arm = parse_arm parser in
        let parser, end_offset, arms = additional_arms end_offset parser in
        (parser, end_offset, arm :: arms)
    | _ -> (parser, end_offset, [])
  in
  (* The first pipe is optional. *)
  let parser, _ = advance_if_kind Token.Pipe parser in
  let parser, end_offset, head_arm = parse_arm parser in
  let parser, end_offset, tail_arms = additional_arms end_offset parser in
  (parser, end_offset, head_arm :: tail_arms)

(* type = simple_type [ { simple_type } "->" simple_type { simple_type } ] *)
let rec data_type parser =
  let rec additional_simple_types parser =
    match simple_type parser with
    | Some (parser, head) ->
        let parser, tail = additional_simple_types parser in
        (parser, head :: tail)
    | None -> (parser, [])
  in
  (* Either map a simple type into a function type (if possible) or return as
     is. *)
  let maybe_into_function_type (parser, (head_type : Ast.DataType.t)) =
    let parser, tail_types = additional_simple_types parser in
    (* If there are no following simple types or a '->' token, then this must
       just be a single simple type rather than a function type. *)
    if List.is_empty tail_types && peek_kind parser <> Some Token.Arrow then
      (parser, head_type)
    else
      let parser, _ =
        expect_kind Token.Arrow "Expected '->' in function type." parser
      in
      let parser, return_head_type = expect_simple_type parser in
      let parser, return_tail_types = additional_simple_types parser in
      let node : Ast.DataType.t =
        {
          (* TODO: end *)
          position =
            { start_offset = head_type.position.start_offset; end_offset = 0 };
          kind =
            Ast.DataType.Function
              (head_type :: tail_types, return_head_type :: return_tail_types);
        }
      in
      (parser, node)
  in
  simple_type parser |> Option.map maybe_into_function_type

and expect_data_type parser =
  expect_or_syntax_error data_type "Expected a type." parser

(* simple_type = [ "@" ] IDENTIFIER *)
and simple_type parser =
  match peek_kind parser with
  | Some Token.At ->
      let parser, at_token = advance parser in
      let parser, identifier_token =
        expect_kind Token.Identifier
          "Expected an identifier for this location type." parser
      in
      let identifier = Token.lexeme parser.source_code identifier_token in
      let node : Ast.DataType.t =
        {
          position =
            {
              start_offset = at_token.position.start_offset;
              end_offset = identifier_token.position.end_offset;
            };
          kind = Ast.DataType.Location identifier;
        }
      in
      Some (parser, node)
  | Some Token.Identifier ->
      let parser, token = advance parser in
      let identifier = Token.lexeme parser.source_code token in
      let node : Ast.DataType.t =
        { position = token.position; kind = Ast.DataType.Identifier identifier }
      in
      Some (parser, node)
  | _ -> None

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
    { position = token.position; kind = Ast.Pattern.Identifier identifier }
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
          position =
            {
              start_offset = token.position.start_offset;
              end_offset = argument.position.end_offset;
            };
          kind = Ast.Pattern.TypeConstructor (identifier, Some argument);
        }
      in
      (parser, node)
  | None ->
      let node : Ast.Pattern.t =
        {
          position = token.position;
          kind = Ast.Pattern.TypeConstructor (identifier, None);
        }
      in
      (parser, node)

(* literal_pattern = NUMBER | STRING *)
and literal_pattern parser =
  let parser, token = advance parser in
  let literal = Token.lexeme parser.source_code token in
  (parser, { position = token.position; kind = Ast.Pattern.Literal literal })

let expect_pattern parser =
  expect_or_syntax_error pattern "Expected a pattern." parser

(* expr = let_in | match | if_then_else | logical_or *)
let rec expr parser =
  match peek_kind parser with
  | Some LetKeyword -> let_in parser
  | Some MatchKeyword -> match_with parser
  | Some IfKeyword -> if_then_else parser
  | _ -> logical_or parser

(* let_in = let "in" expr *)
and let_in parser : t * Ast.Expr.t =
  let parser, start_offset, patterns, data_type, bound_expr =
    parse_let parser
  in
  let parser, _ = expect_kind InKeyword "Expected 'in' keyword." parser in
  let parser, body_expr = expr parser in
  let node : Ast.Expr.t =
    {
      position = { start_offset; end_offset = body_expr.position.end_offset };
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
  let parser, end_offset, arms = parse_arms match_arm parser in
  let node : Ast.Expr.t =
    {
      position =
        { start_offset = match_token.position.start_offset; end_offset };
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
  let arm = (pattern, body) in
  (parser, body.position.end_offset, arm)

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
      position =
        {
          start_offset = if_token.position.start_offset;
          end_offset = else_expr.position.end_offset;
        };
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
          position =
            {
              start_offset = op_token.position.start_offset;
              end_offset = expr.position.end_offset;
            };
          kind = Ast.Expr.UnaryOp (op, expr);
        }
      in
      (parser, node)
  | None -> application parser

(* application = primary primary { primary } *)
and application parser =
  let rec additional_primary_exprs ?(end_offset = 0) parser =
    match primary parser with
    | Some (parser, (expr : Ast.Expr.t)) ->
        let parser, end_offset, exprs =
          additional_primary_exprs ~end_offset:expr.position.end_offset parser
        in
        (parser, end_offset, expr :: exprs)
    | None -> (parser, end_offset, [])
  in
  let parser, (head_expr : Ast.Expr.t) = expect_primary parser in
  let parser, end_offset, tail_exprs = additional_primary_exprs parser in
  if List.is_empty tail_exprs then (parser, head_expr)
  else
    let node : Ast.Expr.t =
      {
        position =
          { start_offset = head_expr.position.start_offset; end_offset };
        kind = Ast.Expr.Application (head_expr, tail_exprs);
      }
    in
    (parser, node)

(* primary = NUMBER | STRING | IDENTIFIER | grouping *)
and primary parser =
  match Option.bind (peek_kind parser) Ast.Expr.token_kind_to_primary_kind with
  | Some primary_kind ->
      let parser, token = advance parser in
      let literal = Token.lexeme parser.source_code token in
      let node : Ast.Expr.t =
        {
          position = token.position;
          kind = Ast.Expr.Primary (primary_kind, literal);
        }
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
          position =
            {
              start_offset = open_token.position.start_offset;
              end_offset = close_token.position.end_offset;
            };
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
          position =
            {
              start_offset = left_expr.position.start_offset;
              end_offset = right_expr.position.end_offset;
            };
          kind = Ast.Expr.BinOp (op, left_expr, right_expr);
        }
      in
      (parser, node)
  | _ -> (parser, left_expr)

(* Helper function for parsing a top-level let definition or the first part of
   a let-in expression. *)
and parse_let parser =
  let rec additional_patterns parser =
    match pattern parser with
    | Some (parser, pattern) ->
        let parser, tail = additional_patterns parser in
        (parser, pattern :: tail)
    | None -> (parser, [])
  in
  let parser, let_token = advance parser in
  let parser, head_pattern = expect_pattern parser in
  let parser, tail_patterns = additional_patterns parser in
  let parser, _ =
    expect_kind Token.Colon
      "Expected ':' token and a type for the binding being introduced." parser
  in
  let parser, data_type = expect_data_type parser in
  let parser, _ = expect_kind Token.Equals "Expected '=' token." parser in
  let parser, bound_expr = expr parser in
  ( parser,
    let_token.position.start_offset,
    head_pattern :: tail_patterns,
    data_type,
    bound_expr )

(* let = "let" pattern { pattern } ":" type "=" expr *)
let let_binding parser =
  let parser, start_offset, patterns, data_type, bound_expr =
    parse_let parser
  in
  let node : Ast.t =
    {
      position = { start_offset; end_offset = bound_expr.position.end_offset };
      kind = Ast.Let (patterns, data_type, bound_expr);
    }
  in
  (parser, node)

(* alias = "alias" IDENTIFIER "=" type *)
let type_alias parser =
  let parser, alias_token = advance parser in
  let parser, token =
    expect_kind Token.Identifier
      "Expected an identifier name for this type alias." parser
  in
  let identifier = Token.lexeme parser.source_code token in
  let parser, _ = expect_kind Token.Equals "Expected '=' token." parser in
  let parser, data_type = expect_data_type parser in
  let node : Ast.t =
    {
      position =
        {
          start_offset = alias_token.position.start_offset;
          end_offset = data_type.position.end_offset;
        };
      kind = Ast.Alias (identifier, data_type);
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
      (parser, data_type.position.end_offset, (identifier, Some data_type))
  | None -> (parser, token.position.end_offset, (identifier, None))

(* data = "data" IDENTIFIER "=" [ "|" ] data_arm { "|" data_arm } *)
let data_definition parser =
  let parser, data_token = advance parser in
  let parser, identifier_token =
    expect_kind Token.Identifier
      "Expected an identifier name for the data definition." parser
  in
  let identifier = Token.lexeme parser.source_code identifier_token in
  let parser, _ = expect_kind Token.Equals "Expected '=' token." parser in
  let parser, end_offset, arms = parse_arms data_arm parser in
  let node : Ast.t =
    {
      position = { start_offset = data_token.position.start_offset; end_offset };
      kind = Ast.Data (identifier, arms);
    }
  in
  (parser, node)

(* top_level = let | alias | data *)
let top_level parser =
  let match_kind = function
    | Token.LetKeyword -> let_binding parser
    | Token.AliasKeyword -> type_alias parser
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
