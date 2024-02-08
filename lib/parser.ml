type t = { source_code : string; tokens : Token.t list }

let init source_code tokens = { source_code; tokens }

(* Exception used to conveniently send syntax errors up the call stack. Must
   not leak to the module's public interface. *)
exception ErrException of Err.t

let raise_syntax_error token msg =
  raise_notrace (ErrException (Err.Syntax { token; msg }))

(* See the token kind of the next token without changing the current position
   in the token stream. *)
let peek_kind parser =
  match parser.tokens with token :: _ -> Some token.kind | [] -> None

(* Return the next token in the token stream and advance the current position
   by one. *)
let advance parser =
  match parser.tokens with
  | token :: tail -> ({ parser with tokens = tail }, token)
  | [] -> raise_notrace (ErrException Err.UnexpectedEOF)

(* If the next token has the given token kind, advance the current position.
   Otherwise, produce an error. *)
let expect_kind kind error_msg parser =
  let parser, token = advance parser in
  if token.kind = kind then (parser, token)
  else raise_syntax_error token error_msg

(* expr = let_in | match | if_then_else | logical_or *)
let rec expr parser =
  match peek_kind parser with
  | Some LetKeyword -> let_in parser
  | Some MatchKeyword -> match_with parser
  | Some IfKeyword -> if_then_else parser
  | _ -> logical_or parser

(* let_in = let "in" expr *)
and let_in parser : t * Ast.Expr.t =
  let parser, start_offset, identifier, bound_expr = let_helper parser in
  let parser, _ = expect_kind InKeyword "Expected 'in' keyword." parser in
  let parser, body_expr = expr parser in
  let node : Ast.Expr.t =
    {
      position = { start_offset; end_offset = body_expr.position.end_offset };
      kind =
        Ast.Expr.LetIn
          ( Ast.Pattern.Identifier identifier,
            Ast.DataType.Identifier "TODO",
            bound_expr,
            body_expr );
    }
  in
  (parser, node)

and match_with parser = exit 0

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
  left_associative_binary_expr parser logical_and (( == ) Ast.Expr.Or)

(* logical_and = equality { "and" equality } *)
and logical_and parser =
  left_associative_binary_expr parser equality (( == ) Ast.Expr.And)

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

(* unary = ( "not" | "-" ) unary | primary *)
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
  | None -> primary parser

(* primary = NUMBER | STRING | IDENTIFIER | grouping *)
and primary parser =
  let open Token in
  match Option.bind (peek_kind parser) Ast.Expr.token_kind_to_primary_kind with
  | Some kind ->
      let parser, token = advance parser in
      let lexeme = Position.substring parser.source_code token.position in
      let node : Ast.Expr.t =
        { position = token.position; kind = Ast.Expr.Primary (kind, lexeme) }
      in
      (parser, node)
  | _ -> grouping parser

(*  grouping = "(" expr ")" *)
and grouping parser =
  let parser, token = advance parser in
  match token.kind with
  | OpenBracket ->
      let parser, expr = expr parser in
      let parser, close_token =
        expect_kind CloseBracket "Expected closing ')' token." parser
      in
      let node : Ast.Expr.t =
        {
          position =
            {
              start_offset = token.position.start_offset;
              end_offset = close_token.position.end_offset;
            };
          kind = Ast.Expr.Grouping expr;
        }
      in
      (parser, node)
  | _ -> raise_syntax_error token "Expected primary expression."

and left_associative_binary_expr parser child_expr is_wanted_op =
  let parser, left_expr = child_expr parser in
  let peeked_kind = peek_kind parser in
  match Option.bind peeked_kind Ast.Expr.token_kind_to_binary_operator with
  | Some op when is_wanted_op op ->
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

and let_helper parser =
  let parser, let_token = advance parser in
  (* TODO: Pattern instead of identifier. *)
  let parser, identifier_token =
    expect_kind Token.Identifier
      "Expected an identifier to bind an expression to." parser
  in
  let identifier =
    Position.substring parser.source_code identifier_token.position
  in
  let parser, _ = expect_kind Token.Equals "Expected '=' token." parser in
  let parser, bound_expr = expr parser in
  (parser, let_token.position.start_offset, identifier, bound_expr)

(* let = "let" pattern ":" type "=" expr *)
let let_binding parser =
  let parser, start_offset, identifier, bound_expr = let_helper parser in
  let ast : Ast.t =
    {
      position = { start_offset; end_offset = bound_expr.position.end_offset };
      kind =
        Ast.Let
          ( Ast.Pattern.Identifier identifier,
            Ast.DataType.Identifier "TODO",
            bound_expr );
    }
  in
  (parser, ast)

(* alias = "alias" IDENTIFIER "=" type *)
let type_alias parser =
  let parser, alias_token = advance parser in
  let parser, identifier_token =
    expect_kind Token.Identifier
      "Expected an identifier name for this type alias." parser
  in
  let identifier =
    Position.substring parser.source_code identifier_token.position
  in
  let parser, _ = expect_kind Token.Equals "Expected '=' token." parser in
  (* TODO: read type *)
  let ast : Ast.t =
    {
      position =
        { start_offset = alias_token.position.start_offset; end_offset = 0 };
      kind = Ast.Alias (identifier, Ast.DataType.Identifier "TODO");
    }
  in
  (parser, ast)

(* data = "data" IDENTIFIER "=" [ "|" ] data_arm { "|" data_arm } *)
let data_definition parser = exit 0

(* top_level = let | alias | data *)
let top_level parser =
  let match_kind = function
    | Token.LetKeyword -> let_binding parser
    | Token.AliasKeyword -> type_alias parser
    | Token.DataKeyword -> data_definition parser
    | _ ->
        let _, unexpected_token = advance parser in
        let msg =
          Printf.sprintf "Expected a top-level definition but got token '%s'."
            (Position.substring parser.source_code unexpected_token.position)
        in
        raise_syntax_error unexpected_token msg
  in
  peek_kind parser |> Option.map match_kind

let next_ast parser =
  try
    match top_level parser with
    | Some (parser, ast) -> (parser, Some (Ok ast))
    | None -> (parser, None)
  with ErrException err -> (parser, Some (Error err))
