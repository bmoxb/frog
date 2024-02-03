type t = { source_code : string; tokens : Token.t list }

let init source_code tokens = { source_code; tokens }

(* Exception used to conveniently send syntax errors up the call stack. Must not
   leak to the module's public interface. *)
exception ErrException of Err.t

let raise_syntax_error token msg =
  raise_notrace (ErrException (Err.Syntax { token; msg }))

(* See the token kind of the next token without changing the current position in
   the token stream. *)
let peek_kind parser =
  match parser.tokens with token :: _ -> Some token.kind | [] -> None

(* Return the next token in the token stream and advance the current position by
   one. *)
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

let rec parse_expr parser =
  if List.is_empty parser.tokens then (parser, None)
  else
    try
      let parser, expr = expr parser in
      (parser, Some (Ok expr))
    with ErrException err -> (parser, Some (Error err))

and expr parser =
  match peek_kind parser with
  | Some LetKeyword -> let_binding parser
  | _ -> logical parser

and let_binding parser : t * Ast.Expr.t =
  let open Token in
  let parser, let_token = advance parser in
  (* TODO: Pattern instead of identifier. *)
  let parser, identifier_token =
    expect_kind Identifier "Expected an identifier to bind an expression to."
      parser
  in
  let identifier =
    Position.substring parser.source_code identifier_token.position
  in
  let parser, _ = expect_kind Equals "Expected '=' token." parser in
  let parser, bound_expr = expr parser in
  let parser, _ = expect_kind InKeyword "Expected 'in' keyword." parser in
  let parser, body_expr = expr parser in
  let ast_node : Ast.Expr.t =
    {
      position =
        {
          start_offset = let_token.position.start_offset;
          end_offset = body_expr.position.end_offset;
        };
      kind =
        Ast.Expr.LetIn
          ( Ast.Pattern.Identifier identifier,
            Ast.DataType.Identifier "temp",
            bound_expr,
            body_expr );
    }
  in
  (parser, ast_node)

and logical parser =
  let open Ast.Expr in
  let is_wanted_op = function And | Or -> true | _ -> false in
  left_associative_binary_expr parser equality is_wanted_op

and equality parser =
  let open Ast.Expr in
  let is_wanted_op = function Equiv | NotEquiv -> true | _ -> false in
  left_associative_binary_expr parser comparison is_wanted_op

and comparison parser =
  let open Ast.Expr in
  let is_wanted_op = function
    | GreaterThan | LessThan | GreaterThanOrEqual | LessThanOrEqual -> true
    | _ -> false
  in
  left_associative_binary_expr parser term is_wanted_op

and term parser =
  let open Ast.Expr in
  let is_wanted_op = function Add | Subtract -> true | _ -> false in
  left_associative_binary_expr parser factor is_wanted_op

and factor parser =
  let open Ast.Expr in
  let is_wanted_op = function Multiply | Divide -> true | _ -> false in
  left_associative_binary_expr parser unary is_wanted_op

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
