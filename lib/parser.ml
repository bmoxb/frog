type t = { tokens : Token.t list }

let init tokens = { tokens }

(* See the token kind of the next token without changing the current position in
   the token stream. *)
let peek_kind parser =
  match parser.tokens with token :: _ -> Some token.kind | [] -> None

(* Return the next token in the token stream and advance the current position by
   one. *)
let advance parser =
  match parser.tokens with
  | token :: tail -> ({ tokens = tail }, token)
  | [] -> exit 0 (* TODO *)

(* If the next token has the given token kind, advance the current position.
   Otherwise, produce an error. *)
let expect_kind kind parser =
  let parser, token = advance parser in
  if token.kind = kind then (parser, token) else exit 0 (* TODO *)

let rec parse_expr parser =
  if List.is_empty parser.tokens then (parser, None)
  else
    let parser, expr = expr parser in
    (parser, Some expr)

and expr parser = let_binding parser

and let_binding parser =
  let open Token in
  match peek_kind parser with
  | Some LetKeyword -> (
      let parser, let_token = advance parser in
      match advance parser with
      | parser, ({ kind = Identifier _; _ } as identifier) ->
          let parser, equals_token = expect_kind Equals parser in
          let parser, bound_expr = expr parser in
          let parser, in_token = expect_kind InKeyword parser in
          let parser, body_expr = expr parser in
          ( parser,
            Ast.LetBinding
              {
                let_token;
                identifier;
                equals_token;
                bound_expr;
                in_token;
                body_expr;
              } )
      | _ -> exit 0)
  | _ -> logical parser

and logical parser =
  let open Token in
  let is_op = function AndKeyword | OrKeyword -> true | _ -> false in
  left_associative_binary_expr parser equality is_op

and equality parser =
  let is_op = function Token.Equiv | Token.NotEquiv -> true | _ -> false in
  left_associative_binary_expr parser comparison is_op

and comparison parser =
  let open Token in
  let is_op = function
    | GreaterThan | LessThan | GreaterThanOrEqual | LessThanOrEqual -> true
    | _ -> false
  in
  left_associative_binary_expr parser term is_op

and term parser =
  let is_op = function Token.Minus | Token.Plus -> true | _ -> false in
  left_associative_binary_expr parser factor is_op

and factor parser =
  let is_op = function Token.Star | Token.Slash -> true | _ -> false in
  left_associative_binary_expr parser unary is_op

and unary parser =
  match peek_kind parser with
  | Some Token.NotKeyword | Some Token.Minus ->
      let parser, token = advance parser in
      let parser, right = unary parser in
      (parser, Ast.UnaryExpr (token, right))
  | _ -> primary parser

and primary parser =
  let open Token in
  match peek_kind parser with
  | Some (NumberLiteral _) | Some (StringLiteral _) | Some (Identifier _) ->
      let parser, token = advance parser in
      (parser, Ast.Literal token)
  | Some OpenBracket ->
      let parser, open_bracket_token = advance parser in
      let parser, expr = expr parser in
      let parser, close_bracket_token = expect_kind CloseBracket parser in
      (parser, Ast.Group (open_bracket_token, expr, close_bracket_token))
  | _ -> exit 0

and left_associative_binary_expr parser child_expr is_op =
  let parser, left = child_expr parser in
  match peek_kind parser with
  | Some kind when is_op kind ->
      let parser, token = advance parser in
      let parser, right =
        left_associative_binary_expr parser child_expr is_op
      in
      (parser, Ast.BinaryExpr (token, left, right))
  | _ -> (parser, left)
