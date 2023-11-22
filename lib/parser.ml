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
  if token.kind = kind then parser else exit 0 (* TODO *)

let rec parse_expr parser =
  let parser, expr = parse_logical parser in
  (parser, Some expr)

and parse_logical parser =
  let open Token in
  let is_op = function AndKeyword | OrKeyword -> true | _ -> false in
  parse_left_associative_binary_expr parser parse_equality is_op

and parse_equality parser =
  let is_op = function Token.Equiv | Token.NotEquiv -> true | _ -> false in
  parse_left_associative_binary_expr parser parse_comparison is_op

and parse_comparison parser =
  let open Token in
  let is_op = function
    | GreaterThan | LessThan | GreaterThanOrEqual | LessThanOrEqual -> true
    | _ -> false
  in
  parse_left_associative_binary_expr parser parse_term is_op

and parse_term parser =
  let is_op = function Token.Minus | Token.Plus -> true | _ -> false in
  parse_left_associative_binary_expr parser parse_factor is_op

and parse_factor parser =
  let is_op = function Token.Star | Token.Slash -> true | _ -> false in
  parse_left_associative_binary_expr parser parse_unary is_op

and parse_unary parser =
  match peek_kind parser with
  | Some Token.NotKeyword | Some Token.Minus ->
      let parser, token = advance parser in
      let parser, right = parse_unary parser in
      (parser, Ast.UnaryExpr (token, right))
  | _ -> parse_primary parser

and parse_primary parser =
  let open Token in
  match peek_kind parser with
  | Some (NumberLiteral _) | Some (StringLiteral _) ->
      let parser, token = advance parser in
      (parser, Ast.Literal token)
  | Some OpenBracket -> (
      let parser, _ = advance parser in
      let parser, expr = parse_expr parser in
      let parser = expect_kind CloseBracket parser in
      match expr with Some expr -> (parser, Ast.Group expr) | None -> exit 0
      (* need an expr inside the brackets, TODO *))
  | _ -> exit 0

and parse_left_associative_binary_expr parser child_expr is_op =
  let parser, left = child_expr parser in
  match peek_kind parser with
  | Some kind when is_op kind ->
      let parser, token = advance parser in
      let parser, right =
        parse_left_associative_binary_expr parser child_expr is_op
      in
      (parser, Ast.BinaryExpr (token, left, right))
  | _ -> (parser, left)
