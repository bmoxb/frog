type t = { tokens : Token.t list }

let init tokens = { tokens }

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
  | token :: tail -> ({ tokens = tail }, token)
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

and expr parser = let_binding parser

and let_binding parser =
  let open Token in
  let peeked_let_keyword parser =
    let parser, let_token = advance parser in
    match advance parser with
    | parser, ({ kind = Identifier _; _ } as identifier) ->
        let parser, equals_token =
          expect_kind Equals "Expected '=' token." parser
        in
        let parser, bound_expr = expr parser in
        let parser, in_token =
          expect_kind InKeyword "Expected 'in' keyword." parser
        in
        let parser, body_expr = expr parser in
        let ast_node =
          Ast.LetBinding
            {
              let_token;
              identifier;
              equals_token;
              bound_expr;
              in_token;
              body_expr;
            }
        in
        (parser, ast_node)
    | _, token ->
        raise_syntax_error token
          "Expected an identifier to bind an expression to."
  in
  match peek_kind parser with
  | Some LetKeyword -> peeked_let_keyword parser
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
      let parser, operator_token = advance parser in
      let parser, expr = unary parser in
      (parser, Ast.UnaryExpr { operator_token; expr })
  | _ -> primary parser

and primary parser =
  let open Token in
  match peek_kind parser with
  | Some (NumberLiteral _) | Some (StringLiteral _) | Some (Identifier _) ->
      let parser, token = advance parser in
      (parser, Ast.Primary token)
  | Some OpenBracket ->
      let parser, open_bracket_token = advance parser in
      let parser, expr = expr parser in
      let parser, close_bracket_token =
        expect_kind CloseBracket "Expected closing ')' token." parser
      in
      (parser, Ast.Group { open_bracket_token; expr; close_bracket_token })
  | _ ->
      let _, token = advance parser in
      raise_syntax_error token "Expected primary expression."

and left_associative_binary_expr parser child_expr is_op =
  let parser, left_expr = child_expr parser in
  match peek_kind parser with
  | Some kind when is_op kind ->
      let parser, operator_token = advance parser in
      let parser, right_expr =
        left_associative_binary_expr parser child_expr is_op
      in
      (parser, Ast.BinaryExpr { operator_token; left_expr; right_expr })
  | _ -> (parser, left_expr)
