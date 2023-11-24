type expr =
  | LetBinding of {
      let_token : Token.t;
      identifier_token : Token.t;
      equals_token : Token.t;
      bound_expr : expr;
      in_token : Token.t;
      body_expr : expr;
    }
  | BinaryExpr of {
      operator_token : Token.t;
      left_expr : expr;
      right_expr : expr;
    }
  | UnaryExpr of { operator_token : Token.t; expr : expr }
  | Group of {
      open_bracket_token : Token.t;
      close_bracket_token : Token.t;
      expr : expr;
    }
  | Primary of Token.t
[@@deriving show]

(* Construct a human-readable representation of an expression that shows just
   token kinds in a simple tree structure. *)
let display_expr expr =
  let open Printf in
  let rec make_indent level dot =
    if level > 0 then
      let tab = if dot then ". " else ", " in
      tab ^ make_indent (level - 1) (not dot)
    else ""
  in
  let rec display_expr' expr indent_level =
    let indent = make_indent indent_level true in
    match expr with
    | LetBinding { identifier_token; bound_expr; body_expr; _ } ->
        let identifier = Token.show_kind identifier_token.kind in
        let bound = display_expr' bound_expr (indent_level + 1) in
        let body = display_expr' body_expr (indent_level + 1) in
        sprintf "%slet %s =\n%s\n%sin%s" indent identifier bound indent body
    | BinaryExpr { operator_token; left_expr; right_expr } ->
        let tok = Token.show_kind operator_token.kind in
        let left = display_expr' left_expr (indent_level + 1) in
        let right = display_expr' right_expr (indent_level + 1) in
        sprintf "%s%s\n%s\n%s" indent tok left right
    | UnaryExpr { operator_token; expr } ->
        let tok = Token.show_kind operator_token.kind in
        let expr = display_expr' expr (indent_level + 1) in
        sprintf "%s%s\n%s" indent tok expr
    | Group { expr; _ } ->
        let expr = display_expr' expr (indent_level + 1) in
        sprintf "%sgroup\n%s" indent expr
    | Primary tok -> indent ^ Token.show_kind tok.kind
  in
  display_expr' expr 0
