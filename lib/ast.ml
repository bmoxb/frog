type expr =
  | LetBinding of {
      let_token : Token.t;
      identifier : Token.t;
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
let show_expr_simple expr =
  let rec make_indent level dot =
    if level > 0 then
      let tab = if dot then ". " else ", " in
      tab ^ make_indent (level - 1) (not dot)
    else ""
  in
  let rec make_representation expr indent_level =
    let indent = make_indent indent_level true in
    match expr with
    | LetBinding { identifier; bound_expr; body_expr; _ } ->
        let show_ident = Token.show_kind identifier.kind in
        let show_bound = make_representation bound_expr (indent_level + 1) in
        let show_body = make_representation body_expr (indent_level + 1) in
        indent ^ "let " ^ show_ident ^ " =\n" ^ show_bound ^ "\n" ^ indent
        ^ "in\n" ^ show_body
    | BinaryExpr { operator_token; left_expr; right_expr } ->
        let show_token = Token.show_kind operator_token.kind in
        let show_left = make_representation left_expr (indent_level + 1) in
        let show_right = make_representation right_expr (indent_level + 1) in
        indent ^ show_token ^ "\n" ^ show_left ^ "\n" ^ show_right
    | UnaryExpr { operator_token; expr } ->
        let show_expr = make_representation expr (indent_level + 1) in
        indent ^ Token.show_kind operator_token.kind ^ "\n" ^ show_expr
    | Group { expr; _ } ->
        let show_expr = make_representation expr (indent_level + 1) in
        indent ^ "group\n" ^ show_expr
    | Primary tok -> indent ^ Token.show_kind tok.kind
  in
  make_representation expr 0
