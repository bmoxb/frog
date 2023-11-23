type expr =
  | BinaryExpr of Token.t * expr * expr
  | UnaryExpr of Token.t * expr
  | Group of expr
  | Literal of Token.t
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
    | BinaryExpr (tok, left, right) ->
        let show_left = make_representation left (indent_level + 1) in
        let show_right = make_representation right (indent_level + 1) in
        indent ^ Token.show_kind tok.kind ^ "\n" ^ show_left ^ "\n" ^ show_right
    | UnaryExpr (tok, right) ->
        let show_right = make_representation right (indent_level + 1) in
        indent ^ Token.show_kind tok.kind ^ "\n" ^ show_right
    | Group grouped ->
        let show_grouped = make_representation grouped (indent_level + 1) in
        indent ^ "group\n" ^ show_grouped
    | Literal tok -> indent ^ Token.show_kind tok.kind
  in
  make_representation expr 0
