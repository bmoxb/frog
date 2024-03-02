let translate_expr expr =
  let open Ast.Expr in
  let open Fmc in
  let rec traverse = function
    | LetIn _ -> exit 0
    | Match _ -> exit 0
    | IfThenElse _ -> exit 0
    | BinOp (op, lhs, rhs) -> translate_bin_op op lhs rhs
    | UnaryOp _ -> exit 0
    | Application _ -> exit 0
    | Grouping expr -> traverse expr.kind
    | Primary (_, lexeme) -> Variable lexeme
  and translate_bin_op op lhs rhs =
    let op_var = Variable (display_binary_operator op) in
    match (lhs.kind, rhs.kind) with
    | Primary (_, lhs), Primary (_, rhs) ->
        Push (Variable rhs, Lambda, Push (Variable lhs, Lambda, op_var))
    | expr, Primary (_, primary) ->
        Choice (Push (Variable primary, Lambda, traverse expr), Star, op_var)
    | Primary (_, primary), expr ->
        Choice (traverse expr, Star, Push (Variable primary, Lambda, op_var))
    | lhs, rhs ->
        Choice (traverse rhs, Star, Choice (traverse lhs, Star, op_var))
  in
  traverse expr.kind

let translate (node : Ast.t) =
  match node.kind with Let (_, _, expr) -> translate_expr expr | _ -> exit 0
