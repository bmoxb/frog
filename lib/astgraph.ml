let display_simple_kind =
  let open Ast.DataType in
  function Identifier -> "identifier" | Location -> "location"

let rec data_type_to_tree_vertex (data_type : Ast.DataType.t) =
  let label, edges =
    match data_type.kind with
    | Simple (kind, identifier) ->
        (display_simple_kind kind, [ Tree.edge (Tree.vertex identifier) ])
    | Function (inputs, outputs) ->
        let data_type_to_edge data_type =
          Tree.edge (data_type_to_tree_vertex data_type)
        in
        ( "function",
          [
            Tree.edge
              (Tree.vertex "inputs" ~edges:(List.map data_type_to_edge inputs));
            Tree.edge
              (Tree.vertex "outputs"
                 ~edges:(List.map data_type_to_edge outputs));
          ] )
  in
  Tree.vertex ~colour:(Tree.rgb 1.0 0.1 0.1) ~edges label

let display_binary_operator =
  let open Ast.Expr in
  function
  | And -> "and"
  | Or -> "or"
  | Equiv -> "=="
  | NotEquiv -> "!="
  | GreaterThan -> ">"
  | LessThan -> "<"
  | GreaterThanOrEqual -> ">="
  | LessThanOrEqual -> "<="
  | Add -> "+"
  | Subtract -> "-"
  | Multiply -> "*"
  | Divide -> "/"

let display_unary_operator =
  let open Ast.Expr in
  function Not -> "not" | Negate -> "-"

let display_primary_kind =
  let open Ast.Expr in
  function
  | NumberLiteral -> "number literal"
  | StringLiteral -> "string literal"
  | Identifier -> "identifier"
  | Location -> "location"
  | Constructor -> "constructor"

let function_parameters_to_edges =
  let parameter_to_edge index parameter =
    Tree.edge
      ~label:(Printf.sprintf "parameter %d" index)
      (Tree.vertex parameter)
  in
  List.mapi parameter_to_edge

let rec expr_to_tree_vertex (expr : Ast.Expr.t) =
  let label, edges =
    match expr.kind with
    | LetIn (identifier, parameters, data_type, bound_expr, body_expr) ->
        ( "let-in",
          Tree.edge ~label:"name" (Tree.vertex identifier)
          :: function_parameters_to_edges parameters
          @ [
              Tree.edge ~label:"type" (data_type_to_tree_vertex data_type);
              Tree.edge ~label:"bound expr" (expr_to_tree_vertex bound_expr);
              Tree.edge ~label:"in" (expr_to_tree_vertex body_expr);
            ] )
    | Match (expr, arms) ->
        ( "match",
          Tree.edge ~label:"expr" (expr_to_tree_vertex expr)
          :: match_arms_to_edges arms )
    | IfThenElse (condition, then_expr, else_expr) ->
        ( "if-then-else",
          [
            Tree.edge ~label:"condition" (expr_to_tree_vertex condition);
            Tree.edge ~label:"then" (expr_to_tree_vertex then_expr);
            Tree.edge ~label:"else" (expr_to_tree_vertex else_expr);
          ] )
    | BinOp (op, lhs, rhs) ->
        ( "binary operation",
          [
            Tree.edge ~label:"lhs" (expr_to_tree_vertex lhs);
            Tree.edge ~label:"op" (Tree.vertex (display_binary_operator op));
            Tree.edge ~label:"rhs" (expr_to_tree_vertex rhs);
          ] )
    | UnaryOp (op, expr) ->
        ( "unary operation",
          [
            Tree.edge ~label:"op" (Tree.vertex (display_unary_operator op));
            Tree.edge ~label:"expr" (expr_to_tree_vertex expr);
          ] )
    | Application (fn, args) ->
        let arg_to_edge index arg =
          Tree.edge
            ~label:(Printf.sprintf "arg %d" index)
            (expr_to_tree_vertex arg)
        in
        ( "function application",
          Tree.edge ~label:"function" (expr_to_tree_vertex fn)
          :: List.mapi arg_to_edge args )
    | Grouping expr ->
        ("grouping", [ Tree.edge ~label:"expr" (expr_to_tree_vertex expr) ])
    | Chain (lhs, rhs) ->
        ( "chain",
          [
            Tree.edge (expr_to_tree_vertex lhs);
            Tree.edge (expr_to_tree_vertex rhs);
          ] )
    | Primary (kind, value) ->
        (* Need to escape quotes properly if a string literal. *)
        let value =
          match kind with
          | StringLiteral ->
              let last_quote_removed =
                String.sub value 0 (String.length value - 1)
              in
              "\\" ^ last_quote_removed ^ "\\\""
          | _ -> value
        in
        ( "primary",
          [ Tree.edge ~label:(display_primary_kind kind) (Tree.vertex value) ]
        )
  in
  Tree.vertex ~colour:(Tree.rgb 0.1 0.1 1.0) ~edges label

and match_arms_to_edges arms =
  let match_arm_to_edge index (_, constructor, parameters, expr) =
    let constructor_edge =
      Tree.edge ~label:"constructor" (Tree.vertex constructor)
    in
    let parameter_edges =
      parameters
      |> List.mapi (fun index parameter ->
             Tree.edge
               ~label:(Printf.sprintf "parameter %d" index)
               (Tree.vertex parameter))
    in
    let expr_edge = Tree.edge ~label:"expr" (expr_to_tree_vertex expr) in
    Tree.edge
      (Tree.vertex
         ~edges:((constructor_edge :: parameter_edges) @ [ expr_edge ])
         (Printf.sprintf "arm %d" index))
  in
  List.mapi match_arm_to_edge arms

let data_arms_to_edges =
  let data_arm_to_edge index (_, constructor, parameter_types) =
    let constructor_edge =
      Tree.edge ~label:"constructor" (Tree.vertex constructor)
    in
    let parameter_type_edges =
      parameter_types
      |> List.mapi (fun i data_type ->
             Tree.edge
               ~label:(Printf.sprintf "parameter type %d" i)
               (data_type_to_tree_vertex data_type))
    in
    Tree.edge
      (Tree.vertex
         ~edges:(constructor_edge :: parameter_type_edges)
         (Printf.sprintf "arm %d" index))
  in
  List.mapi data_arm_to_edge

let node_to_tree_vertex (node : Ast.t) =
  let label, edges =
    match node.kind with
    | Let (identifier, parameters, data_type, expr) ->
        ( "let",
          Tree.edge ~label:"name" (Tree.vertex identifier)
          :: function_parameters_to_edges parameters
          @ [
              Tree.edge ~label:"type" (data_type_to_tree_vertex data_type);
              Tree.edge ~label:"expr" (expr_to_tree_vertex expr);
            ] )
    | Data (identifier, arms) ->
        ( "data",
          Tree.edge ~label:"name" (Tree.vertex identifier)
          :: data_arms_to_edges arms )
  in
  Tree.vertex ~colour:(Tree.rgb 0.2 0.2 0.2) ~edges label
