let rec data_type_to_tree_vertex (data_type : Ast.DataType.t) =
  let label, edges =
    match data_type.kind with
    | Simple (_, identifier) -> (identifier, [])
    | Function { inputs; outputs } ->
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

let function_parameters_to_edges =
  let parameter_to_edge index parameter =
    Tree.edge ~label:(Printf.sprintf "param %d" index) (Tree.vertex parameter)
  in
  List.mapi parameter_to_edge

let rec expr_to_tree_vertex (expr : Ast.Expr.t) =
  let open Ast.Expr in
  let label, edges =
    match expr.kind with
    | LetIn
        {
          info = { name; parameters; data_type; recursive };
          bound_expr;
          in_expr;
        } ->
        ( (if recursive then "letrec-in" else "let-in"),
          Tree.edge ~label:"name" (Tree.vertex name)
          :: function_parameters_to_edges parameters
          @ [
              Tree.edge ~label:"type" (data_type_to_tree_vertex data_type);
              Tree.edge ~label:"bound expr" (expr_to_tree_vertex bound_expr);
              Tree.edge ~label:"in" (expr_to_tree_vertex in_expr);
            ] )
    | Match (expr, arms) ->
        ( "match",
          Tree.edge ~label:"expr" (expr_to_tree_vertex expr)
          :: match_arms_to_edges arms )
    | IfThenElse { condition_expr; then_expr; else_expr } ->
        ( "if-then-else",
          [
            Tree.edge ~label:"condition" (expr_to_tree_vertex condition_expr);
            Tree.edge ~label:"then" (expr_to_tree_vertex then_expr);
            Tree.edge ~label:"else" (expr_to_tree_vertex else_expr);
          ] )
    | BinOp (op, lhs, rhs) ->
        ( "binary operation",
          [
            Tree.edge ~label:"lhs" (expr_to_tree_vertex lhs);
            Tree.edge ~label:"op" (Tree.vertex (show_binary_operator op));
            Tree.edge ~label:"rhs" (expr_to_tree_vertex rhs);
          ] )
    | UnaryOp (op, expr) ->
        ( "unary operation",
          [
            Tree.edge ~label:"op" (Tree.vertex (show_unary_operator op));
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
        (value, [])
  in
  Tree.vertex ~colour:(Tree.rgb 0.1 0.1 1.0) ~edges label

and match_arms_to_edges arms =
  let match_arm_to_edge index (arm : Ast.Expr.match_arm) =
    let constructor_edge =
      Tree.edge ~label:"constructor" (Tree.vertex arm.constructor)
    in
    let parameter_edges =
      arm.parameters
      |> List.mapi (fun index parameter ->
             Tree.edge
               ~label:(Printf.sprintf "param %d" index)
               (Tree.vertex parameter))
    in
    let expr_edge = Tree.edge ~label:"expr" (expr_to_tree_vertex arm.expr) in
    Tree.edge
      (Tree.vertex
         ~edges:((constructor_edge :: parameter_edges) @ [ expr_edge ])
         (Printf.sprintf "arm %d" index))
  in
  List.mapi match_arm_to_edge arms

let data_arms_to_edges =
  let data_arm_to_edge index (arm : Ast.data_arm) =
    let constructor_edge =
      Tree.edge ~label:"constructor" (Tree.vertex arm.constructor)
    in
    let parameter_type_edges =
      arm.data_types
      |> List.mapi (fun i data_type ->
             Tree.edge
               ~label:(Printf.sprintf "param type %d" i)
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
    | Let ({ name; parameters; data_type; recursive }, expr) ->
        ( (if recursive then "letrec" else "let"),
          Tree.edge ~label:"name" (Tree.vertex name)
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
