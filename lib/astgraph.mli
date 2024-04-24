(** Generate Graphviz graphs of AST structures. *)

val data_type_to_tree_vertex : Ast.DataType.t -> Tree.vertex

val expr_to_tree_vertex : Ast.Expr.t -> Tree.vertex

val node_to_tree_vertex : Ast.t -> Tree.vertex
