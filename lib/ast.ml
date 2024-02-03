type identifier = string [@@deriving show]

module Pattern = struct
  (* TODO *)
  type t = Identifier of identifier [@@deriving show]

  let colour = Graph.Pink

  let to_graph pattern : Graph.t =
    match pattern with Identifier identifier -> Graph.leaf identifier colour
end

module DataType = struct
  (* TODO *)
  type t = Identifier of identifier [@@deriving show]

  let colour = Graph.Red

  let to_graph data_type : Graph.t =
    match data_type with Identifier identifier -> Graph.leaf identifier colour
end

module Expr = struct
  type binary_operator =
    | And
    | Or
    | Equiv
    | NotEquiv
    | GreaterThan
    | LessThan
    | GreaterThanOrEqual
    | LessThanOrEqual
    | Add
    | Subtract
    | Multiply
    | Divide
  [@@deriving show]

  let display_binary_operator = function
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

  type unary_operator = Not | Negate [@@deriving show]

  let display_unary_operator = function Not -> "not" | Negate -> "-"

  type t = { position : Position.t; kind : kind } [@@deriving show]

  and kind =
    (* "let" pattern "=" expr "in" expr *)
    | LetIn of Pattern.t * DataType.t * t
    (*| Match of t*)
    (* "if" expr "then" expr "else" expr *)
    | IfThenElse of t * t * t
    (* expr binary_operator expr *)
    | BinOp of binary_operator * t * t
    (* unary_operator expr *)
    | UnaryOp of unary_operator * t
    (* "(" expr ")" *)
    | Grouping of t
    | Identifier of identifier
  [@@deriving show]

  let colour = Graph.Blue

  let rec to_graph expr : Graph.t =
    match expr.kind with
    | LetIn (pattern, data_type, expr) ->
        {
          vertex_label = "let";
          colour;
          edges =
            [
              { edge_label = "pattern"; vertex = Pattern.to_graph pattern };
              { edge_label = "type"; vertex = DataType.to_graph data_type };
              { edge_label = "in"; vertex = to_graph expr };
            ];
        }
    | IfThenElse (condition, then_expr, else_expr) ->
        {
          vertex_label = "if";
          colour;
          edges =
            [
              { edge_label = "condition"; vertex = to_graph condition };
              { edge_label = "then"; vertex = to_graph then_expr };
              { edge_label = "else"; vertex = to_graph else_expr };
            ];
        }
    | BinOp (op, lhs, rhs) ->
        {
          vertex_label = "binary operation";
          colour;
          edges =
            [
              { edge_label = "lhs"; vertex = to_graph lhs };
              {
                edge_label = "op";
                vertex = Graph.leaf (display_binary_operator op) Graph.Black;
              };
              { edge_label = "rhs"; vertex = to_graph rhs };
            ];
        }
    | UnaryOp (op, expr) ->
        {
          vertex_label = "unary operation";
          colour;
          edges =
            [
              {
                edge_label = "op";
                vertex = Graph.leaf (display_unary_operator op) Graph.Black;
              };
              { edge_label = "expr"; vertex = to_graph expr };
            ];
        }
    | Grouping expr ->
        {
          vertex_label = "grouping";
          colour;
          edges = [ { edge_label = "expr"; vertex = to_graph expr } ];
        }
    | Identifier identifier ->
        {
          vertex_label = "identifier";
          colour;
          edges = [ Graph.unlabelled_edge (Graph.leaf identifier Graph.Black) ];
        }
end

type top_level_kind =
  (* "let" pattern ":" type "=" expr *)
  | Let of Pattern.t * DataType.t * Expr.t
  (* "alias" IDENTIFIER "=" type *)
  | Alias of identifier * DataType.t
  (* "data" IDENTIFIER "=" [ "|" ] data_arm { "|" data_arm }
     data_arm = IDENTFIER [ type] *)
  | Data of identifier (* TODO *)
[@@deriving show]

type top_level = { position : Position.t; kind : top_level_kind }
[@@deriving show]
