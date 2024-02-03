type identifier = string [@@deriving show]

module Pattern = struct
  type t = Identifier of identifier | List of t list | Cons of t * t
  [@@deriving show]

  let rec to_graph pattern : Graph.t =
    let label, edges =
      match pattern with
      | Identifier identifier ->
          ( "identifier",
            [ Graph.unlabelled_edge (Graph.leaf identifier Graph.black) ] )
      | List patterns ->
          if List.is_empty patterns then ("empty list", [])
          else
            ( "list",
              List.mapi
                (fun index pattern : Graph.edge ->
                  {
                    edge_label = string_of_int index;
                    vertex = to_graph pattern;
                  })
                patterns )
      | Cons (head, tail) ->
          ( "cons",
            [
              { edge_label = "head"; vertex = to_graph head };
              { edge_label = "tail"; vertex = to_graph tail };
            ] )
    in
    { vertex_label = label; colour = { r = 1.0; g = 0.75; b = 0.8 }; edges }
end

module DataType = struct
  type t = Identifier of identifier | List of t [@@deriving show]

  let rec to_graph data_type : Graph.t =
    let label, edges =
      match data_type with
      | Identifier identifier ->
          ( "identifier",
            [ Graph.unlabelled_edge (Graph.leaf identifier Graph.black) ] )
      | List data_type ->
          ( "list",
            [ { edge_label = "element type"; vertex = to_graph data_type } ] )
    in
    { vertex_label = label; colour = { r = 1.0; g = 0.1; b = 0.1 }; edges }
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

  let token_kind_to_binary_operator = function
    | Token.AndKeyword -> Some And
    | Token.OrKeyword -> Some Or
    | Token.Equiv -> Some Equiv
    | Token.NotEquiv -> Some NotEquiv
    | Token.GreaterThan -> Some GreaterThan
    | Token.LessThan -> Some LessThan
    | Token.GreaterThanOrEqual -> Some GreaterThanOrEqual
    | Token.LessThanOrEqual -> Some LessThanOrEqual
    | Token.Plus -> Some Add
    | Token.Minus -> Some Subtract
    | Token.Star -> Some Multiply
    | Token.Slash -> Some Divide
    | _ -> None

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

  let token_kind_to_unary_operator = function
    | Token.NotKeyword -> Some Not
    | Token.Minus -> Some Negate
    | _ -> None

  let display_unary_operator = function Not -> "not" | Negate -> "-"

  type primary_kind = NumberLiteral | StringLiteral | Identifier
  [@@deriving show]

  let token_kind_to_primary_kind = function
    | Token.NumberLiteral _ -> Some NumberLiteral
    | Token.StringLiteral _ -> Some StringLiteral
    | Token.Identifier _ -> Some Identifier
    | _ -> None

  let display_primary_kind = function
    | NumberLiteral -> "number literal"
    | StringLiteral -> "string literal"
    | Identifier -> "identifier"

  type t = { position : Position.t; kind : kind } [@@deriving show]

  and kind =
    (* "let" pattern "=" expr "in" expr *)
    | LetIn of Pattern.t * DataType.t * t * t
    (* "match" expr "with" [ "|" ] match_arm { "|" match_arm }
       match_arm = pattern "->" expr *)
    | Match of t * (Pattern.t * t) list
    (* "if" expr "then" expr "else" expr *)
    | IfThenElse of t * t * t
    (* expr binary_operator expr *)
    | BinOp of binary_operator * t * t
    (* unary_operator expr *)
    | UnaryOp of unary_operator * t
    (* "(" expr ")" *)
    | Grouping of t
    | Primary of primary_kind * string
  [@@deriving show]

  let rec to_graph expr : Graph.t =
    let label, (edges : Graph.edge list) =
      match expr.kind with
      | LetIn (pattern, data_type, bound_expr, body_expr) ->
          ( "let",
            [
              { edge_label = "pattern"; vertex = Pattern.to_graph pattern };
              { edge_label = "type"; vertex = DataType.to_graph data_type };
              { edge_label = "bound expr"; vertex = to_graph bound_expr };
              { edge_label = "in"; vertex = to_graph body_expr };
            ] )
      | Match (expr, arms) ->
          let arm_to_graph index (pattern, expr) =
            Graph.unlabelled_edge
              {
                vertex_label = Printf.sprintf "arm %d" index;
                colour = Graph.black;
                edges =
                  [
                    {
                      edge_label = "pattern";
                      vertex = Pattern.to_graph pattern;
                    };
                    { edge_label = "expr"; vertex = to_graph expr };
                  ];
              }
          in
          ( "match",
            { edge_label = "expr"; vertex = to_graph expr }
            :: List.mapi arm_to_graph arms )
      | IfThenElse (condition, then_expr, else_expr) ->
          ( "if",
            [
              { edge_label = "condition"; vertex = to_graph condition };
              { edge_label = "then"; vertex = to_graph then_expr };
              { edge_label = "else"; vertex = to_graph else_expr };
            ] )
      | BinOp (op, lhs, rhs) ->
          ( "binary operation",
            [
              { edge_label = "lhs"; vertex = to_graph lhs };
              {
                edge_label = "op";
                vertex = Graph.leaf (display_binary_operator op) Graph.black;
              };
              { edge_label = "rhs"; vertex = to_graph rhs };
            ] )
      | UnaryOp (op, expr) ->
          ( "unary operation",
            [
              {
                edge_label = "op";
                vertex = Graph.leaf (display_unary_operator op) Graph.black;
              };
              { edge_label = "expr"; vertex = to_graph expr };
            ] )
      | Grouping expr ->
          ("grouping", [ { edge_label = "expr"; vertex = to_graph expr } ])
      | Primary (kind, value) ->
          ( "primary",
            [
              {
                edge_label = display_primary_kind kind;
                vertex = Graph.leaf value Graph.black;
              };
            ] )
    in
    { vertex_label = label; colour = { r = 0.1; g = 0.1; b = 1.0 }; edges }
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
