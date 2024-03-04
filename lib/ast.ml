(** Abstract Syntax Tree emitted by the parser. *)

type identifier = string [@@deriving show]

module Pattern = struct
  type t = { pos : Position.t; kind : kind }

  and kind =
    | Identifier of identifier
    | TypeConstructor of identifier * t option
    | Literal of string
  [@@deriving show]

  let rec to_tree_vertex pattern =
    let label, edges =
      match pattern.kind with
      | Identifier identifier ->
          ("identifier", [ Tree.edge (Tree.vertex identifier) ])
      | TypeConstructor (identifier, pattern_opt) ->
          (* If pattern_opt is some then this is a list with a single edge.
             Otherwise, it is an empty list. *)
          let pattern_edge_list =
            pattern_opt
            |> Option.map (fun pattern ->
                   [ Tree.edge ~label:"argument" (to_tree_vertex pattern) ])
            |> Option.value ~default:[]
          in
          ( "type constructor",
            Tree.edge ~label:"identifier" (Tree.vertex identifier)
            :: pattern_edge_list )
      | Literal literal -> ("literal", [ Tree.edge (Tree.vertex literal) ])
    in
    Tree.vertex ~colour:(Tree.rgb 1.0 0.75 0.8) ~edges label

  let to_indexed_tree_edge index pattern =
    Tree.edge
      ~label:(Printf.sprintf "pattern %d" index)
      (to_tree_vertex pattern)
end

module DataType = struct
  type simple_kind = Identifier | Location [@@deriving show]

  let token_kind_to_simple_kind = function
    | Token.Identifier -> Some Identifier
    | Token.LocationIdentifier -> Some Location
    | _ -> None

  let display_simple_kind = function
    | Identifier -> "identifier"
    | Location -> "location"

  type t = { pos : Position.t; kind : kind }

  and kind = Simple of simple_kind * identifier | Function of t list * t list
  [@@deriving show]

  let rec to_tree_vertex data_type =
    let label, edges =
      match data_type.kind with
      | Simple (kind, identifier) ->
          (display_simple_kind kind, [ Tree.edge (Tree.vertex identifier) ])
      | Function (inputs, outputs) ->
          let data_type_to_edge data_type =
            Tree.edge (to_tree_vertex data_type)
          in
          ( "function",
            [
              Tree.edge
                (Tree.vertex "inputs"
                   ~edges:(List.map data_type_to_edge inputs));
              Tree.edge
                (Tree.vertex "outputs"
                   ~edges:(List.map data_type_to_edge outputs));
            ] )
    in
    Tree.vertex ~colour:(Tree.rgb 1.0 0.1 0.1) ~edges label
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

  type primary_kind = NumberLiteral | StringLiteral | Identifier | Location
  [@@deriving show]

  let token_kind_to_primary_kind = function
    | Token.NumberLiteral -> Some NumberLiteral
    | Token.StringLiteral -> Some StringLiteral
    | Token.Identifier -> Some Identifier
    | Token.LocationIdentifier -> Some Location
    | _ -> None

  let display_primary_kind = function
    | NumberLiteral -> "number literal"
    | StringLiteral -> "string literal"
    | Identifier -> "identifier"
    | Location -> "location"

  type t = { pos : Position.t; kind : kind } [@@deriving show]

  and kind =
    (* "let" pattern { pattern } ":" type "=" expr "in" expr *)
    | LetIn of Pattern.t list * DataType.t * t * t
    (* "match" expr "with" [ "|" ] match_arm { "|" match_arm } *)
    | Match of t * match_arm list
    (* "if" expr "then" expr "else" expr *)
    | IfThenElse of t * t * t
    (* expr binary_operator expr *)
    | BinOp of binary_operator * t * t
    (* unary_operator expr *)
    | UnaryOp of unary_operator * t
    (* expr expr { expr } *)
    | Application of t * t list
    (* "(" expr ")" *)
    | Grouping of t
    (* expr ";" expr *)
    | Chain of t * t
    (* NUMBER | STRING | IDENTIFIER *)
    | Primary of primary_kind * string
  [@@deriving show]

  (* pattern "->" expr *)
  and match_arm = Position.t * Pattern.t * t [@@deriving show]

  let rec to_tree_vertex expr =
    let label, edges =
      match expr.kind with
      | LetIn (patterns, data_type, bound_expr, body_expr) ->
          ( "let-in",
            List.mapi Pattern.to_indexed_tree_edge patterns
            @ [
                Tree.edge ~label:"type" (DataType.to_tree_vertex data_type);
                Tree.edge ~label:"bound expr" (to_tree_vertex bound_expr);
                Tree.edge ~label:"in" (to_tree_vertex body_expr);
              ] )
      | Match (expr, arms) ->
          ( "match",
            Tree.edge ~label:"expr" (to_tree_vertex expr)
            :: List.mapi match_arm_to_edge arms )
      | IfThenElse (condition, then_expr, else_expr) ->
          ( "if-then-else",
            [
              Tree.edge ~label:"condition" (to_tree_vertex condition);
              Tree.edge ~label:"then" (to_tree_vertex then_expr);
              Tree.edge ~label:"else" (to_tree_vertex else_expr);
            ] )
      | BinOp (op, lhs, rhs) ->
          ( "binary operation",
            [
              Tree.edge ~label:"lhs" (to_tree_vertex lhs);
              Tree.edge ~label:"op" (Tree.vertex (display_binary_operator op));
              Tree.edge ~label:"rhs" (to_tree_vertex rhs);
            ] )
      | UnaryOp (op, expr) ->
          ( "unary operation",
            [
              Tree.edge ~label:"op" (Tree.vertex (display_unary_operator op));
              Tree.edge ~label:"expr" (to_tree_vertex expr);
            ] )
      | Application (fn, args) ->
          let arg_to_edge index arg =
            Tree.edge
              ~label:(Printf.sprintf "arg %d" index)
              (to_tree_vertex arg)
          in
          ( "function application",
            Tree.edge ~label:"function" (to_tree_vertex fn)
            :: List.mapi arg_to_edge args )
      | Grouping expr ->
          ("grouping", [ Tree.edge ~label:"expr" (to_tree_vertex expr) ])
      | Chain (lhs, rhs) ->
          ( "chain",
            [ Tree.edge (to_tree_vertex lhs); Tree.edge (to_tree_vertex rhs) ]
          )
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

  and match_arm_to_edge index (_, pattern, expr) =
    Tree.edge
      (Tree.vertex
         ~edges:
           [
             Tree.edge ~label:"pattern" (Pattern.to_tree_vertex pattern);
             Tree.edge ~label:"expr" (to_tree_vertex expr);
           ]
         (Printf.sprintf "arm %d" index))
end

(* IDENTFIER [ type ] *)
type data_arm = Position.t * identifier * DataType.t option [@@deriving show]

let data_arm_to_edge index (_, constructor_identifier, data_type_opt) =
  let constructor_edge =
    Tree.edge ~label:"constructor identifier"
      (Tree.vertex constructor_identifier)
  in
  (* If the data type is some, then this is a single edge in a list.
     Otherwise, this is an empty list. *)
  let data_type_edge_list =
    data_type_opt
    |> Option.map (fun data_type ->
           [ data_type |> DataType.to_tree_vertex |> Tree.edge ])
    |> Option.value ~default:[]
  in
  Tree.edge
    (Tree.vertex
       ~edges:(constructor_edge :: data_type_edge_list)
       (Printf.sprintf "arm %d" index))

type kind =
  (* "let" pattern { pattern } ":" type "=" expr *)
  | Let of Pattern.t list * DataType.t * Expr.t
  (* "alias" IDENTIFIER "=" type *)
  | Alias of identifier * DataType.t
  (* "data" IDENTIFIER "=" [ "|" ] data_arm { "|" data_arm } *)
  | Data of identifier * data_arm list
[@@deriving show]

type t = { pos : Position.t; kind : kind } [@@deriving show]
(** A top-level definition. A program is comprised of one or more top-level
    definitions meaning a program's full AST is represented by a list of t. *)

let to_tree_vertex node =
  let label, edges =
    match node.kind with
    | Let (patterns, data_type, expr) ->
        ( "let",
          List.mapi Pattern.to_indexed_tree_edge patterns
          @ [
              Tree.edge ~label:"type" (DataType.to_tree_vertex data_type);
              Tree.edge ~label:"expr" (Expr.to_tree_vertex expr);
            ] )
    | Alias (identifier, data_type) ->
        ( "alias",
          [
            Tree.edge ~label:"identifier" (Tree.vertex identifier);
            Tree.edge ~label:"data type" (DataType.to_tree_vertex data_type);
          ] )
    | Data (identifier, arms) ->
        ( "data",
          Tree.edge ~label:"identifier" (Tree.vertex identifier)
          :: List.mapi data_arm_to_edge arms )
  in
  Tree.vertex ~colour:(Tree.rgb 0.2 0.2 0.2) ~edges label
