(** Abstract Syntax Tree emitted by the parser. *)

type identifier = string [@@deriving show]

module Pattern = struct
  type t = Identifier of identifier | List of t list | Cons of t * t
  [@@deriving show]

  let rec to_tree_vertex pattern =
    let label, edges =
      match pattern with
      | Identifier identifier ->
          ("identifier", [ Tree.edge (Tree.vertex identifier) ])
      | List patterns ->
          if List.is_empty patterns then ("empty list", [])
          else
            ( "list",
              List.mapi
                (fun index pattern ->
                  Tree.edge ~label:(string_of_int index)
                    (to_tree_vertex pattern))
                patterns )
      | Cons (head, tail) ->
          ( "cons",
            [
              Tree.edge ~label:"head" (to_tree_vertex head);
              Tree.edge ~label:"tail" (to_tree_vertex tail);
            ] )
    in
    Tree.vertex ~colour:(Tree.rgb 1.0 0.75 0.8) ~edges label
end

module DataType = struct
  type t = Identifier of identifier | List of t [@@deriving show]

  let rec to_tree_vertex data_type =
    let label, edges =
      match data_type with
      | Identifier identifier ->
          ("identifier", [ Tree.edge (Tree.vertex identifier) ])
      | List data_type ->
          ( "list",
            [ Tree.edge ~label:"element type" (to_tree_vertex data_type) ] )
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

  type primary_kind = NumberLiteral | StringLiteral | Identifier
  [@@deriving show]

  let token_kind_to_primary_kind = function
    | Token.NumberLiteral -> Some NumberLiteral
    | Token.StringLiteral -> Some StringLiteral
    | Token.Identifier -> Some Identifier
    | _ -> None

  let display_primary_kind = function
    | NumberLiteral -> "number literal"
    | StringLiteral -> "string literal"
    | Identifier -> "identifier"

  type t = { position : Position.t; kind : kind } [@@deriving show]

  and kind =
    (* "let" pattern ":" type "=" expr "in" expr *)
    | LetIn of Pattern.t * DataType.t * t * t
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
    | Primary of primary_kind * string
  [@@deriving show]

  (* pattern "->" expr *)
  and match_arm = Pattern.t * t [@@deriving show]

  let rec to_tree_vertex expr =
    let label, edges =
      match expr.kind with
      | LetIn (pattern, data_type, bound_expr, body_expr) ->
          ( "let-in",
            [
              Tree.edge ~label:"pattern" (Pattern.to_tree_vertex pattern);
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
              ~label:(Printf.sprintf "argument %d" index)
              (to_tree_vertex arg)
          in
          ( "function application",
            Tree.edge ~label:"function" (to_tree_vertex fn)
            :: List.mapi arg_to_edge args )
      | Grouping expr ->
          ("grouping", [ Tree.edge ~label:"expr" (to_tree_vertex expr) ])
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

  and match_arm_to_edge index (pattern, expr) =
    Tree.edge
      (Tree.vertex
         ~edges:
           [
             Tree.edge ~label:"pattern" (Pattern.to_tree_vertex pattern);
             Tree.edge ~label:"expr" (to_tree_vertex expr);
           ]
         (Printf.sprintf "arm %d" index))

  let to_tree expr =
    Tree.init ~horizontal_spacing:1.0 ~vertical_spacing:1.0
      (to_tree_vertex expr)
end

(* IDENTFIER [ type ] *)
type data_arm = identifier * DataType.t option [@@deriving show]

let data_arm_to_edge index (constructor_identifier, data_type_opt) =
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
  (* "let" pattern ":" type "=" expr *)
  | Let of Pattern.t * DataType.t * Expr.t
  (* "alias" IDENTIFIER "=" type *)
  | Alias of identifier * DataType.t
  (* "data" IDENTIFIER "=" [ "|" ] data_arm { "|" data_arm } *)
  | Data of identifier * data_arm list
[@@deriving show]

type t = { position : Position.t; kind : kind } [@@deriving show]
(** A top-level definition. A program is comprised of one or more top-level
    definitions meaning a program's full AST is represented by a list of t. *)

let to_tree_vertex node =
  let label, edges =
    match node.kind with
    | Let (pattern, data_type, expr) ->
        ( "let",
          [
            Tree.edge ~label:"pattern" (Pattern.to_tree_vertex pattern);
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

let to_tree node =
  Tree.init ~horizontal_spacing:1.0 ~vertical_spacing:1.0 (to_tree_vertex node)
