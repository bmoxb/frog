open Fmc
open Ast

let rec translate_expr (expr : Expr.t) =
  match expr.kind with
  | LetIn (patterns, _, bound_expr, body_expr) ->
      translate_let_in patterns bound_expr body_expr
  | Match _ -> failwith "unimplemented"
  | IfThenElse _ -> failwith "unimplemented"
  | BinOp (op, lhs, rhs) -> translate_bin_op op lhs rhs
  | UnaryOp _ -> failwith "unimplemented"
  | Application (fn, args) -> translate_application fn args
  | Grouping expr -> translate_expr expr
  | Chain _ -> failwith "unimplemented"
  | Primary (_, lexeme) -> Variable lexeme

and translate_let_in patterns bound_expr body_expr =
  let rec bound_expr_with_args_popped (args : Pattern.t list) =
    match args with
    | { pos = _; kind = Pattern.Identifier identifier } :: tail ->
        Pop (Lambda, identifier, bound_expr_with_args_popped tail)
    | [] -> translate_expr bound_expr
    | _ -> failwith "unimplemented"
  in
  match patterns with
  (* Single identifier is bound to expression that is evaluated immediately. *)
  | [ { pos = _; kind = Identifier identifier } ] ->
      Composition
        ( translate_expr bound_expr,
          Pop (Lambda, identifier, translate_expr body_expr) )
  (* Function with a bound expression that is evaluated only when the function
      is called. *)
  | { pos = _; kind = Identifier identifier } :: arguments ->
      Push
        ( bound_expr_with_args_popped arguments,
          Lambda,
          Pop (Lambda, identifier, translate_expr body_expr) )
  | _ -> failwith "unimplemented"

and translate_bin_op op lhs rhs =
  let op_var = Variable (Expr.display_binary_operator op) in
  match (lhs.kind, rhs.kind) with
  (* primary op primary *)
  | Primary (_, lhs), Primary (_, rhs) ->
      Push (Variable rhs, Lambda, Push (Variable lhs, Lambda, op_var))
  (* expr op primary *)
  | _, Primary (_, primary) ->
      Composition (Push (Variable primary, Lambda, translate_expr lhs), op_var)
  (* primary op expr *)
  | Primary (_, primary), _ ->
      Composition (translate_expr rhs, Push (Variable primary, Lambda, op_var))
  (* expr op expr *)
  | _ ->
      Composition (translate_expr rhs, Composition (translate_expr lhs, op_var))

and translate_application _fn _args = failwith "unimplemented"

let translate (node : Ast.t) =
  match node.kind with
  | Let (_, _, expr) -> translate_expr expr
  | _ -> failwith "unimplemented"
