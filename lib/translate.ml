open Fmc
open Ast

let eval_primary kind lexeme =
  match kind with
  | Expr.Location ->
      let name = String.sub lexeme 1 (String.length lexeme - 1) in
      (* TODO: variable name *)
      Pop (Location name, "x", Variable "x")
  | _ -> Variable lexeme

let push_primary ?(location = Lambda) kind lexeme ~next_term =
  match kind with
  | Expr.Location ->
      let name = String.sub lexeme 1 (String.length lexeme - 1) in
      (* TODO: variable name *)
      Pop (Location name, "x", Push (Variable "x", location, next_term))
  | _ -> Push (Variable lexeme, location, next_term)

let rec translate_expr (expr : Expr.t) =
  match expr.kind with
  | LetIn (patterns, _, bound_expr, body_expr) ->
      translate_let_in patterns bound_expr body_expr
  | Match _ -> failwith "unimplemented"
  | IfThenElse (condition, then_expr, else_expr) ->
      translate_if_then_else condition then_expr else_expr
  | BinOp (op, lhs, rhs) -> translate_bin_op op lhs rhs
  | UnaryOp (op, expr) -> translate_unary_op op expr
  | Application (fn, args) -> translate_application fn args
  | Grouping expr -> translate_expr expr
  | Chain (lhs, rhs) -> Composition (translate_expr lhs, translate_expr rhs)
  | Primary (kind, lexeme) -> push_primary kind lexeme ~next_term:(Jump Star)

(* Convert an expression to an FMC term that directly evaluates to the computed
   value. *)
and eval_expr (expr : Expr.t) =
  match expr.kind with
  | Primary (kind, lexeme) -> eval_primary kind lexeme
  | _ -> Composition (translate_expr expr, Pop (Lambda, "x", Variable "x"))

(* Convert an expression to an FMC term that pushes the computed value. *)
and push_expr (expr : Expr.t) ~next_term =
  match expr.kind with
  | Primary (kind, lexeme) -> push_primary kind lexeme ~next_term
  | _ -> Composition (translate_expr expr, next_term)

and push_expr_to_specific_location (expr : Expr.t) location ~next_term =
  match expr.kind with
  | Primary (kind, lexeme) -> push_primary kind lexeme ~location ~next_term
  | _ ->
      Composition
        ( translate_expr expr,
          Pop (Lambda, "x", Push (Variable "x", location, next_term)) )

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

and translate_if_then_else _condition _then_epxr _else_expr = failwith "todo"

and translate_bin_op op lhs rhs =
  let op_var = Variable (Expr.display_binary_operator op) in
  push_expr rhs ~next_term:(push_expr lhs ~next_term:op_var)

and translate_unary_op op expr =
  match op with
  | Not -> failwith "unimplemented"
  | Negate ->
      push_expr expr ~next_term:(Push (Variable "0", Lambda, Variable "-"))

and translate_application fn args =
  let rec function_application = function
    | expr :: tail -> push_expr expr ~next_term:(function_application tail)
    | [] -> eval_expr fn
  in
  let rec location_push location_name (args : Expr.t list) =
    match args with
    | expr :: tail ->
        push_expr_to_specific_location expr (Location location_name)
          ~next_term:(location_push location_name tail)
    | [] -> Jump Star
  in
  match fn with
  | { pos = _; kind = Expr.Primary (Expr.Location, lexme) } ->
      let location_name = String.sub lexme 1 (String.length lexme - 1) in
      location_push location_name args
  | _ -> function_application (List.rev args)

let translate (node : Ast.t) =
  match node.kind with
  | Let (_, _, expr) -> translate_expr expr
  | _ -> failwith "unimplemented"
