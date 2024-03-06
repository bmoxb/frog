open Fmc
open Ast

let eval_primary kind lexeme =
  match kind with
  | Expr.Location ->
      let name = String.sub lexeme 1 (String.length lexeme - 1) in
      (* TODO: variable name *)
      Pop (Location name, "x", Variable "x")
  | _ -> Variable lexeme

let push_primary kind lexeme ~next_term =
  match kind with
  | Expr.Location ->
      let name = String.sub lexeme 1 (String.length lexeme - 1) in
      (* TODO: variable name *)
      Pop (Location name, "x", Push (Variable "x", Lambda, next_term))
  | _ -> Push (Variable lexeme, Lambda, next_term)

let rec translate_expr (expr : Expr.t) =
  match expr.kind with
  | LetIn (patterns, _, bound_expr, body_expr) ->
      translate_let_in patterns bound_expr body_expr
  | Match _ -> failwith "unimplemented"
  | IfThenElse _ -> failwith "unimplemented"
  | BinOp (op, lhs, rhs) -> translate_bin_op op lhs rhs
  | UnaryOp (op, expr) -> translate_unary_op op expr
  | Application (fn, args) -> translate_application fn args
  | Grouping expr -> translate_expr expr
  | Chain (lhs, rhs) -> Composition (translate_expr lhs, translate_expr rhs)
  | Primary (kind, lexeme) -> eval_primary kind lexeme

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
  | Primary (lhs_kind, lhs_lexeme), Primary (rhs_kind, rhs_lexeme) ->
      push_primary rhs_kind rhs_lexeme
        ~next_term:(push_primary lhs_kind lhs_lexeme ~next_term:op_var)
  (* expr op primary *)
  | _, Primary (kind, lexeme) ->
      Composition
        (push_primary kind lexeme ~next_term:(translate_expr lhs), op_var)
  (* primary op expr *)
  | Primary (kind, lexeme), _ ->
      Composition
        (translate_expr rhs, push_primary kind lexeme ~next_term:op_var)
  (* expr op expr *)
  | _ ->
      Composition (translate_expr rhs, Composition (translate_expr lhs, op_var))

and translate_unary_op op expr =
  match op with
  | Not -> failwith "unimplemented"
  | Negate -> (
      let push_0_and_subtract = Push (Variable "0", Lambda, Variable "-") in
      match expr.kind with
      | Primary (_, primary) ->
          Push (Variable primary, Lambda, push_0_and_subtract)
      | _ -> Composition (translate_expr expr, push_0_and_subtract))

and translate_application fn args =
  let rec function_application : Expr.t list -> Fmc.t = function
    | { pos = _; kind = Expr.Primary (_, lexeme) } :: tail ->
        Push (Variable lexeme, Lambda, function_application tail)
    | expr :: tail ->
        Composition (translate_expr expr, function_application tail)
    | [] -> translate_expr fn
  in
  let rec location_push location_name (args : Expr.t list) =
    let loc = Location location_name in
    match args with
    | { pos = _; kind = Expr.Primary (_, lexeme) } :: tail ->
        Push (Variable lexeme, loc, location_push location_name tail)
    | expr :: tail ->
        (* TODO: variable name *)
        Composition
          ( translate_expr expr,
            Pop
              ( Lambda,
                "x",
                Push (Variable "x", loc, location_push location_name tail) ) )
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
