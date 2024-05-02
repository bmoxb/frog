open Fmc
open Ast

let next_incremental_variable s =
  let len = String.length s in
  let rec traverse index =
    if s.[index] = 'z' then
      if index = len - 1 then String.make (len + 1) 'a' else traverse (index + 1)
    else
      let open Char in
      String.mapi (fun i c -> if i = index then chr (code c + 1) else c) s
  in
  traverse 0

let lexeme_to_location s = Loc (String.sub s 1 (String.length s - 1))

(* Evaluate/execute a primary expression. *)
let eval_primary kind lexeme =
  match kind with
  | Expr.Location -> Pop (lexeme_to_location lexeme, "x", Variable "x")
  | _ -> Variable lexeme

(* Push a primary expression and continue with the specified term. *)
let push_primary ?(location = Lambda) kind lexeme ~next_term =
  match kind with
  | Expr.Location ->
      Pop
        ( lexeme_to_location lexeme,
          "x",
          Push (Variable "x", location, next_term) )
  | _ -> Push (Variable lexeme, location, next_term)

let rec translate_expr (expr : Expr.t) =
  match expr.kind with
  | LetIn { info; bound_expr; in_expr } ->
      translate_let_in info bound_expr in_expr
  | Match (expr, arms) -> translate_match expr arms
  | IfThenElse { condition_expr; then_expr; else_expr } ->
      translate_if_then_else condition_expr then_expr else_expr
  | BinOp (op, lhs, rhs) -> translate_bin_op op lhs rhs
  | UnaryOp (op, expr) -> translate_unary_op op expr
  | Application (fn, args) -> translate_application fn args
  | Grouping expr -> translate_expr expr
  | Primary (kind, lexeme) -> push_primary kind lexeme ~next_term:(Jump Skip)

(* Convert an expression to an FMC term that directly evaluates to the computed
   value. *)
and exec_expr (expr : Expr.t) =
  match expr.kind with
  | Primary (kind, lexeme) -> eval_primary kind lexeme
  | _ -> Compose (translate_expr expr, Pop (Lambda, "x", Variable "x"))

(* Convert an expression to an FMC term that pushes the computed value and
   continues with next_term. *)
and push_expr (expr : Expr.t) ~next_term =
  match (expr.kind, next_term) with
  | Primary (kind, lexeme), _ -> push_primary kind lexeme ~next_term
  | _, Jump Skip -> translate_expr expr
  | _ -> Compose (translate_expr expr, next_term)

and push_expr_to_specific_location (expr : Expr.t) location ~next_term =
  match expr.kind with
  | Primary (kind, lexeme) -> push_primary kind lexeme ~location ~next_term
  | _ ->
      Compose
        ( translate_expr expr,
          Pop (Lambda, "x", Push (Variable "x", location, next_term)) )

and expr_with_parameters_popped expr = function
  | head :: tail ->
      Pop (Lambda, head, Grouping (expr_with_parameters_popped expr tail))
  | [] -> push_expr expr ~next_term:(Jump Skip)

and translate_let_in info bound_expr in_expr =
  translate_let info bound_expr
    ~next_term:(push_expr in_expr ~next_term:(Jump Skip))

and translate_match expr arms =
  let rec translate_arms lhs_term = function
    | (arm : Expr.match_arm) :: tail ->
        let arm_term =
          Grouping
            (expr_with_parameters_popped arm.expr (List.rev arm.parameters))
        in
        let next_lhs_term = Join (lhs_term, Jmp arm.constructor, arm_term) in
        translate_arms next_lhs_term tail
    | [] -> lhs_term
  in
  translate_arms (exec_expr expr) arms

and translate_if_then_else condition_expr then_expr else_expr =
  let condition_term = exec_expr condition_expr in
  Join
    ( Join
        (condition_term, Jmp "True", push_expr then_expr ~next_term:(Jump Skip)),
      Jmp "False",
      push_expr else_expr ~next_term:(Jump Skip) )

and translate_bin_op op lhs rhs =
  let f op_string =
    let op_var = Variable op_string in
    push_expr rhs ~next_term:(push_expr lhs ~next_term:op_var)
  in
  match op with
  (* TODO: For chain, discard the pushed value (if any). *)
  | Chain | MultiReturn ->
      push_expr lhs ~next_term:(push_expr rhs ~next_term:(Jump Skip))
  | And -> f "&&"
  | Or -> f "||"
  | Equiv -> f "=="
  | NotEquiv -> f "!="
  | GreaterThan -> f ">"
  | LessThan -> f "<"
  | GreaterThanOrEqual -> f ">="
  | LessThanOrEqual -> f "<="
  | Add -> f "+"
  | Subtract -> f "-"
  | Multiply -> f "*"
  | Divide -> f "/"

and translate_unary_op op expr =
  match op with
  | Not -> push_expr expr ~next_term:(Variable "!")
  | Negate ->
      push_expr expr ~next_term:(Push (Variable "0", Lambda, Variable "-"))

and translate_application fn args =
  match fn.kind with
  | Primary (Expr.Location, lexeme) ->
      translate_location_application (lexeme_to_location lexeme) args
  | Primary (Expr.Constructor, lexeme) ->
      translate_constructor_application (Jmp lexeme) (List.rev args)
  | _ -> translate_function_application fn (List.rev args)

and translate_location_application location args =
  match args with
  | expr :: tail ->
      push_expr_to_specific_location expr location
        ~next_term:(translate_location_application location tail)
  | [] -> Jump Skip

and translate_constructor_application jmp args =
  let rec jump_with_args_pushed = function
    | var :: tail -> Push (Variable var, Lambda, jump_with_args_pushed tail)
    | [] -> Jump jmp
  in
  let rec prepare_args_and_push_jump var vars_to_push = function
    | expr :: tail ->
        let next_var = next_incremental_variable var in
        let next_arg_term =
          prepare_args_and_push_jump next_var (var :: vars_to_push) tail
        in
        push_expr expr ~next_term:(Pop (Lambda, var, Grouping next_arg_term))
    | [] -> Push (jump_with_args_pushed vars_to_push, Lambda, Jump Skip)
  in
  prepare_args_and_push_jump "a" [] args

and translate_function_application fn = function
  | expr :: tail ->
      push_expr expr ~next_term:(translate_function_application fn tail)
  | [] -> exec_expr fn

and translate_let info expr ~next_term =
  match info.parameters with
  (* Bind an expression that is evaluated immediately. *)
  | [] ->
      push_expr expr ~next_term:(Pop (Lambda, info.name, Grouping next_term))
  (* Bind a function that is evaluated only when called. *)
  | parameters ->
      Push
        ( expr_with_parameters_popped expr parameters,
          Lambda,
          Pop (Lambda, info.name, Grouping next_term) )

let translate nodes =
  let rec translate_tracking_main ?main_expr = function
    (* Identify the main function. *)
    | { kind = Let ({ name = "main"; _ }, expr); _ } :: tail ->
        translate_tracking_main ~main_expr:expr tail
    (* Translate all other bindings. *)
    | { kind = Let (info, expr); _ } :: tail ->
        translate_let info expr
          ~next_term:(translate_tracking_main ?main_expr tail)
    (* Once all nodes are traversed, insert the translated main function at the
       end. *)
    | [] ->
        main_expr |> Option.map translate_expr
        |> Option.value ~default:(Jump Skip)
    (* Skip data definitions as they do not require translation. *)
    | { kind = Data _; _ } :: tail -> translate_tracking_main ?main_expr tail
  in
  translate_tracking_main nodes
