open Fmc
open Ast

let lexeme_to_location s = Loc (String.sub s 1 (String.length s - 1))

let translate_pattern (pattern : Pattern.t) ~next_term =
  match pattern.kind with
  | Identifier identifier -> Pop (Lambda, identifier, Grouping next_term)
  | Constructor (_identifier, _pattern_opt) -> failwith "unimplemented"
  | Grouping _ -> failwith "unimplemented"

let eval_primary kind lexeme =
  match kind with
  | Expr.Location ->
      (* TODO: variable name *)
      Pop (lexeme_to_location lexeme, "x", Variable "x")
  | _ -> Variable lexeme

let push_primary ?(location = Lambda) kind lexeme ~next_term =
  match kind with
  | Expr.Location ->
      (* TODO: variable name *)
      Pop
        ( lexeme_to_location lexeme,
          "x",
          Push (Variable "x", location, next_term) )
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
  | Chain (lhs, rhs) -> Choice (translate_expr lhs, Star, translate_expr rhs)
  | Primary (kind, lexeme) -> push_primary kind lexeme ~next_term:(Jump Star)

(* Convert an expression to an FMC term that directly evaluates to the computed
   value. *)
and eval_expr (expr : Expr.t) =
  match expr.kind with
  | Primary (kind, lexeme) -> eval_primary kind lexeme
  | _ -> Choice (translate_expr expr, Star, Pop (Lambda, "x", Variable "x"))

(* Convert an expression to an FMC term that pushes the computed value. *)
and push_expr (expr : Expr.t) ~next_term =
  match expr.kind with
  | Primary (kind, lexeme) -> push_primary kind lexeme ~next_term
  | _ -> (
      match next_term with
      | Jump Star -> translate_expr expr
      | _ -> Choice (translate_expr expr, Star, next_term))

and push_expr_to_specific_location (expr : Expr.t) location ~next_term =
  match expr.kind with
  | Primary (kind, lexeme) -> push_primary kind lexeme ~location ~next_term
  | _ ->
      Choice
        ( translate_expr expr,
          Star,
          Pop (Lambda, "x", Push (Variable "x", location, next_term)) )

and translate_let_in patterns bound_expr body_expr =
  translate_let patterns bound_expr
    ~next_term:(push_expr body_expr ~next_term:(Jump Star))

and translate_if_then_else condition then_expr else_expr =
  let condition_term = eval_expr condition in
  Choice
    ( Choice
        (condition_term, Jmp "True", push_expr then_expr ~next_term:(Jump Star)),
      Jmp "False",
      push_expr else_expr ~next_term:(Jump Star) )

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
  let rec location_push location (args : Expr.t list) =
    match args with
    | expr :: tail ->
        push_expr_to_specific_location expr location
          ~next_term:(location_push location tail)
    | [] -> Jump Star
  in
  match fn.kind with
  | Primary (Expr.Location, lexeme) ->
      location_push (lexeme_to_location lexeme) args
  | _ -> function_application (List.rev args)

and translate_let patterns expr ~next_term =
  let rec expr_with_args_popped args =
    match args with
    | head :: tail ->
        translate_pattern head ~next_term:(expr_with_args_popped tail)
    | [] -> push_expr expr ~next_term:(Jump Star)
  in
  match patterns with
  (* Bind an expression that is evaluated immediately. *)
  | [ pattern ] ->
      push_expr expr ~next_term:(translate_pattern pattern ~next_term)
  (* Bind a function that is evaluated only when called. *)
  | pattern :: arguments ->
      Push
        ( expr_with_args_popped arguments,
          Lambda,
          translate_pattern pattern ~next_term )
  | [] -> failwith "Expected let binding to contain at least one pattern."

let translate nodes =
  let rec translate_tracking_main ?main_expr = function
    (* Identify the main function. *)
    | { kind = Let ([ { kind = Identifier "main"; _ } ], _, expr); _ } :: tail
      ->
        translate_tracking_main ~main_expr:expr tail
    (* Translate all other bindings. *)
    | { kind = Let (patterns, _, expr); _ } :: tail ->
        translate_let patterns expr
          ~next_term:(translate_tracking_main ?main_expr tail)
    (* Once all nodes are traversed, insert the translated main function at the
       end. *)
    | [] ->
        main_expr |> Option.map eval_expr |> Option.value ~default:(Jump Star)
    | _ -> failwith "unimplemented"
  in
  translate_tracking_main nodes
