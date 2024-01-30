type variable = string [@@deriving show]

type location = string option [@@deriving show]

type jump = string option [@@deriving show]

(*
 * Term of the Functional Machine Calculus augmented with jumps.
 *
 * Grammar:
 * M, N ::= x | j | [N]a.M | a<x>.M | N; j -> M | M^j
 * Where M, N are terms, x is a variable, j is a jump.
 *)
type t =
  | Variable of variable
  | Jump of jump
  (* Application / Push Action
   * [N]a.M *)
  | Application of { arg : t; location : location; func : t }
  (* Abstraction / Pop Action
   * a<x>.M *)
  | Abstraction of { location : location; variable : variable; term : t }
  (* N; j -> M *)
  | Condition of { condition : t; jump : jump; body : t }
  (* M^j *)
  | Loop of { term : t; jump : jump }
[@@deriving show]

let rec display =
  let open Printf in
  let display_cont = function
    | Condition _ as term -> sprintf "(%s)" (display term)
    | term -> display term
  in
  let display_end = function
    | Jump None -> ""
    | term -> "." ^ display_cont term
  in
  let display_loop = function
    | Jump None -> "*"
    | Jump (Some x) | Variable x -> x
    | term -> sprintf "(%s)" (display term)
  in
  function
  | Variable var -> var
  | Jump None -> "*"
  | Jump (Some j) -> j
  | Application { arg; location; func } ->
      sprintf "[%s]%s%s" (display arg)
        (Option.value location ~default:"")
        (display_end func)
  | Abstraction { location; variable; term } ->
      sprintf "%s<%s>%s"
        (Option.value location ~default:"")
        variable (display_end term)
  | Condition { condition; jump; body } ->
      sprintf "%s; %s%s" (display condition)
        (jump |> Option.map (sprintf "%s -> ") |> Option.value ~default:"")
        (display_cont body)
  | Loop { term; jump } ->
      display_loop term
      ^ (jump |> Option.map (sprintf "^%s") |> Option.value ~default:"*")
