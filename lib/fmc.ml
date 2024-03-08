(** Functional Machine Calculus representation. *)

type variable = string [@@deriving show]

type location = Loc of string | Lambda [@@deriving show]

type jump = Jmp of string | Star [@@deriving show]

(**
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
  | Push of t * location * t
  (* Abstraction / Pop Action
   * a<x>.M *)
  | Pop of location * variable * t
  (* N; j -> M *)
  | Choice of t * jump * t
  (* M^j *)
  | Loop of t * jump
  (* (M) *)
  | Grouping of t
[@@deriving show]

let rec display =
  let open Printf in
  let jump = function Jmp j -> j | Star -> "*" in
  let location = function Loc a -> a | Lambda -> "" in
  let dot_term = function Jump Star -> "" | m -> "." ^ display m in
  let choice_arm = function Star -> "" | Jmp j -> j ^ " -> " in
  let loop_body = function
    | Jump j -> jump j
    | Variable v -> v
    | m -> sprintf "(%s)" (display m)
  in
  function
  | Variable x -> x
  | Jump j -> jump j
  | Push (n, a, m) -> sprintf "[%s]%s%s" (display n) (location a) (dot_term m)
  | Pop (a, x, m) -> sprintf "%s<%s>%s" (location a) x (dot_term m)
  | Choice (n, j, m) ->
      sprintf "%s; %s%s" (display n) (choice_arm j) (display m)
  | Loop (m, n) -> sprintf "%s^%s" (loop_body m) (jump n)
  | Grouping m -> sprintf "(%s)" (display m)
