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
[@@deriving show]

let rec display =
  let open Printf in
  let display_location = function Loc s -> s | Lambda -> "" in
  let display_loop_body = function
    | Variable v -> v
    | m -> sprintf "(%s)" (display m)
  in
  function
  | Variable v -> v
  | Jump Star -> "*"
  | Jump (Jmp j) -> j
  | Push (n, a, Jump Star) -> sprintf "[%s]%s" (display n) (display_location a)
  | Push (n, a, m) ->
      sprintf "[%s]%s.%s" (display n) (display_location a) (display m)
  | Pop (a, x, Jump Star) -> sprintf "%s<%s>" (display_location a) x
  | Pop (a, x, m) -> sprintf "%s<%s>.%s" (display_location a) x (display m)
  | Choice (n, Star, m) -> sprintf "%s; %s" (display n) (display m)
  | Choice (n, Jmp j, m) -> sprintf "%s; %s -> %s" (display n) j (display m)
  | Loop (m, Star) -> display_loop_body m ^ "*"
  | Loop (m, Jmp j) -> sprintf "%s^%s" (display_loop_body m) j
