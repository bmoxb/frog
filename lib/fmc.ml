(** Functional Machine Calculus representation. *)

type variable = string [@@deriving show]

type location = Lambda | Location of string [@@deriving show]

let display_location = function Lambda -> "" | Location a -> a

type jump = Star | Jump of string [@@deriving show]

let display_jump = function Star -> "*" | Jump j -> j

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
  function
  | Variable var -> var
  | Jump j -> display_jump j
  | Push (n, a, m) ->
      sprintf "[%s]%s.%s" (display n) (display_location a) (display m)
  | Pop (a, x, m) -> sprintf "%s<%s>.%s" (display_location a) x (display m)
  | Choice (n, Star, m) -> sprintf "%s; %s" (display n) (display m)
  | Choice (n, Jump j, m) -> sprintf "%s; %s -> %s" (display n) j (display m)
  | Loop (m, j) -> sprintf "%s^%s" (show m) (display_jump j)
