(** Functional Machine Calculus representation. *)

type variable = string [@@deriving show]

type location = Lambda | Location of string [@@deriving show]

let display_location = function Lambda -> "" | Location a -> a

type jump = NamedJump of string | Star [@@deriving show]

let display_jump = function NamedJump s -> s | Star -> "*"

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
  (* N; M *)
  | Composition of t * t
  (* N; j -> M *)
  | Choice of t * jump * t
  (* M^j *)
  | Loop of t * jump
[@@deriving show]

let rec display =
  let open Printf in
  function
  | Variable s -> s
  | Jump j -> display_jump j
  | Push (n, a, Jump Star) -> sprintf "[%s]%s" (display n) (display_location a)
  | Push (n, a, m) ->
      sprintf "[%s]%s.%s" (display n) (display_location a) (display m)
  | Pop (a, x, m) -> sprintf "%s<%s>.%s" (display_location a) x (display m)
  | Composition (n, m) -> sprintf "%s; %s" (display n) (display m)
  | Choice (n, j, m) ->
      sprintf "%s; %s -> %s" (display n) (display_jump j) (display m)
  | Loop (m, j) -> sprintf "%s^%s" (display m) (display_jump j)
