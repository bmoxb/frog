(** Functional Machine Calculus representation. *)

type variable = string [@@deriving show]

type location = Loc of string | Lambda [@@deriving show]

type jump = Jmp of string | Skip [@@deriving show]

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
  | Compose of t * t
  (* (M) *)
  | Grouping of t
  (* N; j -> M *)
  | Join of t * jump * t
  (* M^j *)
  | Loop of t * jump
[@@deriving show]

let rec display =
  let open Printf in
  let jump = function Jmp j -> j | Skip -> "*" in
  let location = function Loc a -> a | Lambda -> "" in
  let dot_term = function Jump Skip -> "" | m -> "." ^ display m in
  let loop_body = function
    | Jump j -> jump j
    | Variable v -> v
    | Grouping m -> display m
    | m -> sprintf "(%s)" (display m)
  in
  function
  | Variable x -> x
  | Jump j -> jump j
  | Push (n, a, m) -> sprintf "[%s]%s%s" (display n) (location a) (dot_term m)
  | Pop (a, x, m) -> sprintf "%s<%s>%s" (location a) x (dot_term m)
  | Compose (n, m) -> sprintf "%s; %s" (display n) (display m)
  | Grouping m -> sprintf "(%s)" (display m)
  | Join (n, j, m) -> sprintf "%s; %s -> %s" (display n) (jump j) (display m)
  | Loop (m, n) -> sprintf "%s^%s" (loop_body m) (jump n)
