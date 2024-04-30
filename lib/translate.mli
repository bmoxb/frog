(** Frog's translator. Converts an AST into FMC terms. *)

val translate_expr : Ast.Expr.t -> Fmc.t
(** Convert a single expression into an FMC term. *)

val translate : Ast.t list -> Fmc.t
(** Convert a list of top-level definitions into executable FMC terms. *)
