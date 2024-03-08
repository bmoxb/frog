val translate_expr : Ast.Expr.t -> Fmc.t

val translate : Ast.t list -> Fmc.t
(** Convert a top-level definition in an FMC term. The FMC term produced will
    push the computed value to the stack. *)
