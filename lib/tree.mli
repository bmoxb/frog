(** Basic representation of trees that can be converted into Graphviz DOT
    format. *)

type vertex

type edge

type colour

val rgb : float -> float -> float -> colour
(** Create a colour with the given R, G, and B colour channel values (each in
    range 0.0 to 1.0. *)

val vertex : ?colour:colour -> ?edges:edge list -> string -> vertex
(** Create a vertex/node in a tree. *)

val edge : ?label:string -> vertex -> edge
(** Create an edge/connection to a vertex in a tree. *)

val to_dot : vertex -> string
(** Convert the given tree to Graphviz DOT format. *)
