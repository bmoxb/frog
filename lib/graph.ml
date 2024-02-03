type colour = Black | Blue | Pink | Red

let display_colour = function
  | Black -> "black"
  | Blue -> "blue"
  | Pink -> "pink"
  | Red -> "red"

type t = { vertex_label : string; colour : colour; edges : edge list }

and edge = { edge_label : string; vertex : t }

let leaf label colour = { vertex_label = label; colour; edges = [] }

let unlabelled_edge vertex = { edge_label = ""; vertex }

let to_dot root =
  let open Printf in
  let rec traverse parent_number vertex =
    let vertex_line =
      sprintf "%d [label=\"%s\",shape=box,color=%s];" parent_number
        vertex.vertex_label
        (display_colour vertex.colour)
    in
    let edge_to_lines child_number edge =
      (* Line connecting vertex of parent_number to this edge vertex. *)
      let connect_line =
        sprintf "%d -- %d [label=\"%s\"];" parent_number child_number
          edge.edge_label
      in
      (* Recursive call on the edge vertex. *)
      let next_number, child_lines = traverse child_number edge.vertex in
      (next_number, connect_line :: child_lines)
    in
    let next_number, nested_child_lines =
      List.fold_left_map edge_to_lines (parent_number + 1) vertex.edges
    in
    (next_number, vertex_line :: List.flatten nested_child_lines)
  in
  let _, lines = traverse 0 root in
  "graph {\n" ^ String.concat "\n" lines ^ "}"
