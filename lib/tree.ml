type colour = { r : float; g : float; b : float }

let rgb r g b = { r; g; b }

let hex_of_colour { r; g; b } =
  Printf.sprintf "\"#%.2X%.2X%.2X\""
    (int_of_float (255.0 *. r))
    (int_of_float (255.0 *. g))
    (int_of_float (255.0 *. b))

type vertex = { vertex_label : string; colour : colour; edges : edge list }

and edge = { edge_label : string; vertex : vertex }

type t = { root : vertex; horizontal_spacing : float; vertical_spacing : float }

let init ?(horizontal_spacing = 0.5) ?(vertical_spacing = 0.5) root =
  { root; horizontal_spacing; vertical_spacing }

let to_dot tree =
  let open Printf in
  let rec traverse parent_number vertex =
    let vertex_line =
      sprintf "%d [label=\"%s\",shape=box,color=%s];" parent_number
        vertex.vertex_label
        (hex_of_colour vertex.colour)
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
  let _, lines = traverse 0 tree.root in
  sprintf "graph {\nnodesep = %f;\nranksep = %f;\n%s\n}" tree.horizontal_spacing
    tree.vertical_spacing (String.concat "\n" lines)

let vertex ?(colour = rgb 0.0 0.0 0.0) ?(edges = []) label =
  { vertex_label = label; colour; edges }

let edge ?(label = "") vertex = { edge_label = label; vertex }
