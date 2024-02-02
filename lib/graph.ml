type t = { vertex_label : string; edges : edge list }

and edge = { edge_label : string; vertex : t }

let leaf label = { vertex_label = label; edges = [] }

let unlabelled_edge vertex = { edge_label = ""; vertex }

let to_dot root =
  let open Printf in
  let rec traverse vertex =
    let vertex_line = sprintf "\"%s\" [shape=box];" vertex.vertex_label in
    let connect_edge_lines =
      List.map
        (fun edge ->
          sprintf "\"%s\" -- \"%s\" [label=\"%s\"];" vertex.vertex_label
            edge.vertex.vertex_label edge.edge_label)
        vertex.edges
    in
    let edge_lines =
      List.concat_map (fun edge -> traverse edge.vertex) vertex.edges
    in
    (vertex_line :: connect_edge_lines) @ edge_lines
  in
  "graph {\n" ^ String.concat "\n" (traverse root) ^ "}"
