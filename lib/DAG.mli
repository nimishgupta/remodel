val build_graph: Rules.t -> Rules.target -> unit
val succ: Vertex.t -> Vertex.t list
val ordered_iter: (Vertex.t list -> unit) -> unit
