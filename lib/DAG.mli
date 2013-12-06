val build_graph: Rules.t -> unit
val succ: Vertex.t -> Vertex.t list
val has_cycle: unit -> bool
val ordered_iter: (Vertex.t list -> unit) -> unit
