type t

val build_graph: Rules.t -> Rules.target -> t
val succ: t -> Vertex.t -> Vertex.t list


type build_order_t

val make_build_order: t -> build_order_t

val ordered_iter: build_order_t -> (Vertex.t list -> unit) -> unit

val max_parallelism: build_order_t -> int
