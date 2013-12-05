val build_graph: Rules.t -> unit

val has_cycle: unit -> bool

type inverted_ts
val imap: unit ->  inverted_ts
val ordered_build: inverted_ts -> Vertex.t Event.channel -> unit
