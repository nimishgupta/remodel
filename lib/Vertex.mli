type t
val hash: t -> int
val compare: t -> t -> int
val equal: t -> t -> bool


val to_vertex: Rules.target -> Rules.action -> t
