type t

val start: (Vertex.t -> Build.t) -> 
             (Build.t -> Build.t) -> 
               (Build.t -> unit) -> 
                  int -> t * (Vertex.t list -> unit)
val terminate: t -> unit
