exception Db_error of string

(* Smell of imperative code *)
val init: unit -> unit

val put: string -> string -> unit
val get: string -> string option
val dump: unit -> unit
