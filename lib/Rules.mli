type t

type target
type deps
type action

val to_target: string -> target
val to_deps: string list -> deps
val to_action: string option -> action

(* XXX: Can be to_cmd *)
val exec_action: action -> int option

val to_rules: (target * deps * action) list -> t

val rule_action: t -> target -> action

val iter: (target -> deps -> action -> unit) -> t -> unit

val fold: (target -> deps -> action -> 'a -> 'a) -> t -> 'a -> 'a

val find: target -> t -> deps * action

val deps_to_targets: deps -> target list

val is_pseudo: target -> bool

val to_file: target -> string

val to_string: action -> string

val to_target_string: target -> string
