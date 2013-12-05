type t

type target
type deps
type action

val to_target: string -> target
val to_deps: string list -> deps
val to_action: string option -> action

(* XXX: Can be to_cmd *)
val exec_action: action -> int

val to_rules: (target * deps * action) list -> t

val rule_action: t -> target -> action

val iter: (target -> deps -> action -> unit) -> t -> unit

val fold: (target -> deps -> action -> 'a -> 'a) -> t -> 'a -> 'a

val deps_to_targets: deps -> target list


