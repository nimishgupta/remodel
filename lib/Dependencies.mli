type action = string
type file   = string
type files  = file list

type target =
  | File of file
  | Default

type dependencies = (target * files * action option) list

type rules_map

val assoc_list_to_map: dependencies -> rules_map

(* Add arbitrary target support *)
val build_graph: rules_map -> unit


val has_cycle: unit -> bool

type inverted_ts

val imap: unit -> inverted_ts


module Vertex: sig
  type t = {
             file   : target;
             action : action option;
           }

  val hash: t -> int
  val compare: t -> t -> int
  val equal: t -> t -> bool
end


val ordered_build: inverted_ts -> Vertex.t Event.channel -> unit

