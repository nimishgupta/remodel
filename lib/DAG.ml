open Graph

(* XXX : stop using V *)
module V = Vertex

module DAG = Imperative.Digraph.Concrete (V)

let g = DAG.create ()

let process_rule (action_of : Rules.target -> Rules.action) 
                 (trgt : Rules.target) 
                 (deps : Rules.deps)
                 (actn : Rules.action) : unit =
  let open Vertex in
  let open Rules  in
  let open List   in
  let sink = to_vertex trgt actn in
  let srcs = map (fun t -> to_vertex t (action_of t)) (deps_to_targets deps) in
  iter (fun src -> DAG.add_edge g src sink) srcs
  

(* TODO : Add arbitrary target support *)
let build_graph (rules : Rules.t) : unit =
  let action_of = Rules.rule_action rules in
  Rules.iter (process_rule action_of) rules

let succ (v : V.t) : V.t list = DAG.succ g v

let has_cycle () : bool =
  let module DFS = Graph.Traverse.Dfs (DAG) in DFS.has_cycle g

(* topological sort *)
let rev_topo () : V.t list = 
  let module T = Graph.Topological.Make (DAG) in
    T.fold (fun (v : V.t) (vlst : V.t list) -> v :: vlst) g []

(* TODO : Cleanup *)

(* TODO : Put in a "parallel" module that provides an iter or fold function *)
module TSM = Map.Make (struct
  type t = V.t
  let compare = V.compare
end)

type logical_ts = int TSM.t

let happens_before (vlst : V.t list) : logical_ts = 
  let f = fun (v : V.t) (ts_map : logical_ts) -> 
          if (DAG.in_degree g v) > 0 
          then let preds = DAG.pred g v in 
               let maxdist = List.fold_right (fun (v : V.t) (dist : int) ->
                                                 let cur_dist = try TSM.find v ts_map with Not_found -> 0
                                                 in max dist cur_dist)
                             preds 0 in
                             TSM.add v (maxdist + 1) ts_map
          else TSM.add v 0 ts_map
  in List.fold_right f vlst TSM.empty


(* Construct an inverted map *)
module ITSM = Map.Make (struct
  type t = int
  let compare = Pervasives.compare
end)
type inverted_ts = V.t list ITSM.t
let happens_before' (m : logical_ts) : inverted_ts =
  TSM.fold (fun (v : V.t) (ts : int) (m' : inverted_ts) ->
              ITSM.add ts (v :: (try ITSM.find ts m' with Not_found -> [])) m') m ITSM.empty


let (|>) v f = f v

let imap () = rev_topo () |> happens_before |> happens_before'

(*
let ordered_build (imap : inverted_ts) (chan : 'a Event.channel) : unit =
  let process _ vlst = List.iter (fun v -> Event.sync (Event.send chan v)) vlst
  in ITSM.iter process imap
*)
