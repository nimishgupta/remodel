open Graph

(* XXX : stop using V *)
module V = Vertex

module DAG = Imperative.Digraph.Concrete (V)

type t = DAG.t

(* TODO : Make g local *)
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
  


let has_cycle (dag : t) : bool =
  let module DFS = Graph.Traverse.Dfs (DAG) in DFS.has_cycle dag


module TargetSet = Hashtbl.Make (struct
  type t = Rules.target
  let hash = Hashtbl.hash
  let equal = (=)
end)

let seen = TargetSet.create 100

(* Pre condition that is satisfied is that there cannot be duplicate targets *)
(* Invariant: If target is not psedo then either it should have a rule to build it
 *            or its correspoding file should exist
 * process_rules for target
 * List.iter to_deps and call the same function
 *)

(* TODO : Can be made tail recursive using state *)
let rec _build_graph (rules : Rules.t)
                     (target : Rules.target) : unit =
  let open Rules in
  let action_of = Rules.rule_action rules in
  let target_str = to_target_string target in
  if TargetSet.mem seen target 
  then Log.error ("Cycle detected in graph: \""^target_str^"\"") 1
  else
    let deps, actn = (try Rules.find target rules
                      with Not_found -> 
                             (if (is_pseudo target) || not (Sys.file_exists (to_file target))
                              then Log.error ("No rule to make target "^target_str^"\"") 1);
                             Rules.to_deps [], Rules.to_action None)
    in process_rule action_of target deps actn;
       TargetSet.add seen target 0;
       List.iter (_build_graph rules) (deps_to_targets deps)
  

(* Detect cycle using a seen set *)
let build_graph (rules : Rules.t) (target : Rules.target): t =
  _build_graph rules target;
  assert (not (has_cycle g));
  TargetSet.clear seen;
  g
      
let succ (dag : DAG.t) (v : V.t) : V.t list = DAG.succ dag v


(* topological sort *)
let rev_topo (dag : t) : V.t list = 
  let module T = Graph.Topological.Make (DAG) in
    T.fold (fun (v : V.t) (vlst : V.t list) -> v :: vlst) dag []


(* TODO : Cleanup *)


(* Map from vertex -> its logical time stamp in build order *)
module TSM = Map.Make (struct
  type t = V.t
  let compare = V.compare
end)

type logical_ts = int TSM.t


let happens_before (dag : t) (vlst : V.t list) : logical_ts = 
  let f (v : V.t) (ts_map : logical_ts) =
        if (DAG.in_degree dag v) > 0 
        then let preds = DAG.pred dag v in 
             let maxdist = List.fold_right (fun (v : V.t) (dist : int) ->
                                              let cur_dist = try TSM.find v ts_map 
                                                             with Not_found -> 0
                                              in max dist cur_dist)
                           preds 0 in
                          TSM.add v (maxdist + 1) ts_map
        else TSM.add v 0 ts_map
  in List.fold_right f vlst TSM.empty


(* Construct an inverted map *)
(* Map from logical timestamp to list of vertex *)
module ITSM = Map.Make (struct
  type t = int
  let compare = Pervasives.compare
end)
type inverted_ts = V.t list ITSM.t
let happens_before' (m : logical_ts) : inverted_ts =
  TSM.fold (fun (v : V.t) (ts : int) (m' : inverted_ts) ->
              ITSM.add ts (v :: (try ITSM.find ts m' with Not_found -> [])) m') m ITSM.empty


let (|>) v f = f v

let imap (dag : t) = (rev_topo dag) |> (happens_before dag) |> happens_before'


type build_order_t = inverted_ts


let make_build_order (dag : DAG.t) = imap dag

let max_parallelism (build_info : build_order_t) : int =
  ITSM.fold (fun _ vlst npar -> max npar (List.length vlst)) build_info 0


let ordered_iter (build_info : build_order_t) (f : V.t list -> unit) : unit =
  ITSM.iter (fun _ v -> f v) build_info
