type action = string
type file   = string
type files  = file list

type target =
  | File of file
  | Default

type dependencies = (target * files * action option) list

type rule = target * (files * action option)
type rule_list = rule list

module M = Map.Make (struct 
  type t = target
  let compare = Pervasives.compare
end)

type rules_map = (files * action option) M.t

(* Make map out of assoc list *)
let assoc_list_to_map (alst : dependencies) : rules_map =
  List.fold_left (fun map (t, f, a) -> M.add t (f, a) map) M.empty alst


open Graph

module Vertex = 
struct 
  type t = {
             file   : target;
             action : action option;
           }

  let hash v = Hashtbl.hash v.file
  let compare x y = Pervasives.compare x.file y.file
  let equal x y = x.file = y.file
end



module V = Vertex
module G = Imperative.Digraph.Concrete (V)

let g = G.create ()


let find_target (rules : rules_map) (file : file) : V.t =
  try
    let _, actn = M.find (File file) rules in
    { V.file = File file; V.action = actn }
  with Not_found -> { V.file = File file; V.action = None }


let process_rule (file_to_target : string -> V.t) (trgt : target) ((fs, actn) : files * action option) (_ : unit) : unit =
  let add_dag_edge_sink = G.add_edge g {V.file = trgt; V.action = actn} in
  List.iter add_dag_edge_sink (List.map file_to_target fs)
  
  
(* Add arbitrary target support *)
let build_graph (rules : rules_map) : unit =
  M.fold (process_rule (find_target rules)) rules ()


module D = Graph.Traverse.Dfs (G)

let has_cycle (_ : unit) : bool = D.has_cycle g


(* topological sort *)
module T = Graph.Topological.Make (G)

let rev_topo = T.fold (fun (v : V.t) (vlst : V.t list) -> v :: vlst) g []
