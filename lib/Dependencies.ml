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

let rev_topo () = T.fold (fun (v : V.t) (vlst : V.t list) -> v :: vlst) g []

(* TODO : Put in a "parallel" module that provides an iter or fold function *)
module TSM = Map.Make (struct
  type t = V.t
  let compare = V.compare
end)

type logical_ts = int TSM.t

let happens_before (vlst : V.t list) : logical_ts = 
  let f = fun (v : V.t) (ts_map : logical_ts) -> 
          if (G.in_degree g v) > 0 
          then let preds = G.pred g v in 
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


(* TODO : make it mature *)
let rec worker (chan : 'a Event.channel) = 
  let node = Event.sync (Event.receive chan) in
  ignore (match node.V.action with
    | Some a -> (* Sys.command a *) print_string a; print_newline (); 0
    | None -> 0); worker chan
    

(* TODO : Handle creation errors *)
let rec thread_pool (n : int) (chan : 'a Event.channel) : unit =
  if n > 0 then begin ignore (Thread.create worker chan); thread_pool (n - 1) chan end


let (|>) v f = f v

let imap () = rev_topo () |> happens_before |> happens_before'

let ordered_build (imap : inverted_ts) (chan : 'a Event.channel) : unit =
  let process _ vlst = List.iter (fun v -> Event.sync (Event.send chan v)) vlst
  in ITSM.iter process imap
