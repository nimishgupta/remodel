(* TODO *)

(* Command line arguments *)
let njobs = ref 0
(*
let rmdfile: string option ref = ref None
let demo: bool = ref false
let help: bool = ref false
let debug: bool = ref false
let help: bool = ref false
let silent: bool = ref false
*)



(* Command line switches 
    - -j for controlling parallelism
    - -f for fixed filename
    - -n for only printing info
    - -h/--help for help
    - -d for extra debugging
    - -s for silent mode (no printing of action)
*)

(*
let specs =
[
 ("-j", Arg.Int set_njobs, 
*)


(* TODO : if a filename is specified on command line then change process root as per the directory *)


(* TODO : Use at_exit to dump file *)

let default = ["remodelfile"; "Remodelfile"]


(* TODO *)
(* let error str code = print_string str; print_newline (); *)




  
(* TODO : Reconsider dispatch and process code in wake that target abstraction and vertex abstraction are completely falling apart *)
(* Given a vertex, extract target and action, set a flag if target is in dirty list, gets its optional md5, wrap it up in a record and ship it for parallel execution *)
let dispatch (wrkr_ch : Build.t option Event.channel) (v : Vertex.t) : unit =
  let open Rules in
  let trgt = Vertex.target_in v in
  let warg = { 
               Build.target = trgt;
               Build.action = Vertex.action_in v;
               Build.force  = DirtySet.is_dirty v;
               Build.digest = if not (is_pseudo trgt) then DB.get (to_file trgt) else None;
             }
  in Event.sync (Event.send wrkr_ch (Some warg))

let process (r : Build.rt ) : unit =
  let v = Vertex.to_vertex r.Build.trgt r.Build.actn in
  if r.Build.frc then let vlst = DAG.succ v in
  begin
    List.iter DirtySet.mark vlst;
    (* Update digest *)
  end   (* cleanse *);
  DirtySet.cleanse v



let build_parallel (coll_ch : int option Event.channel)
                   (wrkr_ch : Build.t option Event.channel)
                   (cres_ch : Build.rt list Event.channel)
                   (vlst    : Vertex.t list) : unit =
  assert ([] <> vlst);
  let len = List.length vlst in
  (* 1. Signal collector *)
  Event.sync (Event.send coll_ch (Some len));
  List.iter (dispatch wrkr_ch) vlst;
  (* 3.Collect results *)
  let rlst = Event.sync (Event.receive cres_ch) in
  assert ([] <> rlst);
  List.iter process rlst


let rec collector (coll_ch : int option Event.channel) 
                  (res_ch  : Build.rt Event.channel) 
                  (cres_ch : Build.rt list Event.channel) : unit =
  match Event.sync (Event.receive coll_ch) with
    | None -> ()
    | Some n ->
        let rec results n =
           if n = 0 then []
           else (Event.sync (Event.receive res_ch)) :: results (n - 1)
        in Event.sync (Event.send cres_ch (results n));
        collector coll_ch res_ch cres_ch


let rec worker (wrkr_ch : Build.t option Event.channel) 
               (res_ch  : Build.rt Event.channel) : unit =
  match Event.sync (Event.receive wrkr_ch) with
    | None -> () 
    | Some arg ->
        let res = Build.build_wrap arg in
        Event.sync (Event.send res_ch res);
        worker wrkr_ch res_ch

let rec thread_pool (worker : 'a -> 'b) (arg : 'a) (n : int) : Thread.t list =
  if n = 0 then []
  else (Thread.create worker arg) :: thread_pool worker arg (n - 1)

 
let remodel (file : string) : unit = 
  let cin = open_in file in
  let rules = Parser.program Lexer.token (Lexing.from_channel cin) in
  DAG.build_graph rules;
  (* TODO : Give help in error message as to what is causing a cycle *)
  if DAG.has_cycle () then print_string "remodel: cyclic dependency detected"
  else 
    (* XXX: Imperative code smell *)
    DB.init ();
    let size  = if !njobs > 0 then !njobs else 10 (* TODO *) in
    let collector_ch    = Event.new_channel () in
    let worker_ch       = Event.new_channel () in
    let results_ch      = Event.new_channel () in
    let comb_results_ch = Event.new_channel () in
    let coll_tid  = List.hd (thread_pool (collector collector_ch results_ch) comb_results_ch 1) in
    let wrkr_tids = thread_pool (worker worker_ch) results_ch size in
    DAG.ordered_iter (build_parallel collector_ch worker_ch comb_results_ch);
    List.iter (fun _ -> Event.sync (Event.send worker_ch None)) wrkr_tids;
    Event.sync (Event.send collector_ch None);
    List.iter Thread.join wrkr_tids; Thread.join coll_tid


let () = 
  try let file = List.find Sys.file_exists default in remodel file
  with Not_found -> print_string "remodel: Invalid input file\n"
