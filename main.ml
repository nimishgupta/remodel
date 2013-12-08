let usage_msg = "Usage: remodel [options] [target] ...\n" ^
                "Options:"

(* Command line switches 
    -j for controlling parallelism
    -f for fixed filename
    -n for only printing info
    -d for extra debugging
    -s for silent mode (no printing of action)
*)

let njobs      = ref 0
let rmdfile    = ref None
let demo       = ref false
let spl_target = ref None
(*
let debug: bool = ref false
let silent: bool = ref false
*)

let set_njobs (j : int) : unit =
  if j >= 0 then njobs := j
  else raise (Arg.Bad ("Invalid argument " ^ (string_of_int j)))

let special_target (t : string) : unit =
  spl_target := Some t

let spec =
[
 ("-j", Arg.Int set_njobs, "Number of jobs to execute in parallel. All actions are run in parallel by default");
 ("-f", Arg.String (fun s -> rmdfile := Some s), "Use a file other than default (\"remodelfile\" | \"Remodelfile\").");
 ("-n", Arg.Set demo, "Don't run commands. Prints commands that shall be run.");
]


let invalidated = ref false
let default = ["remodelfile"; "Remodelfile"]

(* TODO : All threading stuff should be moved to Parallel module *)
(* TODO : Reconsider dispatch and process code in wake that target abstraction
          and vertex abstraction are completely falling apart *)
let dispatch (wrkr_ch : Build.t option Event.channel) (v : Vertex.t) : unit =
  let open Rules in
  let trgt = Vertex.target_in v in
  let actn = Vertex.action_in v in
  let is_dirty = (!invalidated) || DirtySet.is_dirty v in
  let dgst = (if not (is_pseudo trgt) then DB.get (to_file trgt) else None) in
  let warg = Build.to_t trgt actn is_dirty dgst
  in Event.sync (Event.send wrkr_ch (Some warg))



let mark_succ_dirty (v : Vertex.t) : unit =
  List.iter (fun v' -> DirtySet.mark v') (DAG.succ v)
  
let process_res (r : Build.t): unit =
  let open Vertex in
  let open DirtySet in
  let open DB in
  let open Rules in
  let v = to_vertex r.Build.target r.Build.action in
  (match is_pseudo r.Build.target, r.Build.force, r.Build.digest with
     | true, _, _    -> cleanse v

     | false, true, Some d ->
         mark_succ_dirty v;
         cleanse v;
         put (to_file r.Build.target) d;

     | false, false, Some d -> ()

     | _, _, _ -> assert false)


let build_parallel (coll_ch : int option Event.channel)
                   (wrkr_ch : Build.t option Event.channel)
                   (cres_ch : Build.t list Event.channel)
                   (vlst    : Vertex.t list) : unit =
  assert ([] <> vlst);
  let len = List.length vlst in
  Event.sync (Event.send coll_ch (Some len));
  List.iter (dispatch wrkr_ch) vlst;
  let rlst = Event.sync (Event.receive cres_ch) in
  assert ([] <> rlst);
  List.iter process_res rlst


let rec collector (coll_ch : int option Event.channel) 
                  (res_ch  : Build.t Event.channel) 
                  (cres_ch : Build.t list Event.channel) : unit =
  match Event.sync (Event.receive coll_ch) with
    | None -> ()
    | Some n ->
        let rec results n =
           if n = 0 then []
           else (Event.sync (Event.receive res_ch)) :: results (n - 1)
        in Event.sync (Event.send cres_ch (results n));
        collector coll_ch res_ch cres_ch


let rec worker (wrkr_ch : Build.t option Event.channel) 
               (res_ch  : Build.t Event.channel) : unit =
  match Event.sync (Event.receive wrkr_ch) with
    | None -> () 
    | Some arg ->
        try
          let res = Build.build arg in
          Event.sync (Event.send res_ch res);
          worker wrkr_ch res_ch
        with Build.Build_error str -> Log.error str 1

let rec thread_pool (worker : 'a -> 'b) (arg : 'a) (n : int) : Thread.t list =
  if n = 0 then []
  else (Thread.create worker arg) :: thread_pool worker arg (n - 1)

 
let remodel (file : string) (target : Rules.target): unit = 
  let cin = open_in file in
  let rules = Parser.program Lexer.token (Lexing.from_channel cin) in
  DAG.build_graph rules target;
  (try DB.init ()
   with DB.Db_error str -> Log.error ("Error initializing remodel index: " ^ str) 1);
  let digest = Digest.to_hex (Digest.file file)
  in (invalidated := match DB.get file with
        | None -> true (* No entry for rules file *)
        | Some digest' when digest <> digest' -> true
        | Some _ -> false);

     at_exit DB.dump;
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
     List.iter Thread.join wrkr_tids; Thread.join coll_tid;
     (* Since we have built everything successfully, every valid
      * dependency should have an entry in DB, its an opportune
      * time to add self's digest and prune dead entries, say on
      * account of target renaming
      *)
    DB.put file digest;
    DB.collect_garbage ()


let main =
    Arg.parse spec special_target usage_msg;

    let candidate_files = (match !rmdfile with 
      | None -> default 
      | Some f -> [f]) in

    let target = (match !spl_target with 
      | None -> Rules.to_target "DEFAULT" 
      | Some t -> Rules.to_target t) in

    try 
      let file = List.find Sys.file_exists candidate_files in
      remodel file target
    with Not_found -> Log.error "Invalid input file" 1

let () =  main
