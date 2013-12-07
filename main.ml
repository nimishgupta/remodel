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
  else raise (Arg.Bad ("Invalid argument " ^ (string_of_int j) ^ " for j"))

let special_target (t : string) : unit = spl_target := Some t

let spec =
[
 ("-j", Arg.Int set_njobs, "Number of jobs to execute in parallel. All actions are run in parallel by default");
 ("-f", Arg.String (fun s -> rmdfile := Some s), "Use a file other than default (\"remodelfile\" | \"Remodelfile\").");
 ("-n", Arg.Set demo, "Don't run commands. Prints commands that shall be run.");
]


let default = ["remodelfile"; "Remodelfile"]


(* TODO *)
(* let error str code = print_string str; print_newline (); *)

(* TODO : Reconsider dispatch and process code in wake that target abstraction
          and vertex abstraction are completely falling apart *)
let dispatch (wrkr_ch : Build.t option Event.channel) (v : Vertex.t) : unit =
  let open Rules in
  let trgt = Vertex.target_in v in
  let actn = Vertex.action_in v in
  let is_dirty = DirtySet.is_dirty v in
  let dgst = (if not (is_pseudo trgt) then DB.get (to_file trgt) else None) in
  let warg = Build.to_t trgt actn is_dirty dgst
  in Event.sync (Event.send wrkr_ch (Some warg))



(* Check if pseudo target and was forced
   then Check for code, if code exists then if not 0 then error

   if not pseudo and force is set then check for code 
*)

let mark_succ_dirty (v : Vertex.t) : unit =
  List.iter (fun v -> DirtySet.mark v) (DAG.succ v)
  
let process_res (r : Build.rt) (acc : bool): bool =
  let open Vertex in
  let open DirtySet in
  let open DB in
  let open Rules in
  let v = to_vertex r.Build.trgt r.Build.actn in
  acc && (match is_pseudo r.Build.trgt, r.Build.frc, r.Build.code, r.Build.dgst with
    | true, true, Some c,  _ -> 
        if c = 0 
        then begin cleanse v; true end
        else false

    | true, _, _, _          -> cleanse v; true

    | false, true, Some c, Some d ->
        if c = 0 then
          begin
            mark_succ_dirty v;
            cleanse v;
            put (to_file r.Build.trgt) d;
            true
          end
        else false

    | false, _, None,   Some d -> cleanse v; put (to_file r.Build.trgt) d; true

    | _, _, _, _ -> assert false)


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
  (* TODO : Remove ignore and process errors if any *)
  ignore (List.fold_right process_res rlst true)


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
        let res = Build.build arg in
        Event.sync (Event.send res_ch res);
        worker wrkr_ch res_ch

let rec thread_pool (worker : 'a -> 'b) (arg : 'a) (n : int) : Thread.t list =
  if n = 0 then []
  else (Thread.create worker arg) :: thread_pool worker arg (n - 1)

 
(* TODO : Make use of target *)
let remodel (file : string) (target : Rules.target): unit = 
  let cin = open_in file in
  let rules = Parser.program Lexer.token (Lexing.from_channel cin) in
  DAG.build_graph rules;
  (* TODO : Give help in error message as to what is causing a cycle *)
  if DAG.has_cycle () then 
    begin
      print_string "remodel: cyclic dependency detected";
      exit 1
    end
  else 
    (* XXX: Imperative code smell *)
    DB.init ();
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
    List.iter Thread.join wrkr_tids; Thread.join coll_tid

(* TODO : if a filename is specified on command line then change process root as per the directory *)
let main =
    Arg.parse spec special_target usage_msg;
    let candidate_files = (match !rmdfile with | None -> default | Some f -> [f]) in
    let target = (match !spl_target with | None -> Rules.to_target "DEFAULT" | Some t -> Rules.to_target t) in
    try 
      let file = List.find Sys.file_exists candidate_files in
      remodel file target
    with Not_found -> print_string "remodel: Invalid input file\n"   


let () =  main
