let usage_msg = "Usage: remodel [options] [target] ...\n" ^
                "Options:"

(* Command line switches 
    -j for controlling parallelism
    -f for fixed filename
    -n for only printing info
    -B for forcing rebuild
    -d for extra debugging
    -s for silent mode (no printing of action)
*)

let njobs      = ref 0
let rmdfile    = ref None
let demo       = ref false
let spl_target = ref None
let invalidate = ref false
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
 ("-B", Arg.Set invalidate, "Force rebuild of all dependencies.");
]


let default = ["remodelfile"; "Remodelfile"]


let process_vertex_to_build (v : Vertex.t) : Build.t =
  let open Rules in
  let trgt = Vertex.target_in v in
  let actn = Vertex.action_in v in
  let is_dirty = (!invalidate) || DirtySet.is_dirty v in
  let dgst = (if not (is_pseudo trgt) then DB.get (to_file trgt) else None) in
  Build.to_t trgt actn is_dirty dgst

let mark_succ_dirty (dag : DAG.t) (v : Vertex.t) : unit =
  List.iter (fun v' -> DirtySet.mark v') (DAG.succ dag v)

(* XXX : target and action are not a good abstraction in retrospect, they are falling apart *)
let process_res_after_build (dag : DAG.t) (r : Build.t): unit =
  let open Vertex in
  let open Rules in
  let v = to_vertex r.Build.target r.Build.action in
  (match is_pseudo r.Build.target, r.Build.force, r.Build.digest with
     | true, _, _    -> DirtySet.cleanse v

     | false, true, Some d ->
         mark_succ_dirty dag v;
         DirtySet.cleanse v;
         DB.put (to_file r.Build.target) d;

     | false, false, Some d -> ()

     | _, _, _ -> assert false)

let init_md5_db () : unit = 
 (try DB.init ()
   with DB.Db_error str -> 
     Log.error ("Error initializing remodel index: " ^ str) 1);
  at_exit DB.dump


let remodel (file : string) (target : Rules.target): unit = 
  let ch = open_in file in
  let rules = Parser.program Lexer.token (Lexing.from_channel ch) in
  let dag = DAG.build_graph rules target in
  let build_info = DAG.make_build_order dag in
  let size = if !njobs > 0 then !njobs 
             else DAG.max_parallelism build_info in
  (if size = 0 then Log.error ("Nothing to be done for \"" ^ (Rules.to_target_string target) ^ "\"") 0);
  Log.info ("Max concurrency: " ^ (string_of_int size));

  init_md5_db (); (* good time to init md5 db *)
  let session, parallel_builder = 
    Parallel.start process_vertex_to_build
      Build.build (process_res_after_build dag) size
  in DAG.ordered_iter build_info parallel_builder;
     Parallel.terminate (session);
     (* Since we have built everything successfully, every valid
      * dependency should have an entry in DB, its an opportune
      * time to prune dead entries, say on account of target renaming
      *)
     DB.collect_garbage ()



let main =
    Arg.parse spec special_target usage_msg;

    let candidate_files = (match !rmdfile with 
      | None -> default 
      | Some f -> [f]) in

    let target = (match !spl_target with 
      | None -> Rules.to_target "DEFAULT" 
      | Some t -> Rules.to_target t) in

    Log.info ("Building target " ^ (Rules.to_target_string target));

    try 
      let file = List.find Sys.file_exists candidate_files in
      remodel file target
    with Not_found -> Log.error "Rules file not found" 1

let () = main
