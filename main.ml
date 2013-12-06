(* TODO *)

(* Command line arguments *)
(*
let njobs: int ref = ref 0
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


(* TODO : Make local *)
let collector_ch = Event.new_channel ()
let worker_ch    = Event.new_channel ()
let results_ch   = Event.new_channel ()
let comb_results_ch = Event.new_channel ()

let rec collector () : unit =
  match Event.sync (Event.receive collector_ch) with
    | None -> ()
    | Some n ->
        let rec results n =
           if n = 0 then []
           else (Event.sync (Event.receive results_ch)) :: results (n - 1)
        in Event.sync (Event.send comb_results_ch (results n));
        collector ()

  

let rec worker () : unit =
  match Event.sync (Event.receive worker_ch) with
    | None -> () 
    | Some arg ->
        let res = Build.build_wrap arg in
        Event.sync (Event.send results_ch res);
        worker ()


let rec thread_pool (worker : 'a -> 'b) (arg : 'a) (n : int) : Thread.t list =
  if n = 0 then []
  else (Thread.create worker arg) :: thread_pool worker arg (n - 1)

  
(* TODO : Reconsider dispatch and process code in wake that target abstraction and vertex abstraction are completely falling apart *)
(* Given a vertex, extract target and action, set a flag if target is in dirty list, gets its optional md5, wrap it up in a record and ship it for parallel execution *)
let dispatch (v : Vertex.t) : unit =
  let open Rules in
  let trgt = Vertex.target_in v in
  let warg = { 
               Build.target = trgt;
               Build.action = Vertex.action_in v;
               Build.force  = DirtySet.is_dirty v;
               Build.digest = if not (is_pseudo trgt) then DB.get (to_file trgt) else None;
             }
  in Event.sync (Event.send worker_ch (Some warg))

let process (r : Build.rt ) : unit =
  let v = Vertex.to_vertex r.Build.trgt r.Build.actn in
  if r.Build.frc then let vlst = DAG.succ v in
  begin
    List.iter DirtySet.mark vlst;
    (* Update digest *)
  end   (* cleanse *);
  DirtySet.cleanse v



let build_parallel (vlst : Vertex.t list) : bool =
  assert ([] <> vlst);
  let len = List.length vlst in
  (* 1. Signal collector *)
  Event.sync (Event.send collector_ch (Some len));
  List.iter dispatch vlst;
  (* 3.Collect results *)
  let rlst = Event.sync (Event.receive comb_results_ch) in
  assert ([] <> rlst);
  (* 4. Process results *)
  List.iter process rlst; true

  
  
let remodel (file : string) : unit = 
  let cin = open_in file in
  let rules = Parser.program Lexer.token (Lexing.from_channel cin) in 
  DAG.build_graph rules;
  (* TODO : Give help in error message as to what is causing a cycle *)
  if DAG.has_cycle () then print_string "remodel: cyclic dependency detected"
  else let chan = Event.new_channel () in
       (* DAG.thread_pool 10 chan; *)
       DAG.ordered_build (DAG.imap ()) chan

(* Initialize vertex set *)


(* TODO : Launch a thread pool *)
(* TODO : Launch a collector *)
let () = 
  try let file = List.find Sys.file_exists default in remodel file
  with Not_found -> print_string "remodel: Invalid input file\n"
