(* TODO *)
(* Command line switches 
    - -j for controlling parallelism
    - -f for fixed filename
    - -n for only printing info
    - -h/--help for help
    - -d for extra debugging
    - -s for silent mode (no printing of action)
*)


(* TODO : if a filename is specified on command line then change process root as per the directory *)


(* TODO : Use at_exit to dump file *)

let default = ["remodelfile"; "Remodelfile"]


(* TODO *)
(* let error str code = print_string str; print_newline (); *)


let remodel (file : string) : unit = 
  let cin = open_in file in
  let rules = Parser.program Lexer.token (Lexing.from_channel cin) in 
  DAG.build_graph rules;
  (* TODO : Give help in error message as to what is cycle is *)
  if DAG.has_cycle () then print_string "remodel: cyclic dependency detected"
  else let chan = Event.new_channel () in
       DAG.thread_pool 10 chan; 
       DAG.ordered_build (DAG.imap ()) chan

let () = 
  try let file = List.find Sys.file_exists default in remodel file
  with Not_found -> print_string "remodel: Invalid input file\n"
