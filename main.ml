
(* Look for -f switch or "remodelfile" or "RemodelFile"
   Parse  and build
*)

(* TODO *)
(* Command line switches 
    - -j for controlling parallelism
    - -f for fixed filename
    - -n for only printing info
    - -h/--help for help
    - -d for extra debugging
    - -s for silent mode (no printing of action)
*)

(* Model as a list *)
let default = ["remodelfile"; "Remodelfile"]

let remodel (file : string) : unit = 
  let cin = open_in file in
  let rules = Parser.program Lexer.token (Lexing.from_channel cin) in 
  let rules_map = Dependencies.assoc_list_to_map rules in
  Dependencies.build_graph rules_map;
  (* TODO : Give help in error message as to what is cycle is *)
  if Dependencies.has_cycle () then print_string "remodel: cyclic dependency detected"
  else let chan = Event.new_channel () in
       Dependencies.thread_pool 10 chan; 
       Dependencies.ordered_build (Dependencies.imap ()) chan

let () = 
  try let file = List.find Sys.file_exists default in remodel file
  with Not_found -> print_string "remodel: Invalid input file\n"
