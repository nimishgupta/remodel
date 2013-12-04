
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

let file1 = "remodelfile"
let file2 = "Remodelfile"

let build (file : string) : unit = 
  let cin = open_in file in
  let rules = Parser.program Lexer.token (Lexing.from_channel cin) in 
  let rules_map = Dependencies.assoc_list_to_map rules in
  Dependencies.build_graph rules_map;
  (* TODO : Give help as to what is cycle is *)
  if Dependencies.has_cycle () then print_string "remodel: cyclic dependency detected"
  else let chan = Event.new_channel () in
       Dependencies.thread_pool 10 chan; 
       Dependencies.ordered_build (Dependencies.imap ()) chan

let () = 
  if Sys.file_exists file1 then ignore (build file1)
  else if Sys.file_exists file2 then ignore (build file2)
  else print_string "remodel: Invalid input file\n"
