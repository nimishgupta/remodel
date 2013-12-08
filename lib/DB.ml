exception Db_error of string

let dirname = ".remodel"
let filename = "index.rmd"
let sep = ':'

let descr = ref None

let tbl = Hashtbl.create 100

(* TODO : Do we need to play around with umask? *)
let create_dir (name : string) : unit =
  try Unix.mkdir name 0o664
  with Unix.Unix_error (_, f, str) ->
         raise (Db_error (f ^ ": failed, " ^ str))

let open_file (path : string) : Unix.file_descr = 
  try Unix.openfile path [Unix.O_RDWR; Unix.O_CREAT] 0o664
  with Unix.Unix_error (_, f, str) ->
         raise (Db_error (f ^ ": failed, " ^ str))

let try_lock_file (fd : Unix.file_descr) : bool =
  try Unix.lockf fd Unix.F_TLOCK 0; true
  with _ -> false
  
let rec split (str : string) (sep : Char.t) : string list =
  let open String in
  try let i = index str sep in
      (sub str 0 i) :: split (sub str (i+1) ((length str) - (i+1))) sep
  with Not_found -> [str]
  
(* Should accept a channel or fd *)
let populate (ch : in_channel): unit = 
  let rec process () = 
    try let line = input_line ch   in
        let parts = split line sep in
        if List.length parts <> 2 then raise (Db_error "File parse error")
        else let file = List.nth parts 0 in
             let hash = List.nth parts 1 in
             Hashtbl.add tbl file hash; process ()
    with End_of_file -> ()
  in process ()
  
let init () : unit = 
  (if not (Sys.file_exists dirname && 
           Sys.is_directory dirname)
   then create_dir dirname);
  let path = Filename.concat dirname filename in
  let fd = open_file path
  in if not (try_lock_file fd)
     then raise (Db_error ("Failed trying to lock file " ^ path))
     else let in_chan = Unix.in_channel_of_descr fd in
          set_binary_mode_in in_chan false;
          populate in_chan;
          descr := Some fd

let put (path : string) (md5 :string) : unit =
  Hashtbl.replace tbl path md5

let get (path : string) : string option = 
  try Some (Hashtbl.find tbl path) with Not_found -> None

let move (src : string) (dst : string) : unit =
  if 0 <> Sys.command ("mv " ^ src ^ " " ^ dst) then raise (Failure "move")

let collect_garbage () : unit =
  let open Hashtbl in
  let tbl' = copy tbl in
  iter (fun k _ -> if not (Sys.file_exists k) then remove tbl k) tbl' 
  
let dump () : unit = 
  let path = Filename.concat dirname filename in
  try let tmp_path, ch = Filename.open_temp_file "index" "rmd" in
      Hashtbl.iter (fun file md5 -> 
                      output_string ch (file ^ (String.make 1 sep) ^ md5 ^ "\n")) tbl;
      close_out ch; 
      move tmp_path path;
      (match !descr with
         | None -> ()
         | Some fd -> try Unix.lockf fd Unix.F_ULOCK 0 with _ -> ())
  with _ -> raise (Db_error "Error dumping file")
