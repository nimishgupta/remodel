(*
 * Key Ideas :
 * This module should server md5, it should provide an init function that shall load up md5 hashes in memory
 * The access needs to be thread safe
 * Client should be able to query md5 sums
 * Should the md5 be incremental?
 *
 *
 *
 * It should provide a dump facility
 *)


(* TODO : Model as key value store *)

exception Db_error of string


let dirname = ".remodel"
let filename = "index.rmd"
let sep = ':'

(* Create a hash table *)
let tbl = Hashtbl.create 100 (* TODO : Arbit, change *)


(* TODO : Do we need to play around with umask? *)
let create_dir (name : string) : unit =
  Unix.mkdir name 0o644;
  if not (Sys.file_exists name)
  then raise (Db_error ("Failed to create directory: " ^ name))

(* TODO : Do we need to play around with umask? *)
let create_file (path : string) : unit =
  close_out (open_out path); (* Create/Truncate *)
  if not (Sys.file_exists path)
  then raise (Db_error ("Failed to create file: " ^ path))


let rec split (str : string) (sep : Char.t) : string list =
  try let i = String.index str sep in
      (String.sub str 0 i) :: split (String.sub str (i+1) ((String.length str) - (i+1))) sep
  with Not_found -> [str]
  

(* TODO : Give types *)
let populate tbl path = 
  let chan = try Some (open_in path) with _ -> None in match chan with
    | None -> raise (Db_error ("Error opening file: " ^ path))
    | Some ch -> let rec process () = 
        try let line = input_line ch   in
            let parts = split line sep in
            if List.length parts <> 2 then raise (Db_error "File parse error")
            else let file = List.nth parts 0 in
                 let hash = List.nth parts 1 in
                 Hashtbl.add tbl file hash; process ()
        with End_of_file -> close_in ch in process ()
  

let init () = 
  let path = Filename.concat dirname filename in
  let dir_exists  = Sys.file_exists dirname in
  let file_exists = Sys.file_exists filename in
  match dir_exists, file_exists with
    | false, _    -> create_dir dirname; create_file path
    | true, false -> create_file path
    | true, true  -> populate tbl path
        

let put (path : string) (md5 :string) : unit = Hashtbl.replace tbl path md5

let get (path : string) : string option = 
  try Some (Hashtbl.find tbl path) with Not_found -> None


let move (src : string) (dst : string) : unit =
   if Sys.file_exists dst then Sys.rename dst (dst ^ ".org");
   Sys.rename src dst;
   Sys.remove src;
   if Sys.file_exists (dst ^ ".orig") then Sys.remove (dst ^ ".org")

  
  
let dump () : unit = 
  let path = Filename.concat dirname filename in
  try let tmp_path, ch = Filename.open_temp_file "index" "rmd" in
      Hashtbl.iter (fun file md5 -> output_string ch (file ^ (String.make 1 sep) ^ md5)) tbl;
      close_out ch; move tmp_path path
  with _ -> raise (Db_error "Error dumping file")
  
  
