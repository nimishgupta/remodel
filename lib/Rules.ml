type action = string
type file   = string
type deps   = file list

type target =
  | File of file
  | Default

(* TODO : check if we need to check for empty string *)
let to_target (file : string) : target =
  if "DEFAULT" = file then Default else File file

let to_deps (flst : file list) : deps = flst

let to_action (action : string option) : action = action

let exec_action (action: action) : int = match action with
  | Some a -> Sys.command a
  | None -> 0


module Rules = Map.Make (String)

type t = (deps * action option) Rules.t

type rule = target * deps * action

let process (rules : t) ((tgt, deps, actn) : rule) : t =
  if t.mem tgt rules then 
    begin 
      print_string ("remodel: Warning, duplicate rule, overriding\n")
      t.add tgt (deps, actn) rules
    end
  else t.add tgt (deps, actn) rules
  

let to_rules (rlst : rule list) : t =
  List.fold_left process rlst t.empty


let rule_action (rules : t) (tgt : target) : action =
  try let _, actn = t.find tgt rules in actn
  with Not_found -> None


let iter f rules =
  let f' trgt (deps, actn) = f trgt deps actn in rules.iter f' rules

let fold f rules acc =
  let f' trgt (deps, actn) acc' = f trgt deps actn acc' in rules.fold f' rules acc

(* XXX : to_target is more elegant *)
let deps_to_targets deps =
  List.map (fun f -> File f) deps
