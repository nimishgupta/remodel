type cmd  = string
type file = string

type action = cmd option
type deps   = file list

type target =
  | File of file
  | Default

let to_target (file : string) : target =
  if "DEFAULT" = file then Default else File file

let to_deps (flst : file list) : deps = flst

let to_action (action : string option) : action = action

let exec_action (action : action) : int option = match action with
  | Some a -> Some (Sys.command a)
  | None -> None


module Rules = Map.Make (struct
  type t = target
  let compare = Pervasives.compare
end)

type t = (deps * action) Rules.t

type rule = target * deps * action

let process (rules : t) ((tgt, deps, actn) : rule) : t =
  if Rules.mem tgt rules then 
    begin 
      Log.warning "duplicate rule found, overriding";
      Rules.add tgt (deps, actn) rules
    end
  else Rules.add tgt (deps, actn) rules
  

let to_rules (rlst : rule list) : t =
  List.fold_left process Rules.empty rlst


let rule_action (rules : t) (tgt : target) : action =
  try let _, actn = Rules.find tgt rules in actn
  with Not_found -> None


let iter f rules =
  let f' trgt (deps, actn) = f trgt deps actn in Rules.iter f' rules

let fold f rules acc =
  let f' trgt (deps, actn) acc' = f trgt deps actn acc' in Rules.fold f' rules acc

let deps_to_targets deps =
  List.map (fun f -> File f) deps

let is_pseudo (trgt : target) : bool = match trgt with
  | File _  -> false
  | Default -> true

(* XXX : Should it raise an exception or return an option *)
let to_file (trgt : target) : string = match trgt with
  | File f  -> f
  | Default -> invalid_arg "remodel: Not a file"

let to_target_string (trgt : target) : string = match trgt with
  | File f  -> f
  | Default -> "DEFAULT"

let find = Rules.find

let print_action (actn : action) : unit = match actn with
  | Some a -> Log.out a
  | None -> ()
