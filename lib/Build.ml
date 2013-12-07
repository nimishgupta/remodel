exception Build_error of string


type t = {
           target : Rules.target;
           action : Rules.action;
           force  : bool;
           digest : Digest.t option;
         }

let to_t (t : Rules.target) (a : Rules.action) (f : bool) (d : Digest.t option) : t =
  {
    target = t;
    action = a;
    force  = f;
    digest = d;
  }


(* Conditions for executing an action 
    - target should not be a pseudo target
    - If target does not exist then build it unconditionally
    - if it is marked as dirty then we surely build it unconditionally
    - if digest do not match then we surely build it unconditionally
*)


let exec (action : Rules.action) (file : string) : unit =
  Rules.print_action action;
  (match Rules.exec_action action with
     | Some c when c <> 0 -> raise (Build_error ("Failed to build target " ^ file))
     | _ -> ())


let build_file (file : string)
               (actn : Rules.action)
               (force : bool)
               (digest : Digest.t option) : bool =
  let open Rules in
  let exists = Sys.file_exists file in
  if force || not exists || digest <> (Some (Digest.to_hex (Digest.file file)))
  then begin exec actn file; true end
  else false


(* Check if target is pseudo target,
   if yes check if force is set
   if it is then check execute action and return
   
   otherwise extract filename and send to build

   Collect results, make sure file exists (raise an error if it doesn't) and compute latest hash
*)

let build_target (trgt   : Rules.target) 
                 (actn   : Rules.action)
                 (force  : bool)
                 (digest : Digest.t option) : t = 
  let open Rules in
  let trgt_str = to_target_string trgt in
  let frc', digest' = (match is_pseudo trgt, force with
    | true,  true  -> true, (exec actn trgt_str; None)
    | true,  false -> false, None
    | false, _     -> 
        let file = to_file trgt in
        let force' = build_file file actn force digest in
        if not (Sys.file_exists file) then raise (Build_error ("Failed to build target "^trgt_str))
        else if force' 
             then let digest' = Some (Digest.to_hex (Digest.file file)) 
             in force', digest'
        else force', digest)
  in to_t trgt actn frc' digest'

(* XXX : provide a dummy to support -n *)
let build (v : t) : t =
  build_target v.target v.action v.force v.digest
