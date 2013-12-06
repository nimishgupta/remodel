(* In : target
        action
        dirty_bit
        old_hash

  Out : was_built
        new_hash
        Node
*)


(* Conditions for executing an action 
    - target should not be a pseudo target
    - If target does not exist then build it unconditionally
    - if it is marked as dirty then we surely build it unconditionally
    - if digest do not match then we surely build it unconditionally

    Digest could not be present in some scenarios
*)
type t = {
           target : Rules.target;
           action : Rules.action;
           force  : bool;
           digest : Digest.t option;
         }

type rt = {
            trgt : Rules.target;
            actn : Rules.action;
            (* Are force and code being redundant over here *)
            frc  : bool;
            code : int option;
            dgst : Digest.t option;
          }


(* TODO On return the caller should check for return code and compute hash *)
let build (file : string) (actn : Rules.action) (force : bool) (digest : Digest.t option) : (bool * int option) =
  let open Rules in
  let exists = Sys.file_exists file in
  if force || not exists ||  digest <> (Some (Digest.file file))
  then true, Rules.exec_action actn
  else false, None


(* Check if target is pseudo target,
   if yes check if force is set
   if it is then check execute action and return
   
   otherwise extract filename and send to build

   Collect results, make sure file exists (raise an error if it doesn't) and compute latest hash
*)

let _build_wrap (trgt: Rules.target) 
                (actn : Rules.action)
                (force : bool)
                (digest : Digest.t option) : (Rules.target * Rules.action * bool * int option * Digest.t option) =
  let open Rules in
  if is_pseudo trgt && force
  then trgt, actn, true, Rules.exec_action actn, None
  else let file = Rules.to_file trgt in
       let force', code = build file actn force digest in
       if not (Sys.file_exists file) then failwith "remodel: Failed to build target\n"
       else let digest' = Some (Digest.file file) in
            trgt, actn, force', code, digest'


let build_wrap (v : t) : rt =
  {
    trgt = v.target;
    actn = v.action;
    frc  = v.force;
    code = None;
    dgst = None;
  }
