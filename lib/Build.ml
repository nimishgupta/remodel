type t = {
           target : Rules.target;
           action : Rules.action;
           force  : bool;
           digest : Digest.t option;
         }

type rt = {
            trgt : Rules.target;
            actn : Rules.action;
            frc  : bool;
            code : int option;
            dgst : Digest.t option;
          }

let to_rt (t : Rules.target) (a : Rules.action) (f : bool) (c : int option) (d : Digest.t option) : rt =
  {
    trgt = t;
    actn = a;
    frc  = f;
    code = c;
    dgst = d;
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

    Digest could not be present in some scenarios
*)

let __build (file : string) (actn : Rules.action) (force : bool) (digest : Digest.t option) : (bool * int option) =
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

let _build (trgt: Rules.target) 
           (actn : Rules.action)
           (force : bool)
           (digest : Digest.t option) : rt = 
  let open Rules in
  let frc', code', digest' = (match is_pseudo trgt, force with
    | true,  true  -> true,  exec_action actn, None
    | true,  false -> false, None, None
    | false, _     -> 
        let file = to_file trgt in
        let force', code = __build file actn force digest in
        if not (Sys.file_exists file) then failwith "remodel: Failed to build target\n"
        else let digest' = Some (Digest.file file) in
            force', code, digest') in
  to_rt trgt actn frc' code' digest'




(* XXX : provide a dummy to support -n *)
let build (v : t) : rt =
  _build v.target v.action v.force v.digest
