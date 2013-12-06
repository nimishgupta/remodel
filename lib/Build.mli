

(* Way too many boxing and unboxing parameters *)
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
            code   : int option;
            dgst : Digest.t option;
          }


val build_wrap: t -> rt
