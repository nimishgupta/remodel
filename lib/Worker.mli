type t = {
           target : Rules.target;
           action : Rules.action;
           force  : bool;
           digest : Digest.t option;
         }

type rt = {
            target : Rules.target;
            action : Rules.action;
            (* Are force and code being redundant over here *)
            force  : bool;
            code   : int option;
            digest : Digest.t option;
          }
