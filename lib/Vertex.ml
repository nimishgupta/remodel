type t = {
           target : Rules.target;
           action : Rules.action;
         }

let hash v = Hashtbl.hash v.target
let compare x y = Pervasives.compare x.target y.target
let equal x y = x.target = y.target


let to_vertex (trgt: Rules.target) (actn : Rules.action) : t =
  { target = trgt; action = actn }

let target_in (v : t) : Rules.target = v.target
let action_in (v : t) : Rules.action = v.action

