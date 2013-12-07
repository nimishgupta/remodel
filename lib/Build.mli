exception Build_error of string

(* Box for communicating with this module *)
type t = {
           target : Rules.target;
           action : Rules.action;
           force  : bool;
           digest : Digest.t option;
         }

val to_t: Rules.target -> Rules.action -> bool -> Digest.t option -> t

val build: t -> t
