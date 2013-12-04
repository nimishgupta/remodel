(* Hash set of verted *)


module VertexSet = Hashtbl.Make (struct
  type t = Vertex.t
  let equal = Vertex.equal
  let hash = Vertex.hash
end)

let set = VertexSet.create 100


let is_dirty (v : VertexSet.key) : bool = VertexSet.mem v set

let cleanse (v : VertexSet.key) : unit = VertexSet.remove v set

let mark (v : VertexSet.key) : unit = VertexSet.replace v 0 set
