(* Hash set of verted *)


module VertexSet = Hashtbl.Make (struct
  type t = Vertex.t
  let equal = Vertex.equal
  let hash = Vertex.hash
end)

let set = VertexSet.create 100


let is_dirty (v : VertexSet.key) : bool = VertexSet.mem set v

let cleanse (v : VertexSet.key) : unit = VertexSet.remove set v

let mark (v : VertexSet.key) : unit = VertexSet.replace set v 0
