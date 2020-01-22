module type OrderedType = sig
  type t
  val compare: t -> t -> int
  val to_string: t -> string
end

module Topology (T: OrderedType) = struct
  type vertex =
    | Vertex of T.t
  and edge =
    | Edge of vertex * vertex
  and t =
    | Graph of edge list * vertex list
end

module Algebra (T: OrderedType) = struct
  type t =
    | Vertex of T.t
    | Connect of t * t
    | Overlay of t * t

  let vertex v = Vertex v
  let connect a b = Connect (a, b)
  let overlay a b = Overlay (a, b)

  let ( +> ) a b = Overlay (a, b)
  let ( *> ) a b = Connect (a, b)

  let rec to_string = function
    | Vertex v -> "Vertex " ^ (T.to_string v)
    | Overlay (a, b) -> "Overlay (" ^ (to_string a) ^ ", " ^ (to_string b) ^ ")"
    | Connect (a, b) -> "Connect (" ^ (to_string a) ^ ", " ^ (to_string b) ^ ")"

  let rec (=) a b =
    match a, b with
    | Vertex a, Vertex b -> Int.equal (T.compare a b) 0
    | Overlay (a0, b0), Overlay (a1, b1) -> a0 = a1 && b0 = b1
    | Connect (a0, b0), Connect (a1, b1) -> a0 = a1 && b0 = b1
    | _ -> false

  module Topology = Topology(T)

  module OrderedEdge = struct
    type t = Topology.edge
    let compare a b =
      let open Topology in
      match a, b with
      | Edge (Vertex a0, Vertex b0), Edge (Vertex a1, Vertex b1) ->
        let ca = compare a0 a1 and cb = compare b0 b1 in
        if (Int.equal ca 0) then cb else ca
  end

  module OrderedVertex = struct
    type t = Topology.vertex
    let compare a b =
      let open Topology in
      match a, b with
      | Vertex a, Vertex b -> compare a b
  end

  module EdgeSet = Set.Make(OrderedEdge)
  module VertexSet = Set.Make(OrderedVertex)

  let rec eval_r a =
    match a with
    | Vertex a ->
      EdgeSet.empty, (VertexSet.add (Topology.Vertex a) VertexSet.empty)
    | Overlay (a, b) ->
      let e_a, v_a = eval_r a and e_b, v_b = eval_r b in
      EdgeSet.union e_a e_b, VertexSet.union v_a v_b
    | Connect (a, b) ->
      let e_a, v_a = eval_r a and e_b, v_b = eval_r b in
      let edges, vertices = EdgeSet.union e_a e_b, VertexSet.union v_a v_b in
      let new_e = VertexSet.fold (fun v0 acc ->
          VertexSet.fold (fun v1 acc ->
              EdgeSet.add (Topology.Edge (v0, v1)) acc) v_b EdgeSet.empty
          |> EdgeSet.union acc)
          v_a EdgeSet.empty
      in
      (EdgeSet.union edges new_e, vertices)

  let eval a =
    let edges, vertices = eval_r a in
    Topology.Graph (EdgeSet.elements edges, VertexSet.elements vertices)
end
