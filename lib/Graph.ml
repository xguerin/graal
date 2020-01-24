module Algebra (T: Types.OrderedType) = struct
  type t =
    | Empty
    | Vertex of string * T.t
    | Connect of t * t
    | Overlay of t * t

  (*
   * Vertex constructor operation.
   *)

  let vertex l v = Vertex(l, v)

  (*
   * Connecting to nothing gets you nothing. The 0 operation is communitative.
   *)

  let connect a b =
    match a, b with
    | Empty, _ | _, Empty -> Empty
    | a, b -> Connect (a, b)

  (*
   * Empty is the identity element for Overlay.
   *)

  let overlay a b =
    match a, b with
    | Empty, a | a, Empty -> a
    | a, b -> Overlay (a, b)

  (*
   * Infix operators.
   *)

  let ( +> ) a b = overlay a b
  let ( *> ) a b = connect a b

  (*
   * Helper functions.
   *)

  let rec to_string = function
    | Empty -> "()"
    | Vertex(l, _) -> l
    | Overlay(a, b) -> "Overlay (" ^ (to_string a) ^ ", " ^ (to_string b) ^ ")"
    | Connect(a, b) -> "Connect (" ^ (to_string a) ^ ", " ^ (to_string b) ^ ")"

  let rec (=) a b =
    match a, b with
    | Empty, Empty -> true
    | Vertex(_, a), Vertex(_, b) -> Int.equal (T.compare a b) 0
    | Overlay(a0, b0), Overlay(a1, b1) -> a0 = a1 && b0 = b1
    | Connect(a0, b0), Connect(a1, b1) -> a0 = a1 && b0 = b1
    | _ -> false

  (*
   * Evaluation functions.
   *)

  let rec eval_r mkedge a =
    let module T =  Topology.Make(T) in
    match a with
    | Empty -> T.Edges.empty, T.Vertices.empty
    | Vertex(l, a) ->
      T.Edges.empty, (T.Vertices.add l (T.Vertex(a, [], [])) T.Vertices.empty)
    | Overlay(a, b) ->
      let e_a, v_a = eval_r mkedge a
      and e_b, v_b = eval_r mkedge b
      in
      T.Edges.union (fun _ a _ -> Some a) e_a e_b,
      T.Vertices.union (fun _ a _ -> Some a) v_a v_b
    | Connect(a, b) ->
      let e_a, v_a = eval_r mkedge a
      and e_b, v_b = eval_r mkedge b
      in
      let edges, vertices = T.Edges.union (fun _ a _ -> Some a) e_a e_b,
                            T.Vertices.union (fun _ a _ -> Some a) v_a v_b
      in
      let new_edges = T.Vertices.fold (fun k0 _ acc ->
          T.Vertices.fold
            (fun k1 _ acc ->
               T.Edges.add (k0 ^ ">" ^ k1) (T.Edge (mkedge ())) acc)
            v_b T.Edges.empty
        |> T.Edges.union (fun _ a _ -> Some a) acc)
        v_a
        T.Edges.empty
      in
      T.Edges.union (fun _ a _ -> Some a) edges new_edges,
      vertices

  let eval mkedge a =
    let module T =  Topology.Make(T) in
    let e, v = eval_r mkedge a in
    T.Graph(e, v)
end
