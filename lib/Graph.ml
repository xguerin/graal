(*
 * Type definitions.
 *)

module EdgeKeyType = struct
  type t = string * string
  [@@deriving ord, show]
end

module Edges = Set.Make(EdgeKeyType)
module Vertices = Map.Make(String)

type 'a t = Edges.t * 'a Vertices.t
let make edges vertices = (edges, vertices)

(*
 * Algebra.
 *)

module Algebra = struct
  type 'a t =
    | Empty
    | Vertex of string * 'a
    | Connect of 'a t * 'a t
    | Overlay of 'a t * 'a t

  (*
   * Vertex constructor operation.
   *)

  let vertex l v = Vertex(l, v)
  let connect a b = Connect (a, b)
  let overlay a b = Overlay (a, b)

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
    | Vertex(l0, _), Vertex(l1, _) -> String.equal l0 l1
    | Overlay(a0, b0), Overlay(a1, b1) -> a0 = a1 && b0 = b1
    | Connect(a0, b0), Connect(a1, b1) -> a0 = a1 && b0 = b1
    | _ -> false

  (*
   * Evaluation functions.
   *)

  let rec eval_r a =
    match a with
    (* Empty reductions *)
    | Empty
    | Connect(Empty, _)
    | Connect(_, Empty) -> Edges.empty, Vertices.empty
    (* Idempotent reductions *)
    | Overlay(a, Empty)
    | Overlay(Empty, a) -> eval_r a
    (* Other reductions *)
    | Vertex(l, a) -> Edges.empty, (Vertices.add l a Vertices.empty)
    | Overlay(a, b) ->
      let e_a, v_a = eval_r a
      and e_b, v_b = eval_r b
      in
      Edges.union e_a e_b, Vertices.union (fun _ a _ -> Some a) v_a v_b
    | Connect(a, b) ->
      let e_a, v_a = eval_r a
      and e_b, v_b = eval_r b
      in
      let e_n, v_n = Edges.union e_a e_b, Vertices.union (fun _ a _ -> Some a) v_a v_b
      in
      let e_x = Vertices.fold
          (fun k0 _ acc ->
             Vertices.fold
               (fun k1 _ acc -> Edges.add (k0, k1) acc) v_b Edges.empty
             |> Edges.union acc)
          v_a Edges.empty
      in
      Edges.union e_n e_x, v_n

  let eval a =
    let e, v = eval_r a in
    make e v

  (*
   * Infix combinator.
   *)

  let sources (e, v) =
    let module E = Edges in
    let module V = Vertices in
    V.filter (fun key _ -> E.filter (fun (_, l) -> String.equal key l) e |> E.is_empty) v
    |> fun sel -> V.fold (fun k v acc -> Vertex(k, v) :: acc) sel []
    |> List.rev

  let sinks (e, v) =
    let module E = Edges in
    let module V = Vertices in
    V.filter (fun key _ -> E.filter (fun (l, _) -> String.equal key l) e |> E.is_empty) v
    |> fun sel -> V.fold (fun k v acc -> Vertex(k, v) :: acc) sel []
    |> List.rev

  let merge lst =
    let rec merge_r = function
      | [] -> Empty
      | v :: [] -> v
      | a :: b :: [] -> Overlay(b, a)
      | a :: tl -> Overlay(merge_r tl, a)
    in
    List.rev lst |> merge_r

  let ( +*> ) a b =
    let sources = eval b |> sources |> merge
    in
    Connect(a, sources)
    |> (fun r -> if b = sources then r else Overlay(r, b))

  let ( *+> ) a b =
    let sinks = eval a |> sinks |> merge
    in
    Connect(sinks, b)
    |> (fun r -> if a = sinks then r else Overlay(a, r))

  let ( */> ) a b =
    let sources = eval b |> sources |> merge
    and sinks = eval a |> sinks |> merge
    in
    Connect(sinks, sources)
    |> (fun r -> if a = sinks then r else Overlay(a, r))
    |> (fun r -> if b = sources then r else Overlay(r, b))

end
