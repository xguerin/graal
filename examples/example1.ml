open Fstream

(*
 * Define a string-based algebra.
 *)

module StringType
  : Graph.OrderedType with type t = string
= struct
  type t = string
  let compare = String.compare
  let to_string v = v
end

module Algebra = Graph.Algebra(StringType)

(*
 * Graph function.
 *)

let algebra_cons () =
  let open Algebra in
  Overlay (
    Overlay (Overlay (Overlay (Connect (Overlay (Vertex "B0", Vertex "B1"), Vertex "M0"),
                               Connect (Vertex "M0", Vertex "X0")),
                      Connect (Vertex "X0", Vertex "Y0")),
             Connect (Vertex "Y0", Vertex "S0")),
    Connect (Vertex "S0", Overlay (Vertex "K0", Vertex "K1")))

let algebra_oper () =
  let open Algebra in
  (Vertex "B0" +> Vertex "B1") *> Vertex "M0"
  +>
  Vertex "M0" *> Vertex "X0"
  +>
  Vertex "X0" *> Vertex "Y0"
  +>
  Vertex "Y0" *> Vertex "S0"
  +>
  Vertex "S0" *> (Vertex "K0" +> Vertex "K1")

let check_graphs () =
  let open Algebra in
  let g1 = algebra_cons ()
  and g2 = algebra_oper ()
  in
  Printf.printf "%s\n" (to_string g1);
  Printf.printf "%s\n" (to_string g2);
  assert (g1 = g2)

let check_comparisons () =
  let open Algebra in
  assert (((Vertex "B0" +> Vertex "B1") *> Vertex "M0") =
          (Connect (Overlay (Vertex "B0", Vertex "B1"), Vertex "M0")));
  assert (((Vertex "B0" +> Vertex "B1") *> Vertex "M0" +> Vertex "M0" *> Vertex "X0") =
          (Overlay (Connect (Overlay (Vertex "B0", Vertex "B1"), Vertex "M0"), Connect (Vertex "M0", Vertex "X0"))))

let check_eval () =
  let open Algebra in
  let alg = algebra_oper () in
  let res = Algebra.eval alg in
  match res with
  | Graph (edges, vertices) ->
    List.iter (fun (Topology.Edge(Vertex v0, Vertex v1)) ->
        Printf.printf "%s -> %s\n" (StringType.to_string v0) (StringType.to_string v1))
    edges;
    List.iter (fun (Topology.Vertex v) ->
        Printf.printf "%s\n" (StringType.to_string v))
      vertices

let () =
  check_comparisons ();
  check_graphs ();
  check_eval ()
