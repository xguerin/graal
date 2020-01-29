open OUnit2

let infix_N_sink_0 _ =
  let open Fstream.Graph.Algebra in
  (* (V0 +> V1) *+> V2 = (V0 +> V1) +> (V0 +> V1) *> V2 *)
  let g0 =
    (Vertex("V0", ()) +> Vertex("V1", ()))
    +>
    (Vertex("V0", ()) +> Vertex("V1", ()))
    *>
    Vertex("V2", ())
  and g1 =
    (Vertex("V0", ()) +> Vertex("V1", ()))
    *+>
    Vertex("V2", ())
  in
  assert_equal g0 g1

let infix_N_sink_1 _ =
  let open Fstream.Graph.Algebra in
  (* (V0 +> V1) *+> V2 *> V3 = ((V0 +> V1) +> (V0 +> V1) *> V2) *> V3 *)
  let g0 =
    ((Vertex("V0", ()) +> Vertex("V1", ()))
     +>
     (Vertex("V0", ()) +> Vertex("V1", ()))
     *>
     Vertex("V2", ()))
    *>
    Vertex("V3", ())
  and g1 =
    (Vertex("V0", ()) +> Vertex("V1", ()))
    *+>
    Vertex("V2", ())
    *>
    Vertex("V3", ())
  in
  assert_equal g0 g1

let infix_N_sink_2 _ =
  let open Fstream.Graph.Algebra in
  (* (V0 *> (V1 +> V2)) *+> V3 = (V0 *> (V1 +> V2)) +> (V1 +> V2) *> V3 *)
  let g0 =
    (Vertex("V0", ()) *> (Vertex("V1", ()) +> Vertex("V2", ())))
    +>
    (Vertex("V1", ()) +> Vertex("V2", ()))
    *>
    Vertex("V3", ())
  and g1 =
    (Vertex("V0", ()) *> (Vertex("V1", ()) +> Vertex("V2", ())))
    *+>
    Vertex("V3", ())
  in
  assert_equal g0 g1

(*
 * Test suite definition and main function.
 *)

let infix_N_sink =
  "infix_N_sink_1_chain" >:::
  [ "infix_N_sink_0" >:: infix_N_sink_0
  ; "infix_N_sink_1" >:: infix_N_sink_1
  ; "infix_N_sink_2" >:: infix_N_sink_2
  ]

let() =
  run_test_tt_main infix_N_sink
