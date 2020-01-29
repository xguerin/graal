open OUnit2

let infix_1_sink_0 _ =
  let open Fstream.Graph.Algebra in
  (* V0 *+> V1 = V0 +> V0 *> V1 *)
  let g0 =
    Vertex("V0", ())
    +>
    Vertex("V0", ())
    *>
    Vertex("V1", ())
  and g1 =
    Vertex("V0", ())
    *+>
    Vertex("V1", ())
  in
  assert_equal g0 g1

let infix_1_sink_1 _ =
  let open Fstream.Graph.Algebra in
  (* V0 *+> V1 *> V2 = (V0 +> V0 *> V1) *> V2 *)
  let g0 =
    (Vertex("V0", ())
     +>
     Vertex("V0", ())
     *>
     Vertex("V1", ()))
    *>
    Vertex("V2", ())
  and g1 =
    Vertex("V0", ())
    *+>
    Vertex("V1", ())
    *>
    Vertex("V2", ())
  in
  assert_equal g0 g1

let infix_1_sink_2 _ =
  let open Fstream.Graph.Algebra in
  (* V0 *+> V1 +> V2 = (V0 +> V0 *> V1) +> V2 *)
  let g0 =
    (Vertex("V0", ())
     +>
     Vertex("V0", ())
     *>
     Vertex("V1", ()))
    +>
    Vertex("V2", ())
  and g1 =
    Vertex("V0", ())
    *+>
    Vertex("V1", ())
    +>
    Vertex("V2", ())
  in
  assert_equal g0 g1

let infix_1_sink_3 _ =
  let open Fstream.Graph.Algebra in
  (* V0 *+> V1 *+> V2 = V0 +> V0 *> V1 +> V1 *> V2 *)
  let g0 =
    Vertex("V0", ())
    +>
    Vertex("V0", ())
    *>
    Vertex("V1", ())
    +>
    Vertex("V1", ())
    *>
    Vertex("V2", ())
  and g1 =
    Vertex("V0", ())
    *+>
    Vertex("V1", ())
    *+>
    Vertex("V2", ())
  in
  assert_equal g0 g1;
  ()

(*
 * Test suite definition and main function.
 *)

let infix_1_sink =
  "infix_1_sink_1_chain" >:::
  [ "infix_1_sink_0" >:: infix_1_sink_0
  ; "infix_1_sink_1" >:: infix_1_sink_1
  ; "infix_1_sink_2" >:: infix_1_sink_2
  ; "infix_1_sink_3" >:: infix_1_sink_3
  ]

let() =
  run_test_tt_main infix_1_sink
