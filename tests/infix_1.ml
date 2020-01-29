open OUnit2

(*
 * Fan-in operator.
 *)

let fanin_1_0 _ =
  let open Fstream.Graph.Algebra in
  (* V0 *+> V1 = V0 *> V1 *)
  let g0 =
    Vertex("V0", ())
    *>
    Vertex("V1", ())
  and g1 =
    Vertex("V0", ())
    *+>
    Vertex("V1", ())
  in
  assert_equal g0 g1

let fanin_1_1 _ =
  let open Fstream.Graph.Algebra in
  (* V0 *+> V1 *> V2 = V0 *> V1 *> V2 *)
  let g0 =
    Vertex("V0", ())
    *>
    Vertex("V1", ())
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

let fanin_1_2 _ =
  let open Fstream.Graph.Algebra in
  (* V0 *+> V1 +> V2 = V0 *> V1 +> V2 *)
  let g0 =
    Vertex("V0", ())
    *>
    Vertex("V1", ())
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

let fanin_1_3 _ =
  let open Fstream.Graph.Algebra in
  (* V0 *+> V1 *+> V2 = V0 *> V1 +> V1 *> V2 *)
  let g0 =
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
 * Fan-out operator.
 *)

let fanout_1_0 _ =
  let open Fstream.Graph.Algebra in
  (* V0 +*> V1 = V0 *> V1 *)
  let g0 =
    Vertex("V0", ())
    *>
    Vertex("V1", ())
  and g1 =
    Vertex("V0", ())
    +*>
    Vertex("V1", ())
  in
  assert_equal g0 g1

let fanout_1_1 _ =
  (* NOTE operator *> has precedence over operator +*> *)
  let open Fstream.Graph.Algebra in
  (* V0 +*> V1 *> V2 = V0 *> V1 +> V1 *> V2 *)
  let g0 =
    Vertex("V0", ())
    *>
    Vertex("V1", ())
    +>
    Vertex("V1", ())
    *>
    Vertex("V2", ())
  and g1 =
    Vertex("V0", ())
    +*>
    Vertex("V1", ())
    *>
    Vertex("V2", ())
  in
  assert_equal g0 g1

let fanout_1_2 _ =
  (* NOTE operator +*> has precedence over operator +> *)
  let open Fstream.Graph.Algebra in
  (* V0 +*> V1 +> V2 = V0 *> V1 +> V2 *)
  let g0 =
    Vertex("V0", ())
    *>
    Vertex("V1", ())
    +>
    Vertex("V2", ())
  and g1 =
    Vertex("V0", ())
    +*>
    Vertex("V1", ())
    +>
    Vertex("V2", ())
  in
  assert_equal g0 g1

let fanout_1_3 _ =
  let open Fstream.Graph.Algebra in
  (* V0 +*> V1 +*> V2 = V0 *> V1 *> V2 *)
  let g0 =
    Vertex("V0", ())
    *>
    Vertex("V1", ())
    *>
    Vertex("V2", ())
  and g1 =
    Vertex("V0", ())
    +*>
    Vertex("V1", ())
    +*>
    Vertex("V2", ())
  in
  assert_equal g0 g1;
  ()

(*
 * Test suite definition and main function.
 *)

let infix_1 =
  "infix_1" >:::
  [ "fanin_1_0" >:: fanin_1_0
  ; "fanin_1_1" >:: fanin_1_1
  ; "fanin_1_2" >:: fanin_1_2
  ; "fanin_1_3" >:: fanin_1_3
  ; "fanout_1_0" >:: fanout_1_0
  ; "fanout_1_1" >:: fanout_1_1
  ; "fanout_1_2" >:: fanout_1_2
  ; "fanout_1_3" >:: fanout_1_3
  ]

let() =
  run_test_tt_main infix_1
