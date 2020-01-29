open OUnit2

(*
 * Fan-in operator.
 *)

let fanin_N_0 _ =
  let open Fstream.Graph.Algebra in
  (* (V0 +> V1) *+> V2 = (V0 +> V1) *> V2 *)
  let g0 =
    (Vertex("V0", ()) +> Vertex("V1", ()))
    *>
    Vertex("V2", ())
  and g1 =
    (Vertex("V0", ()) +> Vertex("V1", ()))
    *+>
    Vertex("V2", ())
  in
  assert_equal g0 g1

let fanin_N_1 _ =
  let open Fstream.Graph.Algebra in
  (* (V0 +> V1) *+> V2 *> V3 = (V0 +> V1) *> V2) *> V3 *)
  let g0 =
    (Vertex("V0", ()) +> Vertex("V1", ()))
    *>
    Vertex("V2", ())
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

let fanin_N_2 _ =
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
 * Fan-out operator.
 *)

let fanout_N_0 _ =
  let open Fstream.Graph.Algebra in
  (* V0 +*> (V1 +> V2) = V0 *> (V1 +> V2) *)
  let g0 =
    Vertex("V0", ())
    *>
    (Vertex("V1", ()) +> Vertex("V2", ()))
  and g1 =
    Vertex("V0", ())
    +*>
    (Vertex("V1", ()) +> Vertex("V2", ()))
  in
  assert_equal g0 g1

let fanout_N_1 _ =
  let open Fstream.Graph.Algebra in
  (* V0 *> V1 +*> (V2 +> V3) = (V0 *> V1) *> (V2 +> V3) *)
  let g0 =
    Vertex("V0", ())
    *>
    Vertex("V1", ())
    *>
    (Vertex("V2", ()) +> Vertex("V3", ()))
  and g1 =
    Vertex("V0", ())
    *>
    Vertex("V1", ())
    +*>
    (Vertex("V2", ()) +> Vertex("V3", ()))
  in
  assert_equal g0 g1

let fanout_N_2 _ =
  let open Fstream.Graph.Algebra in
  (* V0 +*> ((V1 +> V2) *> V3) = V0 *> (V1 +> V2) +> ((V1 +> V2) *> V3) *)
  let g0 =
    (Vertex("V0", ()) *> (Vertex("V1", ()) +> Vertex("V2", ())))
    +>
    ((Vertex("V1", ()) +> Vertex("V2", ())) *> Vertex("V3", ()))
  and g1 =
    Vertex("V0", ())
    +*>
    ((Vertex("V1", ()) +> Vertex("V2", ())) *> Vertex("V3", ()))
  in
  assert_equal g0 g1

(*
 * Mixed.
 *)

let mixed_N_0 _ =
  let open Fstream.Graph.Algebra in
  (*
   * (V0 *> (V1 +> V2)) *+> V3 */> ((V4 +> V5) *> V6)
   * =
   * (V0 *> (V1 +> V2)) +> ((V1 +> V2) *> V3) +> (V3 *> (V4 +> V5)) +> ((V4 +> V5) *> V6)
   *)
  let g0 =
    (Vertex("V0", ()) *> (Vertex("V1", ()) +> Vertex("V2", ())))
    +>
    ((Vertex("V1", ()) +> Vertex("V2", ())) *> Vertex("V3", ()))
    +>
    (Vertex("V3", ()) *> (Vertex("V4", ()) +> Vertex("V5", ())))
    +>
    ((Vertex("V4", ()) +> Vertex("V5", ())) *> Vertex("V6", ()))
  and g1 =
    (Vertex("V0", ()) *> (Vertex("V1", ()) +> Vertex("V2", ())))
    *+>
    Vertex("V3", ())
    */>
    ((Vertex("V4", ()) +> Vertex("V5", ())) *> Vertex("V6", ()))
  in
  Printf.printf "\n";
  Printf.printf "%s\n" (to_string g0);
  Printf.printf "%s\n" (to_string g1);
  assert_equal g0 g1

let mixed_N_1 _ =
  let open Fstream.Graph.Algebra in
  (*
   * (V0 *> (V1 +> V2)) */> ((V3 +> V4) *> V5)
   * =
   * (V0 *> (V1 +> V2)) +> ((V1 +> V2) *> (V3 +> V4)) +> ((V4 +> V5) *> V6)
   *)
  let g0 =
    (Vertex("V0", ()) *> (Vertex("V1", ()) +> Vertex("V2", ())))
    +>
    ((Vertex("V1", ()) +> Vertex("V2", ())) *> (Vertex("V3", ()) +> Vertex("V4", ())))
    +>
    ((Vertex("V3", ()) +> Vertex("V4", ())) *> Vertex("V5", ()))
  and g1 =
    (Vertex("V0", ()) *> (Vertex("V1", ()) +> Vertex("V2", ())))
    */>
    ((Vertex("V3", ()) +> Vertex("V4", ())) *> Vertex("V5", ()))
  in
  assert_equal g0 g1

let mixed_N_2 _ =
  let open Fstream.Graph.Algebra in
  (*
   * (V0 +> V1) */> (V2 +> V3 +> V4) */> V5
   * =
   * (V0 +> V1) +> (V0 +> V1) *> (V2 +> V3 +> V4) +> (V2 +> V3 +> V4) *> V5 +> V5
   *)
  let g0 =
    (Vertex("V0", ()) +> Vertex("V1", ()))
    *>
    (Vertex("V2", ()) +> Vertex("V3", ()) +> Vertex("V4", ()))
    +>
    (Vertex("V2", ()) +> Vertex("V3", ()) +> Vertex("V4", ()))
    *>
    Vertex("V5", ())
  and g1 =
    (Vertex("V0", ()) +> Vertex("V1", ()))
    */>
    (Vertex("V2", ()) +> Vertex("V3", ()) +> Vertex("V4", ()))
    */>
    Vertex("V5", ())
  in
  assert_equal g0 g1

(*
 * Test suite definition and main function.
 *)

let infix_N =
  "infix_N" >:::
  [ "fanin_N_0" >:: fanin_N_0
  ; "fanin_N_1" >:: fanin_N_1
  ; "fanin_N_2" >:: fanin_N_2
  ; "fanout_N_0" >:: fanout_N_0
  ; "fanout_N_1" >:: fanout_N_1
  ; "fanout_N_2" >:: fanout_N_2
  ; "mixed_N_0" >:: mixed_N_0
  ; "mixed_N_1" >:: mixed_N_1
  ; "mixed_N_2" >:: mixed_N_2
  ]

let() =
  run_test_tt_main infix_N
