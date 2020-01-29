open OUnit2
open Fstream

(*
 * Helper functions.
 *)

let zero () = 0
let next v = v  + 1

(*
 * Helper operators.
 *)

class inner ~run reader writer = object
  inherit Types.operator

  method process =
    let%graph (_, procs) =
      Vertex("In", new Std.input ~run ~reader)
      *>
      Vertex("Out", new Std.output ~run ~writer)
    in
    procs
end

(*
 * Tests.
 *)

let beacon_sink_graph _ =
  let check v = assert_equal v 1; Lwt.return v
  in
  let%graph (_, procs) =
    Vertex("Beacon", new Std.beacon ~run:Std.once ~zero ~next)
    *+>
    Vertex("Check", new Std.apply ~run:Std.once ~fn:check)
    *+>
    Vertex("Sink", new Std.sink ~run:Std.once)
  in
  Lwt_main.run procs

let beacon_merge_sink_graph _ =
  let check v = assert_equal v (1, 1); Lwt.return v
  in
  let%graph (_, procs) =
    (Vertex("Beacon0", new Std.beacon ~run:Std.once ~zero ~next)
     +>
     Vertex("Beacon1", new Std.beacon ~run:Std.once ~zero ~next))
    *+>
    Vertex("Merge", new Std.merge ~run:Std.once)
    *+>
    Vertex("Check", new Std.apply ~run:Std.once ~fn:check)
    *+>
    Vertex("Sink", new Std.sink ~run:Std.once)
  in
  Lwt_main.run procs

let beacon_merge_split_sink_graph _ =
  let check v = assert_equal v (1, 1); Lwt.return v
  in
  let%graph (_, procs) =
    (Vertex("Beacon0", new Std.beacon ~run:Std.once ~zero ~next)
     +>
     Vertex("Beacon1", new Std.beacon ~run:Std.once ~zero ~next))
    *+>
    Vertex("Merge", new Std.merge ~run:Std.once)
    *+>
    Vertex("Check", new Std.apply ~run:Std.once ~fn:check)
    *+>
    Vertex("Split", new Std.split ~run:Std.once)
    *+>
    (Vertex("Sink0", new Std.sink ~run:Std.once)
     +>
     Vertex("Sink1", new Std.sink ~run:Std.once))
  in
  Lwt_main.run procs

let beacon_inner_sink_graph _ =
  let check v = assert_equal v 1; Lwt.return v
  in
  let%graph (_, procs) =
    Vertex("Beacon", new Std.beacon ~run:Std.once ~zero ~next)
    *+>
    Vertex("Inner", new inner ~run:Std.once)
    *+>
    Vertex("Check", new Std.apply ~run:Std.once ~fn:check)
    *+>
    Vertex("Sink", new Std.sink ~run:Std.once)
  in
  Lwt_main.run procs

(*
 * Test suite definition and main function.
 *)

let graph_manual =
  "graph_manual" >:::
  [ "beacon_sink_graph" >:: beacon_sink_graph
  ; "beacon_merge_sink_graph" >:: beacon_merge_sink_graph
  ; "beacon_merge_split_sink_graph" >:: beacon_merge_split_sink_graph
  ; "beacon_inner_sink_graph" >:: beacon_inner_sink_graph
  ]

let() =
  run_test_tt_main graph_manual
