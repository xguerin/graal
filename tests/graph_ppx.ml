open OUnit2
open Lwt.Infix
open Fstream

(*
 * Helper classes.
 *)

class beacon reader writer = object(self)
  inherit Types.operator

  method private generate () =
    Lwt.return 1

  method process =
    reader#read
    >>= self#generate
    >>= writer#write
end

class split reader (w0, w1) = object
  inherit Types.operator

  method process =
    reader#read
    >>= fun (v0, v1) -> w0#write v0
    >>= fun () -> w1#write v1
end

class merge (r0, r1) writer = object
  inherit Types.operator

  method process =
    r0#read
    >>= fun v0 -> r1#read
    >>= fun v1 -> writer#write (v0, v1)
end

class ['a] sink ~fn reader writer = object(self)
  inherit Types.operator

  method private notify v =
    Lwt.return (fn v)

  method process =
    reader#read
    >>= self#notify
    >>= writer#write
end

(*
 * Tests.
 *)

let beacon_sink_graph _ =
  let check v =
    assert_equal v 1
  in
  let%graph (_, procs) =
    Vertex("Beacon", new beacon)
    *>
    Vertex("Sink", new sink ~fn:check)
  in
  Lwt_main.run procs

let beacon_merge_sink_graph _ =
  let check v =
    assert_equal v (1, 1)
  in
  let%graph (_, procs) =
    (Vertex("Beacon0", new beacon) +> Vertex("Beacon1", new beacon))
    *+>
    Vertex("Merge", new merge)
    *+>
    Vertex("Sink", new sink ~fn:check)
  in
  Lwt_main.run procs

let beacon_merge_split_sink_graph _ =
  let check v =
    assert_equal v 1
  in
  let%graph (_, procs) =
    (Vertex("Beacon0", new beacon) +> Vertex("Beacon1", new beacon))
    *+>
    Vertex("Merge", new merge)
    *+>
    Vertex("Split", new split)
    *+>
    (Vertex("Sink0", new sink ~fn:check) +> Vertex("Sink1", new sink ~fn:check))
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
  ]

let() =
  run_test_tt_main graph_manual
