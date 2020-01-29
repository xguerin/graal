open OUnit2
open Fstream

(*
 * Helper functions.
 *)

let zero () = 0
let next v = v  + 1

(*
 * Tests.
 *)

let beacon_sink_graph _ =
  let check v = assert_equal v 1; Lwt.return v
  in
  (* Streams *)
  let void = new Streams.Unit.stream
  and stm0 = new Streams.Mailbox.stream
  and stm1 = new Streams.Mailbox.stream
  in
  let beacon = new Std.beacon ~run:Std.once ~zero ~next void stm0
  and apply = new Std.apply ~run:Std.once ~fn:check stm0 stm1
  and sink = new Std.sink ~run:Std.once stm1 void
  in
  (* Processes *)
  [ beacon#process
  ; apply#process
  ; sink#process
  ]
  |> Lwt.join
  |> Lwt_main.run

let beacon_merge_sink_graph _ =
  let check v = assert_equal v (1, 1); Lwt.return v
  in
  (* Streams *)
  let void = new Streams.Unit.stream
  and stm0 = new Streams.Mailbox.stream
  and stm1 = new Streams.Mailbox.stream
  and stm2 = new Streams.Mailbox.stream
  and stm3 = new Streams.Mailbox.stream
  in
  let beacon0 = new Std.beacon ~run:Std.once ~zero ~next void stm0
  and beacon1 = new Std.beacon ~run:Std.once ~zero ~next void stm1
  and merge = new Std.merge ~run:Std.once (stm0, stm1) stm2
  and apply = new Std.apply ~run:Std.once ~fn:check stm2 stm3
  and sink = new Std.sink ~run:Std.once stm3 void
  in
  (* Processes *)
  [ beacon0#process
  ; beacon1#process
  ; merge#process
  ; apply#process
  ; sink#process
  ]
  |> Lwt.join
  |> Lwt_main.run

let beacon_merge_split_sink_graph _ =
  let check v = assert_equal v 1; Lwt.return v
  in
  (* Streams *)
  let void = new Streams.Unit.stream
  and stm0 = new Streams.Mailbox.stream
  and stm1 = new Streams.Mailbox.stream
  and stm2 = new Streams.Mailbox.stream
  and stm3 = new Streams.Mailbox.stream
  and stm4 = new Streams.Mailbox.stream
  and stm5 = new Streams.Mailbox.stream
  and stm6 = new Streams.Mailbox.stream
  in
  let beacon0 = new Std.beacon ~run:Std.once ~zero ~next void stm0
  and beacon1 = new Std.beacon ~run:Std.once ~zero ~next void stm1
  and merge = new Std.merge ~run:Std.once (stm0, stm1) stm2
  and split = new Std.split ~run:Std.once stm2 (stm3, stm5)
  and apply0 = new Std.apply ~run:Std.once ~fn:check stm3 stm4
  and sink0 = new Std.sink ~run:Std.once stm4 void
  and apply1 = new Std.apply ~run:Std.once ~fn:check stm5 stm6
  and sink1 = new Std.sink ~run:Std.once stm6 void
  in
  (* Processes *)
  [ beacon0#process
  ; beacon1#process
  ; merge#process
  ; split#process
  ; apply0#process
  ; sink0#process
  ; apply1#process
  ; sink1#process
  ]
  |> Lwt.join
  |> Lwt_main.run

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
