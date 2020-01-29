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

class ['a] sink ~init reader writer = object(self)
  inherit Types.operator

  val mutable value: 'a = init

  method private set v =
    value <- v;
    Lwt.return ()

  method get = value

  method process =
    reader#read
    >>= self#set
    >>= writer#write
end

(*
 * Tests.
 *)

let beacon_sink_graph _ =
  (* Streams *)
  let void = new Streams.Unit.stream
  and stm0 = new Streams.Mailbox.stream
  in
  let beacon = new beacon void stm0
  and sink = new sink ~init:0 stm0 void
  in
  (* Processes *)
  [ beacon#process
  ; sink#process
  ]
  |> Lwt.join
  |> Lwt_main.run;
  (* Check *)
  assert_equal sink#get 1

let beacon_merge_sink_graph _ =
  (* Streams *)
  let void = new Streams.Unit.stream
  and stm0 = new Streams.Mailbox.stream
  and stm1 = new Streams.Mailbox.stream
  and stm2 = new Streams.Mailbox.stream
  in
  let beacon0 = new beacon void stm0
  and beacon1 = new beacon void stm1
  and merge = new merge (stm0, stm1) stm2
  and sink = new sink ~init:(0, 0) stm2 void
  in
  (* Processes *)
  [ beacon0#process
  ; beacon1#process
  ; merge#process
  ; sink#process
  ]
  |> Lwt.join
  |> Lwt_main.run;
  (* Check *)
  assert_equal sink#get (1, 1)

let beacon_merge_split_sink_graph _ =
  (* Streams *)
  let void = new Streams.Unit.stream
  and stm0 = new Streams.Mailbox.stream
  and stm1 = new Streams.Mailbox.stream
  and stm2 = new Streams.Mailbox.stream
  and stm3 = new Streams.Mailbox.stream
  and stm4 = new Streams.Mailbox.stream
  in
  let beacon0 = new beacon void stm0
  and beacon1 = new beacon void stm1
  and merge = new merge (stm0, stm1) stm2
  and split = new split stm2 (stm3, stm4)
  and sink0 = new sink ~init:0 stm3 void
  and sink1 = new sink ~init:0 stm4 void
  in
  (* Processes *)
  [ beacon0#process
  ; beacon1#process
  ; merge#process
  ; split#process
  ; sink0#process
  ; sink1#process
  ]
  |> Lwt.join
  |> Lwt_main.run;
  (* Check *)
  assert_equal sink0#get 1;
  assert_equal sink1#get 1

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
