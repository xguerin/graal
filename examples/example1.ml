open Lwt.Infix
open Fstream

(*
 * Graph.
 *)

let make_graph () =
  let open Graph.Algebra in
  (Vertex ("B0", `Beacon) +> Vertex ("B1", `Beacon))
  *>
  Vertex ("S0", `Sink)

let eval_graph () =
  let open Graph in
  Algebra.eval (make_graph ())

(*
 * Main.
 *)

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  (* Streams *)
  let void = new Streams.Unit.stream
  and stm0 = new Streams.Mailbox.stream
  and stm1 = new Streams.Mailbox.stream
  and stm2 = new Streams.Mailbox.stream
  and stm3 = new Streams.Mailbox.stream
  and stm4 = new Streams.Mailbox.stream
  and stm5 = new Streams.Mailbox.stream
  in
  (* Operators *)
  [ new Std.beacon ~zero:(fun () -> 0) ~next:(fun v -> v + 1) void stm0
  ; new Std.beacon ~zero:(fun () -> 0) ~next:(fun v -> v + 1) void stm1
  ; new Std.merge (stm0, stm1) stm2
  ; new Std.apply ~fn:(fun (v0, v1) ->
        Logs_lwt.info (fun m -> m "(%d, %d)" v0 v1)
        >>= fun () -> Lwt.return (v0, v1)) stm2 stm3
  ; new Std.split stm3 (stm4, stm5)
  ; new Std.sink stm4 void
  ; new Std.sink stm5 void
  ]
  |> List.map (fun e -> e#process)
  |> Lwt.join
  |> Lwt_main.run
