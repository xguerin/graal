open Lwt.Infix
open Fstream.Std

let%graph value =
  (Vertex("B0", new beacon ~zero:(fun() -> 0) ~next:(fun v -> v + 1))
   +>
   Vertex("B1", new beacon ~zero:(fun() -> 0) ~next:(fun v -> v + 1)))
  *>
  Vertex("M0", new merge)
  *+>
  Vertex("S0", new sink)

let beacon =
  new beacon ~zero:(fun() -> 0) ~next:(fun v -> v + 1)

let apply_fn (v0, v1) =
  Logs_lwt.info (fun m -> m "(%d, %d)" v0 v1)
  >>= fun () -> Lwt.return (v0, v1)

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  (* Graph *)
  let%graph graph =
    (Vertex("B0", beacon) +> Vertex("B1", beacon))
    *>
    Vertex("M0", new merge)
    *+>
    Vertex("A0", new apply ~fn:apply_fn)
    *+>
    Vertex("S0", new sink)
  in
  Lwt_main.run graph
