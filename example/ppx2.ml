open Lwt.Infix
open Fstream.Std

let beacon =
  new beacon ~zero:(fun() -> 0) ~next:(fun v -> v + 1)

let apply_fn ((v0, v1), (v2, v3)) =
  Logs_lwt.info (fun m -> m "((%d, %d), (%d, %d))" v0 v1 v2 v3)
  >>= fun () -> Lwt.return (v0, v1, v2, v3)

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  (* Graph *)
  let%graph processes =
    (((Vertex("B0", beacon) +> Vertex("B1", beacon)) *> Vertex("M0", new merge))
    +>
    ((Vertex("B2", beacon) +> Vertex("B3", beacon)) *> Vertex("M1", new merge)))
    *+>
    Vertex("M2", new merge)
    *+>
    Vertex("A0", new apply ~fn:apply_fn)
    *+>
    Vertex("S0", new sink)
  in
  Lwt_main.run processes
