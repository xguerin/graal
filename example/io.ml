open Lwt.Infix
open Fstream

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  (* Graph *)
  let%graph (_, procs) =
    Vertex("FileSource", new Std.file_source ~path:"example/io.ml")
    *>
    Vertex("PrintLines", new Std.apply ~fn:(fun line ->
        Logs_lwt.info (fun m -> m "%s" line) >>= fun () -> Lwt.return line))
    *+>
    Vertex("Sink", new Std.sink)
  in
  Lwt_main.run procs
