open Lwt.Infix
open Fstream

let beacon ~delay =
  new Std.beacon ~delay ~zero:(fun() -> 0) ~next:(fun v -> v + 1)

class subgraph ~name reader writer : Types.operator = object
  method process =
    let%graph (_, procs) =
      Vertex("Input", new Std.input ~reader)
      *+>
      Vertex("Apply", new Std.apply ~fn:(fun e ->
          Logs_lwt.info (fun m -> m "[%s] processing %d" name e)
          >>= fun () -> Lwt.return e))
      *+>
      Vertex("Output", new Std.output ~writer)
    in
    procs
end

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  (* Graph *)
  let%graph (_, procs) =
    Vertex("Beacon", beacon ~delay:1.0)
    *+>
    Vertex("Dup", new Std.duplicate)
    *+>
    (Vertex("In0", new subgraph ~name:"In0") +> Vertex("In1", new subgraph ~name:"In1"))
    *+>
    Vertex("Merge", new Std.merge)
    *+>
    Vertex("Sink", new Std.sink)
  in
  Lwt_main.run procs
