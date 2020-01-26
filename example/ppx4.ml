open Lwt.Infix
open Fstream.Std

let beacon ~delay =
  new beacon ~delay ~zero:(fun() -> 0) ~next:(fun v -> v + 1)

let apply_fn (label, v) =
  Logs_lwt.info (fun m -> m "[%s] %d" label v) >>= fun () -> Lwt.return v

class custom (r0, r1, r2, r3) writer = object
  inherit Fstream.Types.operator

  method process =
    let rec process_r0 () =
      r0#read
      >>= fun v -> writer#write ("r0", v)
      >>= process_r0
    and process_r1 () =
      r1#read
      >>= fun v -> writer#write ("r1", v)
      >>= process_r1
    and process_r2 () =
      r2#read
      >>= fun v -> writer#write ("r2", v)
      >>= process_r2
    and process_r3 () =
      r3#read
      >>= fun v -> writer#write ("r3", v)
      >>= process_r3
    in
    Lwt.join [ process_r0 ()
             ; process_r1 ()
             ; process_r2 ()
             ; process_r3 ()
             ]
end

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  (* Graph *)
  let%graph (items, _) =
    (Vertex("B0", beacon ~delay:0.25)
     +> Vertex("B1", beacon ~delay:0.5)
     +> Vertex("B2", beacon ~delay:1.0)
     +> Vertex("B3", beacon ~delay:2.0))
    *>
    Vertex("C0", new custom)
    *+>
    Vertex("A0", new apply ~fn:apply_fn)
    *+>
    Vertex("S0", new sink)
  in
  let (edges, vertices) = items in
  Printf.printf "digraph my_graph {\n";
  List.iter (fun e -> Printf.printf "  %s;\n" e) vertices;
  List.iter (fun (a, b) -> Printf.printf "  %s -> %s;\n" a b) edges;
  Printf.printf "}"
