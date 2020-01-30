open Lwt.Infix
open Fstream
open Fstream.Std

let beacon ~delay =
  new beacon ~delay ~zero:(fun() -> 0) ~next:(fun v -> v + 1)

let apply_fn (label, v) =
  Logs_lwt.info (fun m -> m "[%s] %d" label v) >>= fun () -> Lwt.return v

class custom (r0, r1) writer = object(self)
  inherit Types.operator

  val r0_window = new Windows.sliding ~count:5
  val r1_window = new Windows.tumbling ~count:5

  method private process_r0 =
    let rec process () =
      r0#read
      >>= r0_window#process
      >>= begin function
        | Some(v) -> writer#write ("r0", List.fold_left Int.add 0 v)
        | None -> Lwt.return ()
      end
      >>= process
    in
    process ()

  method private process_r1 =
    let rec process () =
      r1#read
      >>= r1_window#process
      >>= begin function
        | Some(v) -> writer#write ("r1", List.fold_left Int.add 0 v)
        | None -> Lwt.return ()
      end
      >>= process
    in
    process ()

  method process =
    Lwt.join [ self#process_r0
             ; self#process_r1
             ]
end

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  (* Graph *)
  let%graph (_, procs) =
    (Vertex("B0", beacon ~delay:0.1) +> Vertex("B1", beacon ~delay:0.1))
    *>
    Vertex("C0", new custom)
    *+>
    Vertex("A0", new apply ~fn:apply_fn)
    *+>
    Vertex("S0", new sink)
  in
  Lwt_main.run procs
