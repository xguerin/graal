open Lwt.Infix

(*
 * Runnner functions.
 *)

let forever cb () : unit Lwt.t = cb ()
let once _ () : unit Lwt.t = Lwt.return ()

(*
 * Beacon.
 *)

class beacon ?(delay=1.0) ?(run=forever) ~zero ~next reader writer = object
  inherit Types.operator

  val mutable state = zero ()

  method process =
    let rec process_r () =
      reader#read
      >>= fun () -> Lwt_unix.sleep delay
      >>= fun () -> state <- next state; writer#write state
      >>= run process_r
    in
    process_r ()
end

(*
 * Functor and filter.
 *)

class apply ?(run=forever) ~fn reader writer = object
  inherit Types.operator

  method process =
    let rec process_r () =
      reader#read
      >>= fn
      >>= writer#write
      >>= run process_r
    in
    process_r ()
end


class filter ?(run=forever) ~fn reader writer = object
  inherit Types.operator

  method process =
    let rec process_r () =
      reader#read
      >>= (fun v -> if (fn v) then writer#write v else Lwt.return ())
      >>= run process_r
    in
    process_r ()
end

(*
 * File source/sink.
 *)

class file_source ?(run=forever) ~path reader writer = object
  inherit Types.operator

  val mutable channel = None

  method process =
    let rec process_r () =
      reader#read
      >>= fun () -> begin
        match channel with
        | Some(c) -> Lwt_io.read_line c >>= writer#write
        | None -> Lwt.return ()
      end
      >>= run process_r
    in
    Lwt_io.open_file ~mode:Lwt_io.Input path
    >>= fun c -> channel <- Some(c); process_r ()
end

class file_sink ?(run=forever) ~path reader writer = object
  inherit Types.operator

  val mutable channel = None

  method process =
    let rec process_r () =
      reader#read
      >>= fun v -> begin
        match channel with
        | Some(c) -> Lwt_io.write_line c v >>= writer#write
        | None -> Lwt.return ()
      end
      >>= run process_r
    in
    Lwt_io.open_file ~mode:Lwt_io.Output path
    >>= fun c -> channel <- Some(c); process_r ()
end

(*
 * Subgraph input/output.
 *)

class input ?(run=forever) ~reader _ writer = object
  inherit Types.operator

  method process =
    let rec process_r () =
      reader#read
      >>= writer#write
      >>= run process_r
    in
    process_r()
end

class output ?(run=forever) ~writer reader _ = object
  inherit Types.operator

  method process =
    let rec process_r () =
      reader#read
      >>= writer#write
      >>= run process_r
    in
    process_r()
end

(*
 * Merge/split.
 *)

class duplicate ?(run=forever) reader (w0, w1) = object
  inherit Types.operator

  method process =
    let rec process_r () =
      reader#read
      >>= fun v -> w0#write v
      >>= fun () -> w1#write v
      >>= run process_r
    in
    process_r ()
end

class merge ?(run=forever) (r0, r1) writer = object
  inherit Types.operator

  method process =
    let rec process_r () =
      r0#read
      >>= fun v0 -> r1#read
      >>= fun v1 -> writer#write (v0, v1)
      >>= run process_r
    in
    process_r ()
end

class split ?(run=forever) reader (w0, w1) = object
  inherit Types.operator

  method process =
    let rec process_r () =
      reader#read
      >>= fun (v0, v1) -> w0#write v0
      >>= fun () -> w1#write v1
      >>= run process_r
    in
    process_r ()
end

(*
 * Generic sink.
 *)

class sink ?(run=forever) reader writer = object
  inherit Types.operator

  method process =
    let rec process_r () =
      reader#read
      >>= fun _ -> writer#write ()
      >>= run process_r
    in
    process_r ()
end
