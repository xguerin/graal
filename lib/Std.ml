open Lwt.Infix

class apply ~fn reader writer = object
  inherit Types.operator

  method process =
    let rec process_r () =
      reader#read
      >>= fn
      >>= writer#write
      >>= process_r
    in
    process_r ()
end

class beacon ?(delay=1.0) ~zero ~next reader writer = object
  inherit Types.operator

  val mutable state = zero ()

  method process =
    let rec process_r () =
      reader#read
      >>= fun () -> Lwt_unix.sleep delay
      >>= fun () -> state <- next state; writer#write state
      >>= process_r
    in
    process_r ()
end

class merge (r0, r1) writer = object
  inherit Types.operator

  method process =
    let rec process_r () =
      r0#read
      >>= fun v0 -> r1#read
      >>= fun v1 -> writer#write (v0, v1)
      >>= process_r
    in
    process_r ()
end

class sink reader writer = object
  inherit Types.operator

  method process =
    let rec process_r () =
      reader#read
      >>= fun _ -> writer#write ()
      >>= process_r
    in
    process_r ()
end

class split reader (w0, w1) = object
  inherit Types.operator

  method process =
    let rec process_r () =
      reader#read
      >>= fun (v0, v1) -> w0#write v0
      >>= fun () -> w1#write v1
      >>= process_r
    in
    process_r ()
end
