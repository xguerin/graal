open Lwt.Infix

(*
 * Unit stream.
 *)

module Unit = struct
  class stream = object
    inherit [unit] Types.stream
    method read = Lwt.return ()
    method write () = Lwt.return ()
  end
end

(*
 * Mailbox stream.
 *)

module Mailbox = struct
  type 'a t =
    { mutable data: 'a option
    ; mutable wake_put: unit Lwt.u option
    ; mutable wake_get: 'a Lwt.u option
    }

  class ['a] stream = object
    inherit ['a] Types.stream

    val state =
      { data = None
      ; wake_put = None
      ; wake_get = None
      }

    method read =
      match state with
      (* Streams with a pre-existing value *)
      | { data = Some(pre); wake_put = None; wake_get = None } ->
        state.data <- None;
        state.wake_get <- None;
        Logs_lwt.debug (fun m -> m "Get data")
        >>= fun () -> Lwt.return pre
      | { data = Some(pre); wake_put = Some(wakener); wake_get = None } ->
        state.data <- None;
        state.wake_put <- None;
        state.wake_get <- None;
        Lwt.wakeup wakener ();
        Logs_lwt.debug (fun m -> m "Get data and wake-up")
        >>= fun () -> Lwt.return pre
      (* Streams with no pre-existing value *)
      | { data = None; wake_get = None; wake_put = None } ->
        let (thread, wakener) = Lwt.wait () in
        state.wake_get <- Some wakener;
        Logs_lwt.debug (fun m -> m "Waiting for get")
        >>= fun () -> thread
      | _ -> failwith "Invalid GET state"

    method write value =
      match state with
      (* Streams with no pre-existing value *)
      | { data = None; wake_get = None; wake_put = None } ->
        state.data <- Some value;
        state.wake_put <- None;
        Logs_lwt.debug (fun m -> m "Put data")
        >>= Lwt.return
      | { data = None; wake_get = Some(wakener); wake_put = None } ->
        state.wake_put <- None;
        state.wake_get <- None;
        Lwt.wakeup wakener value;
        Logs_lwt.debug (fun m -> m "Put data and wake-up")
        >>= Lwt.return
      (* Streams with a pre-existing value *)
      | { data = Some(_); wake_put = None; wake_get = None } ->
        let (thread, wakener) = Lwt.wait () in
        state.wake_put <- Some wakener;
        Logs_lwt.debug (fun m -> m "Waiting for put")
        >>= fun () -> thread
      | _ -> failwith "Invalid PUT state"
  end
end
