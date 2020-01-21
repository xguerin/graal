open Lwt.Infix
open Types

module Mailbox (E: Environment) (T: Tuple)
  : Stream with type data = T.t
= struct
  type data = T.t
  type t =
    { label: string
    ; mutable data: data option
    ; mutable wake_put: unit Lwt.u option
    ; mutable wake_get: data Lwt.u option
    }

  let init ?label () =
    { label = (match label with Some (label) -> label | None -> "Gather")
    ; data = None
    ; wake_put = None
    ; wake_get = None
    }

  let label { label; _ } = label

  let put state value =
    match state with
    (* Streams with no pre-existing value *)
    | { data = None; wake_get = None; wake_put = None; label } ->
      state.data <- Some value;
      state.wake_put <- None;
      Logs_lwt.debug (fun m -> m "[%s] Put data" label)
      >>= Lwt.return
    | { data = None; wake_get = Some(wakener); wake_put = None; label } ->
      state.wake_put <- None;
      state.wake_get <- None;
      Lwt.wakeup wakener value;
      Logs_lwt.debug (fun m -> m "[%s] Put data and wake-up" label)
      >>= Lwt.return
    (* Streams with a pre-existing value *)
    | { data = Some(_); wake_put = None; wake_get = None; label } ->
      let (thread, wakener) = Lwt.wait () in
      state.wake_put <- Some wakener;
      Logs_lwt.debug (fun m -> m "[%s] Waiting for put" label)
      >>= fun () -> thread
    | _ -> failwith "Invalid PUT state"

  let get state () =
    match state with
    (* Streams with a pre-existing value *)
    | { data = Some(pre); wake_put = None; wake_get = None; label } ->
      state.data <- None;
      state.wake_get <- None;
      Logs_lwt.debug (fun m -> m "[%s] Get data" label)
      >>= fun () -> Lwt.return pre
    | { data = Some(pre); wake_put = Some(wakener); wake_get = None; label } ->
      state.data <- None;
      state.wake_put <- None;
      state.wake_get <- None;
      Lwt.wakeup wakener ();
      Logs_lwt.debug (fun m -> m "[%s] Get data and wake-up" label)
      >>= fun () -> Lwt.return pre
    (* Streams with no pre-existing value *)
    | { data = None; wake_get = None; wake_put = None; label } ->
      let (thread, wakener) = Lwt.wait () in
      state.wake_get <- Some wakener;
      Logs_lwt.debug (fun m -> m "[%s] Waiting for get" label)
      >>= fun () -> thread
    | _ -> failwith "Invalid GET state"
end
