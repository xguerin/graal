open Lwt.Infix
open Types

(*
 * Helper functions.
 *)

let null () = Lwt.return ()

(*
 * Operators.
 *)

module Beacon = struct
  module Make
      (E: Environment)
      (T: Tuple)
      (O: Stream with type data = T.t)
    : Operator with type inputs = unit input and type outputs = O.t
  = struct
    type state = T.t
    type inputs = unit input
    type outputs = O.t

    let init () = T.zero ()

    let rec process state inputs outputs () =
      inputs ()
      >>= fun () -> Lwt_unix.sleep 1.0
      >>= fun () -> Logs_lwt.info (fun m -> m "[%s] Sending %s" (E.name ()) (T.to_string state))
      >>= fun () -> O.put outputs state
      >>= fun () -> process (T.next state) inputs outputs |> E.apply
  end
end

module Functor = struct
  module Make
      (E: Environment)
      (T: Tuple)
      (I: Stream with type data = T.t)
      (O: Stream with type data = T.t)
    : Operator with type inputs = I.t and type outputs = O.t
  = struct
    type state = unit
    type inputs = I.t
    type outputs = O.t

    let init () = ()

    let rec process state inputs outputs () =
      I.get inputs ()
      >>= fun v -> Logs_lwt.debug (fun m -> m "[%s] Passing through: %s !" (E.name ()) (T.to_string v))
      >>= fun () -> O.put outputs v
      >>= fun () -> process state inputs outputs |> E.apply
  end
end

module Merger = struct
  module Make
      (E: Environment)
      (T0: Tuple) (T1: Tuple) (T2: Tuple with type t = T0.t * T1.t)
      (I0: Stream with type data = T0.t) (I1: Stream with type data = T1.t)
      (O: Stream with type data = T2.t)
    : Operator with type inputs = I0.t * I1.t and type outputs = O.t
  = struct
    type state = unit
    type inputs = I0.t * I1.t
    type outputs = O.t

    let init () = ()

    let rec process state (i0, i1) o0 () =
      I0.get i0 ()
      >>= fun v0 -> I1.get i1 ()
      >>= fun v1 -> Logs_lwt.info (fun m -> m "[%s] Merging i0 = %s, i1 = %s" (E.name ()) (T0.to_string v0) (T1.to_string v1))
      >>= fun () -> O.put o0 (v0, v1)
      >>= fun () -> process state (i0, i1) o0 |> E.apply
  end
end

module Splitter = struct
  module Make
      (E: Environment)
      (T0: Tuple) (T1: Tuple) (T2: Tuple with type t = T0.t * T1.t)
      (I: Stream with type data = T2.t)
      (O0: Stream with type data = T0.t) (O1: Stream with type data = T1.t)
    : Operator with type inputs = I.t and type outputs = O0.t * O1.t
  = struct
    type state = unit
    type inputs = I.t
    type outputs = O0.t * O1.t

    let init () = ()

    let rec process state i0 (o0, o1) () =
      I.get i0 ()
      >>= fun (v0, v1) -> Logs_lwt.info (fun m -> m "[%s] Splitting (%s, %s)" (E.name ()) (T0.to_string v0) (T1.to_string v1))
      >>= fun () -> O0.put o0 v0
      >>= fun () -> O1.put o1 v1
      >>= fun () -> process state i0 (o0, o1) |> E.apply
  end
end

module Sink = struct
  module Make
      (E: Environment)
      (T: Tuple)
      (I: Stream with type data = T.t)
    : Operator with type inputs = I.t and type outputs = unit output
  = struct
    type state = unit
    type inputs = I.t
    type outputs = unit output

    let init () = ()

    let rec process state inputs outputs () =
      I.get inputs ()
      >>= fun v -> Logs_lwt.info (fun m -> m "[%s] Consuming %s" (E.name ()) (T.to_string v))
      >>= fun () -> outputs ()
      >>= fun () -> process state inputs outputs |> E.apply
  end
end

module Scatter = struct
  type policy = Sequential | Broadcast

  module type Environment = sig
    include Environment
    val policy: unit -> policy
  end

  module Make
      (E: Environment)
      (T: Tuple)
      (I: Stream with type data = T.t)
      (O: Stream with type data = T.t)
    : Operator with type inputs = I.t and type outputs = O.t array
  = struct
    type state = int ref
    type inputs = I.t
    type outputs = O.t array

    let init () = ref 0

    let broadcast _ outputs value =
      Array.to_list outputs
      |> Lwt_list.fold_left_s (fun _ output -> O.put output value) ()

    let sequential state outputs value =
      let index = !state mod (Array.length outputs) in
      let output = Array.get outputs index in
      Logs_lwt.info (fun m -> m "[%s] Scatter to output %d" (E.name ()) index)
      >>= fun () -> state := !state + 1; O.put output value

    let run_policy state outputs value =
      match E.policy () with
      | Broadcast -> broadcast state outputs value
      | Sequential -> sequential state outputs value

    let rec process state inputs outputs () =
      I.get inputs ()
      >>= run_policy state outputs
      >>= fun () -> process state inputs outputs |> E.apply
  end
end

module Gather = struct
  module Make
      (E: Environment)
      (T: Tuple)
      (I: Stream with type data = T.t)
      (O: Stream with type data = T.t)
    : Operator with type inputs = I.t array and type outputs = O.t
  = struct
    type state = unit
    type inputs = I.t array
    type outputs = O.t

    let init () = ()

    let forward input output () =
      I.get input () >>= O.put output

    let rec process (state : unit) (inputs : inputs) (outputs : outputs) () =
      Array.to_list inputs
      |> Lwt_list.fold_left_s (fun _ input -> forward input outputs |> E.apply) ()
      >>= fun () -> process state inputs outputs |> E.apply
  end
end
