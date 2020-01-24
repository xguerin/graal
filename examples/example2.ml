open Lwt.Infix
open Fstream
open Fstream.Types

(*
 * Tuples.
 *)

module OneIntTuple
  : Tuple with type t = int
= struct
  type t = int
  let zero () = 0
  let next v = v + 1
  let to_string = string_of_int
end

module TwoIntTuple
  : Tuple with type t = int * int
= struct
  type t = int * int
  let zero () = (0, 0)
  let next (a, b) = (a + 1, b + 1)
  let to_string (a, b)= "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")"
end

(*
 * Configuration.
 *)

module SigIntEnv = struct
  let state = ref true
  let apply v = if !state then v () else Lwt.return ()
end

(*
 * Define the streams.
 *)

module Unit = struct
  class stream = object
    inherit [unit] Types.stream
    method read = Lwt.return ()
    method write () = Lwt.return ()
  end
end

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

  let make () = new stream;
end

(*
 * Objects.
 *)

module Beacon(T: Tuple) = struct
  class operator reader writer = object
    inherit [unit, T.t] Types.operator reader writer

    val state = T.zero ()

    method process =
      let rec process_r () =
        reader#read
        >>= fun () -> Lwt_unix.sleep 1.0
        >>= fun () -> Logs_lwt.info (fun m -> m "Generating %s" (T.to_string state))
        >>= fun () -> writer#write state
        >>= process_r
      in
      process_r ()
  end
end

module Dup(T: Tuple) = struct
  class operator reader writer = object
    inherit [T.t, T.t * T.t] Types.operator reader writer

    val state = T.zero ()

    method process =
      let rec process_r () =
        reader#read
        >>= fun v -> writer#write (v, v)
        >>= process_r
      in
      process_r ()
  end
end

module Sink(T: Tuple) = struct
  class operator input output = object
    inherit [T.t, unit] Types.operator input output

    val state = T.zero ()

    method process =
      let rec process_r () =
        reader#read
        >>= fun v -> Logs_lwt.info (fun m -> m "Consuming %s" (T.to_string v))
        >>= writer#write
        >>= process_r
      in
      process_r ()
  end
end

module B = Beacon(OneIntTuple)
module D = Dup(OneIntTuple)
module K = Sink(TwoIntTuple)

(*
 * Graph.
 *)

module OrderedType = struct
  type t =
    | Beacon
    | Dup
    | Sink
  [@@deriving ord, show]
end

module Algebra = Graph.Algebra(OrderedType)

let make_graph () =
  let open Algebra in
  (Vertex ("B0", Beacon) +> Vertex ("B1", Beacon)) *> Vertex ("D0", Dup)
  +>
  Vertex ("D0", Dup) *> Vertex ("S0", Sink)

let eval_graph g =
  let open Algebra in
  eval (fun () -> new Mailbox.stream) g

(*
 * Main.
 *)

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  (* Streams *)
  let void = new Unit.stream
  and s0 = new Mailbox.stream
  and s1 = new Mailbox.stream
  in
  let builder = function
    | `Beacon, _ -> new B.operator void s0
    | `Dup, _ -> new D.operator s0 s1
    | `Sink, _ -> new K.operator s1 void
  in
  (* Operators *)
  [ builder (`Beacon, "")
  ; builder (`Dup, "")
  ; builder (`Sink, "")
  ]
  |> List.map (fun e -> e#process)
  |> Lwt.join
  |> Lwt_main.run
