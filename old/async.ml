open Lwt.Infix

module type Stream = sig
  type t
  val run: unit -> t Lwt.t
  val to_string: t -> string
end

module type Processor = sig
  type tuple
  val output: unit -> (module Stream with type t = tuple)
end

module type BeaconParameters = sig
  val delay_in_seconds: float
end

module IntBeacon (P: BeaconParameters) : Processor with type tuple = int = struct
  type tuple = int
  let output () = (module struct
    type t = tuple
    let iterations = ref 0
    let run () =
      iterations := !iterations + 1;
      Lwt_unix.sleep P.delay_in_seconds >>= fun () ->
      Lwt.return !iterations
    let to_string = string_of_int
  end : Stream with type t = tuple)
end

module FloatBeacon (P: BeaconParameters) : Processor with type tuple = float = struct
  type tuple = float
  let output () = (module struct
    type t = tuple
    let iterations = ref 0.
    let run () =
      iterations := !iterations +. 1.;
      Lwt_unix.sleep P.delay_in_seconds >>= fun () ->
      Lwt.return !iterations
    let to_string = string_of_float
  end : Stream with type t = tuple)
end

module OneInputProcessor (I: Stream) : Processor with type tuple = I.t = struct
  type tuple = I.t
  let output () = (module I : Stream with type t = I.t)
end

module TwoInputsProcessor (I0: Stream) (I1: Stream) : Processor with type tuple = I0.t * I1.t = struct
  type tuple = I0.t * I1.t
  let output () = (module struct
    type t = tuple
    let run () =
      I0.run () >>= fun i0 ->
      I1.run () >>= fun i1 ->
      Lwt.return (i0, i1)
    let to_string (a, b) = "(" ^ (I0.to_string a) ^ ", " ^ (I1.to_string b) ^ ")"
  end : Stream with type t = tuple)
end

module Operator (I0: Stream) (I1: Stream) (I2: Stream) = struct
  let outputs () =
    let module O0 = OneInputProcessor(I0) in
    let module O1 = TwoInputsProcessor(I1)(I2) in
    (O0.output (), O1.output ())
end

module SinkProcessor (I: Stream) : Processor with type tuple = unit = struct
  type tuple = unit
  let output () = (module struct
    type t = unit
    let run () =
      I.run ()
      >>= fun v -> Lwt_io.printf "%s\n" (I.to_string v)
      >>= Lwt.return
    let to_string () = "()"
  end : Stream with type t = unit)
end

module SinkOperator (I: Stream) = struct
  let outputs () =
    let module O0 = SinkProcessor(I) in
    (O0.output ())
end

let () =
  let module B0 = IntBeacon(struct let delay_in_seconds = 0.25 end) in
  let module S0 = (val B0.output () : Stream with type t = int) in
  let module B1 = IntBeacon(struct let delay_in_seconds = 0.25 end) in
  let module S1 = (val B1.output () : Stream with type t = int) in
  let module B2 = FloatBeacon(struct let delay_in_seconds = 0.5 end) in
  let module S2 = (val B2.output () : Stream with type t = float) in
  let module Op = Operator(S0)(S1)(S2) in
  let (o0, o1) = Op.outputs () in
  let (s0) =
      let module O = (val o0 : Stream with type t = int) in
      let module S = SinkOperator(O) in
      S.outputs ()
  in
  let (s1) =
      let module O = (val o1 : Stream with type t = int * float) in
      let module S = SinkOperator(O) in
      S.outputs ()
  in
  let run () =
    [s0; s1]
    |> List.map (fun m -> let module M = (val m : Stream with type t = unit) in M.run ())
    |> Lwt.join
  in
  while true do
    Lwt_main.run (run ())
  done
