open Fstream
open Fstream.Std
open Fstream.Streams
open Fstream.Types

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
 * Configuration builder.
 *)

module SigInt = struct
  let state = ref true
  let apply v = if !state then v () else Lwt.return ()
end

let default name = (module struct
  include SigInt
  let name () = name
end: Environment)

let scatter name = (module struct
  include SigInt
  let name () = name
  let policy () = Scatter.Broadcast
end: Scatter.Environment)

(*
 * Define the streams.
 *)

module B0ToM0 = Mailbox(val (default "b0_to_m0"))(OneIntTuple)
module B1ToM0 = Mailbox(val (default "b1_to_m0"))(OneIntTuple)
module M0ToX0 = Mailbox(val (default "m0_to_x0"))(TwoIntTuple)
module X0ToY0 = Mailbox(val (default "x0_to_y0"))(TwoIntTuple)
module Y0ToS0 = Mailbox(val (default "y0_to_s0"))(TwoIntTuple)
module S0ToK0 = Mailbox(val (default "s1_to_k0"))(OneIntTuple)
module S0ToK1 = Mailbox(val (default "s1_to_k1"))(OneIntTuple)

(*
 * Define the operators.
 *)

module B0 = Beacon.Make   (val (default "B0"))(OneIntTuple)(B0ToM0)
module B1 = Beacon.Make   (val (default "B1"))(OneIntTuple)(B1ToM0)
module M0 = Merger.Make   (val (default "M0"))(OneIntTuple)(OneIntTuple)(TwoIntTuple)(B0ToM0)(B1ToM0)(M0ToX0)
module X0 = Scatter.Make  (val (scatter "X0"))(TwoIntTuple)(M0ToX0)(X0ToY0)
module Y0 = Gather.Make   (val (default "Y0"))(TwoIntTuple)(X0ToY0)(Y0ToS0)
module S0 = Splitter.Make (val (default "S0"))(OneIntTuple)(OneIntTuple)(TwoIntTuple)(Y0ToS0)(S0ToK0)(S0ToK1)
module K0 = Sink.Make     (val (default "K0"))(OneIntTuple)(S0ToK0)
module K1 = Sink.Make     (val (default "K1"))(OneIntTuple)(S0ToK1)

(*
 * Graph function.
 *)

(*
 * Main functions.
 *)

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_unix.on_signal Sys.sigint  (fun _ -> SigInt.state := false) |> ignore;
  (* Edges *)
  let b0_to_m0 = B0ToM0.init ()
  and b1_to_m0 = B1ToM0.init ()
  and m0_to_x0 = M0ToX0.init ()
  and x0_to_y0 = Array.init 4 (fun _ -> X0ToY0.init ())
  and y0_to_s0 = Y0ToS0.init ()
  and s0_to_k0 = S0ToK0.init ()
  and s0_to_k1 = S0ToK1.init ()
  in
  (* Vertices *)
  let b0 = B0.init ()
  and b1 = B1.init ()
  and m0 = M0.init ()
  and x0 = X0.init ()
  and y0 = Y0.init ()
  and s0 = S0.init ()
  and k0 = K0.init ()
  and k1 = K1.init ()
  in
  (* Processes *)
  [ B0.process b0 Std.null b0_to_m0 ()
  ; B1.process b1 Std.null b1_to_m0 ()
  ; M0.process m0 (b0_to_m0, b1_to_m0) m0_to_x0 ()

  ; X0.process x0 m0_to_x0 x0_to_y0 ()
  ; Y0.process y0 x0_to_y0 y0_to_s0 ()

  ; S0.process s0 y0_to_s0 (s0_to_k0, s0_to_k1) ()
  ; K0.process k0 s0_to_k0 Std.null ()
  ; K1.process k1 s0_to_k1 Std.null ()
  ]
  |> Lwt.join
  |> Lwt_main.run;

