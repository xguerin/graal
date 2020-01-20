open Fstream
open Fstream.Types

module IntTuple
  : Tuple with type t = int
= struct
  type t = int
  let zero () = 0
  let next v = v + 1
  let to_string = string_of_int
end

module IntPairTuple
  : Tuple with type t = int * int
= struct
  type t = int * int
  let zero () = (0, 0)
  let next (a, b) = (a + 1, b + 1)
  let to_string (a, b)= "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")"
end

module SigInt = struct
  let state = ref true
  let apply v = if !state then v () else Lwt.return ()
end

(*
 * Define the streams.
 *)

module B0ToM0 = Streams.Mailbox(struct include SigInt let name () = "b0_to_m0" end)(IntTuple)
module B1ToM0 = Streams.Mailbox(struct include SigInt let name () = "b1_to_m0" end)(IntTuple)
module M0ToX0 = Streams.Mailbox(struct include SigInt let name () = "m0_to_x0" end)(IntPairTuple)
module X0ToY0 = Streams.Mailbox(struct include SigInt let name () = "x0_to_y0" end)(IntPairTuple)
module Y0ToS0 = Streams.Mailbox(struct include SigInt let name () = "y0_to_s0" end)(IntPairTuple)
module S0ToK0 = Streams.Mailbox(struct include SigInt let name () = "s1_to_k0" end)(IntTuple)
module S0ToK1 = Streams.Mailbox(struct include SigInt let name () = "s1_to_k1" end)(IntTuple)

(*
 * Define the operators.
 *)

module B0 = Std.Beacon.Make   (struct include SigInt let name () = "B0"                                        end)(IntTuple)    (B0ToM0)
module B1 = Std.Beacon.Make   (struct include SigInt let name () = "B1"                                        end)(IntTuple)    (B1ToM0)
module M0 = Std.Merger.Make   (struct include SigInt let name () = "M0"                                        end)(IntTuple)    (IntTuple)(IntPairTuple)(B0ToM0)(B1ToM0)(M0ToX0)
module X0 = Std.Scatter.Make  (struct include SigInt let name () = "X0" let policy () = Std.Scatter.Sequential end)(IntPairTuple)(M0ToX0)  (X0ToY0)
module Y0 = Std.Gather.Make   (struct include SigInt let name () = "Y0"                                        end)(IntPairTuple)(X0ToY0)  (Y0ToS0)
module S0 = Std.Splitter.Make (struct include SigInt let name () = "S0"                                        end)(IntTuple)    (IntTuple)(IntPairTuple)(Y0ToS0)(S0ToK0)(S0ToK1)
module K0 = Std.Sink.Make     (struct include SigInt let name () = "K0"                                        end)(IntTuple)    (S0ToK0)
module K1 = Std.Sink.Make     (struct include SigInt let name () = "K0"                                        end)(IntTuple)    (S0ToK1)

(*
 * Main functions.
 *)

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_unix.on_signal Sys.sigint  (fun _ -> SigInt.state := false) |> ignore;
  (* Mailboxes *)
  let b0_to_m0 = B0ToM0.init ()
  and b1_to_m0 = B1ToM0.init ()
  and m0_to_x0 = M0ToX0.init ()
  and x0_to_y0 = Array.init 4 (fun _ -> X0ToY0.init ())
  and y0_to_s0 = Y0ToS0.init ()
  and s0_to_k0 = S0ToK0.init ()
  and s0_to_k1 = S0ToK1.init ()
  in
  (* Operators *)
  let b0 = B0.init ()
  and b1 = B1.init ()
  and m0 = M0.init ()
  and x0 = X0.init ()
  and y0 = Y0.init ()
  and s0 = S0.init ()
  and k0 = K0.init ()
  and k1 = K1.init ()
  in
  (* Connections *)
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

