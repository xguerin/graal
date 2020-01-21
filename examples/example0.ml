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

module SigIntEnv = struct
  let state = ref true
  let apply v = if !state then v () else Lwt.return ()
end

module ScatterEnv = struct
  include SigIntEnv
  let policy () = Scatter.Broadcast
end

(*
 * Define the streams.
 *)

module OneIntStream = Mailbox(SigIntEnv)(OneIntTuple)
module TwoIntStream = Mailbox(SigIntEnv)(TwoIntTuple)

(*
 * Define the operators.
 *)

module B = Beacon.Make   (SigIntEnv)(OneIntTuple)(OneIntStream)
module M = Merger.Make   (SigIntEnv)(OneIntTuple)(OneIntTuple)(TwoIntTuple)(OneIntStream)(OneIntStream)(TwoIntStream)
module X = Scatter.Make  (ScatterEnv)(TwoIntTuple)(TwoIntStream)(TwoIntStream)
module Y = Gather.Make   (SigIntEnv)(TwoIntTuple)(TwoIntStream)(TwoIntStream)
module S = Splitter.Make (SigIntEnv)(OneIntTuple)(OneIntTuple)(TwoIntTuple)(TwoIntStream)(OneIntStream)(OneIntStream)
module K = Sink.Make     (SigIntEnv)(OneIntTuple)(OneIntStream)

(*
 * Main functions.
 *)

let generate_label base index =
  base ^ "[" ^ (string_of_int index) ^ "]"

let () =
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  Lwt_unix.on_signal Sys.sigint  (fun _ -> SigIntEnv.state := false) |> ignore;
  (* Edges *)
  let b0_to_m0 = OneIntStream.init ~label:"b0_to_m0" ()
  and b1_to_m0 = OneIntStream.init ~label:"b1_to_m0" ()
  and m0_to_x0 = TwoIntStream.init ~label:"m0_to_x0" ()
  and x0_to_y0 = Array.init 4 (fun i -> TwoIntStream.init ~label:(generate_label "x0_to_y0" i) ())
  and y0_to_s0 = TwoIntStream.init ~label:"y0_to_s0" ()
  and s0_to_k0 = OneIntStream.init ~label:"s0_to_k0" ()
  and s0_to_k1 = OneIntStream.init ~label:"s0_to_k1" ()
  in
  (* Vertices *)
  let b0 = B.init ~label:"b0" ()
  and b1 = B.init ~label:"b1" ()
  and m0 = M.init ~label:"m0" ()
  and x0 = X.init ~label:"x0" ()
  and y0 = Y.init ~label:"y0" ()
  and s0 = S.init ~label:"s0" ()
  and k0 = K.init ~label:"k0" ()
  and k1 = K.init ~label:"k1" ()
  in
  (* Processes *)
  [ B.process b0 Std.null b0_to_m0 ()
  ; B.process b1 Std.null b1_to_m0 ()
  ; M.process m0 (b0_to_m0, b1_to_m0) m0_to_x0 ()

  ; X.process x0 m0_to_x0 x0_to_y0 ()
  ; Y.process y0 x0_to_y0 y0_to_s0 ()

  ; S.process s0 y0_to_s0 (s0_to_k0, s0_to_k1) ()
  ; K.process k0 s0_to_k0 Std.null ()
  ; K.process k1 s0_to_k1 Std.null ()
  ]
  |> Lwt.join
  |> Lwt_main.run;

