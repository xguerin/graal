open Lwt.Infix
open Fstream

(*
 * Helper functions
 *)

let parse_time month day time =
  let mon = match month with
    | "Jan" -> 0
    | "Feb" -> 1
    | "Mar" -> 2
    | "Apr" -> 3
    | "May" -> 4
    | "Jun" -> 5
    | "Jul" -> 6
    | "Aug" -> 7
    | "Sep" -> 8
    | "Oct" -> 9
    | "Nov" -> 10
    | "Dec" -> 11
    | _ -> 0
  in
  let (h, m, s) = String.split_on_char ':' time
                  |> List.map int_of_string
                  |> function
                  | [ h; m; s ] -> (h, m, s)
                  | _ -> (0, 0, 0)
  in
  let (res, _) = Unix.({ tm_hour = h
                       ; tm_isdst = false
                       ; tm_mday = int_of_string day
                       ; tm_min = m
                       ; tm_mon = mon
                       ; tm_sec = s
                       ; tm_wday = 0
                       ; tm_yday = 0
                       ; tm_year = 2020
                       })
                 |> Unix.mktime
  in res

(*
 * Types.
 *)

module LogLine = struct
  type t = { ts: float; hostname: string; service: string; message: string }
  [@@deriving show]
end

module Failure = struct
  type t = { ts: float; uid: string; euid: string; tty: string; rhost: string; user: string option }
  [@@deriving show]
end

module Success = struct
  type t = { ts: float; user: string }
  [@@deriving show]
end

module FailureRange = struct
  type t = { max_ts: float ; min_ts: float ; rhost: string ; users: string list }
  [@@deriving show]
end

module Suspect = struct
  type t = { diff: float ; max: float ; attempts: int; rhost: string ; users: string list }
  [@@deriving show]
end

module Breakin = struct
  type t = { ts: float ; rhost: string ; user: string }
  [@@deriving show]
end

(*
 * Suspect finder.
 *)

module StrMap = Map.Make(String)
module StrSet = Set.Make(String)

class aggregate ~attempts reader writer : Types.operator = object
  val mutable windows = StrMap.empty

  method process =
    let window_fn entries =
      let max_ts =
        List.fold_left (fun acc Failure.{ ts; _ } -> if ts > acc then ts else acc)
          0.0 entries
      in
      let min_ts =
        List.fold_left (fun acc Failure.{ ts; _ } -> if ts < acc then ts else acc)
          infinity entries
      in
      let users =
        List.fold_left (fun acc Failure.{ user; _ } ->
            match user with
            | Some(user) -> StrSet.add user acc
            | None -> acc)
          StrSet.empty entries
        |> StrSet.elements
      in
      (max_ts, min_ts, users)
    in
    let rec process_r () =
      reader#read
      >>= fun (Failure.{ rhost; _ } as e) ->
      begin
        match StrMap.find_opt rhost windows with
        | Some(w) -> Lwt.return w
        | None ->
          let window = new Windows.tumbling ~count:attempts ~fn:window_fn in
          windows <- StrMap.add rhost window windows;
          Lwt.return window
      end
      >>= fun window -> window#write e
      >>= begin function
        | Some((max_ts, min_ts, users)) -> writer#write FailureRange.{ max_ts; min_ts; rhost; users }
        | None -> Lwt.return ()
      end
      >>= process_r
    in
    process_r ()
end

class suspect_find ~attempts ~seconds reader writer : Types.operator = object
  method process =
    let%graph (_, procs) =
      Vertex("Input", new Std.input ~reader)
      *>
      Vertex("Range", new aggregate ~attempts)
      *+>
      Vertex("Cutoff", new Std.filter ~fn:(fun FailureRange.{ max_ts; min_ts; _ } ->
          max_ts -. min_ts < seconds))
      *+>
      Vertex("Diff", new Std.apply ~fn:(fun FailureRange.{ rhost; max_ts; min_ts; users} ->
          Lwt.return Suspect.{ diff = max_ts -. min_ts
                             ; max = max_ts
                             ; attempts
                             ; rhost
                             ; users
                             }))
      *+>
      Vertex("WithUsers", new Std.filter ~fn:(fun Suspect.{ users; _ } -> List.length users <> 0))
      *+>
      Vertex("Output", new Std.output ~writer)
    in
    procs
end

(*
 * Utility operators.
 *)

class duplicate reader (w0, w1, w2) : Types.operator = object
  method process =
    let rec process_r () =
      reader#read
      >>= fun v -> w0#write v
      >>= fun () -> w1#write v
      >>= fun () -> w2#write v
      >>= process_r
    in
    process_r ()
end

class join (r0, r1) writer : Types.operator = object(self)
  val mutable window0 = new Windows.sliding ~fn:List.hd ~count:1
  val mutable window1 = new Windows.sliding ~fn:List.hd ~count:1

  method private match_r0 v =
    match window1#content, v with
    | Success.{ ts; user } :: _, Some(Suspect.{ users; rhost; _ }) ->
      if List.exists (String.equal user) users then
        writer#write Breakin.{ ts; rhost; user }
      else
        Lwt.return ()
    | _ -> Lwt.return ()

  method private match_r1 v =
    match window0#content, v with
    | Suspect.{ users; rhost; _ } :: _, Some(Success.{ ts; user }) ->
      if List.exists (String.equal user) users then
        writer#write Breakin.{ ts; rhost; user }
      else
        Lwt.return ()
    | _ -> Lwt.return ()

  method process =
    let rec process_r0 () =
      r0#read
      >>= window0#write
      >>= self#match_r0
      >>= process_r0
    and process_r1 () =
      r1#read
      >>= window1#write
      >>= self#match_r1
      >>= process_r1
    in
    Lwt.join [ process_r0 (); process_r1 () ]
end

(*
 * Real-time subgraph.
 *)

let show_suspect e =
  Logs_lwt.debug (fun m -> m "%s" (Suspect.show e))
  >>= fun () -> Lwt.return e

class real_time ~writer reader _ : Types.operator = object
  method process =
    let%graph (_, procs) =
      Vertex("Input", new Std.input ~reader)
      *>
      Vertex("Finder", new suspect_find ~attempts:5 ~seconds:60.0)
      *+>
      Vertex("Show", new Std.apply ~fn:show_suspect)
      *+>
      Vertex("Output", new Std.output ~writer)
    in
    procs
end

class medium_term reader _ : Types.operator = object
  method process =
    let%graph (_, procs) =
      Vertex("Input", new Std.input ~reader)
      *>
      Vertex("Finder", new suspect_find ~attempts:50 ~seconds:600.0)
      *+>
      Vertex("Show", new Std.apply ~fn:show_suspect)
      *+>
      Vertex("Sink", new Std.sink)
    in
    procs
end

class long_term reader _ : Types.operator = object
  method process =
    let%graph (_, procs) =
      Vertex("Input", new Std.input ~reader)
      *>
      Vertex("Finder", new suspect_find ~attempts:300 ~seconds:3600.0)
      *+>
      Vertex("Show", new Std.apply ~fn:show_suspect)
      *+>
      Vertex("Sink", new Std.sink)
    in
    procs
end

(*
 * Failure pipeline.
 *)

let failure_filter LogLine.{ service; message; _ } =
  String.sub service 0 4  = "sshd" &&
  String.length message >= 22 &&
  String.sub message 0 22 = "authentication failure"

let format_failure LogLine.{ ts; message; _ } =
  (* Grab the properties *)
  let tokens = String.split_on_char ';' message
               |> Array.of_list
  in
  (* Split the properties and remove empty fields *)
  let entries = String.split_on_char ' ' tokens.(1)
                |> List.filter (fun e -> e <> "")
  in
  (* Make key/value pairs *)
  let values = List.map (fun e -> String.split_on_char '=' e |> Array.of_list) entries
               |> Array.of_list
  in
  (* Build the return tuple *)
  let user = if (Array.length values) = 7 then Some values.(6).(1) else None in
  Lwt.return Failure.{ ts
                     ; uid = values.(1).(1)
                     ; euid = values.(2).(1)
                     ; tty = values.(3).(1)
                     ; rhost = values.(5).(1)
                     ; user
                     }

class failures reader writer : Types.operator = object
  method process =
    let%graph (_, procs) =
      Vertex("Input", new Std.input ~reader)
      *>
      Vertex("RawFailures", new Std.filter ~fn:failure_filter)
      *+>
      Vertex("Failures", new Std.apply ~fn:format_failure)
      *+>
      Vertex("Duplicate", new duplicate)
      *+>
      (Vertex("LT", new long_term)
       +>
       Vertex("MT", new medium_term)
       +>
       Vertex("RT", new real_time ~writer))
    in
    procs
end

(*
 * Success pipeline.
 *)

let success_filter LogLine.{ service; message; _ } =
  String.sub service 0 4  = "sshd" &&
  String.length message >= 23 &&
  String.sub message 0 23 = "session opened for user"

let format_success LogLine.{ ts; message; _ } =
  (* Grab the properties *)
  let tokens = String.split_on_char ' ' message |> Array.of_list
  in
  Lwt.return Success.{ ts; user = tokens.(4) }

let show_success e =
  Logs_lwt.debug (fun m -> m "%s" (Success.show e))
  >>= fun () -> Lwt.return e

class successes reader writer : Types.operator = object
  method process =
    let%graph (_, procs) =
      Vertex("Input", new Std.input ~reader)
      *>
      Vertex("RawSuccesses", new Std.filter ~fn:success_filter)
      *+>
      Vertex("Successes", new Std.apply ~fn:format_success)
      *+>
      Vertex("Show", new Std.apply ~fn:show_success)
      *+>
      Vertex("Output", new Std.output ~writer)
    in
    procs
end

(*
 * Top-level graph.
 *)

let parse_line line =
  match String.split_on_char ' ' line with
  | mon :: day :: time :: hostname :: service :: tl ->
    let message = String.concat " " tl in
    Lwt.return LogLine.{ ts = parse_time mon day time; hostname; service; message }
  | _ -> failwith "Line format not supported"

let show_breakin (Breakin.{ rhost; user; _ } as e) =
  Logs_lwt.warn (fun m -> m "Potential break-in rhost:%s user:%s" rhost user)
  >>= fun () -> Lwt.return e

let show_logline e =
  Logs_lwt.debug (fun m -> m "%s" (LogLine.show e))
  >>= fun () -> Lwt.return e

let main path () =
  (*
   * Top-level log watch application.
   *)
  let%graph (_, procs) =
    Vertex("RawLines", new Std.file_source ~path)
    *>
    Vertex("ParseLines", new Std.apply ~fn:parse_line)
    *+>
    Vertex("Line", new Std.apply ~fn:show_logline)
    *+>
    Vertex("Duplicate", new Std.duplicate)
    *+>
    (Vertex("Failures", new failures) +> Vertex("Successes", new successes))
    *+>
    Vertex("Join", new join)
    *+>
    Vertex("Show", new Std.apply ~fn:show_breakin)
    *+>
    Vertex("Sink", new Std.sink)
  in
  Lwt_main.run procs

(*
 * Main entry point.
 *)

let () =
  let path = ref None in
  let set_path p = path := Some(p) in
  (*
   * Command line options.
   *)
  let spec =
    [ ("-p", Arg.String (set_path), "Names directory to list files")
    ]
  and usage_msg = "Log watch:"
  in
  Arg.parse spec print_endline usage_msg;
  (*
   * Logger.
   *)
  Logs.set_reporter (Logs.format_reporter ());
  Logs.set_level (Some Logs.Info);
  (*
   * Make sure options are set.
   *)
  match !path with
  | Some(path) -> main path ();
  | None -> Logs.err (fun m -> m "Path (-p) must be set")
