(*
 * Pure sources.
 *)

module Beacon (T: Graph.Tuple)
  : Graph.Source with type Output.t = int * T.t

(*
 * Pure sinks.
 *)

module Well (T: Graph.Tuple)
  : Graph.Sink with module Input = T
