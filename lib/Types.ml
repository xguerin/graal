(*
 * Type definitions.
 *)

type 'a input = unit -> 'a Lwt.t
type 'a output = 'a -> unit Lwt.t

module type Environment = sig
  val name: unit -> string
  val apply: (unit -> unit Lwt.t) -> unit Lwt.t
end

module type Tuple = sig
  type t
  val zero: unit -> t
  val next: t -> t
  val to_string: t -> string
end

module type Stream = sig
  type data
  type t
  val init: unit -> t
  val put: t -> data output
  val get: t -> data input
end

module type Operator = sig
  type state
  type inputs
  type outputs
  val init: unit -> state
  val process: state -> inputs -> outputs -> unit -> unit Lwt.t
end
