(*
 * Classes.
 *)

class ['a] reader = object
  method read: 'a Lwt.t =
    failwith "unimplemented"
end

class ['a] writer = object
  method write (_: 'a): unit Lwt.t =
    failwith "unimplemented"
end

class ['a] stream = object
  inherit ['a] reader
  inherit ['a] writer
end

class ['a, 'b] operator reader writer = object
  val reader: 'a reader = (reader :> 'a reader)
  val writer: 'b writer = (writer :> 'b writer)

  method process: unit Lwt.t =
    failwith "unimplemented"
end

(*
 * Modules and types.
 *)

type 'a input = unit -> 'a Lwt.t
type 'a output = 'a -> unit Lwt.t

module type Environment = sig
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
  val init: ?label:string -> unit -> t
  val label: t -> string
  val put: t -> data output
  val get: t -> data input
end

module type Operator = sig
  type t
  type inputs
  type outputs
  val init: ?label:string -> unit -> t
  val label: t -> string
  val process: t -> inputs -> outputs -> unit -> unit Lwt.t
end
