(*
 * The Tuple type.
 *)

module type Tuple = sig
  type t
  val return: unit -> t
end

module Unit : Tuple with type t = unit

(*
 * The Operator type, with some handy shortcuts.
 *)

module type Operator = sig
  module Input : Tuple
  module Output : Tuple
  val process: Input.t -> Output.t
end

module type Source = Operator with module Input = Unit

module type Sink = Operator with module Output = Unit

(*
 * Monadic bind.
 *)

val (>>=):
  (module Tuple with type t = 'a) ->
  (module Operator with type Input.t = 'a and type Output.t = 'b) -> 
  (module Tuple with type t = 'b)

(*
 * Splitter and merger. These can be generated in a PPX.
 *)

val apply: (unit -> 'a) -> 'a

val split:
  (module Tuple with type t = 'a * 'b) ->
  (unit -> (module Tuple with type t = 'a) * (module Tuple with type t = 'b))

val merge:
  (module Tuple with type t = 'a) * (module Tuple with type t = 'b) ->
  (module Tuple with type t = 'a * 'b)
