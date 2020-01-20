open Types

(*
 * Helper functions.
 *)

val null: unit input

(*
 * Operators.
 *)

module Beacon : sig
  module Make
      (E: Environment)
      (T: Tuple)
      (O: Stream with type data = T.t)
    : Operator with type inputs = unit input and type outputs = O.t
end

module Functor : sig
  module Make
      (E: Environment)
      (T: Tuple)
      (I: Stream with type data = T.t)
      (O: Stream with type data = T.t)
    : Operator with type inputs = I.t and type outputs = O.t
end

module Merger : sig
  module Make
      (E: Environment)
      (T0: Tuple) (T1: Tuple) (T2: Tuple with type t = T0.t * T1.t)
      (I0: Stream with type data = T0.t) (I1: Stream with type data = T1.t)
      (O: Stream with type data = T2.t)
    : Operator with type inputs = I0.t * I1.t and type outputs = O.t
end

module Splitter : sig
  module Make
      (E: Environment)
      (T0: Tuple) (T1: Tuple) (T2: Tuple with type t = T0.t * T1.t)
      (I: Stream with type data = T2.t)
      (O0: Stream with type data = T0.t) (O1: Stream with type data = T1.t)
    : Operator with type inputs = I.t and type outputs = O0.t * O1.t
end

module Sink : sig
  module Make
      (E: Environment)
      (T: Tuple)
      (I: Stream with type data = T.t)
    : Operator with type inputs = I.t and type outputs = unit output
end

module Scatter : sig
  type policy = Sequential | Broadcast

  module type Environment = sig
    include Environment
    val policy: unit -> policy
  end

  module Make
      (E: Environment)
      (T: Tuple)
      (I: Stream with type data = T.t)
      (O: Stream with type data = T.t)
    : Operator with type inputs = I.t and type outputs = O.t array
end

module Gather : sig
  module Make
      (E: Environment)
      (T: Tuple)
      (I: Stream with type data = T.t)
      (O: Stream with type data = T.t)
    : Operator with type inputs = I.t array and type outputs = O.t
end
