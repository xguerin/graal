(*
 * Monad definition, for remembrance.
 *
 * module type Monad = sig
 *   type 'a t
 *   val return: 'a -> 'a t
 *   val bind: 'a t -> ('a -> 'b t) -> 'b t
 * end
 *)

(*
 * The Tuple type.
 *)

module type Tuple = sig
  type t
  val return: unit -> t
end

module Unit
  : Tuple with type t = unit
= struct
  type t = unit
  let return () = ()
end

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

let (>>=) (type input) (type output)
    (t: (module Tuple with type t = input))
    (o: (module Operator with type Input.t = input and type Output.t = output))
  =
  let module T = (val t: Tuple with type t = input) in
  let module O = (val o: Operator with type Input.t = T.t and type Output.t = output)
  in
  (module struct
    type t = output
    let return () = O.process (T.return ())
  end: Tuple with type t = output)

(*
 * Splitters and merger.
 *)

let apply fn = fn ()

let split (type a) (type b) t =
  let module T = (val t: Tuple with type t = a * b) in
  let iterator () =
    let (v0, v1) = T.return ()
    in
    let r0 = (module struct type t = a let return () = v0 end : Tuple with type t = a)
    and r1 = (module struct type t = b let return () = v1 end : Tuple with type t = b)
    in
    (r0, r1)
  in
  iterator

let merge (type a) (type b) (t0, t1) =
  let module T0 = (val t0: Tuple with type t = a) in
  let module T1 = (val t1: Tuple with type t = b)
  in
  (module struct type t = a * b let return () = (T0.return (), T1.return ()) end : Tuple with type t = a * b)
