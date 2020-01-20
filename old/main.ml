open Fstream.Graph
open Fstream.Std

(*
 * Stateless vertex. Stateful would require a type t.
 *)

module Passthrough (T: Tuple)
  : Operator with module Input = T and module Output = T
= struct
  module Input = T
  module Output = T
  let process t = t
end

(*
 * I need an operation to map a Operator.output to a Operator.input.
 *
 * Tuple >>= Something >>= Something
 *
 * Tuple -> Operator -> Tuple
 *
 *)

module IntTuple : Tuple with type t = int = struct
  type t = int
  let return () = 0
end

module IntPairTuple : Tuple with type t = int * int = struct
  type t = int * int
  let return () = (0, 0)
end

module BeaconOutput (Data: Tuple): Tuple with type t = int * Data.t = struct
  type t = int * (Data.t)
  let return () = (0, Data.return ())
end

module PassthroughFilter (T: Tuple)
  : Operator with module Input = BeaconOutput(T) and module Output = T
= struct
  module Input = BeaconOutput(T)
  module Output = T
  let process (iteration, t) = Printf.printf "%d\n" iteration; t
end

module IntSum
  : Operator with module Input = IntPairTuple and module Output = IntTuple
= struct
  module Input = IntPairTuple
  module Output = IntTuple
  let process (a1, a2) = a1 + a2
end

(*
 *)

let graph_a tuple =
  tuple
  >>= (module Passthrough(IntTuple))

let graph_b tuple =
  tuple
  >>= (module Passthrough(IntTuple))

let result =
  (module Unit)
  >>= (module Beacon(IntPairTuple))
  >>= (module PassthroughFilter(IntPairTuple))
  |> split |> apply |> merge

(*
  >>= (module IntSum)
  >>= (module Well(IntTuple))
*)

let () =
  let module R = (val result: Tuple with type t = IntPairTuple.t) in
  for _ = 0 to 10 do
    R.return () |> ignore
  done
