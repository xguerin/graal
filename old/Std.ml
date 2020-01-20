open Graph

(*
 * Pure sources.
 *)

module Beacon (T: Tuple)
  : Source with type Output.t = int * T.t
= struct
  module Input = Unit

  module Output = struct
    type t = int * T.t
    let return () = (0, T.return ())
  end

  let iteration = ref 0
  let process () =
    iteration := !iteration + 1;
    (!iteration, T.return ())
end


(*
 * Pure sinks.
 *)

module Well (T: Graph.Tuple)
  : Graph.Sink with module Input = T
= struct
  module Input = T
  module Output = Unit
  let process _ = ()
end
