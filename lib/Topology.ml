module Make(T: Types.OrderedType) = struct
  module Edges = Map.Make(String)
  module Vertices = Map.Make(String)

  type vertex = | Vertex of T.t * string list * string list
  type 'a edge = | Edge of 'a
  type 'a t = | Graph of 'a Edges.t * vertex Vertices.t
end
