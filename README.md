# GRAph ALgebra

## About

Source of inspiration: [ALGA](https://github.com/snowleopard/alga).

## Overview

### Graph

Graal defines the following graph type:
```ocaml
type edge = string * string
type 'a vertex = string * 'a
type 'a t = Edges.t * 'a Vertices.t
```
With the following definitions:
```ocaml
module EdgeKeyType = struct
  type t = string * string
end

module Edges = Set.Make(EdgeKeyType)
module Vertices = Set.Make(String)
```
### Algebra

Graal defines the following graph algebra:
```ocaml
type 'a t =
  | Empty
  | Vertex of string * 'a
  | Connect of 'a t * 'a t
  | Overlay of 'a t * 'a t
```
Constructor semantics:

* `Empty` defines an empty graph (∅, ∅)
* `Vertex(l, c)` constructs a graph with a single vertex (∅, (l, c))
* `Connect(x, y)` connects graphs `(Ex, Vx)` and `(Ey, Vy)` constructing `(Ex ∪ Ey ∪ Ex × Ey, Vx ∪ Vy)`
* `Overlay(x, y)` overlays  graphs `(Ex, Vx)` and `(Ey, Vy)` constructing `(Ex ∪ Ey, Vx ∪ Vy)`

Infix operators:

* `x +> y`: `Overlay(x, y)`
* `x *> y`: `Connect(x, y)`
* `x *+> y`: constructs an overlays of the sink vertices in `x` and connect them to `y`
* `x +*> y`: constructs an overlays of the source vertices in `y` and connect them to `x`
* `x */> y`: constructs an overlays of the sink vertices in `x` and source vertices in `y` and connect them together

## Working with the PPX 

To see the parse tree of a ML file:
```
ocamlc -dparsetree foo.ml
```
To see the output of a development version of the extension:
```
_build/default/.ppx/.../ppx.exe example/preprocessor.ml
```
