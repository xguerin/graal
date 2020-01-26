open Migrate_parsetree
open Ast_409
open Ast_convenience_409
open Ast_mapper
open Asttypes
open Parsetree

open Fstream.Graph
open Fstream.Graph.Algebra

(* Exception *)

let location_exn ~loc msg =
  Location.Error (Location.error ~loc msg)
  |> raise
;;

(*
 * Helpers.
 *)

let ident ~loc lid =
  { pexp_desc = Pexp_ident(lid)
  ; pexp_loc = loc
  ; pexp_loc_stack = []
  ; pexp_attributes = []
  }

let value_binding ~loc label expr =
  { pvb_pat = pvar ~loc label
  ; pvb_expr = expr
  ; pvb_attributes = []
  ; pvb_loc = loc
  }

let to_list ~loc v =
  List.fold_right (fun e acc -> constr ~loc "::" [e; acc])
    v (constr ~loc "[]" [])

(*
 * Algebra parser and converter.
 *)

let combine e0 e1 =
  let module E = Edges in
  let module V = Vertices in
  eval e0
  |> fun (e, v) ->
  V.filter (fun key _ -> E.filter (fun (l, _) -> String.equal key l) e |> E.is_empty) v
  |> fun sel -> V.fold (fun k v acc -> Vertex(k, v) :: acc) sel []
  |> function
  | [] -> Overlay(e0, e1)
  | v0 :: [] -> Overlay(e0, Connect(v0, e1))
  | v0 :: v1 :: tl ->
  let group = List.fold_left (fun acc e -> Overlay(acc, e)) (Overlay(v0, v1)) tl in
  Overlay(e0, Connect(group, e1))

let rec convert = function
  (* Overlay *)
  | [%expr [%e? e0] +> [%e? e1]] -> Overlay(convert e0, convert e1)
  (* Connect *)
  | [%expr [%e? e0] *> [%e? e1]] ->
    Connect(convert e0, convert e1)
  | [%expr [%e? e0] *+> [%e? e1]] ->
    combine (convert e0) (convert e1)
  (* Vertex *)
  | [%expr Vertex[%e?
        { pexp_desc = Pexp_tuple([ { pexp_desc = Pexp_constant(Pconst_string(label, _))
                                   ; _ }; expr])
        ; _ }
    ]] -> Vertex(label, expr)
  (* Exception *)
  | expr -> location_exn ~loc:expr.pexp_loc "[ppx_fstream] invalid expression"

let edge_name (f, t) =
  String.lowercase_ascii (f ^ "_" ^ t)

(*
 * Graph generator.
 *)

let generate_graph ~loc (edges, vertices) =
  let tuples = Edges.fold (fun (a, b) acc ->
      tuple ~loc [str ~loc a; str ~loc b] :: acc) edges []
  and vertices = Vertices.fold (fun k _ acc ->
      (str ~loc k) :: acc) vertices []
  in
  tuple ~loc [to_list ~loc tuples; to_list ~loc vertices]

(*
 * Streams and processes generators.
 *)

let generate_edges ~loc (edges, _) expr =
  let void = value_binding ~loc "void" [%expr new Fstream.Streams.Unit.stream] in
  let vbs = Edges.fold (fun e acc ->
      let vname = edge_name e in
      let vb = value_binding ~loc vname [%expr new Fstream.Streams.Mailbox.stream] in
      vb :: acc)
      edges [ void ]
  in
  let_in ~loc vbs expr

let find_sources label =
  Edges.filter (fun (s, _) -> String.equal s label)

let find_targets label =
  Edges.filter (fun (_, t) -> String.equal t label)

let make_edge_argument ~loc edges =
  if Edges.is_empty edges then
    ident ~loc (lid "void")
  else
    Edges.fold (fun e acc -> ident ~loc (lid (edge_name e)) :: acc)
      edges []
    |> List.rev
    |> tuple ~loc

let generate_process_list ~loc (edges, vertices) =
  let exprs = Vertices.fold (fun label expr acc ->
      let source_edges = find_sources label edges
      and target_edges = find_targets label edges
      in
      let sources = make_edge_argument ~loc source_edges
      and targets = make_edge_argument ~loc target_edges
      in
      let res = [%expr [%e expr] [%e targets] [%e sources]]
      in
      res :: acc)
      vertices []
  in
  List.fold_right (fun e acc ->
      let tuple = tuple ~loc (e :: [acc]) in
      constr ~loc "::" [tuple])
    exprs (constr ~loc "[]" [])

let generate_processes ~loc graph =
  generate_process_list ~loc graph
  |> fun expr -> [%expr List.map (fun e -> e#process) [%e expr]]
  |> fun expr -> [%expr Lwt.join [%e expr]]

let generate expr =
  let result = convert expr |> eval
  and loc = expr.pexp_loc
  in
  let graph = generate_graph ~loc result
  and procs = generate_processes ~loc result |> generate_edges ~loc result
  in
  let_in ~loc [ value_binding ~loc "graph" graph
              ; value_binding ~loc "procs" procs
              ]
    (tuple ~loc [evar ~loc "graph"; evar ~loc "procs"])

(*
 * Rewriter.
 *)

let extension expr =
  let loc = expr.pexp_loc in
  match expr.pexp_desc with
  | Pexp_let (Nonrecursive, bindings, expr) ->
    let eval = List.map (fun ({ pvb_expr; _} as vb) ->
        { vb with pvb_expr = generate pvb_expr })
        bindings
    in
    { expr with pexp_desc = Pexp_let (Nonrecursive, eval, expr) }
  | Pexp_apply(_, _) -> generate expr
  | _ -> location_exn ~loc "[ppx_fstream] only 'let' or [%graph] supported"

let expression mapper = function
  | [%expr [%graph [%e? e0]]] -> mapper.expr mapper (extension e0)
  | expr -> Ast_mapper.default_mapper.expr mapper expr

let structure_item_mapper mapper = function
  | [%stri [%%graph let [%p? var] = [%e? e0]]] ->
    [%stri let [%p mapper.pat mapper var] = [%e mapper.expr mapper (extension e0)]]
  | stri -> Ast_mapper.default_mapper.structure_item mapper stri

let rewriter _ _ = {
  Ast_mapper.default_mapper with
  expr = expression;
  structure_item = structure_item_mapper;
}

let () =
  Driver.register ~name:"ppx_fstream" ~args:[] Versions.ocaml_409 rewriter
