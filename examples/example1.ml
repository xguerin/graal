open Fstream

(*
 * Define a string-based algebra.
 *)

module VoidType = struct
  type t = unit
  let compare _ _ = 0
  let show _ = "()"
end

(*
 * Graph function.
 *)

let algebra_cons () =
  let open Graph.Algebra in
  Overlay (
    Overlay (Overlay (Overlay (Connect (Overlay (Vertex ("B0", ()), Vertex ("B1", ())), Vertex ("M0", ())),
                               Connect (Vertex ("M0", ()), Vertex ("X0", ()))),
                      Connect (Vertex ("X0", ()), Vertex ("Y0", ()))),
             Connect (Vertex ("Y0", ()), Vertex ("S0", ()))),
    Connect (Vertex ("S0", ()), Overlay (Vertex ("K0", ()), Vertex ("K1", ()))))

let algebra_oper () =
  let open Graph.Algebra in
  (Vertex ("B0", ()) +> Vertex ("B1", ())) *> Vertex ("M0", ())
  +>
  Vertex ("M0", ()) *> Vertex ("X0", ())
  +>
  Vertex ("X0", ()) *> Vertex ("Y0", ())
  +>
  Vertex ("Y0", ()) *> Vertex ("S0", ())
  +>
  Vertex ("S0", ()) *> (Vertex ("K0", ()) +> Vertex ("K1", ()))

let check_graphs () =
  let open Graph.Algebra in
  let g1 = algebra_cons ()
  and g2 = algebra_oper ()
  in
  Printf.printf "%s\n" (to_string g1);
  Printf.printf "%s\n" (to_string g2);
  assert (g1 = g2)

let check_comparisons () =
  let open Graph.Algebra in
  assert (((Vertex ("B0", ()) +> Vertex ("B1", ())) *> Vertex ("M0", ())) =
          (Connect (Overlay (Vertex ("B0", ()), Vertex ("B1", ())), Vertex ("M0", ()))));
  assert (((Vertex ("B0", ()) +> Vertex ("B1", ())) *> Vertex ("M0", ()) +> Vertex ("M0", ()) *> Vertex ("X0", ())) =
          (Overlay (Connect (Overlay (Vertex ("B0", ()), Vertex ("B1", ())), Vertex ("M0", ())), Connect (Vertex ("M0", ()), Vertex ("X0", ())))))

let () =
  check_comparisons ();
  check_graphs ()
