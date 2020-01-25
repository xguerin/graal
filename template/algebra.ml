let () =
  let value =
    (Vertex ("B0", 1 + 1) +> Vertex ("B1", 2 + 2))
    *>
    Vertex ("S0", 3 + 3)
  in
  (1, 2, 3)
