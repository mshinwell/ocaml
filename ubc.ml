let f x =
  let g y =
    x + y
  in
  (g [@inlined never]) 42
