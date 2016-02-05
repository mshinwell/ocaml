let bar c =
  let rec g zs fv =
    match zs with
    | [] -> []
    | z::zs ->
      let rec map2 f = function
        | [] -> []
(*        | a::l -> let r = fv + ((f [@inlined never]) a) in r :: map2 f l *)
        | a::l -> let r = fv + (f a) in r :: map2 f l
      in
      (map2 (fun y -> z + y) [z; 2; 3; 4]) @ g zs fv
  in
  Printf.printf "%d" (List.length (g [1;2;3;4] c))
