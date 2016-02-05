let f x =
  let pair = x, x in
  Printf.printf "foo\n";
  fun y ->
   (* (Obj.size (Obj.repr pair)) + *) fst pair + fst pair + y

let g x =
  let clos_in_g () = x + 2 in
  Printf.printf "foo\n";
  fun y ->
    clos_in_g () + y

let () =
  Printf.printf "%d" ((f 1) 2)
