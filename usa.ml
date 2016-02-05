let rec loop inv xs =
  match xs with
  | [] -> fst inv + snd inv
  | x::xs -> x + loop inv xs

let f x =
  Printf.printf "%d\n" (loop (x + 42, x + 43) [1;2;3])
