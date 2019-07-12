(*type t =
  | A
  | B of int
  | C of int

let foo () = A
let foo () = A
*)

external array_get: 'a array -> int -> 'a = "%array_safe_get"

let foo arr i =
  array_get arr i, array_get arr i

(*
let bar x =
  if x then if x then 42 else 1 else 0
*)
