(*type t =
  | A
  | B of int
  | C of int

let foo () = A
let foo () = A
*)

external ( + ) : int -> int -> int = "%addint"
external array_get: 'a array -> int -> 'a = "%array_safe_get"
external array_set: 'a array -> int -> 'a -> unit = "%array_safe_set"

let foo arr f i =
  array_set arr i (f (array_get arr i))

(*
let bar x =
  if x then if x then 42 else 1 else 0
*)
