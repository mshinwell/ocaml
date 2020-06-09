(*
let f x =
  match x = 27 with
  | true -> 7
  | false -> 77
*)

type a = A | C of int

let f = function A -> 4 | C _ -> 5
