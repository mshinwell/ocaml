type t =
  | A
  | B of int
  | C of int

let f () =
  let x = A in
  match x with
  | A -> 0
  | B _ -> 1
  | C _ -> 2

let g () =
  let x = B 42 in
  match x with
  | A -> 0
  | B _ -> 1
  | C _ -> 2
