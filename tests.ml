let [@inline always] to_inline _x _y = 42

(*
let f x y =
  match x with
  | None -> 0
  | Some a ->
    match y with
    | None -> 1
    | Some b ->
      to_inline a b
*)

external ( + ) : int -> int -> int = "%addint"

let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l

let length l = length_aux 0 l
