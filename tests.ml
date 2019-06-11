external ( + ) : int -> int -> int = "%addint"
external ( < ) : 'a -> 'a -> bool = "%lessthan"

let [@inline always] to_inline _x _y = 42

let f c m n x' y' =
  let x = if c < 0 then x' else x' + 1 in
  let y = if c < 0 then y' else y' + 1 in
  match m with
  | None -> 0
  | Some a ->
    match n with
    | None -> 1
    | Some b ->
      to_inline (x + y) (a + b)

let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l

let length l = (length_aux [@inlined never]) 0 l
