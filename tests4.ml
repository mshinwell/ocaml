external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( < ) : 'a -> 'a -> bool = "%lessthan"

external opaque_identity : 'a -> 'a = "%opaque"

let f x =
  let a = opaque_identity 3 in
  let b =
    if a < 2 then
      (a, x)
    else
      (x, a)
  in
  let (u, v) = b in
  u + v
