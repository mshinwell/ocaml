external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( < ) : 'a -> 'a -> bool = "%lessthan"

external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"

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

let g foo =
  let b = foo () in
  let (u, v) = b in
  u + v

let fl x =
  let a = opaque_identity 3. in
  let b =
    if a < 2. then
      a +. x
    else
      a -. x
  in
  let u = b +. 1. in
  u *. 2.
