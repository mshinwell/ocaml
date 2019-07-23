external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( < ) : 'a -> 'a -> bool = "%lessthan"

external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"

external opaque_identity : 'a -> 'a = "%opaque"

(*
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
*)
external float_of_int : int -> float = "%floatofint"

type t = {x: float; y: float}

let pr2162_1 z x y =
  let a, b =
    if z then (x * 2, y *. 3.)
    else (x, y)
  in
  float_of_int a -. b

let pr2162_2 z x y =
  let a, b =
    if z then (x * 2, y *. 3.)
    else (x, y +. 0.)
  in
  float_of_int a -. b

let pr2162_3 z x y =
  let pair =
    if z then (x * 2, y *. 3.)
    else (x, y +. 0.)
  in
  let a, b = pair in
  float_of_int a -. b

(*
let pr2162_2 = function
  | Some {x; y = 0.} | Some {x = 0.; y = x} -> x +. x
  | _ -> 0.
*)
