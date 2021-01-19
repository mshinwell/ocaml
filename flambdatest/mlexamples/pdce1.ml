external ( *. ) : float -> float -> float = "%mulfloat"
external int_of_float : float -> int = "%intoffloat"

let f g x b =
  let y = x *. x in
  if b then
    g y
  else
    int_of_float y
