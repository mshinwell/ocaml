type 'a ref = { mutable contents : 'a; }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( +. ) : float -> float -> float = "%addfloat"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external opaque : 'a -> 'a = "%opaque"
external float_of_int : int -> float = "%floatofint"

(*
let unbox_float_refs () =
  let r = ref 0. in
  for i = 1 to 1000 do r := !r +. float_of_int i done;
  opaque !r
*)

let unbox_float_refs () =
  let r = ref 0. in
  for i = 1 to 1000 do r := !r +. 1. done;
  opaque !r
