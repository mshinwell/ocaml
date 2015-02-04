type t =
 | A of float
 | B of (int * int)

let rec foo = function
 | A x -> x
 | B (x, y) -> float x +. float y

let (_ : float) = foo (A 4.)
