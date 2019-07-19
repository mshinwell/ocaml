external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external array_get: 'a array -> int -> 'a = "%array_safe_get"
external array_set: 'a array -> int -> 'a -> unit = "%array_safe_set"

let foo arr f i =
  array_set arr i (f (array_get arr i))

let f c m n x' y' =
  let x = if c < 0 then x' else x' + 10 in
  let y = if c < 0 then y' else y' + 20 in
  x + y


(* Notes:

- Extra continuation required when function calls in branches
- Are the extra parameters removed if unused? - Looks like it

For unboxing:
- extra_params_and_args needs to go first (maybe it always should?)
- Ordering imposed on parameters?
- extra_params needs to be able to add let bindings e.g. to get the field
before Apply_cont for unboxing a pair
- Presumably need to stop at the turn-around point when simplifying the
handler and make some decision.  Sounds like Cmmgen actually does this for
checking allocation.

- Stop creating exception continuations that just call other ones

*)
