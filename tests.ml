external ( + ) : int -> int -> int = "%addint"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
(*
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

let length l = (length_aux [@unrolled 3]) 0 l
*)
module Int32 = struct
  external add : int32 -> int32 -> int32 = "%int32_add"
  external mul : int32 -> int32 -> int32 = "%int32_mul"

  let succ x = mul (add x 1l) 2l
end

module Int64 = struct
  external add : int64 -> int64 -> int64 = "%int64_add"
  external mul : int64 -> int64 -> int64 = "%int64_mul"

  let succ x = mul (add x 1L) 2L
end
