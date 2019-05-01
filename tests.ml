external ( + ) : int -> int -> int = "%addint"

let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l

let length l = length_aux 0 l
