type +'a node =
  | Nil
  | Cons of 'a * 'a t

and 'a t = unit -> 'a node

let rec map f seq () =
  match seq () with
  | Nil -> Nil
  | Cons (x, next) ->
    let _ = map f seq () in
    Cons (f x, map f next)
