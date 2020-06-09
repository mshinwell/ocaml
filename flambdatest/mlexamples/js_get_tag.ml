type t =
  | A
  | B
  | C of int
  | D of int

type 'a ref = { mutable contents : 'a; }

let [@inline always] bar rf f g h p =
  match p with
  | A ->
    let b = rf.contents in
    let r = f () in
    if g () then () else h r b
  | B -> ()
  | C _ | D _ -> ()

let foo p rf f g h x y =
  bar rf f g h p;
  match p with
  | A | B -> x ()
  | C _ | D _ -> y ()
