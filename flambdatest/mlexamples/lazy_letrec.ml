type t = {
  f : int -> int;
}

let [@inline never] bar _ = assert false

let rec foo = lazy (fun x -> bar (Lazy.force t))
and t = lazy { f = (fun y -> (Lazy.force foo) y); }
