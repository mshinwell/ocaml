
let [@inline never] kfprintf k o _ = 0

let fprintf oc fmt = kfprintf (fun x -> x) oc fmt
let printf fmt = fprintf 0 fmt
