let f x =
  let cow =
    if x > 42 then
      let a = Printf.sprintf "%d" x in
      let b = a ^ "bar" in
      String.length b
    else
      100
  in
  let sheep = cow * 47 in
  (string_of_int sheep), cow

let () =
  let a, b = f 45 in
  Printf.printf "%s %d\n" a b
