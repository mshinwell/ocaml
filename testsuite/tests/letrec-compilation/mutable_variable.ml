(* TEST *)

let n, a =
  let r = ref 0 in
  let rec a =
    let n = 12 in
    r := n;
    !r :: a
  in
  !r, a

let () =
  print_int n;
  print_newline ();
  print_int (List.hd a);
  print_newline ()

let a =
  let rec a =
    let r = ref 0 in
    let n = 42 in
    r := n;
    !r :: a
  in
  a

let () =
  print_int (List.hd a);
  print_newline ()

type t = A : (unit -> t list) -> t

let a =
  let rec a =
    let _r = ref (print_endline "plop"; A (fun () -> a)) in
    (A (fun () -> a)) :: a
  in
  a

let () =
  let A l = List.hd a in
  assert((l ()) == a)

let a =
  let rec l =
    let _r = ref l in
    1 :: l
  in
  l

let a =
  let rec l =
    let r = ref 0 in
    let rec b =
      incr r;
      let v = !r :: l in
      incr r;
      v
    in
    incr r;
    !r :: b
  in
  l

let () =
  print_int (List.hd a);
  print_newline ();
  print_int (List.hd (List.tl a));
  print_newline ();
  print_int (List.hd (List.tl (List.tl a)));
  print_newline ()


