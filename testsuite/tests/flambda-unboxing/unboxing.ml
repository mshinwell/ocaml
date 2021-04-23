(* TEST
* flambda
** native
*)

external minor_words : unit -> (float [@unboxed])
  = "caml_gc_minor_words" "caml_gc_minor_words_unboxed"

let[@inline never] check_no_alloc line f x y expected_res =
  let before = minor_words () in
  let b = (f[@inlined never]) x y expected_res in
  let after = minor_words () in
  let diff = after -. before in
  assert b;
  if diff = 0. then
    Format.printf "OK@."
  else
    Format.printf "KO at line %d@." line

module Complex = struct
  type t = { re: float; im: float }
  let zero = { re = 0.0; im = 0.0 }
  let one = { re = 1.0; im = 0.0 }
  let[@inline] add x y = { re = x.re +. y.re; im = x.im +. y.im }

  let[@inline] arg x = atan2 x.im x.re

  let[@inline] norm x =
    (* Watch out for overflow in computing re^2 + im^2 *)
    let r = abs_float x.re and i = abs_float x.im in
    if r = 0.0 then i
    else if i = 0.0 then r
    else if r >= i then
      let q = i /. r in r *. sqrt(1.0 +. q *. q)
    else
      let q = r /. i in i *. sqrt(1.0 +. q *. q)
end

module T = struct

  type t =
    | A
    | B
    | C of int
    | D of int * int

end

let () =

  (* Check floats *)
  let aux x y res =
    let z = x +. y in
    z = res
  in
  check_no_alloc __LINE__ aux 1. 2. 3.;
  (* Check floats *)
  let aux b y res =
    let z =
      if b then y else y +. 1.
    in
    z *. 2. = res
  in
  check_no_alloc __LINE__ aux true 1. 2.;
  check_no_alloc __LINE__ aux false 1. 4.;
  (* Check tuple *)
  let aux b ((x, y) as t) res =
    let x', y' = if b then y, x else t in
    let z = x' - y' + 1 in
    z = res
  in
  check_no_alloc __LINE__ aux true (1,2) 2;
  check_no_alloc __LINE__ aux false (1,2) 0;
  (* Check nested tuples *)
  let aux b (((x, y), z) as t) res =
    let (x', y'), z' =
      if b then (y, z), x else t
    in
    let a = x' + (y' - z') in
    a = res
  in
  check_no_alloc __LINE__ aux true ((1,2),3) 4;
  check_no_alloc __LINE__ aux false ((1,2),3) 0;
  (* Check other way to nest tuples *)
  let aux (b, b') (x, y, z) res =
    let r = if b then (x, y) else (y, z) in
    let s = if b' then (x, z) else r in
    let a = match s with (u, v) -> u + v in
    a = res
  in
  check_no_alloc __LINE__ aux (true, false) (1,2,3) 3;
  check_no_alloc __LINE__ aux (false, true) (1,2,3) 4;
  (* Check large tuples *)
  let aux b ((r, s, t, u, v) as tuple) res =
    let r', s', t', u', v' = if b then (s, t, v, u, r) else tuple in
    r' + s' + t' + u' - v' = res
  in
  check_no_alloc __LINE__ aux true (1,2,3,4,5) 13;
  check_no_alloc __LINE__ aux false (1,2,3,4,5) 5;
  (* Check tuple+floats *)
  let aux b ((x, y) as t) res =
    let x', y' = if b then y, x else t in
    let z = x' -. y' +. 1. in
    z = res
  in
  check_no_alloc __LINE__ aux true (1.,2.) 2.;
  check_no_alloc __LINE__ aux false (1.,2.) 0.;
  (* Check blocks and floats *)
  let aux b (c1, c2) res =
    let c3 = (Complex.add[@inlined]) c1 c2 in
    let f = if b then (Complex.arg[@inlined]) c3 else (Complex.norm[@inlined]) c3 in
    f = res
  in
  check_no_alloc __LINE__ aux true (Complex.zero, Complex.one) 0.;
  check_no_alloc __LINE__ aux false (Complex.zero, Complex.one) 1.;
  (* Check variants *)
  let aux b x res =
    let opt =
      if b then None else Some x
    in
    let r =
      match opt with
      | None -> 1
      | Some x -> x
    in
    r = res
  in
  check_no_alloc __LINE__ aux true 0 1;
  check_no_alloc __LINE__ aux false 0 0;
  (* Check more complicated variant *)
  let aux (x, y) (a, b) res =
    let (t : T.t) =
      match x, y with
      | true, true -> A
      | true, false -> B
      | false, true -> C a
      | false, false ->
        let toto =
          if Sys.opaque_identity false then T.C b else T.D (a, b)
        in
        (* adding this line currently makes the test fail:
           let _ = id () in
           we could try to improve unboxing to handle this case *)
        toto
    in
    let e =
      match (t : T.t) with
      | C a -> a
      | D (a, b) -> a + b
      | A -> 42
      | B -> 50
    in
    e = res
  in
  check_no_alloc __LINE__ aux (true, true) (1,2) 42;
  check_no_alloc __LINE__ aux (true, false) (1,2) 50;
  check_no_alloc __LINE__ aux (false, true) (1,2) 1;
  check_no_alloc __LINE__ aux (false, false) (1,2) 3;
  (* More complex case for variants *)
  let aux (x, y) (a, b) res =
    let (t : T.t) =
      match x, y with
      | true, true -> A
      | true, false -> B
      | false, true -> C a
      | false, false -> D (b, a)
    in
    let c =
      match (t : T.t) with
      | C a -> a
      | D (_, a) -> a
      | A
      | B -> a
    in
    c = res
    (* Here, we might want to try and make sure that the type system will
       correctly identify that we can statically prove that c = a, and thus
       remove the unused branch of the conditional and finally remove the
       now unneeded allocation of the ref.
    let t = ref c in
    if c = a then (
      true
    ) else (
      !(Sys.opaque_identity t) = res
       )
    *)
  in
  check_no_alloc __LINE__ aux (true, true) (1,2) 1;
  check_no_alloc __LINE__ aux (true, false) (1,2) 1;
  (* Check closures *)
  let aux b x res =
    let[@inline] bar y =
      Sys.opaque_identity ();
      let aux z = x + y + z in
      aux
    in
    let f, _ =
      if b then (bar[@inlined]) 3, 1 else (bar[@inlined]) 42, 2
    in
    f x = res
  in
  check_no_alloc __LINE__ aux true 1 5;
  check_no_alloc __LINE__ aux false 1 44;
  (* END *)
  ()

