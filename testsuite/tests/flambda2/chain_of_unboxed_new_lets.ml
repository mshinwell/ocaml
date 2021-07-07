(* TEST
   * flambda
   ** native
*)

(* The value_kind annotated on the x argument of the function tells that it
   contains a boolean. Hence at the join point "let x' =" enough information
   on both branches is known and the record is unboxed, and the inner boolean
   is untagged. An argument for the unboxed field b is added and for the
   untagged boolean. Since b is not used tagged, the 'b' argument is removed
   from the continuation. In the else branch, the contents for b is not available
   in scope, so it has to be introduced through a new let, so does the untagged
   version, which requires the new bound variable for the b field.
   This test verifies that the new let introduced for the b field is kept.
*)

type t = { b : bool }

let f1 cond (x:t) =
  let x' =
    if cond then
      { b = true }
    else
      x
  in
  if x'.b then Sys.opaque_identity 1 else Sys.opaque_identity 2
[@@inline never]

(* Same test with a chain of loads *)

type cond = A | B | Allocates
type t1 = { v : int }
type t2 = { t1 : t1 }

let f2 (cond, v) (t2:t2) =
  let t2' =
    match cond with
    | A ->
      { t1 = { v } }
    | B ->
      t2
    | Allocates ->
      Sys.opaque_identity { t1 = { v } }
  in
  t2'.t1.v + 42
[@@inline never]

(* We test that those functions were really unboxed *)

external minor_words : unit -> (float [@unboxed])
  = "caml_gc_minor_words" "caml_gc_minor_words_unboxed"

let[@inline never] check_no_alloc line f x y expected_res =
  let before = minor_words () in
  let r = (f[@inlined never]) x y in
  let after = minor_words () in
  let diff = after -. before in
  assert (r = expected_res);
  if diff = 0. then
    Format.printf "OK@."
  else
    Format.printf "KO at line %d@." line

let () =

  check_no_alloc __LINE__ f1 true { b = false } 1;
  check_no_alloc __LINE__ f1 false { b = false } 2;

  check_no_alloc __LINE__ f2 (A, 5) { t1 = { v = -42 } } 47;
  check_no_alloc __LINE__ f2 (B, 5) { t1 = { v = -42 } } 0;
  (* Test that the test works *)
  check_no_alloc __LINE__ f2 (Allocates, 5) { t1 = { v = -42 } } 47;

  ()
