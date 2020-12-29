(* TEST
 * native-compiler
 ** arch_amd64
*)

(* CR mshinwell: I'm not sure whether here is the right place, but we need
   a test suite that thoroughly exercises some of these, including various
   edge cases and tricky cases (32-bit ints on 64-bit platforms; negative and
   positive numbers / extremities of the ranges, etc).  Maybe we could generate
   a test file using another OCaml program? *)

(* converted in Selection *)
external builtin_rdtsc : unit -> (int64[@unboxed]) = "caml_rdtsc" "caml_rdtsc_unboxed"
[@@noalloc] [@@builtin]

let[@inline never] builtin_rdtsc () =
  builtin_rdtsc ()

(* converted in Cmmgen to Cclz, and then again to Ilzcnt in Selection
   unless compiled with -fno-lzcnt *)
external builtin_clz
  :  int
  -> (int[@untagged])
  = "caml_int_clz" "caml_int_clz_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

let[@inline never] builtin_clz () =
  builtin_clz (Sys.opaque_identity 5)

(* Converted in Selection.emit_expr and then in Selection.select_operation *)
external builtin_bsr : int -> (int[@untagged]) = "caml_int_bsr" "caml_int_bsr_untagged"
[@@noalloc] [@@builtin] [@@no_effects] [@@no_coeffects]

let[@inline never] builtin_bsr () =
  builtin_bsr (Sys.opaque_identity 5)

let () =
  Printf.printf "rdtsc: builtin %b\n" (not (Int64.(builtin_rdtsc () = builtin_rdtsc ())));
  Printf.printf "clz: builtin %d\n" (builtin_clz ());
  Printf.printf "bsr: builtin %d\n" (builtin_bsr ());
  ()

