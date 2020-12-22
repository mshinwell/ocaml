(* TEST
 * native-compiler
 ** arch_amd64
 ocamlopt_flags += " -c -S"
 *** setup-ocamlopt.byte-build-env
 **** ocamlopt.byte
 ***** script
 script = "sh ${test_source_directory}/check_builtin.sh"
 output = "compiler-output"
 ****** check-ocamlopt.byte-output
 compiler_output = "compiler-output"
*)

external rdtsc : unit -> (int64[@unboxed]) = "caml_rdtsc" "caml_rdtsc_unboxed"
[@@noalloc]

let[@inline never] rdtsc () =
  rdtsc ()

external clz
  :  int
  -> (int[@untagged])
  = "caml_int_clz" "caml_int_clz_untagged"
[@@noalloc]

let[@inline never] clz () =
  clz (Sys.opaque_identity 5)

external bsr : int -> (int[@untagged]) = "caml_int_bsr" "caml_int_bsr_untagged"
[@@noalloc]

let[@inline never] bsr () =
  bsr (Sys.opaque_identity 5)
