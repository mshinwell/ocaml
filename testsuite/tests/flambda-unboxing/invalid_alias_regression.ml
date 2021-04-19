(* TEST
 * flambda
 ** native
*)

(* This test checks that no invalid aliases between unboxed parameters
   are introduced. See comment at the top of `unbox_continuation_params.ml`
   for more information. *)

type file =
  | Unit of int * int * int
  | Library of int * int

let scan_file () =
  let v =
    let file_name = Sys.opaque_identity 55 in
    if Sys.opaque_identity true then begin
      Unit (file_name,Sys.opaque_identity 1, Sys.opaque_identity 2)
    end
    else begin
      Library (file_name,Sys.opaque_identity 3)
    end
  in
  match v with
  | Unit (file_name,info,crc) ->
    assert (Sys.opaque_identity file_name = Sys.opaque_identity 55);
    let _ = Sys.opaque_identity crc in
    let _ = Sys.opaque_identity file_name in
    let _ = Sys.opaque_identity info in
    ()
  | Library (file_name,infos) ->
    assert (Sys.opaque_identity file_name = Sys.opaque_identity 55);
    let _ = Sys.opaque_identity file_name in
    let _ = Sys.opaque_identity infos in
    ()

let () = scan_file ()

