(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module TEE = Typing_env_extension

module Meet = Meet_or_join.Make (Lattice_ops.For_meet)
module Join = Meet_or_join.Make (Lattice_ops.For_join)

let meet env t1 t2 =
let ty, ext =
  Meet.meet_or_join env t1 t2
in
(*
Format.eprintf "meet:@ %a@ and@ %a@ =@ %a@ with extension:@ %a\n%!"
  Type_printers.print t1
  Type_printers.print t2
  Type_printers.print ty
  TEE.print ext;
*)
ty, ext

let join ?bound_name env t1 t2 =
  let env = Meet_env.create env in
  let joined, env_extension = Join.meet_or_join ?bound_name env t1 t2 in
  if not (TEE.is_empty env_extension) then begin
    Misc.fatal_errorf "Non-empty environment extension produced from a \
        [join] operation:@ %a"
      TEE.print env_extension
  end;
  joined
