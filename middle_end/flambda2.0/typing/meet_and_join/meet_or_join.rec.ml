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

module KI = Kind_independent_meet_or_join
module T = Flambda_types
module TEE = Typing_env_extension

module Make
  (E : Lattice_ops_intf.S
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  (* First specialise generic meet-and-join code to either meet or join. *)
  module Of_kind_value = Meet_and_join_value.Make (E)
  module Of_kind_naked_immediate = Meet_and_join_naked_immediate.Make (E)
  module Of_kind_naked_float = Meet_and_join_naked_float.Make (E)
  module Of_kind_naked_int32 = Meet_and_join_naked_int32.Make (E)
  module Of_kind_naked_int64 = Meet_and_join_naked_int64.Make (E)
  module Of_kind_naked_nativeint = Meet_and_join_naked_nativeint.Make (E)
  module Of_kind_fabricated = Meet_and_join_fabricated.Make (E)

  (* Next lift the meet or join operations from "of_kind_..." to "ty_...". *)
  module Value = KI.Make (E) (Of_kind_value)
  module Naked_immediate = KI.Make (E) (Of_kind_naked_immediate)
  module Naked_float = KI.Make (E) (Of_kind_naked_float)
  module Naked_int32 = KI.Make (E) (Of_kind_naked_int32)
  module Naked_int64 = KI.Make (E) (Of_kind_naked_int64)
  module Naked_nativeint = KI.Make (E) (Of_kind_naked_nativeint)
  module Fabricated = KI.Make (E) (Of_kind_fabricated)

  (* This function then lifts the meet or join operation from "ty_..." to
     Flambda types. *)
  let meet_or_join ?bound_name env (t1 : T.t) (t2 : T.t) : T.t * _ =
    match t1, t2 with
    | Value ty1, Value ty2 ->
      begin match Value.meet_or_join_ty ?bound_name env ty1 ty2 with
      | Ok (ty, env_extension) -> Value ty, env_extension
      | Bottom -> Flambda_type0_core.bottom K.value, TEE.empty
      end
    | Naked_number (ty1, Naked_immediate),
        Naked_number (ty2, Naked_immediate) ->
      begin match Naked_immediate.meet_or_join_ty ?bound_name env ty1 ty2 with
      | Ok (ty, env_extension) ->
        Naked_number (ty, Naked_immediate), env_extension
      | Bottom -> Flambda_type0_core.bottom K.naked_immediate, TEE.empty
      end
    | Naked_number (ty1, Naked_float), Naked_number (ty2, Naked_float) ->
      begin match Naked_float.meet_or_join_ty ?bound_name env ty1 ty2 with
      | Ok (ty, env_extension) ->
        Naked_number (ty, Naked_float), env_extension
      | Bottom -> Flambda_type0_core.bottom K.naked_float, TEE.empty
      end
    | Naked_number (ty1, Naked_int32), Naked_number (ty2, Naked_int32) ->
      begin match Naked_int32.meet_or_join_ty ?bound_name env ty1 ty2 with
      | Ok (ty, env_extension) ->
        Naked_number (ty, Naked_int32), env_extension
      | Bottom -> Flambda_type0_core.bottom K.naked_int32, TEE.empty
      end
    | Naked_number (ty1, Naked_int64), Naked_number (ty2, Naked_int64) ->
      begin match Naked_int64.meet_or_join_ty ?bound_name env ty1 ty2 with
      | Ok (ty, env_extension) ->
        Naked_number (ty, Naked_int64), env_extension
      | Bottom -> Flambda_type0_core.bottom K.naked_int64, TEE.empty
      end
    | Naked_number (ty1, Naked_nativeint),
        Naked_number (ty2, Naked_nativeint) ->
      begin match Naked_nativeint.meet_or_join_ty ?bound_name env ty1 ty2 with
      | Ok (ty, env_extension) ->
        Naked_number (ty, Naked_nativeint), env_extension
      | Bottom -> Flambda_type0_core.bottom K.naked_nativeint, TEE.empty
      end
    | Fabricated ty1, Fabricated ty2 ->
      begin match Fabricated.meet_or_join_ty ?bound_name env ty1 ty2 with
      | Ok (ty, env_extension) -> Fabricated ty, env_extension
      | Bottom -> Flambda_type0_core.bottom K.fabricated, TEE.empty
      end
    | (Value _ | Naked_number _ | Fabricated _), _ ->
      Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
        (E.name ())
        Type_printers.print t1
        Type_printers.print t2
end
