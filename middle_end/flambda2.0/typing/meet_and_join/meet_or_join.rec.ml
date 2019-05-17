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

module Make
  (E : Either_meet_or_join_intf.S
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension) =
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
      let ty, env_extension = Value.meet_or_join_ty ?bound_name env ty1 ty2 in
      Value ty, env_extension
    | Naked_number (ty1, Naked_immediate),
        Naked_number (ty2, Naked_immediate) ->
      let ty, env_extension =
        Naked_immediate.meet_or_join_ty ?bound_name env ty1 ty2
      in
      Naked_number (ty, Naked_immediate), env_extension
    | Naked_number (ty1, Naked_float), Naked_number (ty2, Naked_float) ->
      let ty, env_extension =
        Naked_float.meet_or_join_ty ?bound_name env ty1 ty2
      in
      Naked_number (ty, Naked_float), env_extension
    | Naked_number (ty1, Naked_int32), Naked_number (ty2, Naked_int32) ->
      let ty, env_extension =
        Naked_int32.meet_or_join_ty ?bound_name env ty1 ty2
      in
      Naked_number (ty, Naked_int32), env_extension
    | Naked_number (ty1, Naked_int64), Naked_number (ty2, Naked_int64) ->
      let ty, env_extension =
        Naked_int64.meet_or_join_ty ?bound_name env ty1 ty2
      in
      Naked_number (ty, Naked_int64), env_extension
    | Naked_number (ty1, Naked_nativeint),
        Naked_number (ty2, Naked_nativeint) ->
      let ty, env_extension =
        Naked_nativeint.meet_or_join_ty ?bound_name env ty1 ty2
      in
      Naked_number (ty, Naked_nativeint), env_extension
    | Fabricated ty1, Fabricated ty2 ->
      let ty, env_extension =
        Fabricated.meet_or_join_ty ?bound_name env ty1 ty2
      in
      Fabricated ty, env_extension
    | (Value _ | Naked_number _ | Fabricated _), _ ->
      Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
        (E.name ())
        Type_printers.print t1
        Type_printers.print t2
end
