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

module Make
  (E : Either_meet_or_join_intf.S
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension) =
struct
  type of_kind_foo = Float.Set.t Flambda_types.of_kind_naked_number

  let kind () = K.naked_float
  let to_type ty : Flambda_types.t = Naked_number (ty, Naked_float)
  let force_to_kind = Flambda_type0_core.force_to_kind_naked_float
  let print_ty = Type_printers.print_ty_naked_float_with_cache

  let meet_or_join_of_kind_foo _meet_or_join_env
        (of_kind1 : Float.Set.t Flambda_types.of_kind_naked_number)
        (of_kind2 : Float.Set.t Flambda_types.of_kind_naked_number)
        : (Float.Set.t Flambda_types.of_kind_naked_number
            * Typing_env_extension.t) Or_bottom_or_absorbing.t =
    match of_kind1, of_kind2 with
    | Float fs1, Float fs2 ->
      let fs = E.Float.Set.union_or_inter fs1 fs2 in
      if Float.Set.is_empty fs then Bottom
      else Ok (Float fs, Typing_env_extension.empty)
end
