(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind

module Make (W : Typing_world.S)
    (E : Either_meet_or_join_intf.S with module W := W) =
struct
  let meet_or_join env t1 t2 : t * env_extension =
    if Join_env.fast_check_extensions_same_both_sides env
      && Type_equality.fast_equal t1 t2
    then t1, Typing_env_extension.empty
    else begin
      Join_env.invariant env;
      let descr, equations =
        match t1.descr, t2.descr with
        | Value ty_value1, Value ty_value2 ->
          let ty_value, equations =
            Meet_and_join_value.meet_or_join_ty env perm1 perm2
              ty_value1 ty_value2
          in
          if ty_value == ty_value1 then t1.descr, equations
          else if ty_value == ty_value2 then t2.descr, equations
          else Value ty_value, equations
        | Naked_number (ty_naked_number1, kind1),
            Naked_number (ty_naked_number2, kind2) ->
          let module N = K.Naked_number in
          begin match kind1, kind2 with
          | N.Naked_immediate, N.Naked_immediate ->
            let ty_naked_number, equations =
              Meet_and_join_naked_immediate.meet_or_join_ty env perm1 perm2
                ty_naked_number1 ty_naked_number2
            in
            Naked_number (ty_naked_number, N.Naked_immediate), equations
          | N.Naked_float, N.Naked_float ->
            let ty_naked_number, equations =
              Meet_and_join_naked_float.meet_or_join_ty env perm1 perm2
                ty_naked_number1 ty_naked_number2
            in
            Naked_number (ty_naked_number, N.Naked_float), equations
          | N.Naked_int32, N.Naked_int32 ->
            let ty_naked_number, equations =
              Meet_and_join_naked_int32.meet_or_join_ty env perm1 perm2
                ty_naked_number1 ty_naked_number2
            in
            Naked_number (ty_naked_number, N.Naked_int32), equations
          | N.Naked_int64, N.Naked_int64 ->
            let ty_naked_number, equations =
              Meet_and_join_naked_int64.meet_or_join_ty env perm1 perm2
                ty_naked_number1 ty_naked_number2
            in
            Naked_number (ty_naked_number, N.Naked_int64), equations
          | N.Naked_nativeint, N.Naked_nativeint ->
            let ty_naked_number, equations =
              Meet_and_join_naked_nativeint.meet_or_join_ty env perm1 perm2
                ty_naked_number1 ty_naked_number2
            in
            Naked_number (ty_naked_number, N.Naked_nativeint), equations
          | _, _ ->
            Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
              E.name
              print t1
              print t2
          end
        | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
          let ty_fabricated, equations =
            Meet_and_join_fabricated.meet_or_join_ty env perm1 perm2
              ty_fabricated1 ty_fabricated2
          in
          if ty_fabricated == ty_fabricated1 then
            t1.descr, equations
          else if ty_fabricated == ty_fabricated2 then
            t2.descr, equations
          else
            Fabricated ty_fabricated, equations
        | (Value _ | Naked_number _ | Fabricated _), _ ->
          Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
            E.name
            print t1
            print t2
      in
      let t =
        if t1.descr == descr then t1
        else if t2.descr == descr then t2
        else {
          descr;
        }
      in
      t, equations
    end
end
