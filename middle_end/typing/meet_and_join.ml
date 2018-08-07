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

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Flambda_types = struct end
module Join_env = struct end
module Meet_env = struct end
module Meet_and_join_value = struct end
module Meet_and_join_naked_immediate = struct end
module Meet_and_join_naked_int32 = struct end
module Meet_and_join_naked_int64 = struct end
module Meet_and_join_naked_nativeint = struct end
module Meet_and_join_fabricated = struct end
module Type_equality = struct end
module Type_printers = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  module Make
    (E : Either_meet_or_join_intf.S
      with module Join_env := W.Join_env
      with module Meet_env := W.Meet_env
      with module Typing_env := W.Typing_env
      with module Typing_env_extension := W.Typing_env_extension) =
  struct
    let meet_or_join env (t1 : Flambda_types.t) (t2 : Flambda_types.t)
          : Flambda_types.t * Typing_env_extension.t =
      let module Meet_and_join_of_kind_value =
        Meet_and_join_value.Make (E)
      in
      let module Meet_and_join_of_kind_naked_immediate =
        Meet_and_join_naked_immediate.Make (E)
      in
      let module Meet_and_join_of_kind_naked_float =
        Meet_and_join_naked_float.Make (E)
      in
      let module Meet_and_join_of_kind_naked_int32 =
        Meet_and_join_naked_int32.Make (E)
      in
      let module Meet_and_join_of_kind_naked_int64 =
        Meet_and_join_naked_int64.Make (E)
      in
      let module Meet_and_join_of_kind_naked_nativeint =
        Meet_and_join_naked_nativeint.Make (E)
      in
      let module Meet_and_join_of_kind_fabricated =
        Meet_and_join_fabricated.Make (E)
      in
      let module Meet_and_join_value =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_value)
      in
      let module Meet_and_join_naked_immediate =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_immediate)
      in
      let module Meet_and_join_naked_float =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_float)
      in
      let module Meet_and_join_naked_int32 =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_int32)
      in
      let module Meet_and_join_naked_int64 =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_int64)
      in
      let module Meet_and_join_naked_nativeint =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_naked_nativeint)
      in
      let module Meet_and_join_fabricated =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_fabricated)
      in
      if Join_env.shortcut_precondition env
        && Type_equality.fast_equal t1 t2
      then t1, Typing_env_extension.empty
      else begin
        Join_env.invariant env;
        let t, env_extension =
          match t1, t2 with
          | Value ty_value1, Value ty_value2 ->
            let ty_value, env_extension =
              Meet_and_join_value.meet_or_join_ty env ty_value1 ty_value2
            in
            if ty_value == ty_value1 then t1, env_extension
            else if ty_value == ty_value2 then t2, env_extension
            else Flambda_types.Value ty_value, env_extension
          | Naked_number (ty_naked_number1, kind1),
              Naked_number (ty_naked_number2, kind2) ->
            let module N = K.Naked_number in
            begin match kind1, kind2 with
            | N.Naked_immediate, N.Naked_immediate ->
              let ty_naked_number, env_extension =
                Meet_and_join_naked_immediate.meet_or_join_ty env
                  ty_naked_number1 ty_naked_number2
              in
              if ty_naked_number == ty_naked_number1 then t1, env_extension
              else if ty_naked_number == ty_naked_number2 then t2, env_extension
              else
                Flambda_types.Naked_number (ty_naked_number, N.Naked_immediate),
                  env_extension
            | N.Naked_float, N.Naked_float ->
              let ty_naked_number, env_extension =
                Meet_and_join_naked_float.meet_or_join_ty env
                  ty_naked_number1 ty_naked_number2
              in
              if ty_naked_number == ty_naked_number1 then t1, env_extension
              else if ty_naked_number == ty_naked_number2 then t2, env_extension
              else
                Flambda_types.Naked_number (ty_naked_number, N.Naked_float),
                  env_extension
            | N.Naked_int32, N.Naked_int32 ->
              let ty_naked_number, env_extension =
                Meet_and_join_naked_int32.meet_or_join_ty env
                  ty_naked_number1 ty_naked_number2
              in
              if ty_naked_number == ty_naked_number1 then t1, env_extension
              else if ty_naked_number == ty_naked_number2 then t2, env_extension
              else
                Flambda_types.Naked_number (ty_naked_number, N.Naked_int32),
                  env_extension
            | N.Naked_int64, N.Naked_int64 ->
              let ty_naked_number, env_extension =
                Meet_and_join_naked_int64.meet_or_join_ty env
                  ty_naked_number1 ty_naked_number2
              in
              if ty_naked_number == ty_naked_number1 then t1, env_extension
              else if ty_naked_number == ty_naked_number2 then t2, env_extension
              else
                Flambda_types.Naked_number (ty_naked_number, N.Naked_int64),
                  env_extension
            | N.Naked_nativeint, N.Naked_nativeint ->
              let ty_naked_number, env_extension =
                Meet_and_join_naked_nativeint.meet_or_join_ty env
                  ty_naked_number1 ty_naked_number2
              in
              if ty_naked_number == ty_naked_number1 then t1, env_extension
              else if ty_naked_number == ty_naked_number2 then t2, env_extension
              else
                Flambda_types.Naked_number (ty_naked_number, N.Naked_nativeint),
                  env_extension
            | _, _ ->
              Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
                E.name
                Type_printers.print t1
                Type_printers.print t2
            end
          | Fabricated ty_fabricated1, Fabricated ty_fabricated2 ->
            let ty_fabricated, env_extension =
              Meet_and_join_fabricated.meet_or_join_ty env
                ty_fabricated1 ty_fabricated2
            in
            if ty_fabricated == ty_fabricated1 then
              t1, env_extension
            else if ty_fabricated == ty_fabricated2 then
              t2, env_extension
            else
              Flambda_types.Fabricated ty_fabricated, env_extension
          | (Value _ | Naked_number _ | Fabricated _), _ ->
            Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
              E.name
              Type_printers.print t1
              Type_printers.print t2
        in
        t, env_extension
      end

    module Flambda_types = W.Flambda_types
    module Join_env = W.Join_env
    module Meet_env = W.Meet_env
    module Typing_env = W.Typing_env
    module Typing_env_extension = W.Typing_env_extension
  end

  module Flambda_types = W.Flambda_types
  module Join_env = W.Join_env
  module Meet_env = W.Meet_env
  module Typing_env = W.Typing_env
  module Typing_env_extension = W.Typing_env_extension
end
