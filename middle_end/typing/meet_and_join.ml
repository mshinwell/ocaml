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
module Flambda_types = struct type t end
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
    (E : Either_meet_or_join_intf.S) =
  struct
    let meet_or_join env t1 t2 : Flambda_types.t * Typing_env_extension.t =
      if Join_env.shortcut_precondition env
        && Type_equality.fast_equal t1 t2
      then t1, Typing_env_extension.empty
      else begin
        Join_env.invariant env;
        let descr, env_extension =
          match t1, t2 with
          | Value ty_value1, Value ty_value2 ->
            let ty_value, env_extension =
              Meet_and_join_value.meet_or_join_ty env ty_value1 ty_value2
            in
            if ty_value == ty_value1 then t1, env_extension
            else if ty_value == ty_value2 then t2, env_extension
            else Value ty_value, env_extension
          | Naked_number (ty_naked_number1, kind1),
              Naked_number (ty_naked_number2, kind2) ->
            let module N = K.Naked_number in
            begin match kind1, kind2 with
            | N.Naked_immediate, N.Naked_immediate ->
              let ty_naked_number, env_extension =
                Meet_and_join_naked_immediate.meet_or_join_ty env
                  ty_naked_number1 ty_naked_number2
              in
              (* CR mshinwell: add phys-equal checks *)
              Naked_number (ty_naked_number, N.Naked_immediate), env_extension
            | N.Naked_float, N.Naked_float ->
              let ty_naked_number, env_extension =
                Meet_and_join_naked_float.meet_or_join_ty env
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_float), env_extension
            | N.Naked_int32, N.Naked_int32 ->
              let ty_naked_number, env_extension =
                Meet_and_join_naked_int32.meet_or_join_ty env
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_int32), env_extension
            | N.Naked_int64, N.Naked_int64 ->
              let ty_naked_number, env_extension =
                Meet_and_join_naked_int64.meet_or_join_ty env
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_int64), env_extension
            | N.Naked_nativeint, N.Naked_nativeint ->
              let ty_naked_number, env_extension =
                Meet_and_join_naked_nativeint.meet_or_join_ty env
                  ty_naked_number1 ty_naked_number2
              in
              Naked_number (ty_naked_number, N.Naked_nativeint), env_extension
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
              Fabricated ty_fabricated, env_extension
          | (Value _ | Naked_number _ | Fabricated _), _ ->
            Misc.fatal_errorf "Kind mismatch upon %s:@ %a@ versus@ %a"
              E.name
              Type_printers.print t1
              Type_printers.print t2
        in
        let t =
          if t1 == descr then t1
          else if t2 == descr then t2
          else {
            descr;
          }
        in
        t, env_extension
      end

    module Flambda_types = W.Flambda_types
    module Join_env = W.Join_env
    module Meet_env = W.Meet_env
    module Meet_and_join_value = W.Meet_and_join_value
    module Meet_and_join_naked_immediate = W.Meet_and_join_naked_immediate
    module Meet_and_join_naked_int32 = W.Meet_and_join_naked_int32
    module Meet_and_join_naked_int64 = W.Meet_and_join_naked_int64
    module Meet_and_join_naked_nativeint = W.Meet_and_join_naked_nativeint
    module Meet_and_join_fabricated = W.Meet_and_join_fabricated
    module Type_equality = W.Type_equality
    module Type_printers = W.Type_printers
    module Typing_env = W.Typing_env
    module Typing_env_extension = W.Typing_env_extension
  end

  module Flambda_types = W.Flambda_types
  module Join_env = W.Join_env
  module Meet_env = W.Meet_env
  module Meet_and_join_value = W.Meet_and_join_value
  module Meet_and_join_naked_immediate = W.Meet_and_join_naked_immediate
  module Meet_and_join_naked_int32 = W.Meet_and_join_naked_int32
  module Meet_and_join_naked_int64 = W.Meet_and_join_naked_int64
  module Meet_and_join_naked_nativeint = W.Meet_and_join_naked_nativeint
  module Meet_and_join_fabricated = W.Meet_and_join_fabricated
  module Type_equality = W.Type_equality
  module Type_printers = W.Type_printers
  module Typing_env = W.Typing_env
  module Typing_env_extension = W.Typing_env_extension
end
