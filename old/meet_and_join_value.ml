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
module Flambda_type0_core = struct end
module Flambda_types = struct end
module Join_env = struct end
module Meet_env = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make (W : Typing_world.S) = struct
  open! W

  module Make
    (E : Either_meet_or_join_intf.S
      with module Join_env := Join_env
      with module Meet_env := Meet_env
      with module Typing_env := Typing_env
      with module Typing_env_extension := Typing_env_extension) =
  struct
    type of_kind_foo = Flambda_types.of_kind_value

    let kind = K.value ()

    let to_type ty : Flambda_types.t = Value ty

    let force_to_kind = Flambda_type0_core.force_to_kind_value
    let print_ty = Type_printers.print_ty_value_with_cache

    let meet_or_join_blocks_and_tagged_immediates env
          ({ blocks = blocks1; immediates = immediates1; }
            : Flambda_types.blocks_and_tagged_immediates)
          ({ blocks = blocks2; immediates = immediates2; }
            : Flambda_types.blocks_and_tagged_immediates)
          : (Flambda_types.blocks_and_tagged_immediates
              * Typing_env_extension.t) Or_bottom.t =
      let blocks =
        E.switch Blocks.meet Blocks.join env blocks1 blocks2
      in
      let immediates =
        E.switch Immediates.meet Immediates.join env immediates1 immediates2
      in
      match blocks, immediates with
      | Ok (blocks, env_extension1), Ok (immediates, env_extension2) ->
        let env_extension =
          Typing_env_extension.join env env_extension1 env_extension2
        in
        Ok ({ blocks; immediates; }, env_extension)
      | Bottom, _ | _, Bottom -> Bottom

    let meet_or_join_closures_entry env
          ({ function_decl = function_decl1;
             ty = ty1;
             closure_elements = closure_elements1;
             set_of_closures = set_of_closures1;
           } : Flambda_types.closures_entry)
          ({ function_decl = function_decl2;
             ty = ty2;
             closure_elements = closure_elements2;
             set_of_closures = set_of_closures2;
           } : Flambda_types.closures_entry)
          : (Flambda_types.closures_entry * Typing_env_extension.t)
              Or_absorbing.t =
      let function_decl : Flambda_types.function_declaration =
        match function_decl1, function_decl2 with
        | Non_inlinable, (Non_inlinable | Inlinable _)
        | Inlinable _, Non_inlinable -> Non_inlinable
        | Inlinable {
            closure_origin = closure_origin1;
            continuation_param = continuation_param1;
            exn_continuation_param = exn_continuation_param1;
            is_classic_mode = is_classic_mode1;
            params = params1;
            body = _;
            code_id = code_id1;
            free_names_in_body = free_names_in_body1;
            stub = stub1;
            result_arity = result_arity1;
            dbg = dbg1;
            inline = inline1;
            specialise = specialise1;
            is_a_functor = is_a_functor1;
            invariant_params = invariant_params1;
            size = size1;
            direct_call_surrogate = direct_call_surrogate1;
            my_closure = my_closure1;
          },
          Inlinable {
            closure_origin = closure_origin2;
            continuation_param = continuation_param2;
            exn_continuation_param = exn_continuation_param2;
            is_classic_mode = is_classic_mode2;
            params = params2;
            body = _;
            code_id = code_id2;
            free_names_in_body = free_names_in_body2;
            stub = stub2;
            result_arity = result_arity2;
            dbg = dbg2;
            inline = inline2;
            specialise = specialise2;
            is_a_functor = is_a_functor2;
            invariant_params = invariant_params2;
            size = size2;
            direct_call_surrogate = direct_call_surrogate2;
            my_closure = my_closure2;
          } ->
          match E.op with
          | Join ->
            if Code_id.equal code_id1 code_id2 then begin
              assert (Closure_origin.equal closure_origin1 closure_origin2);
              assert (Continuation.equal continuation_param1
                continuation_param2);
              assert (Continuation.equal exn_continuation_param1
                exn_continuation_param2);
              assert (Pervasives.(=) is_classic_mode1 is_classic_mode2);
              assert (Misc.Stdlib.List.equal Kinded_parameter.equal
                params1 params2);
              assert (Name_occurrences.equal free_names_in_body1
                free_names_in_body2);
              assert (Pervasives.(=) stub1 stub2);
              assert (Flambda_arity.equal result_arity1 result_arity2);
              assert (Debuginfo.equal dbg1 dbg2);
              assert (Inline_attribute.equal inline1 inline2);
              assert (Specialise_attribute.equal specialise1 specialise2);
              assert (Pervasives.(=) is_a_functor1 is_a_functor2);
              assert (Variable.Set.equal
                (Lazy.force invariant_params1)
                (Lazy.force invariant_params2));
              assert (Misc.Stdlib.Option.equal Pervasives.(=)
                (Lazy.force size1) (Lazy.force size2));
              assert (Misc.Stdlib.Option.equal Closure_id.equal
                direct_call_surrogate1 direct_call_surrogate2);
              assert (Variable.equal my_closure1 my_closure2);
              function_decl1
            end else begin
              Non_inlinable
            end
          | Meet ->
            (* We can arbitrarily pick one of the functions, since they must
               both behave in the same way, even if we cannot prove it. *)
            function_decl1
      in
      let ty =
        E.switch Function_type.meet_fresh Function_type.join_fresh env ty1 ty2
      in
      let closure_elements =
        E.switch Closure_elements.meet Closure_elements.join env
          closure_elements1 closure_elements2
      in
      let module Meet_and_join_of_kind_fabricated =
        Meet_and_join_fabricated.Make (E)
      in
      let module Meet_and_join_fabricated =
        Make_meet_or_join.Make (E) (Meet_and_join_of_kind_fabricated)
      in
      let (set_of_closures, env_extension1) =
        Meet_and_join_fabricated.meet_or_join_ty
          env set_of_closures1 set_of_closures2
      in
      match ty, closure_elements with
      | Ok (ty, env_extension2), Ok (closure_elements, env_extension3) ->
        let env_extension =
          let env = Join_env.central_environment env in
          Typing_env_extension.meet env env_extension1
            (Typing_env_extension.meet env env_extension2 env_extension3)
        in
        let closures_entry : Flambda_types.closures_entry =
          { function_decl;
            ty;
            closure_elements;
            set_of_closures;
          }
        in
        Ok (closures_entry, env_extension)
      | _, _ -> Absorbing

    let meet_or_join_of_kind_foo env
          (of_kind1 : Flambda_types.of_kind_value)
          (of_kind2 : Flambda_types.of_kind_value)
          : (Flambda_types.of_kind_value * Typing_env_extension.t)
              Or_absorbing.t =
      if Join_env.shortcut_precondition env
        && of_kind1 == of_kind2
      then
        Ok (of_kind1, Typing_env_extension.empty)
      else
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
        match of_kind1, of_kind2 with
        | Blocks_and_tagged_immediates blocks_imms1,
            Blocks_and_tagged_immediates blocks_imms2 ->
          let blocks_imms =
            meet_or_join_blocks_and_tagged_immediates env
              blocks_imms1 blocks_imms2
          in
          begin match blocks_imms with
          | Ok (blocks_imms, equations) ->
            Ok (Blocks_and_tagged_immediates blocks_imms, equations)
          | Bottom -> Absorbing
          end
        | Boxed_number (Boxed_float n1),
            Boxed_number (Boxed_float n2) ->
          let (n : _ Flambda_types.ty_naked_number), equations =
            Meet_and_join_naked_float.meet_or_join_ty env n1 n2
          in
          Ok (Boxed_number (Boxed_float n), equations)
        | Boxed_number (Boxed_int32 n1),
          Boxed_number (Boxed_int32 n2) ->
          let (n : _ Flambda_types.ty_naked_number), equations =
            Meet_and_join_naked_int32.meet_or_join_ty env n1 n2
          in
          Ok (Boxed_number (Boxed_int32 n), equations)
        | Boxed_number (Boxed_int64 n1),
            Boxed_number (Boxed_int64 n2) ->
          let (n : _ Flambda_types.ty_naked_number), equations =
            Meet_and_join_naked_int64.meet_or_join_ty env n1 n2
          in
          Ok (Boxed_number (Boxed_int64 n), equations)
        | Boxed_number (Boxed_nativeint n1),
            Boxed_number (Boxed_nativeint n2) ->
          let (n : _ Flambda_types.ty_naked_number), equations =
            Meet_and_join_naked_nativeint.meet_or_join_ty env
              n1 n2
          in
          Ok (Boxed_number (Boxed_nativeint n), equations)
        | Closures { by_closure_id = by_closure_id1; },
            Closures { by_closure_id = by_closure_id2; } ->
          let by_closure_id =
            E.switch Closures_entry_by_closure_id.meet
              Closures_entry_by_closure_id.join
              env by_closure_id1 by_closure_id2
          in
          begin match by_closure_id with
          | Ok (by_closure_id, env_extension) ->
            Ok (Closures { by_closure_id; }, env_extension)
          | Bottom -> Absorbing
          end
        | String strs1, String strs2 ->
          let strs = E.String_info.Set.union_or_inter strs1 strs2 in
          if String_info.Set.is_empty strs then Absorbing
          else Ok (String strs, Typing_env_extension.empty)
        | (Blocks_and_tagged_immediates _
            | Boxed_number _
            | Closures _
            | String _), _ ->
          Absorbing
  end
end
