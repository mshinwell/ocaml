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

module Make (E : Either_meet_or_join_intf.S) (T : Typing_world.S) =
  Make_meet_and_join (struct
    module JE = T.Join_env
    module TEE = T.Typing_env_extension

    type of_kind_foo = T.Flambda_type.of_kind_value

    let kind = K.value ()

    let to_type ty : t =
      { descr = Value ty;
      }

    let force_to_kind = T.Flambda_type.force_to_kind_value
    let print_ty = T.Flambda_type.print_ty_value

    let meet_or_join_blocks_and_tagged_immediates env perm1 perm2
          { blocks = blocks1; immediates = imms1; }
          { blocks = blocks2; immediates = imms2; }
          : (blocks_and_tagged_immediates * TEE.t) Or_bottom.t =
      let blocks, env_extension1 =
        E.switch T.Blocks.meet T.Blocks.join env blocks1 blocks2
      in
      let immediates, env_extension2 =
        E.switch T.Immediates.meet T.Immediates.join env immediates1 immediates2
      in
      match blocks, immediates with
      | Ok blocks, Ok immediates ->
        let env_extension_immediates =
          if not (T.Blocks.is_empty blocks) then TEE.empty
          else
            match T.Immediates.get_singleton immediates with
            | None -> TEE.empty
            | Some block_fields ->
              T.Immediates.RP.standalone_extension block_fields
        in
        let env_extension_blocks =
          if not (T.Immediates.is_empty immediates) then TEE.empty
          else
            match T.Blocks.get_singleton blocks with
            | None -> TEE.empty
            | Some immediates ->
              T.Blocks.RP.standalone_extension immediates
        in
        let env_extension =
          TEE.meet env (TEE.meet env env_extension1 env_extension2)
            (TEE.meet env env_extension_blocks env_extension_immediates)
        in
        Ok ({ blocks; immediates; }, env_extension)
      | Bottom, _ | _, Bottom -> Bottom

    let meet_or_join_closures_entry env perm1 perm2
          ({ function_decl = function_decl1;
             ty = ty1;
             closure_elements = closure_elements1;
             set_of_closures = set_of_closures1;
           } : closures_entry)
          ({ function_decl = function_decl2;
             ty = ty2;
             closure_elements = closure_elements2;
             set_of_closures = set_of_closures2;
           } : closures_entry) =
      let function_decl : T.function_declaration =
        match function_decl1, function_decl2 with
        | Non_inlinable, (Non_inlinable | Inlinable _)
        | (Non_inlinable | Inlinable _), Non_inlinable -> Non_inlinable
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
              Inlinable function_decl1
            end else begin
              Non_inlinable
            end
          | Meet ->
            (* We can arbitrarily pick one of the functions, since they must
               both behave in the same way, even if we cannot prove it. *)
            Inlinable function_decl1
      in
      let ty =
        E.switch T.Function_type.meet T.Function_type.join perm1 perm2 ty1 ty2
      in
      let closure_elements =
        E.switch T.Closure_elements.meet T.Closure_elements.join perm1 perm2
          closure_elements1 closure_elements2
      in
      let set_of_closures =
        E.switch T.Meet_or_join_of_kind_fabricated.meet_ty_fabricated
          T.Meet_or_join_of_kind_fabricated.join_ty_fabricated
          perm1 perm2 set_of_closures1 set_of_closures2
      in
      match ty, closure_elements, set_of_closures with
      | Ok ty, Ok closure_elements, Ok set_of_closures ->
        Ok {
          function_decl;
          ty;
          closure_elements;
          set_of_closures;
        }
      | _, _, _, _ -> Absorbing

    let meet_or_join_of_kind_foo env perm1 perm2
          (of_kind1 : of_kind_value) (of_kind2 : of_kind_value)
          : (of_kind_value * env_extension) Or_absorbing.t =
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          meet_or_join_blocks_and_tagged_immediates env perm1 perm2
            blocks_imms1 blocks_imms2
        in
        begin match blocks_imms with
        | Ok (blocks_imms, equations) ->
          Ok (Blocks_and_tagged_immediates blocks_imms, equations)
        | Bottom -> Absorbing
        end
      | Boxed_number (Boxed_float n1),
          Boxed_number (Boxed_float n2) ->
        let (n : _ ty_naked_number), equations =
          T.Meet_and_join_naked_float.meet_or_join_ty env perm1 perm2 n1 n2
        in
        Ok (Boxed_number (Boxed_float n), equations)
      | Boxed_number (Boxed_int32 n1),
        Boxed_number (Boxed_int32 n2) ->
        let (n : _ ty_naked_number), equations =
          T.Meet_and_join_naked_int32.meet_or_join_ty env perm1 perm2 n1 n2
        in
        Ok (Boxed_number (Boxed_int32 n), equations)
      | Boxed_number (Boxed_int64 n1),
          Boxed_number (Boxed_int64 n2) ->
        let (n : _ ty_naked_number), equations =
          T.Meet_and_join_naked_int64.meet_or_join_ty env perm1 perm2 n1 n2
        in
        Ok (Boxed_number (Boxed_int64 n), equations)
      | Boxed_number (Boxed_nativeint n1),
          Boxed_number (Boxed_nativeint n2) ->
        let (n : _ ty_naked_number), equations =
          T.Meet_and_join_naked_nativeint.meet_or_join_ty env perm1 perm2 n1 n2
        in
        Ok (Boxed_number (Boxed_nativeint n), equations)
      | Closures closures1, Closures closures2 ->
        let closures =
          E.switch T.Closures_entry_by_closure_id.meet
            T.Closures_entry_by_closure_id.join
            env perm1 perm2 closures1 closures2
        in
        begin match closures with
        | Ok closures -> Ok (Closures closures)
        | Bottom -> Absorbing
        end
      | String strs1, String strs2 ->
        let strs = String_info.Set.inter strs1 strs2 in
        if String_info.Set.is_empty strs then Absorbing
        else Ok (String strs, TEE.empty)
      | (Blocks_and_tagged_immediates _
          | Boxed_number _
          | Closures _
          | String _), _ ->
        Absorbing
  end)
