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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module K = Flambda_kind

module Make
    (T : Typing_world.S)
    (Make_meet_and_join : functor
      (S : Meet_and_join_spec_intf.S with module T := T)
        -> Meet_and_join_intf.S
             with module T := T
             with type of_kind_foo = S.of_kind_foo)
    (Meet_and_join_naked_immediate : Meet_and_join_intf.S
      with module T := T
      with type of_kind_foo = T.of_kind_naked_immediate)
    (Meet_and_join_naked_float : Meet_and_join_intf.S
      with module T := T
      with type of_kind_foo = T.of_kind_naked_float)
    (Meet_and_join_naked_int32 : Meet_and_join_intf.S
      with module T := T
      with type of_kind_foo = T.of_kind_naked_int32)
    (Meet_and_join_naked_int64 : Meet_and_join_intf.S
      with module T := T
      with type of_kind_foo = T.of_kind_naked_int64)
    (Meet_and_join_naked_nativeint : Meet_and_join_intf.S
      with module T := T
      with type of_kind_foo = T.of_kind_naked_nativeint)
    (Meet_and_join_fabricated : Meet_and_join_intf.S
      with module T := T
      with type of_kind_foo = T.of_kind_fabricated)
    (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
    (E : Either_meet_or_join_intf.S with module T := T) =
struct
  module rec Meet_and_join_value : sig
    include Meet_and_join_intf.S
      with module T := T
      with type of_kind_foo = T.of_kind_value
  end = Make_meet_and_join (struct
    open T

    module JE = Join_env
    module TEE = Typing_env_extension

    type of_kind_foo = T.of_kind_value

    let kind = K.value ()

    let to_type ty : t =
      { descr = Value ty;
      }

    let force_to_kind = force_to_kind_value
    let print_ty = print_ty_value

    let meet_or_join_blocks_and_tagged_immediates env perm1 perm2
          { blocks = blocks1; immediates = imms1; }
          { blocks = blocks2; immediates = imms2; }
          : (blocks_and_tagged_immediates * TEE.t) Or_bottom.t =
      let blocks, env_extension1 =
        E.switch Blocks.meet Blocks.join env blocks1 blocks2
      in
      let immediates, env_extension2 =
        E.switch Immediates.meet Immediates.join env immediates1 immediates2
      in
      match blocks, immediates with
      | Ok blocks, Ok immediates ->
        let env_extension_immediates =
          if not (Blocks.is_empty blocks) then TEE.empty
          else
            match Immediates.get_singleton immediates with
            | None -> TEE.empty
            | Some block_fields ->
              Block_fields.standalone_extension block_fields
        in
        let env_extension_blocks =
          if not (Immediates.is_empty immediates) then TEE.empty
          else
            match Blocks.get_singleton blocks with
            | None -> TEE.empty
            | Some immediates ->
              Immediates.standalone_extension immediates
        in
        let env_extension =
          TEE.meet env (TEE.meet env env_extension1 env_extension2)
            (TEE.meet env env_extension_blocks env_extension_immediates)
        in
        Ok ({ blocks; immediates; }, env_extension)
      | Bottom, _ | _, Bottom -> Bottom

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
          Meet_and_join_naked_float.meet_or_join_ty env perm1 perm2 n1 n2
        in
        Ok (Boxed_number (Boxed_float n), equations)
      | Boxed_number (Boxed_int32 n1),
          Boxed_number (Boxed_int32 n2) ->
        let (n : _ ty_naked_number), equations =
          Meet_and_join_naked_int32.meet_or_join_ty env perm1 perm2 n1 n2
        in
        Ok (Boxed_number (Boxed_int32 n), equations)
      | Boxed_number (Boxed_int64 n1),
          Boxed_number (Boxed_int64 n2) ->
        let (n : _ ty_naked_number), equations =
          Meet_and_join_naked_int64.meet_or_join_ty env perm1 perm2 n1 n2
        in
        Ok (Boxed_number (Boxed_int64 n), equations)
      | Boxed_number (Boxed_nativeint n1),
          Boxed_number (Boxed_nativeint n2) ->
        let (n : _ ty_naked_number), equations =
          Meet_and_join_naked_nativeint.meet_or_join_ty env perm1 perm2 n1 n2
        in
        Ok (Boxed_number (Boxed_nativeint n), equations)
      | Closures closures1, Closures closures2 ->
        let equations = ref TEE.empty in
        let params1 =
          Parameters.apply_name_permutation closures1.ty.params perm1
        in
        let params2 =
          Parameters.apply_name_permutation closures2.ty.params perm2
        in
        let params =
          E.switch' Parameters.meet_fresh Parameters.join_fresh
            env params1 params2
        in
        let results1 =
          Parameters.apply_name_permutation closures1.ty.results perm1
        in
        let results2 =
          Parameters.apply_name_permutation closures2.ty.results perm2
        in
        let results =
          E.switch' Parameters.meet_fresh Parameters.join_fresh
            env results1 results2
        in
        let ty : T.dependent_function_type = { params; results; } in
        let by_closure_id =
          E.Closure_id.Map.union_or_inter
            (fun _closure_id
                 (closures_entry1 : closures_entry)
                 (closures_entry2 : closures_entry) : closures_entry option ->
              let set1 = closures_entry1.set_of_closures in
              let set2 = closures_entry2.set_of_closures in
              let set, new_equations =
                Meet_and_join_fabricated.meet_or_join_ty env perm1 perm2
                  set1 set2
              in
              if ty_is_obviously_bottom set then begin
                None
              end else begin
                equations :=
                  TEE.meet (JE.central_environment env)
                    new_equations !equations;
                Some { set_of_closures = set; }
              end)
            closures1.by_closure_id
            closures2.by_closure_id
        in
        if Closure_id.Map.is_empty by_closure_id then Absorbing
        else Ok (Closures { ty; by_closure_id; }, !equations)
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

  include Meet_and_join_value
end
