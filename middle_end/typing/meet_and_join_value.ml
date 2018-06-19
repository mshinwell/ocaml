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
    (T : Flambda_type0_internal_intf.S)
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
    (Typing_env : Typing_env_intf.S with module T := T)
    (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
    (Join_env : Join_env_intf.S with module T := T)
    (Parameters : Parameters_intf.S with module T := T)
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

    let meet_or_join_immediate_case env perm1 perm2
          ({ env_extension = env_extension1; } : immediate_case)
          ({ env_extension = env_extension2; } : immediate_case)
          : immediate_case =
      let env_extension1 = TEE.apply_name_permutation env_extension1 perm1 in
      let env_extension2 = TEE.apply_name_permutation env_extension2 perm2 in
      let env_extension =
        E.switch' TEE.meet TEE.join env env_extension1 env_extension2
      in
      { env_extension; }

    let meet_or_join_immediates env perm1 perm2 immediates1 immediates2
          : _ Or_bottom.t =
      let immediates =
        E.Immediate.Map.union_or_inter (fun _imm imm_case1 imm_case2 ->
            Some (meet_or_join_immediate_case env perm1 perm2
              imm_case1 imm_case2))
          immediates1
          immediates2
      in
      if Immediate.Map.is_empty immediates then Bottom
      else Ok immediates

    let meet_or_join_blocks env perm1 perm2
          ({ known_tags_and_sizes = known_tags_and_sizes1;
             size_at_least_n = size_at_least_n1;
           } : blocks)
          ({ known_tags_and_sizes = known_tags_and_sizes2;
             size_at_least_n = size_at_least_n2;
           } : blocks) : blocks =
      let one_side_only params1 perm1
            size_at_least_n2 ~get_equations_to_deposit1 =
        let size1 = Parameters.size params1 in
        let from_size_at_least_n2 =
          Targetint.OCaml.Map.find_last_opt
            (fun size -> Targetint.OCaml.(<=) size size1)
            size_at_least_n2
        in
        begin match from_size_at_least_n2 with
        | None ->
          begin match E.op with
          | Meet -> None
          | Join ->
            let params1 =
              Parameters.add_or_meet_equations
                (Parameters.apply_name_permutation params1 perm1)
                (JE.central_environment env)
                (get_equations_to_deposit1 env)
            in
            Some params1
          end
        | Some (size2, from_size_at_least_n2) ->
          assert (Targetint.OCaml.(<=) size2 size1);
          let params1 = Parameters.apply_name_permutation params1 perm1 in
          Some (Parameters.join env params1 from_size_at_least_n2)
        end
      in
      let merge size params1 params2 =
        match params1, params2 with
        | Some params1, None ->
          assert (size = Parameters.size params1);
          one_side_only params1 perm1 size_at_least_n2
            ~get_equations_to_deposit1:JE.holds_on_left
        | None, Some params2 ->
          assert (size = Parameters.size params2);
          one_side_only params2 perm2 size_at_least_n1
            ~get_equations_to_deposit1:JE.holds_on_right
        | Some params1, Some params2 ->
          assert (size = Parameters.size params1);
          assert (size = Parameters.size params2);
          let params1 = Parameters.apply_name_permutation params1 perm1 in
          let params2 = Parameters.apply_name_permutation params2 perm2 in
          Some (E.switch' Parameters.meet_fresh Parameters.join_fresh
            env params1 params2)
        | None, None -> None
      in
      let known_tags_and_sizes =
        Tag_and_size.Map.merge (fun tag_and_size params1 params2 ->
            let size = Tag_and_size.size tag_and_size in
            merge size params1 params2)
          known_tags_and_sizes1
          known_tags_and_sizes2
      in
      let size_at_least_n =
        Targetint.OCaml.Map.merge (fun size params1 params2 ->
            merge size params1 params2)
          size_at_least_n1
          size_at_least_n2
      in
      { known_tags_and_sizes;
        size_at_least_n;
      }

    let meet_or_join_blocks_and_tagged_immediates env perm1 perm2
          { blocks = blocks1; immediates = imms1; }
          { blocks = blocks2; immediates = imms2; }
          : (blocks_and_tagged_immediates * env_extension) Or_absorbing.t =
      let (blocks : _ Or_unknown.t), equations =
        match blocks1, blocks2 with
        | Unknown, _ when E.unknown_is_identity ->
          blocks2, TEE.empty
        | _, Unknown when E.unknown_is_identity ->
          blocks1, TEE.empty
        | Unknown, _ ->
          assert E.unknown_is_absorbing;
          Or_unknown.Unknown, TEE.empty
        | _, Unknown ->
          assert E.unknown_is_absorbing;
          Or_unknown.Unknown, TEE.empty
        | Known blocks1, Known blocks2 ->
          let blocks =
            meet_or_join_blocks env perm1 perm2 blocks1 blocks2
          in
          Or_unknown.Known blocks, TEE.empty
      in
      let immediates : _ Or_unknown.t =
        match imms1, imms2 with
        | Unknown, _ when E.unknown_is_identity -> imms2
        | _, Unknown when E.unknown_is_identity -> imms1
        | Unknown, _ ->
          assert E.unknown_is_absorbing;
          Unknown
        | _, Unknown ->
          assert E.unknown_is_absorbing;
          Unknown
        | Known imms1, Known imms2 ->
          match meet_or_join_immediates env perm1 perm2 imms1 imms2 with
          | Bottom -> Known Immediate.Map.empty
          | Ok immediates -> Known immediates
      in
      let is_bottom =
        begin match blocks with
        | Known { known_tags_and_sizes; size_at_least_n; }
            when Tag_and_size.Map.is_empty known_tags_and_sizes
              && Targetint.OCaml.Map.is_empty size_at_least_n -> true
        | Known _ | Unknown -> false
        end
          && begin match immediates with
             | Known imms when Immediate.Map.is_empty imms -> true
             | Known _ | Unknown -> false
             end
      in
      if is_bottom then Absorbing
      else
        let equations, blocks =
          match immediates with
          | Unknown -> equations, blocks
          | Known imms ->
            if not (Immediate.Map.is_empty imms) then
              equations, blocks
            else
              match blocks with
              | Unknown -> equations, blocks
              | Known { known_tags_and_sizes; _ } ->
                match Tag_and_size.Map.get_singleton known_tags_and_sizes with
                | None -> equations, blocks
                | Some (_tag_and_size, parameters) ->
                  let env_extension =
                    Parameters.standalone_extension parameters
                  in
                  let equations =
                    TEE.meet (JE.central_environment env)
                      env_extension equations
                  in
                  equations, blocks
        in
        Ok ({ blocks; immediates; }, equations)

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
        | Absorbing -> Absorbing
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
