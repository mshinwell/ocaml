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

module Make (T : sig
  include Flambda_type0_internal_intf.S

  val print_ty_value
     : Format.formatter
    -> ty_value
    -> unit

  val ty_is_obviously_bottom : 'a ty -> bool

  val force_to_kind_value : t -> of_kind_value ty
end) (Make_meet_and_join : functor
    (S : sig
      include Meet_and_join_spec_intf.S
        with type flambda_type := T.flambda_type
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
     end)
    -> sig
       include Meet_and_join_intf.S
         with type of_kind_foo := S.of_kind_foo
         with type typing_environment := T.typing_environment
         with type env_extension := T.env_extension
         with type 'a ty := 'a T.ty
    end) (Meet_and_join_naked_immediate : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := Immediate.Set.t T.of_kind_naked_number
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
    end) (Meet_and_join_naked_float : sig
      include Meet_and_join_intf.S
        with type of_kind_foo :=
          Numbers.Float_by_bit_pattern.Set.t T.of_kind_naked_number
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
    end) (Meet_and_join_naked_int32 : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := Numbers.Int32.Set.t T.of_kind_naked_number
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
    end) (Meet_and_join_naked_int64 : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := Numbers.Int64.Set.t T.of_kind_naked_number
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
    end) (Meet_and_join_naked_nativeint : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := Targetint.Set.t T.of_kind_naked_number
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
    end) (Meet_and_join_fabricated : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := T.of_kind_fabricated
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type 'a ty := 'a T.ty
    end) (Meet_and_join : sig
      include Meet_and_join_intf.S_for_types
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type flambda_type := T.flambda_type
    end) (Typing_env : sig
      include Typing_env_intf.S
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type flambda_type := T.flambda_type
        with type t_in_context := T.t_in_context
        with type 'a ty := 'a T.ty
        with type 'a unknown_or_join := 'a T.unknown_or_join
    end) (Typing_env_extension : sig
      include Typing_env_extension_intf.S
        with type env_extension := T.env_extension
        with type typing_environment := T.typing_environment
        with type flambda_type := T.flambda_type
    end) (Join_env : sig
      include Join_env_intf.S
        with type env_extension := T.env_extension
        with type typing_environment := T.typing_environment
        with type join_env := T.join_env
    end) (E : sig
      include Either_meet_or_join_intf.S
        with type typing_environment := T.typing_environment
        with type env_extension := T.env_extension
        with type flambda_type := T.flambda_type
    end) =
struct
  module rec Meet_and_join_value : sig
    include Meet_and_join_intf.S
      with type of_kind_foo := T.of_kind_value
      with type typing_environment := T.typing_environment
      with type env_extension := T.env_extension
      with type 'a ty := 'a T.ty
  end = Make_meet_and_join (struct
    open T

    type env_extension = T.env_extension

    type of_kind_foo = of_kind_value

    let kind = K.value ()

    let to_type ty : t = { descr = Value ty; phantom = None; }
    let force_to_kind = force_to_kind_value
    let print_ty = print_ty_value

    let meet_or_join_immediate_case meet_or_join_env
          ({ env_extension = env_extension1; } : immediate_case)
          ({ env_extension = env_extension2; } : immediate_case)
          : immediate_case =
      let env_extension =
        E.Typing_env_extension.meet_or_join meet_or_join_env
          env_extension1 env_extension2
      in
      { env_extension; }

    let meet_or_join_immediates meet_or_join_env immediates1 immediates2
          : _ Or_bottom.t =
      let immediates =
        E.Immediate.Map.union_or_inter_merge (fun imm1 imm2 ->
            meet_or_join_immediate_case meet_or_join_env imm1 imm2)
          immediates1
          immediates2
      in
      if Immediate.Map.is_empty immediates then Bottom
      else Ok immediates

    let meet_or_join_singleton_block meet_or_join_env
          ({ env_extension = env_extension1;
             fields = fields1;
           } : singleton_block)
          ({ env_extension = env_extension2;
             fields = fields2;
           } : singleton_block)
          : singleton_block * env_extension =
      let env_extension =
        E.Typing_env_extension.meet_or_join meet_or_join_env
          env_extension1 env_extension2
      in
      let meet_or_join_env =
        Meet_or_join_env.add_extensions meet_or_join_env
          ~holds_for_meet_or_join:env_extension
          ~holds_on_left:env_extension1
          ~holds_on_right:env_extension2
      in
      assert (Array.length fields1 = Array.length fields2);
      let equations = ref Typing_env_extension.empty in
      let fields =
        Array.map2
          (fun (field1 : _ mutable_or_immutable)
               (field2 : _ mutable_or_immutable) : _ mutable_or_immutable ->
            match field1, field2 with
            | Mutable, _ | _, Mutable -> Mutable
            | Immutable field1, Immutable field2 ->
              let field, equations =
                E.Flambda_type.meet_or_join meet_or_join_env field1 field2
              in
              equations :=
                E.Typing_env_extension.meet_or_join meet_or_join_env
                  equations !equations;
              Immutable field)
          fields1
          fields2
      in
      { env_extension;
        fields;
      }, !equations

    let meet_or_join_block_cases meet_or_join_env
          ((Blocks { by_length = by_length1; }) : block_cases)
          ((Blocks { by_length = by_length2; }) : block_cases)
          : (block_cases * env_extension) E.Or_bottom.t =
      let equations = ref Typing_env_extension.empty in
      let by_length =
        E.Targetint.OCaml.Map.union_or_inter_both
          ~in_left_only:(fun singleton_block : singleton_block ->
            let env_extension =
              Meet_or_join_env.holds_on_left meet_and_join_env
            in
            { singleton_block with env_extension; })
          ~in_right_only:(fun singleton_block : singleton_block ->
            let env_extension =
              Meet_or_join_env.holds_on_right meet_and_join_env
            in
            { singleton_block with env_extension; })
          ~in_both:(fun singleton_block1 singleton_block2 ->
            let singleton_block, new_equations =
              meet_or_join_singleton_block meet_or_join_env
                singleton_block1 singleton_block2
            in
            equations :=
              E.Typing_env_extension.meet_or_join meet_or_join_env
                new_equations !equations;
            singleton_block)
          by_length1
          by_length2
      in
      if Targetint.OCaml.Map.is_empty by_length then Bottom
      else Ok (((Blocks { by_length; }) : block_cases), !equations)

    let meet_or_join_blocks meet_or_join_env blocks1 blocks2
          : (blocks * env_extension) E.Or_bottom.t =
      let equations = ref Typing_env_extension.empty in
      let blocks =
        E.Tag.Map.union_or_inter_both
          ~in_left_only:(fun block_cases : block_cases ->
            let env_extension =
              Meet_or_join_env.holds_on_left meet_and_join_env
            in
            { block_cases with env_extension; })
          ~in_right_only:(fun block_cases : block_cases ->
            let env_extension =
              Meet_or_join_env.holds_on_right meet_and_join_env
            in
            { block_cases with env_extension; })
          ~in_both:(fun block_cases1 block_cases2 ->
            match meet_or_join_block_cases meet_or_join_env
              block_cases1 block_cases2
            with
            | Ok (block_cases, new_equations) ->
              equations :=
                E.Typing_env_extension.meet meet_or_join_env
                  new_equations !equations;
              Some block_cases
            | Bottom -> None)
          by_length1
          by_length2
      in
      if Tag.Map.is_empty blocks then Bottom
      else Ok (blocks, !equations)

    let meet_or_join_blocks_and_tagged_immediates meet_or_join_env
          { blocks = blocks1; immediates = imms1; }
          { blocks = blocks2; immediates = imms2; }
          : (blocks_and_tagged_immediates * env_extension) E.Or_bottom.t =
      let (blocks : _ Or_unknown.t), equations =
        match blocks1, blocks2 with
        | Unknown, _ when E.unknown_is_identity ->
          blocks2, Typing_env_extension.empty
        | _, Unknown when E.unknown_is_identity ->
          blocks1, Typing_env_extension.empty
        | Unknown, _ when E.unknown_is_bottom ->
          Unknown, Typing_env_extension.empty
        | _, Unknown when E.unknown_is_bottom ->
          Unknown, Typing_env_extension.empty
        | Known blocks1, Known blocks2 ->
          match meet_or_join_blocks meet_or_join_env blocks1 blocks2 with
          | Bottom ->
            Or_unknown.Known Tag.Map.empty, Typing_env_extension.empty
          | Ok (blocks, equations) ->
            Or_unknown.Known blocks, equations
      in
      let immediates : _ Or_unknown.t =
        match imms1, imms2 with
        | Unknown, _ when E.unknown_is_identity -> imms2
        | _, Unknown when E.unknown_is_identity -> imms1
        | Unknown, _ when E.unknown_is_bottom -> Unknown
        | _, Unknown when E.unknown_is_bottom -> Unknown
        | Known imms1, Known imms2 ->
          match meet_immediates env imms1 imms2 with
          | Bottom -> Known Immediate.Map.empty
          | Ok immediates -> Known immediates
      in
      let is_bottom =
        begin match blocks with
        | Known blocks when Tag.Map.is_empty blocks -> true
        | Known _ | Unknown -> false
        end
          && begin match immediates with
             | Known imms when Immediate.Map.is_empty imms -> true
             | Known _ | Unknown -> false
             end
      in
      (* CR mshinwell: Should we propagate up the meet of equations across all
         blocks, rather than only propagating upwards in the singleton
         case? *)
      if is_bottom then Bottom
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
              | Known blocks ->
                match Tag.Map.get_singleton blocks with
                | None -> equations, Or_unknown.Known blocks
                | Some (tag, Blocks { by_length; }) ->
                  match Targetint.OCaml.Map.get_singleton by_length with
                  | None -> equations, Or_unknown.Known blocks
                  | Some (length, singleton_block) ->
                    let equations =
                      Typing_env_extension.meet meet_or_join_env
                        singleton_block.env_extension equations
                    in
                    let singleton_block : singleton_block =
                      { singleton_block with
                        env_extension = Typing_env_extension.empty;
                      }
                    in
                    let by_length =
                      Targetint.OCaml.Map.singleton length singleton_block
                    in
                    let block_cases : block_cases = Blocks { by_length; } in
                    let blocks : _ Or_unknown.t =
                      Known (Tag.Map.singleton tag block_cases)
                    in
                    equations, blocks
        in
        Ok ({ blocks; immediates; }, equations)

    let meet_or_join_of_kind_foo meet_or_join_env
          (of_kind1 : of_kind_value) (of_kind2 : of_kind_value)
          : (of_kind_value * env_extension) E.Or_absorbing.t =
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          meet_or_join_blocks_and_tagged_immediates meet_or_join_env
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
          E.Meet_and_join_naked_float.meet_or_join_ty env n1 n2
        in
        Ok (Boxed_number (Boxed_float n), equations)
      | Boxed_number (Boxed_int32 n1),
          Boxed_number (Boxed_int32 n2) ->
        let (n : _ ty_naked_number), equations =
          E.Meet_and_join_naked_int32.meet_or_join_ty meet_or_join_env n1 n2
        in
        Ok (Boxed_number (Boxed_int32 n), equations)
      | Boxed_number (Boxed_int64 n1),
          Boxed_number (Boxed_int64 n2) ->
        let (n : _ ty_naked_number), equations =
          E.Meet_and_join_naked_int64.meet_or_join_ty meet_or_join_env n1 n2
        in
        Ok (Boxed_number (Boxed_int64 n), equations)
      | Boxed_number (Boxed_nativeint n1),
          Boxed_number (Boxed_nativeint n2) ->
        let (n : _ ty_naked_number), equations =
          E.Meet_and_join_naked_nativeint.meet_or_join_ty meet_or_join_env n1 n2
        in
        Ok (Boxed_number (Boxed_nativeint n), equations)
      | Closures closures1, Closures closures2 ->
        let equations = ref (Typing_env_extension.empty) in
        let closures =
          E.Closure_id.Map.union_or_inter
            (fun (closures_entry1 : closures_entry)
                 (closures_entry2 : closures_entry) : closures_entry option ->
              let set1 = closures_entry1.set_of_closures in
              let set2 = closures_entry2.set_of_closures in
              let set, new_equations =
                E.Meet_and_join_fabricated.meet_or_join_ty meet_or_join_env
                  set1 set2
              in
              if ty_is_obviously_bottom set then begin
                None
              end else begin
                equations :=
                  Typing_env_extension.meet_or_join meet_or_join_env
                    new_equations !equations;
                Some { set_of_closures = set; }
              end)
            closures1
            closures2
        in
        if Closure_id.Map.is_empty closures then Absorbing
        else Ok (Closures closures, !equations)
      | String strs1, String strs2 ->
        let strs = String_info.Set.inter strs1 strs2 in
        if String_info.Set.is_empty strs then Absorbing
        else Ok (String strs, Typing_env_extension.empty)
      | (Blocks_and_tagged_immediates _
          | Boxed_number _
          | Closures _
          | String _), _ ->
        Absorbing
  end)

  include Meet_and_join_value
end
