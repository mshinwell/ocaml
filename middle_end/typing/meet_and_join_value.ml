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
        with type t_in_context := T.t_in_context
        with type env_extension := T.env_extension
        with type flambda_type := T.flambda_type
    end) (Typing_env0 : sig
      include Typing_env0_intf.S
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

    let meet_immediate_case _env
          ({ env_extension = env_extension1; } : immediate_case)
          ({ env_extension = env_extension2; } : immediate_case)
          : immediate_case =
      let env_extension =
        Typing_env_extension.meet env env_extension1 env_extension2
      in
      { env_extension; }

    let join_immediate_case env env_extension1 env_extension2
          ({ env_extension = env_extension1'; } : immediate_case)
          ({ env_extension = env_extension2'; } : immediate_case)
          : immediate_case =
      let env_extension =
        Typing_env_extension.join env env_extension1 env_extension2
          env_extension1' env_extension2'
      in
      { env_extension; }

    let meet_immediates env immediates1 immediates2 : _ Or_bottom.t =
      let immediates =
        Immediate.Map.inter_merge (fun imm1 imm2 ->
            meet_immediate_case env imm1 imm2)
          immediates1
          immediates2
      in
      if Immediate.Map.is_empty immediates then Bottom
      else Ok immediates

    let join_immediates env env_extension1 env_extension2
          immediates1 immediates2 =
      Immediate.Map.union_merge (fun imm1 imm2 ->
          join_immediate_case env env_extension1 env_extension2 imm1 imm2)
        immediates1
        immediates2

    (* CR mshinwell: As for [join], [meet] should add env_extensions which
       it finds during traversal into the environment. *)

    let meet_singleton_block env
          ({ env_extension = env_extension1;
             fields = fields1;
           } : singleton_block)
          ({ env_extension = env_extension2;
             fields = fields2;
           } : singleton_block) : singleton_block * env_extension =
      let env_extension =
        Typing_env_extension.meet env env_extension1 env_extension2
      in
      assert (Array.length fields1 = Array.length fields2);
      let env_extension_from_meet = ref (Typing_env_extension.create ()) in
      let fields =
        let env = Typing_env0.add_env_extension env env_extension in
        Array.map2
          (fun (field1 : _ mutable_or_immutable)
               (field2 : _ mutable_or_immutable) : _ mutable_or_immutable ->
            match field1, field2 with
            | Mutable, _ | _, Mutable -> Mutable
            | Immutable field1, Immutable field2 ->
              let field, new_env_extension_from_meet =
                Meet_and_join.meet env field1 field2
              in
              env_extension_from_meet :=
                Typing_env_extension.meet env new_env_extension_from_meet
                  !env_extension_from_meet;
              Immutable field)
          fields1
          fields2
      in
      { env_extension;
        fields;
      }, !env_extension_from_meet

    let join_singleton_block env env_extension1 env_extension2
          ({ env_extension = env_extension1';
             fields = fields1;
           } : singleton_block)
          ({ env_extension = env_extension2';
             fields = fields2;
           } : singleton_block) : singleton_block =
      let env_extension =
        Meet_and_join.join_env_extension env env_extension1 env_extension2
          env_extension1' env_extension2'
      in
      assert (Array.length fields1 = Array.length fields2);
      let fields =
        let env = Typing_env0.add_env_extension env env_extension in
        Array.map2
          (fun (field1 : _ mutable_or_immutable)
               (field2 : _ mutable_or_immutable) : _ mutable_or_immutable ->
            match field1, field2 with
            | Mutable, _ | _, Mutable -> Mutable
            | Immutable field1, Immutable field2 ->
              Immutable (Meet_and_join.join env
                env_extension1 env_extension2 field1 field2)))
          fields1
          fields2
      in
      { env_extension;
        fields;
      }

    let meet_block_cases env
          ((Blocks { by_length = singleton_blocks1; }) : block_cases)
          ((Blocks { by_length = singleton_blocks2; }) : block_cases)
          : (block_cases * env_extension) Or_bottom.t =
      let env_extension_from_meet = ref (Typing_env_extension.create ()) in
      let by_length =
        Targetint.OCaml.Map.inter_merge
          (fun singleton_block1 singleton_block2 ->
            let singleton_block, new_env_extension_from_meet =
              meet_singleton_block env singleton_block1 singleton_block2
            in
            env_extension_from_meet :=
              Typing_env_extension.meet env
                new_env_extension_from_meet !env_extension_from_meet;
            singleton_block)
          singleton_blocks1
          singleton_blocks2
      in
      if Targetint.OCaml.Map.is_empty by_length then Bottom
      else
        Ok (((Blocks { by_length; }) : block_cases), !env_extension_from_meet)

    let add_env_extension_singleton_block env env_extension
          ({ env_extension = env_extension'; fields; }
            : singleton_block) : singleton_block =
      let env_extension =
        Typing_env_extension.meet env env_extension env_extension'
      in
      { env_extension; fields; }

    let join_block_cases env env_extension1 env_extension2
          ((Blocks { by_length = by_length1; }) : block_cases)
          ((Blocks { by_length = by_length2; }) : block_cases)
          : block_cases =
      let by_length =
        Targetint.OCaml.Map.union_merge
          (fun singleton_block1 singleton_block2 ->
            join_singleton_block env env_extension1 env_extension2
              singleton_block1 singleton_block2)
          by_length1
          by_length2
      in
      let by_length =
        Targetint.OCaml.Map.mapi (fun length singleton_block ->
            let in_blocks1 = Targetint.OCaml.Map.mem length by_length1 in
            let in_blocks2 = Targetint.OCaml.Map.mem length by_length2 in
            let only_in_blocks1 = in_blocks1 && (not in_blocks2) in
            let only_in_blocks2 = (not in_blocks1) && in_blocks2 in
            if only_in_blocks1 then
              add_env_extension_singleton_block env
                env_extension1 singleton_block
            else if only_in_blocks2 then
              add_env_extension_singleton_block env
                env_extension2 singleton_block
            else
              singleton_blocks)
          by_length
      in
      Blocks { by_length; }

    let meet_blocks env blocks1 blocks2 : _ Or_bottom.t =
      let env_extension_from_meet = ref (Typing_env_extension.create ()) in
      let blocks =
        Tag.Map.inter (fun block_cases1 block_cases2 ->
            match meet_block_cases env block_cases1 block_cases2 with
            | Ok (block_cases, new_env_extension_from_meet) ->
              env_extension_from_meet :=
                Typing_env_extension.meet env
                  new_env_extension_from_meet !env_extension_from_meet;
              Some block_cases
            | Bottom -> None)
          blocks1
          blocks2
      in
      if Tag.Map.is_empty blocks then Bottom
      else Ok (blocks, !env_extension_from_meet)

    let add_env_extension_block_cases env env_extension
          ((Blocks { by_length; }) : block_cases)
          : block_cases =
      let by_length =
        Targetint.OCaml.Map.map (fun singleton_blocks ->
            add_env_extension_singleton_block env env_extension
              singleton_blocks)
          by_length
      in
      Blocks { by_length; }

    let join_blocks env env_extension1 env_extension2 blocks1 blocks2 =
      let blocks =
        Tag.Map.union_merge (fun block_cases1 block_cases2 ->
            join_block_cases env env_extension1 env_extension2
              block_cases1 block_cases2)
          blocks1
          blocks2
      in
      Tag.Map.mapi (fun tag block_cases ->
          let in_blocks1 = Tag.Map.mem tag blocks1 in
          let in_blocks2 = Tag.Map.mem tag blocks2 in
          let only_in_blocks1 = in_blocks1 && (not in_blocks2) in
          let only_in_blocks2 = (not in_blocks1) && in_blocks2 in
          if only_in_blocks1 then
            add_env_extension_block_cases env env_extension1 block_cases
          else if only_in_blocks2 then
            add_env_extension_block_cases env env_extension2 block_cases
          else
            block_cases)
        blocks

    let meet_blocks_and_tagged_immediates env
          { blocks = blocks1; immediates = imms1; }
          { blocks = blocks2; immediates = imms2; }
          : (blocks_and_tagged_immediates * env_extension) Or_bottom.t =
      let (blocks : _ Or_unknown.t), env_extension_from_meet =
        match blocks1, blocks2 with
        | Unknown, _ -> blocks2, Typing_env_extension.create ()
        | _, Unknown -> blocks1, Typing_env_extension.create ()
        | Known blocks1, Known blocks2 ->
          match meet_blocks env blocks1 blocks2 with
          | Bottom ->
            Or_unknown.Known Tag.Map.empty, Typing_env_extension.create ()
          | Ok (blocks, env_extension_from_meet) ->
            Or_unknown.Known blocks, env_extension_from_meet
      in
      let immediates : _ Or_unknown.t =
        match imms1, imms2 with
        | Unknown, _ -> imms2
        | _, Unknown -> imms1
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
      (* CR mshinwell: If we end up with [Bottom], should that be signalled
         as a judgement? *)
      (* CR mshinwell: Should we propagate up the meet of equations across all
         blocks, rather than only propagating upwards in the singleton
         case? *)
      if is_bottom then Bottom
      else
        let env_extension_from_meet, blocks =
          match immediates with
          | Unknown -> env_extension_from_meet, blocks
          | Known imms ->
            if not (Immediate.Map.is_empty imms) then
              env_extension_from_meet, blocks
            else
              match blocks with
              | Unknown -> env_extension_from_meet, blocks
              | Known blocks ->
                match Tag.Map.get_singleton blocks with
                | None -> env_extension_from_meet, blocks
                | Some (tag, Blocks { by_length; }) ->
                  match Targetint.OCaml.Map.get_singleton by_length with
                  | None -> env_extension_from_meet, blocks
                  | Some (length, singleton_block) ->
                    let env_extension_from_meet =
                      Typing_env_extension.meet env
                        singleton_block.env_extension env_extension_from_meet
                    in
                    let singleton_block : singleton_block =
                      { singleton_block with
                        env_extension = Typing_env_extension.create ();
                      }
                    in
                    let by_length =
                      Targetint.OCaml.Map.singleton length singleton_block
                    in
                    let block_cases : block_cases = Blocks { by_length; } in
                    let blocks : _ Or_unknown.t =
                      Known (Tag.Map.singleton tag block_cases)
                    in
                    env_extension_from_meet, blocks
        in
        Ok ({ blocks; immediates; }, env_extension_from_meet)

    let join_blocks_and_tagged_immediates env env_extension1 env_extension2
          { blocks = blocks1; immediates = imms1; }
          { blocks = blocks2; immediates = imms2; }
          : blocks_and_tagged_immediates =
      let blocks : _ Or_unknown.t =
        match blocks1, blocks2 with
        | Unknown, _ | _, Unknown -> Unknown
        | Known blocks1, Known blocks2 ->
          Known (join_blocks env env_extension1 env_extension2 blocks1 blocks2)
      in
      let immediates : _ Or_unknown.t =
        match imms1, imms2 with
        | Unknown, _ | _, Unknown -> Unknown
        | Known imms1, Known imms2 ->
          Known (join_immediates env env_extension1 env_extension2 imms1 imms2)
      in
      { blocks; immediates; }

    let meet_of_kind_foo env
          (of_kind1 : of_kind_value) (of_kind2 : of_kind_value)
          : (of_kind_value * env_extension) Or_bottom.t =
      let resolver = env1.resolver in
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          meet_blocks_and_tagged_immediates env
            blocks_imms1 blocks_imms2
        in
        begin match blocks_imms with
        | Ok (blocks_imms, env_extension_from_meet) ->
          Ok (Blocks_and_tagged_immediates blocks_imms, env_extension_from_meet)
        | Bottom -> Bottom
        end
      | Boxed_number (Boxed_float n1),
          Boxed_number (Boxed_float n2) ->
        let (n : _ ty_naked_number), env_extension_from_meet =
          Meet_and_join_naked_float.meet_ty env n1 n2
        in
        Ok (Boxed_number (Boxed_float n), env_extension_from_meet)
      | Boxed_number (Boxed_int32 n1),
          Boxed_number (Boxed_int32 n2) ->
        let (n : _ ty_naked_number), env_extension_from_meet =
          Meet_and_join_naked_int32.meet_ty env n1 n2
        in
        Ok (Boxed_number (Boxed_int32 n), env_extension_from_meet)
      | Boxed_number (Boxed_int64 n1),
          Boxed_number (Boxed_int64 n2) ->
        let (n : _ ty_naked_number), env_extension_from_meet =
          Meet_and_join_naked_int64.meet_ty env n1 n2
        in
        Ok (Boxed_number (Boxed_int64 n), env_extension_from_meet)
      | Boxed_number (Boxed_nativeint n1),
          Boxed_number (Boxed_nativeint n2) ->
        let (n : _ ty_naked_number), env_extension_from_meet =
          Meet_and_join_naked_nativeint.meet_ty env n1 n2
        in
        Ok (Boxed_number (Boxed_nativeint n), env_extension_from_meet)
      | Closures closures1, Closures closures2 ->
        let env_extension_from_meet = ref (Typing_env_extension.create ()) in
        let closures =
          Closure_id.Map.inter
            (fun (closures_entry1 : closures_entry)
                 (closures_entry2 : closures_entry) : closures_entry option ->
              let set1 = closures_entry1.set_of_closures in
              let set2 = closures_entry2.set_of_closures in
              let set, new_env_extension_from_meet =
                Meet_and_join_fabricated.meet_ty env set1 set2
              in
              if ty_is_obviously_bottom set then begin
                None
              end else begin
                env_extension_from_meet :=
                  Typing_env_extension.meet new_env_extension_from_meet
                    !env_extension_from_meet;
                Some { set_of_closures = set; }
              end)
            closures1
            closures2
        in
        if Closure_id.Map.is_empty closures then Bottom
        else Ok (Closures closures, !env_extension_from_meet)
      | String strs1, String strs2 ->
        let strs = String_info.Set.inter strs1 strs2 in
        if String_info.Set.is_empty strs then Bottom
        else Ok (String strs, Typing_env_extension.create ())
      | (Blocks_and_tagged_immediates _
          | Boxed_number _
          | Closures _
          | String _), _ ->
        Bottom

    let join_of_kind_foo env env_extension1 env_extension2
          (of_kind1 : of_kind_value) (of_kind2 : of_kind_value)
          : of_kind_value Or_unknown.t =
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          join_blocks_and_tagged_immediates env env_extension1 env_extension2
            blocks_imms1 blocks_imms2
        in
        Known (Blocks_and_tagged_immediates blocks_imms)
      | Boxed_number (Boxed_float n1), Boxed_number (Boxed_float n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_float.join_ty env
            env_extension1 env_extension2 n1 n2
        in
        Known (Boxed_number (Boxed_float n))
      | Boxed_number (Boxed_int32 n1),
          Boxed_number (Boxed_int32 n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_int32.join_ty env
            env_extension1 env_extension2 n1 n2
        in
        Known (Boxed_number (Boxed_int32 n))
      | Boxed_number (Boxed_int64 n1),
          Boxed_number (Boxed_int64 n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_int64.join_ty env
            env_extension1 env_extension2 n1 n2
        in
        Known (Boxed_number (Boxed_int64 n))
      | Boxed_number (Boxed_nativeint n1),
          Boxed_number (Boxed_nativeint n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_nativeint.join_ty env
            env_extension1 env_extension2 n1 n2
        in
        Known (Boxed_number (Boxed_nativeint n))
      | Closures closures1, Closures closures2 ->
        let closures =
          Closure_id.Map.union_merge
            (fun (closures_entry1 : closures_entry)
                 (closures_entry2 : closures_entry) : closures_entry ->
              let set1 = closures_entry1.set_of_closures in
              let set2 = closures_entry2.set_of_closures in
              let set =
                Meet_and_join_fabricated.join_ty env
                  env_extension1 env_extension2 set1 set2
              in
              { set_of_closures = set; })
            closures1
            closures2
        in
        Known (Closures closures)
      | String strs1, String strs2 ->
        let strs = String_info.Set.union strs1 strs2 in
        Known (String strs)
      | (Blocks_and_tagged_immediates _
          | Boxed_number _
          | Closures _
          | String _), _ ->
        Unknown
  end)

  include Meet_and_join_value
end
