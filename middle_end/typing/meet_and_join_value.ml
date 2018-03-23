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
        with type equations := T.equations
        with type 'a ty := 'a T.ty
     end)
  -> sig
       include Meet_and_join_intf.S
         with type of_kind_foo := S.of_kind_foo
         with type typing_environment := T.typing_environment
         with type equations := T.equations
         with type 'a ty := 'a T.ty
    end) (Meet_and_join_naked_immediate : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := Immediate.Set.t T.of_kind_naked_number
        with type typing_environment := T.typing_environment
        with type equations := T.equations
        with type 'a ty := 'a T.ty
    end) (Meet_and_join_naked_float : sig
      include Meet_and_join_intf.S
        with type of_kind_foo :=
          Numbers.Float_by_bit_pattern.Set.t T.of_kind_naked_number
        with type typing_environment := T.typing_environment
        with type equations := T.equations
        with type 'a ty := 'a T.ty
    end) (Meet_and_join_naked_int32 : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := Numbers.Int32.Set.t T.of_kind_naked_number
        with type typing_environment := T.typing_environment
        with type equations := T.equations
        with type 'a ty := 'a T.ty
    end) (Meet_and_join_naked_int64 : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := Numbers.Int64.Set.t T.of_kind_naked_number
        with type typing_environment := T.typing_environment
        with type equations := T.equations
        with type 'a ty := 'a T.ty
    end) (Meet_and_join_naked_nativeint : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := Targetint.Set.t T.of_kind_naked_number
        with type typing_environment := T.typing_environment
        with type equations := T.equations
        with type 'a ty := 'a T.ty
    end) (Meet_and_join_fabricated : sig
      include Meet_and_join_intf.S
        with type of_kind_foo := T.of_kind_fabricated
        with type typing_environment := T.typing_environment
        with type equations := T.equations
        with type 'a ty := 'a T.ty
    end) (Meet_and_join : sig
      include Meet_and_join_intf.S_for_types
        with type t_in_context := T.t_in_context
        with type equations := T.equations
        with type flambda_type := T.flambda_type
    end) (Typing_environment0 : sig
      include Typing_environment0_intf.S
        with type typing_environment := T.typing_environment
        with type equations := T.equations
        with type flambda_type := T.flambda_type
        with type t_in_context := T.t_in_context
        with type 'a ty := 'a T.ty
        with type 'a unknown_or_join := 'a T.unknown_or_join
    end) (Equations : sig
      include Equations_intf.S
        with type equations := T.equations
        with type typing_environment := T.typing_environment
        with type flambda_type := T.flambda_type
    end) =
struct
  module rec Meet_and_join_value : sig
    include Meet_and_join_intf.S
      with type of_kind_foo := T.of_kind_value
      with type typing_environment := T.typing_environment
      with type equations := T.equations
      with type 'a ty := 'a T.ty
  end = Make_meet_and_join (struct
    open T

    type equations = T.equations

    type of_kind_foo = of_kind_value

    let kind = K.value ()

    let to_type ty : t = { descr = Value ty; phantom = None; }
    let force_to_kind = force_to_kind_value
    let print_ty = print_ty_value

    let meet_immediate_case env1 _env2
          ({ equations = equations1; } : immediate_case)
          ({ equations = equations2; } : immediate_case)
          : immediate_case =
      let resolver = env1.resolver in
      let equations =
        Meet_and_join.meet_equations ~resolver equations1 equations2
      in
      { equations; }

    let join_immediate_case env1 _env2
          ({ equations = equations1; } : immediate_case)
          ({ equations = equations2; } : immediate_case)
          : immediate_case =
      let resolver = env1.resolver in
      let equations =
        Meet_and_join.join_equations ~resolver equations1 equations2
      in
      { equations; }

    let meet_immediates env1 env2 immediates1 immediates2 : _ Or_bottom.t =
      let immediates =
        Immediate.Map.inter_merge (fun imm1 imm2 ->
            meet_immediate_case env1 env2 imm1 imm2)
          immediates1
          immediates2
      in
      if Immediate.Map.is_empty immediates then Bottom
      else Ok immediates

    let join_immediates env1 env2 immediates1 immediates2 =
      Immediate.Map.union_merge (fun imm1 imm2 ->
          join_immediate_case env1 env2 imm1 imm2)
        immediates1
        immediates2

    let meet_singleton_block env1 env2
          ({ equations = equations1;
             fields = fields1;
           } : singleton_block)
          ({ equations = equations2;
             fields = fields2;
           } : singleton_block) : singleton_block * equations =
      let resolver = env1.resolver in
      let equations =
        Meet_and_join.meet_equations ~resolver equations1 equations2
      in
      assert (Array.length fields1 = Array.length fields2);
      let equations_from_meet = ref (Equations.create ()) in
      let fields =
        Array.map2
          (fun (field1 : _ mutable_or_immutable)
               (field2 : _ mutable_or_immutable) : _ mutable_or_immutable ->
            match field1, field2 with
            | Mutable, _ | _, Mutable -> Mutable
            | Immutable field1, Immutable field2 ->
              let field, new_equations_from_meet =
                Meet_and_join.meet ~bias_towards:(env1, field1) (env2, field2)
              in
              equations_from_meet :=
                Meet_and_join.meet_equations ~resolver new_equations_from_meet
                  !equations_from_meet;
              Immutable field)
          fields1
          fields2
      in
      { equations;
        fields;
      }, !equations_from_meet

    let join_singleton_block env1 env2
          ({ equations = equations1;
             fields = fields1;
           } : singleton_block)
          ({ equations = equations2;
             fields = fields2;
           } : singleton_block) : singleton_block =
      let resolver = env1.resolver in
      let equations =
        Meet_and_join.join_equations ~resolver equations1 equations2
      in
      assert (Array.length fields1 = Array.length fields2);
      let fields =
        Array.map2
          (fun (field1 : _ mutable_or_immutable)
               (field2 : _ mutable_or_immutable) : _ mutable_or_immutable ->
            match field1, field2 with
            | Mutable, _ | _, Mutable -> Mutable
            | Immutable field1, Immutable field2 ->
              Immutable (Meet_and_join.join (env1, field1) (env2, field2)))
          fields1
          fields2
      in
      { equations;
        fields;
      }

    let meet_block_cases env1 env2
          ((Blocks { by_length = singleton_blocks1; }) : block_cases)
          ((Blocks { by_length = singleton_blocks2; }) : block_cases)
          : (block_cases * equations) Or_bottom.t =
      let resolver = env1.resolver in
      let equations_from_meet = ref (Equations.create ()) in
      let by_length =
        Targetint.OCaml.Map.inter_merge
          (fun singleton_block1 singleton_block2 ->
            let singleton_block, new_equations_from_meet =
              meet_singleton_block env1 env2
                singleton_block1 singleton_block2
            in
            equations_from_meet :=
              Meet_and_join.meet_equations ~resolver new_equations_from_meet
                !equations_from_meet;
            singleton_block)
          singleton_blocks1
          singleton_blocks2
      in
      if Targetint.OCaml.Map.is_empty by_length then Bottom
      else Ok (((Blocks { by_length; }) : block_cases), !equations_from_meet)

    let join_block_cases env1 env2
          ((Blocks { by_length = singleton_blocks1; }) : block_cases)
          ((Blocks { by_length = singleton_blocks2; }) : block_cases)
          : block_cases =
      let by_length =
        Targetint.OCaml.Map.union_merge
          (fun singleton_block1 singleton_block2 ->
            join_singleton_block env1 env2
              singleton_block1 singleton_block2)
          singleton_blocks1
          singleton_blocks2
      in
      Blocks { by_length; }

    let meet_blocks env1 env2 blocks1 blocks2 : _ Or_bottom.t =
      let resolver = env1.resolver in
      let equations_from_meet = ref (Equations.create ()) in
      let blocks =
        Tag.Map.inter (fun block_cases1 block_cases2 ->
            match meet_block_cases env1 env2 block_cases1 block_cases2 with
            | Ok (block_cases, new_equations_from_meet) ->
              equations_from_meet :=
                Meet_and_join.meet_equations ~resolver new_equations_from_meet
                  !equations_from_meet;
              Some block_cases
            | Bottom -> None)
          blocks1
          blocks2
      in
      if Tag.Map.is_empty blocks then Bottom
      else Ok (blocks, !equations_from_meet)

    let join_blocks env1 env2 blocks1 blocks2 =
      Tag.Map.union_merge (fun block_cases1 block_cases2 ->
          join_block_cases env1 env2 block_cases1 block_cases2)
        blocks1
        blocks2

    let meet_blocks_and_tagged_immediates env1 env2
          { blocks = blocks1; immediates = imms1; is_int = is_int1;
            get_tag = get_tag1; }
          { blocks = blocks2; immediates = imms2; is_int = is_int2;
            get_tag = get_tag2; }
          : (blocks_and_tagged_immediates * equations) Or_bottom.t =
      let resolver = env1.resolver in
      let (blocks : _ Or_unknown.t), equations_from_meet =
        match blocks1, blocks2 with
        | Unknown, _ -> blocks2, Equations.create ()
        | _, Unknown -> blocks1, Equations.create ()
        | Known blocks1, Known blocks2 ->
          match meet_blocks env1 env2 blocks1 blocks2 with
          | Bottom -> Or_unknown.Known Tag.Map.empty, Equations.create ()
          | Ok (blocks, equations_from_meet) ->
            Or_unknown.Known blocks, equations_from_meet
      in
      let immediates : _ Or_unknown.t =
        match imms1, imms2 with
        | Unknown, _ -> imms2
        | _, Unknown -> imms1
        | Known imms1, Known imms2 ->
          match meet_immediates env1 env2 imms1 imms2 with
          | Bottom -> Known Immediate.Map.empty
          | Ok immediates -> Known immediates
      in
      let is_int =
        match is_int1, is_int2 with
        | None, None -> None
        | None, Some _ -> is_int2
        | Some _, None -> is_int1
        | Some is_int1, Some is_int2 ->
          if Name.equal is_int1 is_int2 then Some is_int1 else None
      in
      let get_tag =
        match get_tag1, get_tag2 with
        | None, None -> None
        | None, Some _ -> get_tag2
        | Some _, None -> get_tag1
        | Some get_tag1, Some get_tag2 ->
          if Name.equal get_tag1 get_tag2 then Some get_tag1 else None
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
      if is_bottom then Bottom
      else
        let equations_from_meet =
          match immediates with
          | Unknown -> equations_from_meet
          | Known imms ->
            if not (Immediate.Map.is_empty imms) then equations_from_meet
            else  (* CR mshinwell: This should maybe meet across all blocks *)
              match blocks with
              | Unknown -> equations_from_meet
              | Known blocks ->
                match Tag.Map.get_singleton blocks with
                | None -> equations_from_meet
                | Some (_, Blocks { by_length; }) ->
                  (* CR mshinwell: This should remove equations propagated
                     upwards from the block cases *)
                  match Targetint.OCaml.Map.get_singleton by_length with
                  | None -> equations_from_meet
                  | Some (_, singleton_block) ->
                    Meet_and_join.meet_equations ~resolver
                      singleton_block.equations equations_from_meet
        in
        Ok ({ blocks; immediates; is_int; get_tag; }, equations_from_meet)

    let join_blocks_and_tagged_immediates env1 env2
          { blocks = blocks1; immediates = imms1; is_int = is_int1;
            get_tag = get_tag1; }
          { blocks = blocks2; immediates = imms2; is_int = is_int2;
            get_tag = get_tag2; }
          : blocks_and_tagged_immediates =
      let blocks : _ Or_unknown.t =
        match blocks1, blocks2 with
        | Unknown, _ | _, Unknown -> Unknown
        | Known blocks1, Known blocks2 ->
          Known (join_blocks env1 env2 blocks1 blocks2)
      in
      let immediates : _ Or_unknown.t =
        match imms1, imms2 with
        | Unknown, _ | _, Unknown -> Unknown
        | Known imms1, Known imms2 ->
          Known (join_immediates env1 env2 imms1 imms2)
      in
      (* CR mshinwell: Refactor between is_int / get_tag; then share with
         meet. *)
      let is_int =
        match is_int1, is_int2 with
        | None, None -> None
        | None, Some _ -> is_int2
        | Some _, None -> is_int1
        | Some is_int1, Some is_int2 ->
          if Name.equal is_int1 is_int2 then Some is_int1 else None
      in
      let get_tag =
        match get_tag1, get_tag2 with
        | None, None -> None
        | None, Some _ -> get_tag2
        | Some _, None -> get_tag1
        | Some get_tag1, Some get_tag2 ->
          if Name.equal get_tag1 get_tag2 then Some get_tag1 else None
      in
      { blocks; immediates; is_int; get_tag; }

    let meet_of_kind_foo env1 env2
          (of_kind1 : of_kind_value) (of_kind2 : of_kind_value)
          : (of_kind_value * equations) Or_bottom.t =
      let resolver = env1.resolver in
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          meet_blocks_and_tagged_immediates env1 env2
            blocks_imms1 blocks_imms2
        in
        begin match blocks_imms with
        | Ok (blocks_imms, equations_from_meet) ->
          Ok (Blocks_and_tagged_immediates blocks_imms, equations_from_meet)
        | Bottom -> Bottom
        end
      | Boxed_number (Boxed_float n1),
          Boxed_number (Boxed_float n2) ->
        let (n : _ ty_naked_number), equations_from_meet =
          Meet_and_join_naked_float.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_float n), equations_from_meet)
      | Boxed_number (Boxed_int32 n1),
          Boxed_number (Boxed_int32 n2) ->
        let (n : _ ty_naked_number), equations_from_meet =
          Meet_and_join_naked_int32.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_int32 n), equations_from_meet)
      | Boxed_number (Boxed_int64 n1),
          Boxed_number (Boxed_int64 n2) ->
        let (n : _ ty_naked_number), equations_from_meet =
          Meet_and_join_naked_int64.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_int64 n), equations_from_meet)
      | Boxed_number (Boxed_nativeint n1),
          Boxed_number (Boxed_nativeint n2) ->
        let (n : _ ty_naked_number), equations_from_meet =
          Meet_and_join_naked_nativeint.meet_ty env1 env2 n1 n2
        in
        Ok (Boxed_number (Boxed_nativeint n), equations_from_meet)
      | Closures closures1, Closures closures2 ->
        let equations_from_meet = ref (Equations.create ()) in
        let closures =
          Closure_id.Map.inter
            (fun (closures_entry1 : closures_entry)
                 (closures_entry2 : closures_entry) : closures_entry option ->
              let set1 = closures_entry1.set_of_closures in
              let set2 = closures_entry2.set_of_closures in
              let set, new_equations_from_meet =
                Meet_and_join_fabricated.meet_ty env1 env2 set1 set2
              in
              if ty_is_obviously_bottom set then begin
                None
              end else begin
                equations_from_meet :=
                  Meet_and_join.meet_equations ~resolver
                    new_equations_from_meet !equations_from_meet;
                Some { set_of_closures = set; }
              end)
            closures1
            closures2
        in
        if Closure_id.Map.is_empty closures then Bottom
        else Ok (Closures closures, !equations_from_meet)
      | String strs1, String strs2 ->
        let strs = String_info.Set.inter strs1 strs2 in
        if String_info.Set.is_empty strs then Bottom
        else Ok (String strs, Equations.create ())
      | (Blocks_and_tagged_immediates _
          | Boxed_number _
          | Closures _
          | String _), _ ->
        Bottom

    let join_of_kind_foo env1 env2
          (of_kind1 : of_kind_value) (of_kind2 : of_kind_value)
          : of_kind_value Or_unknown.t =
      match of_kind1, of_kind2 with
      | Blocks_and_tagged_immediates blocks_imms1,
          Blocks_and_tagged_immediates blocks_imms2 ->
        let blocks_imms =
          join_blocks_and_tagged_immediates env1 env2
            blocks_imms1 blocks_imms2
        in
        Known (Blocks_and_tagged_immediates blocks_imms)
      | Boxed_number (Boxed_float n1), Boxed_number (Boxed_float n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_float.join_ty env1 env2 n1 n2
        in
        Known (Boxed_number (Boxed_float n))
      | Boxed_number (Boxed_int32 n1),
          Boxed_number (Boxed_int32 n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_int32.join_ty env1 env2 n1 n2
        in
        Known (Boxed_number (Boxed_int32 n))
      | Boxed_number (Boxed_int64 n1),
          Boxed_number (Boxed_int64 n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_int64.join_ty env1 env2 n1 n2
        in
        Known (Boxed_number (Boxed_int64 n))
      | Boxed_number (Boxed_nativeint n1),
          Boxed_number (Boxed_nativeint n2) ->
        let n : _ ty_naked_number =
          Meet_and_join_naked_nativeint.join_ty env1 env2 n1 n2
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
                Meet_and_join_fabricated.join_ty env1 env2 set1 set2
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
