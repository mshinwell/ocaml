(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Int = Numbers.Int
module TEE = Typing_env_extension

module Make
  (Tag : Identifiable.S)
  (Index : sig
     include Identifiable.S
     val subset : t -> t -> bool
  end)
  (Tag_and_index : sig
    type t = Tag.t * Index.t
    include Identifiable.S with type t := t
  end)
  (Tag_or_unknown_and_index : sig
    type t = Tag.t Or_unknown.t * Index.t
    include Identifiable.S with type t := t
  end)
  (Maps_to : Row_like_maps_to_intf.S
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type meet_or_join_env := Meet_or_join_env.t
    with type typing_env_extension := Typing_env_extension.t) =
struct
  type t = {
    known : Maps_to.t Tag_and_index.Map.t;
    at_least : Maps_to.t Tag_or_unknown_and_index.Map.t;
  }

  let is_bottom { known; at_least; } =
    Tag_and_index.Map.is_empty known
      && Tag_or_unknown_and_index.Map.is_empty at_least

  let print_with_cache ~cache ppf (({ known; at_least } as t) : t) =
    if is_bottom t then
      (* CR mshinwell: factor out (also in [Type_descr]) *)
      let colour = Flambda_colours.top_or_bottom_type () in
      if !Clflags.flambda2_unicode then
        Format.fprintf ppf "@<0>%s@<1>\u{22a5}@<0>%s"
          colour (Flambda_colours.normal ())
      else
        Format.fprintf ppf "%s_|_%s" colour (Flambda_colours.normal ())
    else
      Format.fprintf ppf 
        "@[<hov 1>(\
           @[<hov 1>(known@ %a)@]@ \
           @[<hov 1>(at_least@ %a)@]\
           )@]"
        (Tag_and_index.Map.print (Maps_to.print_with_cache ~cache)) known
        (Tag_or_unknown_and_index.Map.print (Maps_to.print_with_cache ~cache))
        at_least

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let _invariant _t = ()

  let create_bottom () =
    { known = Tag_and_index.Map.empty;
      at_least = Tag_or_unknown_and_index.Map.empty;
    }

  let create_exactly tag index maps_to =
    { known = Tag_and_index.Map.singleton (tag, index) maps_to;
      at_least = Tag_or_unknown_and_index.Map.empty;
    }

  let create_exactly_multiple known =
    { known;
      at_least = Tag_or_unknown_and_index.Map.empty;
    }

  let create_at_least tag_or_unknown_and_index maps_to =
    { known = Tag_and_index.Map.empty;
      at_least =
        Tag_or_unknown_and_index.Map.singleton tag_or_unknown_and_index maps_to;
    }

  let create_at_least_multiple at_least =
    { known = Tag_and_index.Map.empty;
      at_least;
    }

  module Row_like_meet_or_join
    (E : Lattice_ops_intf.S
      with type typing_env := Typing_env.t
      with type meet_env := Meet_env.t
      with type meet_or_join_env := Meet_or_join_env.t
      with type typing_env_extension := Typing_env_extension.t) =
  struct
    let meet_or_join (env : Meet_or_join_env.t) t1 t2 : _ Or_bottom.t =
      let ({ known = known1; at_least = at_least1; } : t) = t1 in
      let ({ known = known2; at_least = at_least2; } : t) = t2 in
      let env_extension = ref None in
      let join_env_extension ext =
        match !env_extension with
        | None -> env_extension := Some ext
        | Some ext2 -> env_extension := Some (TEE.join env ext2 ext)
      in
      let one_side_only env (tag_or_unknown1 : _ Or_unknown.t) index1
            maps_to1 at_least2 =
        let from_at_least2 =
          Tag_or_unknown_and_index.Map.find_last_opt
            (fun (tag_or_unknown_and_index2 : _ Or_unknown.t * _) ->
              match tag_or_unknown1, tag_or_unknown_and_index2 with
              | _, (Unknown, index2)
              | Unknown, (Known _, index2) -> Index.subset index2 index1
              | Known tag1, (Known tag2, index2) ->
                Tag.equal tag2 tag1 && Index.subset index2 index1)
            at_least2
        in
        begin match from_at_least2 with
        | None ->
          begin match E.op () with
          | Meet -> None
          | Join ->
            (* CR mshinwell: Same comment as per
               Type_descr.join_head_or_unknown_or_bottom *)
            let env =
              Meet_or_join_env.create_for_join
                (Meet_or_join_env.target_join_env env)
                ~left_env:(Meet_or_join_env.left_join_env env)
                ~right_env:(Meet_or_join_env.left_join_env env)
            in
            let maps_to =
              E.switch Maps_to.meet Maps_to.join env
                maps_to1 maps_to1
            in
            match maps_to with
            | Bottom -> None
            | Ok (maps_to, env_extension') ->
              join_env_extension env_extension';
              Some maps_to
          end
        | Some ((_tag_or_unknown, index2), from_at_least2) ->
          assert (Index.subset index2 index1);
          let maps_to =
            E.switch Maps_to.meet Maps_to.join env
              maps_to1
              from_at_least2
          in
          match maps_to with
          | Bottom -> None
          | Ok (maps_to, env_extension') ->
(*
Format.eprintf "Existing env extension, case 1:@ %a\n%!"
  TEE.print !env_extension;
Format.eprintf "New env extension, case 1:@ %a\n%!"
  TEE.print env_extension';
*)
            join_env_extension env_extension';
(*
Format.eprintf "Resulting env extension, case 1:@ %a\n%!"
  TEE.print !env_extension;
*)
            Some maps_to
        end
      in
      let merge tag index maps_to1 maps_to2 =
        match maps_to1, maps_to2 with
        | Some maps_to1, None ->
          one_side_only env tag index maps_to1 at_least2
        | None, Some maps_to2 ->
          one_side_only (Meet_or_join_env.flip_join_envs env)
            tag index maps_to2 at_least1
        | Some maps_to1, Some maps_to2 ->
          let maps_to =
            E.switch Maps_to.meet Maps_to.join env maps_to1 maps_to2
          in
          begin match maps_to with
          | Bottom -> None
          | Ok (maps_to, env_extension') ->
(*
Format.eprintf "Existing env extension, case 2:@ %a\n%!"
  TEE.print !env_extension;
Format.eprintf "New env extension, case 2:@ %a\n%!"
  TEE.print env_extension';
*)
            join_env_extension env_extension';
(*
Format.eprintf "Resulting env extension, case 2:@ %a\n%!"
  TEE.print !env_extension;
*)
            Some maps_to
          end
        | None, None -> None
      in
      let known =
        Tag_and_index.Map.merge (fun (tag, index) maps_to1 maps_to2 ->
            merge (Known tag) index maps_to1 maps_to2)
          known1
          known2
      in
      let at_least =
        Tag_or_unknown_and_index.Map.merge
          (fun (tag_or_unknown, index) maps_to1 maps_to2 ->
            merge tag_or_unknown index maps_to1 maps_to2)
          at_least1
          at_least2
      in
      if Tag_and_index.Map.is_empty known &&
        Tag_or_unknown_and_index.Map.is_empty at_least
      then begin
(*
Format.eprintf "RL meet is returning bottom\n%!";
*)
        Bottom
      end else
        let env_extension =
          match !env_extension with
          | None -> TEE.empty ()
          | Some ext -> ext
        in
        Ok ({ known; at_least; }, env_extension)
  end

  module Meet = Row_like_meet_or_join (Lattice_ops.For_meet)
  module Join = Row_like_meet_or_join (Lattice_ops.For_join)

  let meet env t1 t2 =
    Meet.meet_or_join (Meet_or_join_env.create_for_meet env) t1 t2

  let join env t1 t2 =
    match Join.meet_or_join env t1 t2 with
    | Ok (t, _env_extension) -> t
    | Bottom -> create_bottom ()

  let get_singleton { known; at_least; } =
    if not (Tag_or_unknown_and_index.Map.is_empty at_least) then None
    else Tag_and_index.Map.get_singleton known

  let all_tags { known; at_least; } : _ Or_unknown.t =
    let from_at_least =
      Tag_or_unknown_and_index.Map.fold
        (fun ((tag_or_unk : _ Or_unknown.t), _index) _ from_at_least ->
          match from_at_least with
          | None -> None
          | Some from_at_least ->
            match tag_or_unk with
            | Unknown -> None
            | Known tag -> Some (Tag.Set.add tag from_at_least))
        at_least
        (Some Tag.Set.empty)
    in
    match from_at_least with
    | None -> Unknown
    | Some tags ->
      let tags =
        Tag_and_index.Set.fold (fun (tag, _index) tags ->
            Tag.Set.add tag tags)
          (Tag_and_index.Map.keys known)
          tags
      in
      Known tags

  let all_tags_and_indexes { known; at_least; } : _ Or_unknown.t =
    if not (Tag_or_unknown_and_index.Map.is_empty at_least) then Unknown
    else Known (Tag_and_index.Map.keys known)

  let free_names { known; at_least; } =
    let from_known =
      Tag_and_index.Map.fold (fun _tag_and_index maps_to free_names ->
          Name_occurrences.union free_names
            (Maps_to.free_names maps_to))
        known
        Name_occurrences.empty
    in
    let from_at_least =
      Tag_or_unknown_and_index.Map.fold (fun _index maps_to free_names ->
          Name_occurrences.union free_names
            (Maps_to.free_names maps_to))
        at_least
        Name_occurrences.empty
    in
    Name_occurrences.union from_known from_at_least

  let map_maps_to { known; at_least; }
        ~(f : Maps_to.t -> Maps_to.t Or_bottom.t)
        : _ Or_bottom.t =
    let found_bottom = ref false in
    let known =
      Tag_and_index.Map.map (fun maps_to ->
          match f maps_to with
          | Bottom ->
            found_bottom := true;
            maps_to
          | Ok maps_to -> maps_to)
        known
    in
    let at_least =
      Tag_or_unknown_and_index.Map.map (fun maps_to ->
          match f maps_to with
          | Bottom ->
            found_bottom := true;
            maps_to
          | Ok maps_to -> maps_to)
        at_least
    in
    if !found_bottom then Bottom
    else
      Ok { 
        known;
        at_least;
      }

  let apply_name_permutation ({ known; at_least; } as t) perm =
    let known' =
      Tag_and_index.Map.map_sharing (fun maps_to ->
          Maps_to.apply_name_permutation maps_to perm)
        known
    in
    let at_least' =
      Tag_or_unknown_and_index.Map.map_sharing (fun maps_to ->
          Maps_to.apply_name_permutation maps_to perm)
        at_least
    in
    if known == known' && at_least == at_least' then t
    else
      { known = known';
        at_least = at_least';
      }
end

module Targetint_ocaml_index = struct
  include Targetint.OCaml
  let subset t1 t2 = Stdlib.(<=) (compare t1 t2) 0
end

module For_blocks = struct
  include Make (Tag) (Targetint_ocaml_index) (Tag_and_size)
    (Tag_or_unknown_and_size) (Product.Int_indexed)

  type open_or_closed = Open of Tag.t Or_unknown.t | Closed of Tag.t

  let create ~(field_kind : Flambda_kind.t) ~field_tys
        (open_or_closed : open_or_closed) =
    if !Clflags.flambda_invariant_checks then begin
      let field_kind' =
        List.map Type_grammar.kind field_tys
        |> Flambda_kind.Set.of_list
        |> Flambda_kind.Set.get_singleton
      in
      begin match field_kind' with
      | None ->
        if List.length field_tys <> 0 then begin
          Misc.fatal_error "[field_tys] must all be of the same kind"
        end
      | Some field_kind' ->
        if not (Flambda_kind.equal field_kind field_kind') then begin
          Misc.fatal_errorf "Declared field kind %a doesn't match [field_tys]"
            Flambda_kind.print field_kind
        end
      end
    end;
    let tag : _ Or_unknown.t =
      let tag : _ Or_unknown.t =
        match open_or_closed with
        | Open (Known tag) -> Known tag
        | Open Unknown -> Unknown
        | Closed tag -> Known tag
      in
      match tag with
      | Unknown ->
        begin match field_kind with
        | Value -> Unknown
        | Naked_number Naked_float -> Known Tag.double_array_tag
        | Naked_number Naked_immediate | Naked_number Naked_int32
        | Naked_number Naked_int64 | Naked_number Naked_nativeint
        | Fabricated ->
          Misc.fatal_errorf "Bad kind %a for fields"
            Flambda_kind.print field_kind
        end
      | Known tag ->
        begin match field_kind with
        | Value ->
          begin match Tag.Scannable.of_tag tag with
          | Some _ -> Known tag
          | None ->
            Misc.fatal_error "Blocks full of [Value]s must have a tag \
              less than [No_scan_tag]"
          end
        | Naked_number Naked_float ->
          if not (Tag.equal tag Tag.double_array_tag) then begin
            Misc.fatal_error "Blocks full of naked floats must have tag \
              [Tag.double_array_tag]"
          end;
          Known tag
        | Naked_number Naked_immediate | Naked_number Naked_int32
        | Naked_number Naked_int64 | Naked_number Naked_nativeint
        | Fabricated ->
          Misc.fatal_errorf "Bad kind %a for fields"
            Flambda_kind.print field_kind
        end
    in
    let product = Product.Int_indexed.create_from_list field_kind field_tys in
    let size = Product.Int_indexed.width product in
    match open_or_closed with
    | Open _ -> create_at_least (tag, size) product
    | Closed _ ->
      match tag with
      | Known tag -> create_exactly tag size product
      | Unknown -> assert false  (* see above *)

  let create_blocks_with_these_tags ~field_kind tags =
    let at_least =
      Tag.Set.fold (fun tag at_least ->
          Tag_or_unknown_and_size.Map.add (Known tag, Targetint.OCaml.zero)
            (Product.Int_indexed.create_top field_kind)
            at_least)
        tags
        Tag_or_unknown_and_size.Map.empty
    in
    create_at_least_multiple at_least

  let all_tags_and_sizes t : _ Or_unknown.t =
    match all_tags_and_indexes t with
    | Unknown -> Unknown
    | Known tags_and_indexes ->
      let by_tag =
        Tag_and_size.Set.fold (fun (tag, size) all_tags ->
            match Tag.Map.find tag all_tags with
            | exception Not_found -> Tag.Map.add tag size all_tags
            | _ ->
              Misc.fatal_errorf "More than one size for the same tag:@ %a"
                print t)
          tags_and_indexes
          Tag.Map.empty
      in
      Known by_tag
end

module For_closures_entry_by_set_of_closures_contents = struct
  include Make (Closure_id) (Set_of_closures_contents)
    (Set_of_closures_contents.With_closure_id)
    (Set_of_closures_contents.With_closure_id_or_unknown)
    (Closures_entry)

  let map_function_decl_types t ~f =
    map_maps_to t ~f:(fun closures_entry ->
      Closures_entry.map_function_decl_types closures_entry ~f)
end
