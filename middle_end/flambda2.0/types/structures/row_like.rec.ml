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
      with type typing_env_extension := Typing_env_extension.t) =
  struct
    let meet_or_join env t1 t2 : _ Or_bottom.t =
(*
Format.eprintf "RL meet/join: %a@ and@ %a\n%!" print t1 print t2;
*)
      let ({ known = known1; at_least = at_least1; } : t) = t1 in
      let ({ known = known2; at_least = at_least2; } : t) = t2 in
      let env_extension = ref (TEE.empty ()) in
      let one_side_only (tag_or_unknown1 : _ Or_unknown.t) index1
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
          | Join -> Some maps_to1
          end
        | Some ((_tag_or_unknown, index2), from_at_least2) ->
          assert (Index.subset index2 index1);
          let maps_to =
            E.switch Maps_to.meet Maps_to.join env
              maps_to1
              (Maps_to.widen from_at_least2 ~to_match:maps_to1)
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
            env_extension := TEE.meet env !env_extension env_extension';
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
          one_side_only tag index maps_to1 at_least2
        | None, Some maps_to2 ->
          one_side_only tag index maps_to2 at_least1
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
             env_extension := TEE.meet env !env_extension env_extension';
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
        Ok ({ known; at_least; }, !env_extension)
  end

  module Meet = Row_like_meet_or_join (Lattice_ops.For_meet)
  module Join = Row_like_meet_or_join (Lattice_ops.For_join)

  let meet env t1 t2 = Meet.meet_or_join env t1 t2

  let join env t1 t2 =
    match Join.meet_or_join (Meet_env.create env) t1 t2 with
    | Ok (t, _env_extension) -> t
    | Bottom -> create_bottom ()

  let known t = t.known
  let at_least t = t.at_least

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

module Make_trivial (Thing : Identifiable.S) = struct
  module Thing_and_unit = struct
    type t = Thing.t * unit
    include Identifiable.Make_pair (Thing) (Unit)
  end

  module Thing_or_unknown = Or_unknown.Lift (Thing)

  module Thing_or_unknown_and_unit = struct
    type t = Thing.t Or_unknown.t * unit
    include Identifiable.Make_pair (Thing_or_unknown) (Unit)
  end

  module Unit_maps_to = struct
    include Unit

    let print_with_cache ~cache:_ ppf () = print ppf ()
    let meet _env () () = Or_bottom.Ok ((), TEE.empty ())
    let join _env () () = ()
    let create_bottom () = ()
    let widen () ~to_match:() = ()
  end

  include Make (Thing) (Unit) (Thing_and_unit)
    (Thing_or_unknown_and_unit) (Unit_maps_to)

  let create things =
    let things =
      Thing.Set.fold (fun thing result ->
          Thing_and_unit.Map.add (thing, ()) () result)
        things
        Thing_and_unit.Map.empty
    in
    create_exactly_multiple things

  let all t : _ Or_unknown.t =
    let indexes = at_least t in
    let known = known t in
    if not (Thing_or_unknown_and_unit.Map.is_empty indexes) then Unknown
    else
      let things =
        Thing_and_unit.Set.fold (fun (thing, ()) things ->
            Thing.Set.add thing things)
          (Thing_and_unit.Map.keys known)
          Thing.Set.empty
      in
      Known things

  let get_singleton t =
    match get_singleton t with
    | None -> None
    | Some ((thing, ()), ()) -> Some thing
end

module Targetint_ocaml_index = struct
  include Targetint.OCaml
  let subset t1 t2 = Stdlib.(<=) (compare t1 t2) 0
end

module For_blocks = struct
  include Make (Tag) (Targetint_ocaml_index) (Tag_and_size)
    (Tag_or_unknown_and_size) (Product.Int_indexed)

  type open_or_closed = Open of Tag.t Or_unknown.t | Closed of Tag.t

  let create ~field_tys (open_or_closed : open_or_closed) =
    let fields = List.mapi (fun index ty -> index, ty) field_tys in
    let product = Product.Int_indexed.create (Int.Map.of_list fields) in
    let size = Targetint.OCaml.of_int (List.length field_tys) in
    match open_or_closed with
    | Open tag -> create_at_least (tag, size) product
    | Closed tag -> create_exactly tag size product

  let create_blocks_with_these_tags tags =
    let at_least =
      Tag.Set.fold (fun tag at_least ->
          Tag_or_unknown_and_size.Map.add (Known tag, Targetint.OCaml.zero)
            (Product.Int_indexed.create Int.Map.empty)
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

module For_discriminants = Make_trivial (Discriminant)

module For_immediates = Make_trivial (Immediate)
