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

(* CR mshinwell: Should be able to enforce kinds *)

module Make
  (W : Typing_world.S)
  (Tag : sig
    type t
    include Hashtbl.With_map with type t := t
    include Contains_names.S with type t := t
  end)
  (Index : sig
    type t

    include Hashtbl.With_map with type t := t
    include Contains_names.S with type t := t
  end)
  (Maps_to : sig
    type t

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val add_or_meet_equations
       : t
      -> W.Typing_env.t
      -> W.Typing_env_extension.t
      -> t

    val meet
       : W.Typing_env.t
      -> Name_permutation.t
      -> Name_permutation.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> t Or_bottom.t * W.Typing_env_extension.t

    val join
       : W.Join_env.t
      -> Name_permutation.t
      -> Name_permutation.t
      -> Relational_product_intf.fresh_component_semantics
      -> t
      -> t
      -> t

    include Contains_names.S with type t := t
  end) =
struct
  open W

  module MT = Maps_to

  module Tag = Tag
  module Index = Index

  module Tag_and_index = struct
    include Hashtbl.Make_with_map_pair (Tag) (Index)

    let create tag index = tag, index
    let index (_tag, index) = index

    let apply_name_permutation (tag, index) perm =
      tag, Index.apply_name_permutation index perm
  end

  type t = {
    known : MT.t Tag_and_index.Map.t;
    at_least : MT.t Index.Map.t;
  }

  let print ~cache ppf ({ known; at_least } : t) =
    Format.fprintf ppf 
      "@[<hov 1>(\
         @[<hov 1>(known@ %a)@]@ \
         @[<hov 1>(at_least@ %a)@])@]"
      (Tag_and_index.Map.print (MT.print_with_cache ~cache)) known
      (Index.Map.print (MT.print_with_cache ~cache)) at_least

  let create () =
    { known = Tag_and_index.Map.empty;
      at_least = Index.Map.empty;
    }

  let create_exactly tag index maps_to =
    let tag_and_index = Tag_and_index.create tag index in
    { known = Tag_and_index.Map.singleton tag_and_index maps_to;
      at_least = Index.Map.empty;
    }

  let create_exactly_multiple known =
    { known;
      at_least = Index.Map.empty;
    }

  let create_at_least index maps_to =
    { known = Tag_and_index.Map.empty;
      at_least = Index.Map.singleton index maps_to;
    }

  let create_at_least_multiple at_least =
    { known = Tag_and_index.Map.empty;
      at_least;
    }

  let is_empty { known; at_least; } =
    Tag_and_index.Map.is_empty known && Index.Map.is_empty at_least

  module Meet_or_join
    (E : Either_meet_or_join_intf.S
      with module Join_env := Join_env
      with module Typing_env := Typing_env
      with module Typing_env_extension := Typing_env_extension) =
  struct
    let meet_or_join env perm1 perm2 fresh_component_semantics
          ({ known = known1; at_least = at_least1; } : t)
          ({ known = known2; at_least = at_least2; } : t) : t Or_bottom.t =
      let one_side_only index1 maps_to1 perm1 at_least2
            ~get_equations_to_deposit1 =
        let from_at_least2 =
          Index.Map.find_last_opt
            (fun index -> Index.compare index index1 <= 0)
            at_least2
        in
        begin match from_at_least2 with
        | None ->
          begin match E.op with
          | Meet -> None
          | Join ->
            let maps_to1 =
              MT.add_or_meet_equations
                (MT.apply_name_permutation maps_to1 perm1)
                (Join_env.central_environment env)
                (get_equations_to_deposit1 env)
            in
            Some maps_to1
          end
        | Some (index2, from_at_least2) ->
          assert (Index.compare index2 index1 <= 0);
          let maps_to1 = MT.apply_name_permutation maps_to1 perm1 in
          (* CR mshinwell: What happens to any generated equations in the
             [meet] case (same below)? *)
          Some (E.switch'_with_param MT.meet MT.join env perm1 perm2
            fresh_component_semantics maps_to1 from_at_least2)
        end
      in
      let merge index maps_to1 maps_to2 =
        match maps_to1, maps_to2 with
        | Some maps_to1, None ->
          one_side_only index maps_to1 perm1 at_least2
            ~get_equations_to_deposit1:Join_env.holds_on_left
        | None, Some maps_to2 ->
          one_side_only index maps_to2 perm2 at_least1
            ~get_equations_to_deposit1:Join_env.holds_on_right
        | Some maps_to1, Some maps_to2 ->
          let maps_to1 = MT.apply_name_permutation maps_to1 perm1 in
          let maps_to2 = MT.apply_name_permutation maps_to2 perm2 in
          Some (E.switch'_with_param MT.meet MT.join env perm1 perm2
            fresh_component_semantics maps_to1 maps_to2)
        | None, None -> None
      in
      let known =
        Tag_and_index.Map.merge (fun tag_and_index maps_to1 maps_to2 ->
            let index = Tag_and_index.index tag_and_index in
            merge index maps_to1 maps_to2)
          known1
          known2
      in
      let at_least =
        Index.Map.merge (fun index maps_to1 maps_to2 ->
            merge index maps_to1 maps_to2)
          at_least1
          at_least2
      in
      if Tag_and_index.Map.is_empty known && Index.Map.is_empty at_least then
        Bottom
      else
        Ok {
          known;
          at_least;
        }
  end

  module For_meet = Either_meet_or_join.For_meet (W)
  module For_join = Either_meet_or_join.For_join (W)

  module Meet = Meet_or_join (For_meet)
  module Join = Meet_or_join (For_join)

  let meet = Meet.meet_or_join
  let join = Join.meet_or_join

  let get_singleton { known; at_least; } =
    match
      Tag_and_index.Map.get_singleton known,
        Index.Map.get_singleton at_least
    with
    | Some maps_to, None | None, Some maps_to -> Some maps_to
    | Some _, Some _ | None, None -> None

  let is_bottom { known; at_least; } =
    Tag_and_index.Map.is_empty known && Index.Map.is_empty at_least

  let free_names { known; at_least; } =


  let bound_names { known; at_least; } =


  let apply_name_permutation { known; at_least; } perm =
    let known =
      Tag_and_index.Map.fold (fun tag_and_index maps_to known ->
          let tag_and_index =
            Tag_and_index.apply_name_permutation tag_and_index perm
          in
          let maps_to = MT.apply_name_permutation maps_to perm in
          Tag_and_index.Map.add tag_and_index maps_to known)
        known
        Tag_and_index.Map.empty
    in
    let at_least =
      Index.Map.fold (fun index maps_to at_least ->
          let index = Index.apply_name_permutation index perm in
          let maps_to = MT.apply_name_permutation maps_to perm in
          Index.Map.add index maps_to at_least)
        at_least
        Index.Map.empty
    in
    { known;
      at_least;
    }

  let freshen t freshening =
    apply_name_permutation t (Freshening.name_permutation freshening)
end
