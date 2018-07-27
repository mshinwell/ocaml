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
  (T : Typing_world.S)
  (Tag : sig
    type t
    include Contains_names.S with type t := t
  end)
  (Index : sig
    type t

    val equal : t -> t -> bool
    val compare : t -> t -> int

    include Map.With_set with type t := t
    include Contains_names.S with type t := t
  end)
  (Maps_to : sig
    type t

    val add_or_meet_equations
       : t
      -> Typing_env.t
      -> Typing_env_extension.t
      -> t

    include Contains_names.S with type t := t
  end)
struct
  open T

  module MT = Maps_to

  module Tag_and_index = struct
    type t = Tag.t * Index.t

    include Hashtbl.Make_with_map_pair (Tag) (Index)

    let create tag index = tag, index
    let index (_tag, index) = index

    let apply_name_permutation (tag, index) perm =
      tag, Index.apply_name_permutation index perm
  end

  module JE = Join_env

  type t = {
    known : MT.t Tag_and_index.Map.t;
    at_least : MT.t Index.Map.t;
  }

  let print ~cache ppf ({ known; at_least } : t) =
    Format.fprintf ppf 
      "@[<hov 1>(\
         @[<hov 1>(known@ %a)@]@ \
         @[<hov 1>(at_least@ %a)@])@]"
      (Tag_and_index.Map.print (MT.print ~cache)) known
      (Index.Map.print (MT.print ~cache)) at_least

  let create () =
    { known = Tag_and_index.Map.empty;
      at_least = Index.Map.empty;
    }

  let create_exactly tag index maps_to =
    let tag_and_index = Tag_and_index.create tag index in
    { known = Tag_and_index.Map.singleton tag_and_index maps_to;
      at_least = Index.Map.empty;
    }

  let create_at_least index maps_to =
    { known = Tag_and_index.Map.empty;
      at_least = Index.Map.singleton index maps_to;
    }

  let is_empty { known; at_least; } =
    Tag_and_index.Map.is_empty known && Index.Map.is_empty at_least

  module Meet_or_join
    (E : Either_meet_or_join_intf.S with module T := T) =
  struct
    let meet_or_join env perm1 perm2
          ({ known = known1; at_least = at_least1; } : t)
          ({ known = known2; at_least = at_least2; } : t) : t Or_bottom.t =
      let one_side_only params1 perm1 at_least2 ~get_equations_to_deposit1 =
        let index1 = MT.index params1 in
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
            let params1 =
              MT.add_or_meet_equations
                (MT.apply_name_permutation params1 perm1)
                (JE.central_environment env)
                (get_equations_to_deposit1 env)
            in
            Some params1
          end
        | Some (index2, from_at_least2) ->
          assert (Index.compare index2 index1 <= 0);
          let params1 = MT.apply_name_permutation params1 perm1 in
          Some (E.switch' MT.meet MT.join env params1 from_at_least2)
        end
      in
      let merge index params1 params2 =
        match params1, params2 with
        | Some params1, None ->
          assert (Index.equal index (MT.index params1));
          one_side_only params1 perm1 at_least2
            ~get_equations_to_deposit1:JE.holds_on_left
        | None, Some params2 ->
          assert (Index.equal index (MT.index params2));
          one_side_only params2 perm2 at_least1
            ~get_equations_to_deposit1:JE.holds_on_right
        | Some params1, Some params2 ->
          assert (Index.equal index (MT.index params1));
          assert (Index.equal index (MT.index params2));
          let params1 = MT.apply_name_permutation params1 perm1 in
          let params2 = MT.apply_name_permutation params2 perm2 in
          Some (E.switch' MT.meet MT.join env params1 params2)
        | None, None -> None
      in
      let known =
        Tag_and_index.Map.merge (fun tag_and_index params1 params2 ->
            let index = Tag_and_index.index tag_and_index in
            merge index params1 params2)
          known1
          known2
      in
      let at_least =
        Index.Map.merge (fun index params1 params2 ->
            merge index params1 params2)
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

  module For_meet =
    Either_meet_or_join.Meet (T) (Typing_env) (Typing_env_extension) (Join_env)

  module For_join =
    Either_meet_or_join.Join (T) (Typing_env) (Typing_env_extension) (Join_env)

  module Meet = Meet_or_join (For_meet)
  module Join = Meet_or_join (For_join)

  let meet = Meet.meet_or_join
  let join = Join.meet_or_join

  let apply_name_permutation { known; at_least; } perm =
    let known =
      Tag_and_index.Map.fold (fun tag_and_index params known ->
          let tag_and_index =
            Tag_and_index.apply_name_permutation tag_and_index perm
          in
          let params = MT.apply_name_permutation params perm in
          Tag_and_index.Map.add tag_and_index params known)
        known
        Tag_and_index.Map.empty
    in
    let at_least =
      Index.Map.fold (fun index params at_least ->
          let index = Index.apply_name_permutation index perm in
          let params = MT.apply_name_permutation params perm in
          Index.Map.add index params at_least)
        at_least
        Index.Map.empty
    in
    { known;
      at_least;
    }

  let apply_freshening t freshening =
    apply_name_permutation t (Freshening.name_permutation freshening)
end
