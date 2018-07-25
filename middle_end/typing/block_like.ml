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
  (T : Flambda_type0_internal_intf.S)
  (Typing_env : Typing_env_intf.S with module T := T)
  (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
  (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
  (Join_env : Join_env_intf.S with module T := T) =
struct
  module Index = Targetint.OCaml
  module Size = Index

  module Tag_and_size = struct
    type t = Tag.t * Size.t

    include Hashtbl.Make_with_map_pair (Tag) (Size)

    let create tag index = tag, index
    let index (_tag, index) = index

    let apply_name_permutation (tag, index) perm =
      tag, Size.apply_name_permutation index perm
  end

  module JE = Join_env

  module P =
    Parameters.Make (Index) (Logical_variable)
      (T) (Typing_env) (Typing_env_extension) (Meet_and_join) (Join_env)

  type t = {
    known : P.t Tag_and_size.Map.t;
    at_least : P.t Size.Map.t;
  }

  let print ~cache ppf ({ known; at_least } : t) =
    Format.fprintf ppf 
      "@[<hov 1>(\
         @[<hov 1>(known@ %a)@]@ \
         @[<hov 1>(at_least@ %a)@])@]"
      (Tag_and_size.Map.print (P.print ~cache)) known
      (Size.Map.print (P.print ~cache)) at_least

  let create () =
    { known = Tag_and_size.Map.empty;
      at_least = Size.Map.empty;
    }

  let create_with_known_types tag field_tys =
    let size = Targetint.OCaml.of_int (List.length field_tys) in
    let tag_and_size = Tag_and_size.create tag size in
    let params = P.create_from_types field_tys in
    { known = Tag_and_size.Map.singleton tag_and_size params;
      at_least = Size.Map.empty;
    }

  let create_size_at_least size =
    (* CR mshinwell: Introduce something like [P.Cardinal_structure] to avoid
       having to create these sets? *)
    let rec create_indexes n =
      if Index.(<) n size then
        Index.Set.add n (create_indexes (Index.add n Index.one))
      else
        Index.Set.empty
    in
    let params = P.create (create_indexes Index.zero) in
    { known = Tag_and_size.Map.empty;
      at_least = Size.Map.singleton size params;
    }

  let is_empty { known; at_least; } =
    Tag_and_size.Map.is_empty known && Size.Map.is_empty at_least

  module Meet_or_join
    (E : Either_meet_or_join_intf.S with module T := T) =
  struct
    let meet_or_join env perm1 perm2
          ({ known = known1; at_least = at_least1; } : t)
          ({ known = known2; at_least = at_least2; } : t) : t =
      let one_side_only params1 perm1 at_least2 ~get_equations_to_deposit1 =
        let index1 = P.index params1 in
        let from_at_least2 =
          Size.Map.find_last_opt
            (fun index -> Size.compare index index1 <= 0)
            at_least2
        in
        begin match from_at_least2 with
        | None ->
          begin match E.op with
          | Meet -> None
          | Join ->
            let params1 =
              P.add_or_meet_equations
                (P.apply_name_permutation params1 perm1)
                (JE.central_environment env)
                (get_equations_to_deposit1 env)
            in
            Some params1
          end
        | Some (index2, from_at_least2) ->
          assert (Size.compare index2 index1 <= 0);
          let params1 = P.apply_name_permutation params1 perm1 in
          Some (E.switch' P.meet P.join env params1 from_at_least2)
        end
      in
      let merge index params1 params2 =
        match params1, params2 with
        | Some params1, None ->
          assert (Size.equal index (P.index params1));
          one_side_only params1 perm1 at_least2
            ~get_equations_to_deposit1:JE.holds_on_left
        | None, Some params2 ->
          assert (Size.equal index (P.index params2));
          one_side_only params2 perm2 at_least1
            ~get_equations_to_deposit1:JE.holds_on_right
        | Some params1, Some params2 ->
          assert (Size.equal index (P.index params1));
          assert (Size.equal index (P.index params2));
          let params1 = P.apply_name_permutation params1 perm1 in
          let params2 = P.apply_name_permutation params2 perm2 in
          Some (E.switch' P.meet P.join env params1 params2)
        | None, None -> None
      in
      let known =
        Tag_and_size.Map.merge (fun tag_and_index params1 params2 ->
            let index = Tag_and_size.index tag_and_index in
            merge index params1 params2)
          known1
          known2
      in
      let at_least =
        Size.Map.merge (fun index params1 params2 ->
            merge index params1 params2)
          at_least1
          at_least2
      in
      { known;
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
      Tag_and_size.Map.fold (fun tag_and_index params known ->
          let tag_and_index =
            Tag_and_size.apply_name_permutation tag_and_index perm
          in
          let params = P.apply_name_permutation params perm in
          Tag_and_size.Map.add tag_and_index params known)
        known
        Tag_and_size.Map.empty
    in
    let at_least =
      Size.Map.fold (fun index params at_least ->
          let index = Size.apply_name_permutation index perm in
          let params = P.apply_name_permutation params perm in
          Size.Map.add index params at_least)
        at_least
        Size.Map.empty
    in
    { known;
      at_least;
    }

  let apply_freshening t freshening =
    apply_name_permutation t (Freshening.name_permutation freshening)
end
