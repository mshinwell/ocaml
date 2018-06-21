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

module Make (Index : sig
    type t

    val compare : t -> t -> t

    module Map : Map.S with type t = t
  end)
  (T : Flambda_type0_internal_intf.S)
  (Join_env : Join_env_intf.S with module T := T)
  (Parameters : Parameters_intf.S with module T := T)
  (E : Either_meet_or_join_intf.S with module T := T) =
struct
  module Tag_and_index = struct
    type t = Tag.t * Index.t

    include Map.With_set (...

    )

    let create tag index = tag, index
    let index (_tag, index) = index
  end

  module JE = Join_env
  module P = Parameters.Make (Index)

  type t = {
    known : P.t Tag_and_index.Map.t;
    at_least : P.t Index.Map.t;
  }

  let print ~cache ppf ({ known; at_least } : t) =
    Format.fprintf ppf 
      "@[<hov 1>(\
         @[<hov 1>(known@ %a)@]@ \
         @[<hov 1>(at_least@ %a)@])@]"
      (Tag_and_index.Map.print (P.print ~cache)) known
      (Index.Map.print (P.print ~cache)) at_least

  let create () =
    { known = Tag_and_index.Map.empty;
      at_least = Index.Map.empty;
    }

  let create_with_known_types tag index field_tys =
    let tag_and_index = Tag_and_index.create tag index in
    { known = Tag_and_index.Map.singleton tag_and_index params;
      at_least = 

  let is_empty { known; at_least; } =
    Tag_and_index.Map.is_empty known && Index.Map.is_empty at_least

  let meet_or_join env perm1 perm2
        ({ known = known1; at_least = at_least1; } : t)
        ({ known = known2; at_least = at_least2; } : t) : t =
    let one_side_only params1 perm1 at_least2 ~get_equations_to_deposit1 =
      let index1 = P.index params1 in
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
            P.add_or_meet_equations
              (P.apply_name_permutation params1 perm1)
              (JE.central_environment env)
              (get_equations_to_deposit1 env)
          in
          Some params1
        end
      | Some (index2, from_at_least2) ->
        assert (Index.compare index2 index1 <= 0);
        let params1 = P.apply_name_permutation params1 perm1 in
        Some (P.join env params1 from_at_least2)
      end
    in
    let merge index params1 params2 =
      match params1, params2 with
      | Some params1, None ->
        assert (Index.equal index (P.index params1));
        one_side_only params1 perm1 at_least2
          ~get_equations_to_deposit1:JE.holds_on_left
      | None, Some params2 ->
        assert (Index.equal index (P.index params2));
        one_side_only params2 perm2 at_least1
          ~get_equations_to_deposit1:JE.holds_on_right
      | Some params1, Some params2 ->
        assert (Index.equal index (P.index params1));
        assert (Index.equal index (P.index params2));
        let params1 = P.apply_name_permutation params1 perm1 in
        let params2 = P.apply_name_permutation params2 perm2 in
        Some (E.switch' P.meet_fresh P.join_fresh env params1 params2)
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
    { known;
      at_least;
    }
end
