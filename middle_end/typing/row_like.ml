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

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Flambda_type0_core = struct end
module Join_env = struct end
module Meet_env = struct end
module Typing_env = struct end
module Typing_env_extension = struct end

module Make_types
  (T : Typing_world_abstract.S) =
struct
  open! T

  module Make_types
    (Tag : Map.With_set)
    (Index : Map.With_set)
    (Maps_to : Map.With_set)
  = struct
    module Tag_and_index = struct
      type t = Hashtbl.Make_with_map_pair (Tag) (Index).t
    end

    (* CR mshinwell: Think about what means bottom and what means unknown for
       this structure *)
    type t = {
      known : Maps_to.t Tag_and_index.Map.t;
      at_least : Maps_to.t Index.Map.t;
    }
  end
end

module type Strengthened_world = sig
  module Recursive_world : sig
    module rec Types : (Typing_world_types.Types_nonrec
      with module Abstract_types := Types
      and module Abstract_functor_types := Functor_types)
    and Functor_types : (Typing_world_types.Functor_types_nonrec
      with module Abstract_types := Types
      with module Row_like = Make_types (Types))
  end
  include Typing_world.S with module Recursive_world := Recursive_world
end

module Make (W : Strengthened_world) = struct
  open! W

  module Make
    (Tag : Map.With_set)
    (Index : Map.With_set)
    (Maps_to : sig
      type t

      val bottom : unit -> t

      val print_with_cache
         : cache:Printing_cache.t
        -> Format.formatter
        -> t
        -> unit

      val equal : Type_equality_env.t -> t -> t -> bool

      val add_or_meet_equations
         : t
        -> Meet_env.t
        -> Typing_env_extension.t
        -> t

      val meet
         : Meet_env.t
        -> Relational_product_intf.fresh_component_semantics
        -> t
        -> t
        -> (t * Typing_env_extension.t) Or_bottom.t

      val join
         : Join_env.t
        -> Relational_product_intf.fresh_component_semantics
        -> t
        -> t
        -> t

      include Contains_names.S with type t := t
    end) =
  struct
    module Tag = Tag
    module Index = Index
    module Maps_to = Maps_to

    module MT = Maps_to

    module Tag_and_index = struct
      include Hashtbl.Make_with_map_pair (Tag) (Index)

      let create tag index = tag, index
      let index (_tag, index) = index
    end

    (* CR mshinwell: Think about what means bottom and what needs unknown for
       this structure *)
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

    let equal env
          { known = known1; at_least = at_least1; }
          { known = known2; at_least = at_least2; } =
      Tag_and_index.Map.equal (MT.equal env) known1 known2
        && Index.Map.equal (MT.equal env) at_least1 at_least2

    let apply_name_permutation { known; at_least; } perm =
      let known =
        (* CR mshinwell: Can just use [Tag_and_index.Map.map] now. *)
        Tag_and_index.Map.fold (fun tag_and_index maps_to known ->
            let maps_to = MT.apply_name_permutation maps_to perm in
            Tag_and_index.Map.add tag_and_index maps_to known)
          known
          Tag_and_index.Map.empty
      in
      let at_least =
        Index.Map.fold (fun index maps_to at_least ->
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

    module Meet_or_join
      (E : Either_meet_or_join_intf.S
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env := Typing_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      let meet_or_join env fresh_component_semantics t1 t2 =
        let t1 = apply_name_permutation t1 (Join_env.perm_left env) in
        let t2 = apply_name_permutation t2 (Join_env.perm_right env) in
        let env = Join_env.clear_name_permutations env in
        let ({ known = known1; at_least = at_least1; } : t) = t1 in
        let ({ known = known2; at_least = at_least2; } : t) = t2 in
        let one_side_only index1 maps_to1 at_least2
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
                  maps_to1
                  (Join_env.central_environment env)
                  (get_equations_to_deposit1 env)
              in
              Some maps_to1
            end
          | Some (index2, from_at_least2) ->
            assert (Index.compare index2 index1 <= 0);
            (* CR mshinwell: What happens to any generated equations in the
               [meet] case (same below)? *)
            let maps_to =
              E.switch'_with_param MT.meet MT.join env
                fresh_component_semantics maps_to1 from_at_least2
            in
            match maps_to with
            | Bottom -> None
            | Ok maps_to -> Some maps_to
          end
        in
        let merge index maps_to1 maps_to2 =
          match maps_to1, maps_to2 with
          | Some maps_to1, None ->
            one_side_only index maps_to1 at_least2
              ~get_equations_to_deposit1:Join_env.holds_on_left
          | None, Some maps_to2 ->
            one_side_only index maps_to2 at_least1
              ~get_equations_to_deposit1:Join_env.holds_on_right
          | Some maps_to1, Some maps_to2 ->
            let maps_to =
              E.switch'_with_param MT.meet MT.join env
                fresh_component_semantics maps_to1 maps_to2
            in
            begin match maps_to with
            | Bottom -> None
            | Ok maps_to -> Some maps_to
            end
          | None, None -> None
        in
        (* CR mshinwell: Shouldn't we be applying name permutations to
           these two as well? *)
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
        { known;
          at_least;
        }
    end

    let all_maps_to { known; at_least; } =
      (Tag_and_index.Map.data known) @ (Index.Map.data at_least)

    module For_meet = Either_meet_or_join.For_meet (W)
    module For_join = Either_meet_or_join.For_join (W)

    module Meet = Meet_or_join (For_meet)
    module Join = Meet_or_join (For_join)

    let meet env fresh_component_semantics t1 t2 : _ Or_bottom.t =
      let t =
        Meet.meet_or_join (Join_env.create env)
          fresh_component_semantics t1 t2
      in
      if Tag_and_index.Map.is_empty t.known && Index.Map.is_empty t.at_least
      then Bottom
      else
        let join_of_all_maps_to =
          (* Any name permutations have already been applied during
             [Meet.meet_or_join], above. *)
          let env = Join_env.clear_name_permutations (Join_env.create env) in
          List.fold_left (fun result maps_to ->
              MT.join env fresh_component_semantics maps_to result)
            (MT.bottom ())
            (all_maps_to t)
        in
        Ok (t, join_of_all_maps_to)

    let join = Join.meet_or_join

    let is_bottom { known; at_least; } =
      Tag_and_index.Map.is_empty known && Index.Map.is_empty at_least

    let free_names t =
      let { known; at_least; } = t in
      let from_known =
        Tag_and_index.Map.fold (fun _tag_and_index maps_to free_names ->
            Name_occurrences.union free_names
              (Maps_to.free_names maps_to))
          known
          (Name_occurrences.create ())
      in
      let from_at_least =
        Index.Map.fold (fun _index maps_to free_names ->
            Name_occurrences.union free_names
              (Maps_to.free_names maps_to))
          at_least
          (Name_occurrences.create ())
      in
      Name_occurrences.union from_known from_at_least

    let bound_names _ =
      Misc.fatal_error "let's try to do without this"

    module Join_env = W.Join_env
    module Meet_env = W.Meet_env
    module Typing_env = W.Typing_env
    module Typing_env_extension = W.Typing_env_extension
  end

  module Join_env = W.Join_env
  module Meet_env = W.Meet_env
  module Typing_env = W.Typing_env
  module Typing_env_extension = W.Typing_env_extension
end
