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

(* Next attempt:

- always use Kinded_parameter.t
- parameterise the main thing on the singleton container.  We should be
  able to derive all of the operations.  The only point of the parameterisation
  is to impose the ordering, or not.
  * In fact: it should take the _functor_ to make the container
- then have the extra layer which keeps track of the <thing> -> KP map *)

module Make0
  (Index : sig
    type t

    val name : t -> Name.t
    val kind : t -> Flambda_kind.t

    val freshen : t -> t

    type container
    type pair_container

    module Container : sig
      type t = container

      val nth : t -> index:Targetint.OCaml.t -> Index.t option

      val augment_map : t -> f:(Index.t -> Index.t) -> pair_container

      val fold
         : t
        -> init:'a
        -> f:('a -> Index.t -> 'a)
        -> 'a

      val inter_and_check_kinds : t -> t -> t
      val union_and_check_kinds : t -> t -> t

      val apply_name_permutation : t -> Name_permutation.t -> t
      val apply_freshening : t -> Freshening.t -> t
    end

    module Pair_container : sig
      type t = pair_container

      val fold
         : t
        -> init:'a
        -> f:('a -> Index.t * Index.t -> 'a)
        -> 'a

      val filter_map_pair_to_singleton
         : t
        -> f:(Index.t * Index.t -> Index.t option)
        -> container
    end
  end)
  (T : Flambda_type0_internal_intf.S)
  (Typing_env : Typing_env_intf.S with module T := T)
  (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
  (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
  (Join_env : Join_env_intf.S with module T := T)
= struct
  open T

  module JE = Join_env
  module TE = Typing_env
  module TEE = Typing_env_extension

  module Pair_container =
    Make_pair_container (Kinded_parameter) (Kinded_parameter)

  module Singleton_container = struct
    include Make_container (Kinded_parameter)

    let augment_map t ~f =
      fold (fun index pair_container ->
          Pair_container.add (index, f index) pair_container)
        t
        Pair_container.empty
  end

  module SC = Singleton_container
  module PC = Pair_container

  type t = {
    params : SC.t;
    env_extension : TEE.t;
  }

  let invariant _t =
    (* CR mshinwell: This should check that the [env_extension] never contains
       [Definition]s for the [params]. *)
    ()

  let create params : t =
    { params;
      env_extension = TEE.empty;
    }

  let create_with_env_extension params env_extension : t =
    let t =
      { params;
        env_extension;
      }
    in
    invariant t;
    t

  let create_same_params_no_extension (t : t) : t =
    { params = t.params;
      env_extension = TEE.empty;
    }

  let create_from_types = T.create_parameters_from_types

  let print ppf t =
    T.print_parameters ~cache:(Printing_cache.create ()) ppf t

  let introduce (t : t) env =
    let scope_level = Typing_env.max_level env in
    let env =
      SC.fold t.params
        ~init:env
        ~f:(fun env param ->
          let name = Index.name param in
          let kind = Index.kind param in
          let ty = T.bottom kind in
          TE.add env name scope_level (Definition ty))
    in
    TE.add_or_meet_env_extension env t.env_extension scope_level

  let arity (t : t) =
    List.map (fun kinded_param -> Kinded_parameter.kind kinded_param) t.params

  let kinded_params t = t.params

  let nth t index = SC.nth t.params ~index

  let add_or_meet_equations t env env_extension =
    { t with
      env_extension = TEE.meet env t.env_extension env_extension;
    }

  type fresh_name_semantics =
    | Fresh
    | Left
    | Right

  type meet_or_join =
    | Meet
    | Join

  let environment_for_meet_or_join ?(fresh_name_semantics = Fresh)
        (op : meet_or_join) (t1 : t) (t2 : t) =
    let all_params =
      SC.union_and_check_kinds t1.params t2.params
    in
    let params_to_bind =
      match op with
      | Meet -> SC.inter_and_check_kinds t1.params t2.params
      | Join -> all_params
    in
    let with_fresh_params =
      match fresh_name_semantics with
      | Fresh ->
        SC.augment_map with_fresh_params ~f:(fun param ->
          Index.freshen_name param)
      | Left -> SC.augment_map t1.params ~f:(fun param -> param)
      | Right -> SC.augment_map t2.params ~f:(fun param -> param)
    in
    let env =
      SC.fold with_fresh_params
        ~init:env
        ~f:(fun env (_our_param, fresh_param) ->
          let fresh_name = Index.name fresh_param in
          let kind = Index.kind fresh_param in
          JE.add_definition_central_environment env fresh_name (T.bottom kind))
    in
    let add_definitions_and_equalities_to_extension (t : t) =
      SC.fold with_fresh_params
        ~init:t.env_extension
        ~f:(fun env_extension (our_param, fresh_param) ->
          assert (Index.equal_kinds our_param fresh_param);
          let our_name = Index.name our_param in
          let our_param_kind = Index.kind our_param in
          let env_extension =
            TEE.add_definition_at_beginning env_extension our_name
              (T.bottom our_param_kind)
          in
          let fresh_name = Index.name fresh_param in
          let fresh_name_ty =
            T.alias_type_of our_param_kind (Simple.name our_name)
          in
          TEE.add_equation env_extension fresh_name fresh_name_ty)
    in
    let env_extension1 =
      match fresh_name_semantics with
      | Fresh | Right -> add_definitions_and_equalities_to_extension t1
      | Left -> t1.env_extension
    in
    let env_extension2 =
      match fresh_name_semantics with
      | Fresh | Left -> add_definitions_and_equalities_to_extension t2
      | Right -> t2.env_extension
    in
    let fresh_params_to_bind =
      SC.filter_map_pair_to_singleton with_fresh_params
        ~f:(fun (our_param, fresh_param) ->
          if SC.mem params_to_bind our_param then Some fresh_param
          else None)
    in
    env, env_extension1, env_extension2, fresh_params_to_bind

  let meet_or_join ?fresh_name_semantics env t1 t2 ~op =
    if t1 == t2 then t1
    else
      let env, env_extension1, env_extension2, params =
        environment_for_meet_or_join ?fresh_name_semantics env t1 t2
      in
      let env_extension = op env env_extension1 env_extension2 in
      { params;
        env_extension;
      }

  let meet ?fresh_name_semantics env t1 t2 =
    let env = JE.create env in
    meet_or_join ?fresh_name_semantics env t1 t2
      ~op:(fun join_env env_extension1 env_extension2 ->
        TEE.meet (JE.central_environment join_env)
          env_extension1 env_extension2)

  let join ?fresh_name_semantics env t1 t2 =
    meet_or_join ?fresh_name_semantics env t1 t2 ~op:TEE.join

  let meet_fresh env t1 t2 : t = meet env t1 t2
  let join_fresh env t1 t2 : t = join env t1 t2

  let standalone_extension t =
    SC.fold t.params
      ~init:t.env_extension
      ~f:(fun env_extension param ->
        let name = Index.name param in
        let param_kind = Index.kind param in
        TEE.add_definition_at_beginning env_extension name
          (T.bottom param_kind))

  let apply_name_permutation { params; env_extension; } perm =
    { params = SC.apply_name_permutation params perm;
      env_extension = TEE.apply_name_permutation env_extension perm;
    }

  let freshen t perm =
    let fresh_params =
      Freshening.freshen_kinded_parameters (Freshening.create ()) t.params
    in
    apply_name_permutation (Freshening.name_permutation fresh_params)
end

module With_names
  (Index : sig
    (* Values of type [t] must not contain any names. *)
    type t

    val kind : t -> Flambda_kind.t

    type container
    type pair_container

    module Container : sig
      type t = container

      val nth : t -> index:Targetint.OCaml.t -> Index.t option

      val augment_map : t -> f:(Index.t -> Index.t) -> pair_container

      val fold
         : t
        -> init:'a
        -> f:('a -> Index.t -> 'a)
        -> 'a

      val inter_and_check_kinds : t -> t -> t
      val union_and_check_kinds : t -> t -> t
    end

    module Pair_container : sig
      type t = pair_container

      val fold
         : t
        -> init:'a
        -> f:('a -> Index.t * Index.t -> 'a)
        -> 'a

      val filter_map_pair_to_singleton
         : t
        -> f:(Index.t * Index.t -> Index.t option)
        -> container
    end
  end)
= struct
  type t = Index.t * Name.t

  type container = {
    container : Index.Container.t;
    indexes_to_names : Name.t Index.Map.t;
  }

  type pair_container = {
    container : Index.Pair_container.t;
    indexes_to_names : Name.t Index.Map.t;
  }

  module Container = struct
    type t = container

    let nth t ~index =
      Index.Container.nth t.container ~index

    let augment_map t ~f : pair_container =
      { container = Index.Container.augment_map t.container;
        indexes_to_names = t.indexes_to_names;
      }

    let fold t ~init ~f =
      Index.Container.fold t ~init ~f:(fun acc index ->
          let name =
            match Index.Map.find index t.indexes_to_names with
            | name -> name
            | exception Not_found ->
              Misc.fatal_errorf "No name found for index %a"
                Index.print index
          in
          f (index, name))

    let inter_and_check_kinds
          { container = container1; indexes_to_names = indexes_to_names1; }
          { container = container2; indexes_to_names = indexes_to_names2; } =
      { container =
          Index.Container.inter_and_check_kinds container1 container2;
        indexes_to_names =
          Index.Map.inter ...; (* XXX *)
      }

    let union_and_check_kinds ... = ...
  end

end

module Ordered_kinded_parameters = struct
  type t = Kinded_parameter.t

  let name t = Kinded_parameter.name t
  let kind t = Kinded_parameter.kind t

  let freshen_name t = Kinded_parameter.freshen t

  type container = Kinded_parameter.t array
  type pair_container = (Kinded_parameter.t * Kinded_parameter.t) array

  module Container = struct
    type t = container

    let nth t ~index =
      match Targetint.OCaml.to_int_opt index with
      | Some index ->
        if index < 0 || index >= Array.length t then begin
          Misc.fatal_error "Index %d out of range" index
        end;
        t.(index)
      | None ->
        (* CR mshinwell: This should be a proper compilation error *)
        Misc.fatal_error "Parameter list too long for host"

    let augment_map t ~f =
      Array.map (fun param -> param, f param) t

    let fold t ~init ~f =
      Array.fold_left (fun acc param -> f acc param) init t

    let inter_and_check_kinds t1 t2 =

    let union_and_check_kinds t1 t2 =

    let apply_name_permutation t perm =
      Array.map (fun param ->
          Kinded_parameter.apply_name_permutation param perm)
        t

    let apply_freshening t freshening =
      apply_name_permutation t (Freshening.name_permutation freshening)
  end

  module Pair_container = struct
    type t = pair_container

    let fold t ~init ~f =
      Array.fold_left (fun acc param_pair -> f acc param_pair) init t

    let filter_map_pair_to_singleton t ~f =
      Array.of_list (Misc.Stdlib.List.filter_map f (Array.to_list t))
  end
end

module Unordered_vars_within_closure = struct
  type t = Var_within_closure.t

  let kind _ = Flambda_kind.value ()

  type container = Var_within_closure.Set.t

  module Pair =
    Hashtbl.With_map.Make_pair (Var_within_closure) (Var_within_closure)

  type pair_container = Pair.Set.t

  module Container = struct
    type t = container

    include Singleton

    let nth t ~index =
      match Targetint.OCaml.to_int_opt index with
      | Some index ->
        if index < 0 || index >= Array.length t then begin
          Misc.fatal_error "Index %d out of range" index
        end;
        t.(index)
      | None ->
        (* CR mshinwell: This should be a proper compilation error *)
        Misc.fatal_error "Parameter list too long for host"

    let augment_map t ~f =
      Array.map (fun param -> param, f param) t

    let fold t ~init ~f =
      Array.fold_left (fun acc param -> f acc param) init t

    let inter_and_check_kinds t1 t2 =

    let union_and_check_kinds t1 t2 =
  end

  module Pair_container = struct
    type t = pair_container

    include Pair

    let fold t ~init ~f =
      Array.fold_left (fun acc param_pair -> f acc param_pair) init t

    let filter_map_pair_to_singleton t ~f =
      Array.of_list (Misc.Stdlib.List.filter_map f (Array.to_list t))
  end
end

(* Thinking again, we need three of these, indexed by:

1. Kinded_parameter.t, with ordering (for parameters / results)
2. Var_within_closure.t, without ordering (for closure elements)
3. Targetint.OCaml.t, with ordering (for blocks)

1. doesn't need names assigning to the indexes.  2. and 3. do, however.
So maybe there should be an intermediate functor which does name assignment,
which would only be used for 2. and 3.

*)
