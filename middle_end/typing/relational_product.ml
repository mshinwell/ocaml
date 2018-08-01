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

module type Name_like = sig
  type t

  include Map.With_set with type t := t
  include Contains_names.S with type t := t
end

(* CR mshinwell: After 4.08, when aliases to functor parameters are supported,
   we should be able to:
   - remove these dummy module definitions, which are there to stop ocamldep
     from producing erroneous circular dependencies in the presence of
     the "open W" below;
   - remove "open W" below (thus removing the ocamldep problem);
   - reinstate e.g. "module JE = W.Join_env" after "= struct" below;
   - substitute out the long names for the abbreviations.
   The same in all other files involved in the recursive definition.
*)
[@@@ocaml.warning "-60"]
module JE = struct end
module T = struct end
module TE = struct end
module TEE = struct end

module Make
  (Index : Name_like)
  (Component : sig
    include Name_like
    val create : Flambda_kind.t -> t
    val equal : t -> t -> bool
    val name : t -> Name.t
    val kind : t -> Flambda_kind.t
  end)
  (W : Typing_world.S)
= struct
  open W

(*
  module JE = W.Join_env
  module T = W.Flambda_type0_core
  module TE = W.Typing_env
  module TEE = W.Typing_env_extension
*)

  type fresh_component_semantics =
    | Fresh
    | Left
    | Right

  module Indexed_product = struct
    type t = {
      components_by_index : Component.t Index.Map.t;
      env_extension : Typing_env_extension.t;
    }

    let invariant _t =
      (* CR mshinwell: This should check that the [env_extension] never contains
         [Definition]s for [Name]s occurring in the indexes. *)
      ()

    let create components_by_index env_extension : t =
      let t =
        { components_by_index;
          env_extension;
        }
      in
      invariant t;
      t

    let print ppf { components_by_index; env_extension; } =
      Format.fprintf ppf
        "@[<hov 1>(\
          @[<hov 1>(components_by_index@ %a)@]@ \
          @[<hov 1>(env_extension@ %a)@])@]"
        (Index.Map.print Component.print) components_by_index
        Typing_env_extension.print env_extension

    let equal
          { components_by_index = components_by_index1;
            env_extension = env_extension1;
          }
          { components_by_index = components_by_index2;
            env_extension = env_extension2;
          } =
      Index.Map.equal Component.equal components_by_index1 components_by_index2
        && Typing_env_extension.equal env_extension1 env_extension2

    let indexes t = Index.Map.keys t.components_by_index

    (* CR mshinwell: The [kind] may not be needed in [Component] but it isn't
       clear yet.  We can sort this out later.  At present all relational
       products map to components of kind [Value]. *)
    let kind = Flambda_kind.value ()

    let environment_for_meet_or_join env (t1 : t) (t2 : t)
          ~fresh_component_semantics ~indexes =
      let components_by_index_in_result, env =
        Index.Set.fold (fun index (components_by_index_in_result, env) ->
            let component =
              match fresh_component_semantics with
              | Fresh -> Component.create kind
              | Left ->
                begin match Index.Map.find index t1.components_by_index with
                | exception Not_found -> Component.create kind
                | component_on_left -> component_on_left
                end
              | Right ->
                begin match Index.Map.find index t2.components_by_index with
                | exception Not_found -> Component.create kind
                | component_on_right -> component_on_right
                end
            in
            let components_by_index_in_result =
              Index.Map.add index component components_by_index_in_result
            in
            let env =
              Join_env.add_definition_central_environment env
                (Component.name component) (Flambda_type0_core.bottom kind)
            in
            components_by_index_in_result, env)
          indexes
          (Index.Map.empty, env)
      in
      let result_components =
        Component.Set.of_list (Index.Map.data components_by_index_in_result)
      in
      let add_definitions_to_extension t =
        Index.Map.fold (fun _index component env_extension ->
            if Component.Set.mem component result_components then
              env_extension
            else
              let name = Component.name component in
              let kind = Component.kind component in
              Typing_env_extension.add_definition_at_beginning env_extension
                name (Flambda_type0_core.bottom kind))
          t.components_by_index
          t.env_extension
      in
      let env_extension1 = add_definitions_to_extension t1 in
      let env_extension2 = add_definitions_to_extension t2 in
      let add_equalities_to_extension t env_extension =
        Index.Map.fold (fun index component env_extension ->
            let name = Component.name component in
            let kind = Component.kind component in
            match Index.Map.find index t.components_by_index with
            | exception Not_found -> env_extension
            | stale_component ->
              if Component.equal component stale_component then env_extension
              else
                let stale_name = Component.name stale_component in
                let name_ty =
                  Flambda_type0_core.alias_type_of kind (Simple.name stale_name)
                in
                Typing_env_extension.add_equation env_extension name name_ty)
          components_by_index_in_result
          env_extension
      in
      let env_extension1 = add_equalities_to_extension t1 env_extension1 in
      let env_extension2 = add_equalities_to_extension t2 env_extension2 in
      env, env_extension1, env_extension2, components_by_index_in_result

    let meet env t1 t2 ~fresh_component_semantics : _ Or_bottom.t =
      if t1 == t2 then Ok (t1, env, t1.env_extension, t1.env_extension)
      else
        let indexes = Index.Set.inter (indexes t1) (indexes t2) in
        if Index.Set.is_empty indexes then Bottom
        else
          let env = Join_env.create env in
          let env, env_extension1, env_extension2, components_by_index =
            environment_for_meet_or_join env t1 t2 ~fresh_component_semantics
              ~indexes
          in
          let env = Join_env.central_environment env in
          let env_extension =
            Typing_env_extension.meet env env_extension1 env_extension2
          in
          let t =
            { components_by_index;
              env_extension;
            }
          in
          Ok (t, env, env_extension1, env_extension2)

    let join env t1 t2 ~fresh_component_semantics =
      if t1 == t2 then
        t1, env, t1.env_extension, t1.env_extension
      else
        let indexes = Index.Set.union (indexes t1) (indexes t2) in
        let env, env_extension1, env_extension2, components_by_index =
          environment_for_meet_or_join env t1 t2 ~fresh_component_semantics
            ~indexes
        in
        let env_extension =
          Typing_env_extension.join env env_extension1 env_extension2
        in
        let t =
          { components_by_index;
            env_extension;
          }
        in
        t, env, env_extension1, env_extension2

    let standalone_extension t =
      Index.Map.fold (fun _index component env_extension ->
          let name = Component.name component in
          let kind = Component.kind component in
          Typing_env_extension.add_definition_at_beginning env_extension name
            (Flambda_type0_core.bottom kind))
        t.components_by_index
        t.env_extension

    let free_names_in_indexes t =
      Index.Set.fold (fun index free_names ->
          Name_occurrences.union (Index.free_names index) free_names)
        (Index.Map.keys t.components_by_index)
        (Name_occurrences.create ())

    let free_names_in_components t =
      List.fold_left (fun free_names component ->
          Name_occurrences.union (Component.free_names component) free_names)
        (Name_occurrences.create ())
        (Index.Map.data t.components_by_index)

    (* CR mshinwell: Do we really need [bound_names]?  It seems like this
       might be problematic with the two notions of freshness (the usual one
       for terms; and for existentials, which are binding but not freshened
       until opened). *)
    let bound_names t = free_names_in_components t

    let free_names ({ components_by_index = _; env_extension; } as t) =
      Name_occurrences.union (free_names_in_indexes t)
        (Name_occurrences.diff (Typing_env_extension.free_names env_extension)
          (bound_names t))

    let append_extension t env extension =
      { t with
        env_extension = Typing_env_extension.meet env t.env_extension extension;
      }

    let apply_name_permutation { components_by_index; env_extension; } perm =
      let components_by_index =
        Index.Map.fold (fun index component components_by_index ->
            let index = Index.apply_name_permutation index perm in
            let component = Component.apply_name_permutation component perm in
            Index.Map.add index component components_by_index)
          components_by_index
          Index.Map.empty
      in
      let env_extension =
        Typing_env_extension.apply_name_permutation env_extension perm
      in
      { components_by_index;
        env_extension;
      }

    let freshen t freshening =
      apply_name_permutation t (Freshening.name_permutation freshening)

    let add_or_meet_equations t env new_equations =
      let env_extension =
        Typing_env_extension.meet env t.env_extension new_equations
      in
      { t with env_extension; }

    let components t = Index.Map.data t.components_by_index
  end

  module IP = Indexed_product

  type t = IP.t list

  let invariant t =
    List.iter (fun ip -> IP.invariant ip) t

  let print ppf t =
    Format.fprintf ppf "@[(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        IP.print) t

  let create components_by_index_list =
    List.map (fun (components_by_index, env_extension) ->
        IP.create components_by_index env_extension)
      components_by_index_list

  let equal t1 t2 =
    Misc.Stdlib.List.equal IP.equal t1 t2

  let components t =
    List.map (fun ip -> IP.components ip) t

  let meet env t1 t2 ~fresh_component_semantics : _ Or_bottom.t =
    if t1 == t2 then begin
      Ok t1
    end else begin
      if List.compare_lengths t1 t2 <> 0 then begin
        Misc.fatal_errorf "Cannot meet relational products of different lengths"
      end;
      let t_rev, _env, _env_extension1, _env_extension2 =
        List.fold_left2
          (fun ((t_rev : _ Or_bottom.t), env, env_extension1, env_extension2)
                ip1 ip2 ->
            match t_rev with
            | Bottom -> Or_bottom.Bottom, env, env_extension1, env_extension2
            | Ok t_rev ->
              let ip1 = IP.append_extension ip1 env env_extension1 in
              let ip2 = IP.append_extension ip2 env env_extension2 in
              match IP.meet env ip1 ip2 ~fresh_component_semantics with
              | Bottom -> Or_bottom.Bottom, env, env_extension1, env_extension2
              | Ok (ip, env, env_extension1, env_extension2) ->
                Or_bottom.Ok (ip :: t_rev), env, env_extension1, env_extension2)
          (Or_bottom.Ok [], env,
            Typing_env_extension.empty, Typing_env_extension.empty)
          t1 t2
      in
      match t_rev with
      | Bottom -> Bottom
      | Ok t_rev -> Ok (List.rev t_rev)
    end

  let join env t1 t2 ~fresh_component_semantics =
    if t1 == t2 then begin
      t1
    end else begin
      if List.compare_lengths t1 t2 <> 0 then begin
        Misc.fatal_errorf "Cannot join relational products of different lengths"
      end;
      let t_rev, _env, _env_extension1, _env_extension2 =
        List.fold_left2
          (fun (t_rev, env, env_extension1, env_extension2) ip1 ip2 ->
            let ip1 =
              IP.append_extension ip1 (Join_env.central_environment env)
                env_extension1
            in
            let ip2 =
              IP.append_extension ip2 (Join_env.central_environment env)
                env_extension2
            in
            let ip, env, env_extension1, env_extension2 =
              IP.join env ip1 ip2 ~fresh_component_semantics
            in
            ip :: t_rev, env, env_extension1, env_extension2)
          ([], env, Typing_env_extension.empty, Typing_env_extension.empty)
          t1 t2
      in
      List.rev t_rev
    end

  let standalone_extension t env =
    List.fold_left (fun extension ip ->
        Typing_env_extension.meet env extension (IP.standalone_extension ip))
      Typing_env_extension.empty
      t

  let introduce t env =
    let scope_level = Typing_env.max_level env in
    Typing_env.add_or_meet_env_extension env (standalone_extension t env)
      scope_level

  let apply_name_permutation t perm =
    List.map (fun ip -> IP.apply_name_permutation ip perm) t

  let freshen t freshening =
    List.map (fun ip -> IP.freshen ip freshening) t

  let add_or_meet_equations t env new_equations =
    List.map (fun ip -> IP.add_or_meet_equations ip env new_equations) t

  let free_names t =
    let free_names, _bound_names =
      List.fold_left (fun (free_names, bound_names) ip ->
          let free_names =
            Name_occurrences.union free_names
              (Name_occurrences.diff (IP.free_names ip) bound_names)
          in
          let bound_names =
            Name_occurrences.union (IP.bound_names ip) bound_names
          in
          free_names, bound_names)
        (Name_occurrences.create (), Name_occurrences.create ())
        t
    in
    free_names

  let bound_names t =
    List.fold_left (fun names ip ->
        Name_occurrences.union names (IP.bound_names ip))
      (Name_occurrences.create ())
      t
end
