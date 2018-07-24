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

module LV = Logical_variable

module type External_var_sig = sig
  type t

  include Map.With_set with type t := t
  include Contains_names.S with type t := t

  val kind : t -> Flambda_kind.t
end

module Make
  (External_var : External_var_sig)
  (Make_structure : functor
    External_var_sig with type t = External_var.t
    ->
    sig
      type t

      val print : Format.formatter -> t -> unit
      val fold : ('a -> External_type.t -> 'a) -> 'a -> t -> 'a
      val to_set : t -> External_type.Set.t

      val meet : t -> t -> t Or_bottom.t
      val join : t -> t -> t Or_unknown.t
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

  module EV = External_var
  module EVS = Make_structure (EV)

  type t = {
    external_structure : EVS.t;
    logical_vars : LV.t EV.Map.t;
    env_extension : TEE.t;
  }

  let invariant _t =
    (* CR mshinwell: This should check that the [env_extension] never contains
       [Definition]s for the [logical_vars]. *)
    ()

  let create_with_env_extension external_structure env_extension : t =
    let logical_vars =
      let external_vars = EVS.to_set external_structure in
      EV.Set.fold (fun external_var logical_vars ->
          let kind = EV.kind external_var in
          let fresh_logical_var = LV.create kind in
          let logical_vars =
            EV.Map.add external_var fresh_logical_var logical_vars_in_result
          in
          logical_vars)
        external_vars
        EV.Map.empty
    in
    let t =
      { external_structure;
        logical_vars;
        env_extension;
      }
    in
    invariant t;
    t

  let create external_structure =
    create_with_env_extension external_structure TEE.empty

  let print ppf { external_structure; logical_vars; env_extension; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(external_structure@ %a)@]@ \
        @[<hov 1>(logical_vars@ %a)@]@ \
        @[<hov 1>(env_extension@ %a)@])@]"
      EVS.print external_structure
      (EV.Map.print LV.print) logical_vars
      TEE.print env_extension

  let external_structure t = t.external_structure

  type meet_or_join =
    | Meet
    | Join

  let environment_for_meet_or_join (t1 : t) (t2 : t) ~external_structure =
    let external_vars_in_result = EVS.to_set external_structure in
    let logical_vars_in_result, env =
      EV.Set.fold (fun external_var (logical_vars_in_result, env) ->
          let kind = EV.kind external_var in
          let fresh_logical_var = LV.create kind in
          let logical_vars_in_result =
            EV.Map.add external_var fresh_logical_var logical_vars_in_result
          in
          let env =
            JE.add_definition_central_environment env fresh_name (T.bottom kind)
          in
          logical_vars_in_result, env)
        external_vars_in_result
        (EV.Map.empty, env)
    in
    let add_definitions_and_equalities_to_extension (t : t) =
      EV.Map.fold (fun (external_var, fresh_logical_var) env_extension ->
          let stale_name = LV.name stale_logical_var in
          let kind = LV.kind stale_logical_var in
          let env_extension =
            TEE.add_definition_at_beginning env_extension stale_name
              (T.bottom kind)
          in
          match LV.find external_var t.logical_vars with
          | exception Not_found -> env_extension
          | stale_logical_var ->
            let fresh_name = LV.name fresh_logical_var in
            let fresh_name_ty =
              T.alias_type_of kind (Simple.name stale_name)
            in
            TEE.add_equation env_extension fresh_name fresh_name_ty)
        with_fresh_logical_vars
        t.env_extension
    in
    let env_extension1 = add_definitions_and_equalities_to_extension t1 in
    let env_extension2 = add_definitions_and_equalities_to_extension t2 in
    env, env_extension1, env_extension2, external_structure,
      logical_vars_in_result

  let meet env t1 t2 : _ Or_bottom.t =
    if t1 == t2 then Ok t1
    else
      let env = JE.create env in
      match EVS.meet t1.external_structure t2.external_structure with
      | None -> Bottom
      | Ok external_structure ->
        let env, env_extension1, env_extension2, logical_vars =
          environment_for_meet_or_join env t1 t2 ~external_structure
        in
        let env_extension =
          TEE.meet (JE.central_environment env) env_extension1 env_extension2
        in
        Ok {
          external_structure;
          logical_vars;
          env_extension;
        }

  let join env t1 t2 : _ Or_unknown.t =
    if t1 == t2 then Ok t1
    else
      let env = JE.create env in
      match EVS.join t1.external_structure t2.external_structure with
      | Unknown -> Unknown
      | Ok external_structure ->
        let env, env_extension1, env_extension2, logical_vars =
          environment_for_meet_or_join env t1 t2 ~external_structure
        in
        let env_extension =
          TEE.join (JE.central_environment env) env_extension1 env_extension2
        in
        Ok {
          external_structure;
          logical_vars;
          env_extension;
        }

  let add_or_meet_equations t env env_extension =
    { t with
      env_extension = TEE.meet env t.env_extension env_extension;
    }

  let standalone_extension t =
    EV.Map.fold (fun _external_var logical_var env_extension ->
        let stale_name = LV.name logical_var in
        let kind = LV.kind logical_var in
        TEE.add_definition_at_beginning env_extension stale_name
          (T.bottom kind))
      t.logical_vars
      t.env_extension

  let introduce t env =
    let scope_level = Typing_env.max_level env in
    TE.add_or_meet_env_extension env (standalone_extension t) scope_level

  let free_names_in_logical_vars t =
    LV.Set.fold (fun logical_var free_names ->
        Name_occurrences.union (LV.free_names logical_var) free_names)
      (EV.Map.data t.logical_vars)
      free_names

  let free_names { external_structure = _; logical_vars; env_extension; } =
    let free_names = TEE.free_names env_extension in
    let bound_names = free_names_in_logical_vars t in
    Name_occurrences.diff free_names bound_names

  (* CR mshinwell: Do we really need [bound_names]?  It seems like this
     might be problematic with the two notions of freshness (the usual one
     for terms; and for existentials, which are binding but not freshened
     until opened). *)
  let bound_names { external_structure; logical_vars; env_extension = _; } =
    let bound_names =
      Name_occurrences.create ()
      (* XXX Have removed until CR above is resolved.  The logical var
         names aren't fresh.
         free_names_in_logical_vars t *)
    in
    EV.Set.fold (fun external_var bound_names ->
        Name_occurrences.union (EV.free_names external_var) bound_names)
      (EVS.to_set external_structure)
      bound_names

  let apply_name_permutation
        { external_structure; logical_vars; env_extension; } perm =
    let external_structure =
      EVS.apply_name_permutation external_structure perm
    in
    let logical_vars =
      EV.Map.fold (fun external_var logical_var logical_vars ->
          let external_var = EV.apply_name_permutation external_var perm in
          let logical_var = LV.apply_name_permutation logical_var perm in
          EV.Map.add external_var logical_var logical_vars)
        logical_vars
        EV.Map.empty
    in
    let env_extension = TEE.apply_name_permutation env_extension perm in
    { external_structure;
      logical_vars;
      env_extension;
    }

  let freshen t freshening =
    apply_name_permutation t (Freshening.name_permutation freshening)
end

module Targetint_dot_ocaml = struct
  include Targetint.OCaml

  let free_names _ = Name.Set.empty
  let bound_names _ = Name.Set.empty
  let apply_name_permutation t _perm = t
  let freshen t _freshening = t
end

module List_structure (EV : External_var_sig) = struct
  type t = EV.t list

  let fold f init t = List.fold_left f init t
  let to_set t = EV.Set.of_list t

  let meet t1 t2 : _ Or_bottom.t =
    if Misc.Stdlib.List.compare EV.compare t1 t2 = 0 then Ok t1
    else Bottom

  let join t1 t2 : _ Or_unknown.t =
    if Misc.Stdlib.List.compare EV.compare t1 t2 = 0 then Ok t1
    else Unknown
end

module Set_structure (EV : External_var_sig) = struct
  type t = EV.Set.t

  let fold f init t = EV.Set.fold f init t
  let to_set t = t

  let meet t1 t2 : _ Or_bottom.t =
    let t = EV.Set.inter t1 t2 in
    if is_empty t then Bottom
    else Ok t

  let join t1 t2 : _ Or_unknown.t =
    Ok (EV.Set.union t1 t2)
end
