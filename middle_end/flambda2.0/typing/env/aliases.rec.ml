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

type t = Name.Set.t Simple.Map.t

type still_unresolved =
  | Resolved
  | Still_unresolved

let resolve_aliases_on_ty0 (type a) t ?bound_name ~force_to_kind
      ~print_ty (ty : a Flambda_types.ty)
      : (a Flambda_types.ty) * (Simple.t option) * Name_or_export_id.Set.t
          * still_unresolved =
  let rec resolve_aliases names_seen ~canonical_simple
        (ty : a Flambda_types.ty) =
    let resolve (name : Name_or_export_id.t)
          : _ * _ * _ * still_unresolved =
      if Name_or_export_id.Set.mem name names_seen then begin
        Misc.fatal_errorf "Loop on %a whilst resolving aliases"
          Name_or_export_id.print name
      end;
      let continue_resolving t ~canonical_simple =
        let names_seen = Name_or_export_id.Set.add name names_seen in
        let ty = force_to_kind t in
        resolve_aliases names_seen ~canonical_simple ty
      in
      match name with
      | Name name ->
        begin match find_exn t name with
        | exception Not_found ->
          Misc.fatal_errorf "Unbound name %a whilst resolving aliases \
              for type:@ %a@ in environment:@ %a"
            Name.print name
            print_ty ty
            print t
        | ty, _ ->
          continue_resolving ty ~canonical_simple:(Some (Simple.name name))
        end
      | Export_id export_id ->
        match t.resolver export_id with
        | Some ty -> continue_resolving ty ~canonical_simple
        | None -> ty, None, Name_or_export_id.Set.empty, Still_unresolved
    in
    match ty with
    | No_alias _ -> ty, canonical_simple, names_seen, Resolved
    | Type export_id -> resolve (Name_or_export_id.Export_id export_id)
    | Equals (Name name) -> resolve (Name_or_export_id.Name name)
    | Equals ((Const _ | Discriminant _) as simple) ->
      ty, Some simple, names_seen, Resolved
  in
  let seen =
    match bound_name with
    | None -> Name_or_export_id.Set.empty
    | Some bound_name -> Name_or_export_id.Set.singleton (Name bound_name)
  in
  let canonical_simple =
    match bound_name with
    | None -> None
    | Some bound_name -> Some (Simple.name bound_name)
  in
  resolve_aliases seen ~canonical_simple ty

let resolve_aliases_on_ty t ?bound_name ~force_to_kind ~print_ty ty =
  let ty, canonical_name, names_seen, _still_unresolved =
    resolve_aliases_on_ty0 t ?bound_name ~force_to_kind ~print_ty ty
  in
  ty, canonical_name, names_seen

let resolve_aliases_and_squash_unresolved_names_on_ty' env ?bound_name
      ~print_ty ~force_to_kind ty
      : _ Flambda_types.unknown_or_join * (Simple.t option) =
  let ty, canonical_name, _names_seen, _still_unresolved =
    try resolve_aliases_on_ty0 env ?bound_name ~force_to_kind ~print_ty ty
    with Misc.Fatal_error -> begin
      Format.eprintf "\n%sContext is: \
          resolve_aliases_and_squash_unresolved_names_on_ty':%s\
          @ %a@ Environment:@ %a\n"
        (Misc_color.bold_red ())
        (Misc_color.reset ())
        print_ty ty
        print env;
      raise Misc.Fatal_error
    end
  in
  match ty with
  | No_alias uoj -> uoj, canonical_name
  | Type _ | Equals _ -> Unknown, canonical_name

let resolve_aliases ?bound_name t (ty : Flambda_types.t)
      : Flambda_types.t * (Simple.t option) =
  match ty with
  | Value ty_value ->
    let force_to_kind = Flambda_type0_core.force_to_kind_value in
    let ty_value, canonical_name, _names_seen =
      resolve_aliases_on_ty t ?bound_name ~force_to_kind
        ~print_ty:Type_printers.print_ty_value
        ty_value
    in
    Value ty_value, canonical_name
  | Naked_number (ty_naked_number, kind) ->
    let force_to_kind =
      Flambda_type0_core.force_to_kind_naked_number kind
    in
    let ty_naked_number, canonical_name, _names_seen =
      resolve_aliases_on_ty t ?bound_name ~force_to_kind
        ~print_ty:Type_printers.print_ty_naked_number
        ty_naked_number
    in
    Naked_number (ty_naked_number, kind), canonical_name
  | Fabricated ty_fabricated ->
    let force_to_kind = Flambda_type0_core.force_to_kind_fabricated in
    let ty_fabricated, canonical_name, _names_seen =
      resolve_aliases_on_ty t ?bound_name ~force_to_kind
        ~print_ty:Type_printers.print_ty_fabricated
        ty_fabricated
    in
    Fabricated ty_fabricated, canonical_name

let aliases_of_simple (t : t) (simple : Simple.t) =
  match
    Simple.Map.find simple (snd t.current_level).just_after_level.aliases
  with
  | exception Not_found ->
    begin match simple with
    | Const _ | Discriminant _ -> Name.Set.empty
    | Name name -> Name.Set.singleton name
    end
  | aliases ->
    begin match simple with
    | Const _ | Discriminant _ -> aliases
    | Name name -> Name.Set.add name aliases
    end
