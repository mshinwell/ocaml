(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

let rec load_cmx_file_contents backend comp_unit ~imported_names
      ~imported_code =
  let module Backend = (val backend : Flambda2_backend_intf.S) in
  match Backend.get_global_info comp_unit with
  | None -> None
  | Some cmx ->
    let cmx = ((Obj.obj cmx) : Flambda_cmx_format.t) in
    let resolver comp_unit =
      load_cmx_file_contents backend comp_unit ~imported_names ~imported_code
    in
    let get_imported_names () = !imported_names in
    let typing_env =
      Flambda_cmx_format.final_typing_env cmx
      |> TE.Serializable.to_typing_env ~resolver ~get_imported_names
    in
    Format.eprintf "For unit %a loaded the following env:@ %a"
      Compilation_unit.print comp_unit
      TE.print typing_env;
    let newly_imported_names = TE.name_domain typing_env in
    if not (Name.Set.is_empty (
      Name.Set.inter newly_imported_names !imported_names))
    then begin
      (* CR mshinwell: I suspect this could be caused by a user error, e.g.
         renaming a .cmx file or something? *)
      Misc.fatal_errorf "The .cmx file for %a contains names that are \
          already defined:@ %a"
        Compilation_unit.print comp_unit
        Name.Set.print newly_imported_names
    end;
    imported_names := Name.Set.union newly_imported_names !imported_names;
    let code = Flambda_cmx_format.all_code cmx in
    imported_code := Code_id.Map.disjoint_union code !imported_code;
    Some typing_env

let prepare_cmx_file_contents ~return_cont_env:cont_uses_env
      ~return_continuation all_code =
  match
    Continuation_uses_env.get_typing_env_no_more_than_one_use
      cont_uses_env return_continuation
  with
  | None -> None
  | Some _ when !Clflags.opaque -> None
  | Some final_typing_env ->
    let final_typing_env =
      TE.make_vars_on_current_level_irrelevant final_typing_env
      |> TE.Serializable.create
    in
    Some (Flambda_cmx_format.create ~final_typing_env ~all_code)