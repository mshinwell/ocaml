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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

type t = {
  const : Static_const.t;
  free_names : Name_occurrences.t;
}

type const_wfn = t

let create const ~free_names =
  { const;
    free_names;
  }

let const t = t.const

let free_names t = t.free_names

let is_fully_static t = Static_const.is_fully_static t.const

let print ppf t = Static_const.print ppf t.const

module Group = struct
  type t = {
    consts : const_wfn list;
    mutable free_names : Name_occurrences.t Or_unknown.t;
  }

  let create consts =
    { consts;
      free_names = Unknown;
    }

  let group t =
    (* The length of [t.consts] should be short and is usually going to be
       just one, so this seems ok. *)
    ListLabels.map t.consts ~f:(fun (const : const_wfn) -> const.const)
    |> Static_const.Group.create

  let print ppf t = Static_const.Group.print ppf (group t)

  let free_names t =
    match t.free_names with
    | Known free_names -> free_names
    | Unknown ->
      let free_names =
        ListLabels.fold_left t.consts ~init:Name_occurrences.empty
          ~f:(fun free_names (const : const_wfn) ->
            Name_occurrences.union free_names const.free_names)
      in
      t.free_names <- Known free_names;
      free_names

  let pieces_of_code t =
    t.consts
    |> List.filter_map (fun (const : const_wfn) ->
      match Static_const.to_code const.const with
      | None -> None
      | Some code -> Some (code, const.free_names))
    |> List.filter_map (fun (code, free_names) ->
      if Code.is_deleted code then None
      else Some (Code.code_id code, (code, free_names)))
    |> Code_id.Map.of_list

  let match_against_bound_symbols t bound_symbols ~init ~code ~set_of_closures
        ~block_like =
    Static_const.Group.match_against_bound_symbols (group t)
      bound_symbols ~init ~code ~set_of_closures ~block_like

  let map t ~f =
    let changed = ref false in
    let consts =
      ListLabels.map t.consts ~f:(fun const ->
        let const' = f const in
        if const != const' then begin
          changed := true;
        end;
        const')
    in
    if not !changed then t
    else
      { consts;
        free_names = Unknown;
      }

  let concat t1 t2 =
    let free_names : _ Or_unknown.t =
      match t1.free_names, t2.free_names with
      | Known free_names1, Known free_names2 ->
        Known (Name_occurrences.union free_names1 free_names2)
      | Known _, Unknown | Unknown, Known _ | Unknown, Unknown -> Unknown
    in
    { consts = t1.consts @ t2.consts;
      free_names;
    }
end
