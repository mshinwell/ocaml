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

let create (const : Static_const.t) ~(free_names : _ Or_unknown.t) =
  let free_names =
    match const, free_names with
    | Code code, Unknown -> Code.free_names code
    | Code _, Known _ ->
      Misc.fatal_errorf "Free names of [Code] static constant must not be \
          provided to [Static_const.create]:@ %a"
        Static_const.print const
    | Set_of_closures _, Known free_names
    | Block _, Known free_names
    | Boxed_float _, Known free_names
    | Boxed_int32 _, Known free_names
    | Boxed_int64 _, Known free_names
    | Boxed_nativeint _, Known free_names
    | Immutable_float_block _, Known free_names
    | Immutable_float_array _, Known free_names
    | Mutable_string _, Known free_names
    | Immutable_string _, Known free_names -> free_names
    | Set_of_closures _, Unknown
    | Block _, Unknown
    | Boxed_float _, Unknown
    | Boxed_int32 _, Unknown
    | Boxed_int64 _, Unknown
    | Boxed_nativeint _, Unknown
    | Immutable_float_block _, Unknown
    | Immutable_float_array _, Unknown
    | Mutable_string _, Unknown
    | Immutable_string _, Unknown ->
      Misc.fatal_errorf "Free names of non-[Code] static constant not \
          provided to [Static_const.create]:@ %a"
        Static_const.print const
  in
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

  let empty = {
    consts = [];
    free_names = Known Name_occurrences.empty;
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

  let consts t =
    ListLabels.map t.consts ~f:(fun (const : const_wfn) -> const.const)
    |> Static_const.Group.create

  let to_list t = t.consts

  let pieces_of_code t =
    t.consts
    |> List.filter_map (fun (const : const_wfn) ->
      Static_const.to_code const.const)
    |> List.filter_map (fun code ->
      if Code.is_deleted code then None
      else Some (Code.code_id code, code))
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
