(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Reg_width_things.Rec_info_expr

let rec print ppf = function
  | Initial ->
    Format.pp_print_string ppf "0"
  | Var dv ->
    Format.fprintf ppf "@<0>%s%a@<0>%s"
      (Flambda_colours.depth_variable ())
      Variable.print dv
      (Flambda_colours.normal ())
  | Succ t ->
    Format.fprintf ppf "@[<hov 1>(succ@ %a)@]" print t
  | Unroll_to (unroll_depth, t) ->
    Format.fprintf ppf "@[<hov 1>(unroll_to@ %d@ %a)@]" unroll_depth print t

let print_with_cache ~cache:_ ppf t = print ppf t

let rec apply_renaming t perm =
  match t with
  | Initial -> Initial
  | Var dv ->
    let new_dv = Renaming.apply_variable perm dv in
    if dv == new_dv then t else Var new_dv
  | Succ t ->
    Succ (apply_renaming t perm)
  | Unroll_to (unroll_depth, t) ->
    Unroll_to (unroll_depth, apply_renaming t perm)

let rec free_names t =
  match t with
  | Initial -> Name_occurrences.empty
  | Var dv -> Name_occurrences.singleton_variable dv Name_mode.normal
  | Succ t
  | Unroll_to (_, t) -> free_names t

let invariant _ _ = ()

let all_ids_for_export _ =
  (* Depth variables don't use the integer id system *)
  Ids_for_export.empty
