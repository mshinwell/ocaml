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

module Pattern = struct
  type t =
    | Code of Code_id.t
    | Set_of_closures of Symbol.t Closure_id.Lmap.t
    | Block_like of Symbol.t

  let code code_id = Code code_id
  let set_of_closures closure_symbols = Set_of_closures closure_symbols
  let block_like symbol = Block_like symbol

  let print ppf t =
    match t with
    | Code code_id ->
      Format.fprintf ppf "@[<hov 1>(Code@ %a)@]" Code_id.print code_id
    | Set_of_closures closure_symbols ->
      Format.fprintf ppf "@[<hov 1>(Set_of_closures@ %a)@]"
        (Closure_id.Lmap.print Symbol.print) closure_symbols
    | Block_like symbol ->
      Format.fprintf ppf "@[<hov 1>(Block_like@ %a)@]" Symbol.print symbol

  let free_names t =
    match t with
    | Code code_id ->
      Name_occurrences.singleton_code_id code_id Name_mode.normal
    | Set_of_closures closure_symbols ->
      Closure_id.Lmap.fold (fun _ symbol free_names ->
          Name_occurrences.add_symbol free_names symbol Name_mode.normal)
        closure_symbols
        Name_occurrences.empty
    | Block_like symbol ->
      Name_occurrences.singleton_symbol symbol Name_mode.normal

  let being_defined t =
    match t with
    | Code _ -> Symbol.Set.empty
    | Set_of_closures closure_symbols ->
      closure_symbols
      |> Closure_id.Lmap.data
      |> Symbol.Set.of_list
    | Block_like symbol -> Symbol.Set.singleton symbol

  let code_being_defined t =
    match t with
    | Code code_id -> Code_id.Set.singleton code_id
    | Set_of_closures _ | Block_like _ -> Code_id.Set.empty

  let closure_symbols_being_defined t =
    match t with
    | Code _ | Block_like _ -> Symbol.Set.empty
    | Set_of_closures closure_symbols ->
      closure_symbols
      |> Closure_id.Lmap.data
      |> Symbol.Set.of_list

  let everything_being_defined t =
    match t with
    | Code code_id -> Code_id_or_symbol.Set.singleton (Code_id code_id)
    | Set_of_closures closure_symbols ->
      closure_symbols
      |> Closure_id.Lmap.data
      |> Symbol.Set.of_list
      |> Code_id_or_symbol.set_of_symbol_set
    | Block_like symbol -> Code_id_or_symbol.Set.singleton (Symbol symbol)

  let all_ids_for_export t =
    match t with
    | Code code_id -> Ids_for_export.singleton_code_id code_id
    | Set_of_closures closure_symbols ->
      let symbols =
        closure_symbols
        |> Closure_id.Lmap.data
        |> Symbol.Set.of_list
      in
      Ids_for_export.create ~symbols ()
    | Block_like symbol -> Ids_for_export.singleton_symbol symbol

  let import import_map t =
    let module IM = Ids_for_export.Import_map in
    match t with
    | Code code_id -> Code (IM.code_id import_map code_id)
    | Set_of_closures closure_symbols ->
      let closure_symbols =
        Closure_id.Lmap.map (IM.symbol import_map) closure_symbols
      in
      Set_of_closures closure_symbols
    | Block_like symbol -> Block_like (IM.symbol import_map symbol)
end

type t = Pattern.t list

let empty = []

let create pattern_list = pattern_list

let singleton pattern = [pattern]

let to_list t = t

let print ppf t =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Pattern.print) t

let print_with_cache ~cache:_ ppf t = print ppf t

(* CR mshinwell: This should have an [invariant] function.  One thing to
   check is that the [closure_symbols] are all distinct. *)

let invariant _ _ = ()

let being_defined t =
  List.map Pattern.being_defined t
  |> Symbol.Set.union_list

let closure_symbols_being_defined t =
  List.map Pattern.closure_symbols_being_defined t
  |> Symbol.Set.union_list

let non_closure_symbols_being_defined t =
  Symbol.Set.diff (being_defined t) (closure_symbols_being_defined t)

let code_being_defined t =
  List.map Pattern.code_being_defined t
  |> Code_id.Set.union_list

let everything_being_defined t =
  List.map Pattern.everything_being_defined t
  |> Code_id_or_symbol.Set.union_list

let apply_name_permutation t _perm = t

let free_names t =
  List.map Pattern.free_names t
  |> Name_occurrences.union_list

let all_ids_for_export t =
  List.map Pattern.all_ids_for_export t
  |> Ids_for_export.union_list

let import import_map t =
  List.map (Pattern.import import_map) t

let concat t1 t2 = t1 @ t2
