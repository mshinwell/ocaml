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

module K = Flambda_kind

module Code_and_set_of_closures = struct
  type t = {
    code_ids : Code_id.Set.t;
    closure_symbols : Symbol.t Closure_id.Lmap.t;
  }

  (* CR mshinwell: Share with [Bindable_let_bound] and below *)
  let print_closure_binding ppf (closure_id, sym) =
    Format.fprintf ppf "@[%a @<0>%s\u{21a4}@<0>%s %a@]"
      Symbol.print sym
      (Flambda_colours.elide ())
      (Flambda_colours.elide ())
      Closure_id.print closure_id

  let print ppf { code_ids; closure_symbols; } =
    match
      Code_id.Set.elements code_ids, Closure_id.Lmap.bindings closure_symbols
    with
    | [code_id], [] ->
      Format.fprintf ppf "%a" Code_id.print code_id
    | [], [closure_binding] ->
      Format.fprintf ppf "@<0>%s%a@<0>%s"
        (Flambda_colours.symbol ())
        print_closure_binding closure_binding
        (Flambda_colours.normal ())
    | _, _ ->
      Format.fprintf ppf "@[<hov 1>\
          @[<hov 1>(code_ids@ (%a))@]@ \
          @[<hov 1>(closure_symbols@ {%a})@]\
          @]"
        Code_id.Set.print code_ids
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          print_closure_binding)
        (Closure_id.Lmap.bindings closure_symbols)

  let being_defined { code_ids = _; closure_symbols; } =
    Closure_id.Lmap.fold (fun _closure_id symbol being_defined ->
        Symbol.Set.add symbol being_defined)
      closure_symbols
      Symbol.Set.empty

  let closure_symbols_being_defined t = being_defined t

  let code_being_defined { code_ids; closure_symbols = _; } = code_ids

  let free_names { code_ids; closure_symbols; } =
    let from_code_ids =
      Code_id.Set.fold (fun code_id from_code_ids ->
          Name_occurrences.add_code_id from_code_ids code_id Name_mode.normal)
        code_ids
        Name_occurrences.empty
    in
    Closure_id.Lmap.fold (fun _closure_id closure_sym bound_names ->
        Name_occurrences.add_symbol bound_names closure_sym Name_mode.normal)
      closure_symbols
      from_code_ids

  let all_ids_for_export { code_ids; closure_symbols; } =
    let symbols =
      Closure_id.Lmap.fold (fun _closure_id sym symbols ->
          Symbol.Set.add sym symbols)
        closure_symbols
        Symbol.Set.empty
    in
    Ids_for_export.create ~code_ids ~symbols ()

  let import import_map { code_ids; closure_symbols; } =
    let code_ids =
      Code_id.Set.map (Ids_for_export.Import_map.code_id import_map) code_ids
    in
    let closure_symbols =
      Closure_id.Lmap.map (Ids_for_export.Import_map.symbol import_map)
        closure_symbols
    in
    { code_ids; closure_symbols; }
end

module Pattern = struct
  type t =
    | Code of Code_id.t
    | Set_of_closures of Symbol.t Closure_id.Lmap.t
    | Block_like of Symbol.t

  let code code_id = Code t
  let set_of_closures closure_symbols = Set_of_closures closure_symbols
  let block_like symbol = Block_like symbol

  val print : Format.formatter -> t -> unit
end

type t = Pattern.t list

let print ppf t =
  match t with
  | Singleton sym ->
    Format.fprintf ppf "@[%a@ \u{2237}@ %a@]"
      Symbol.print sym
      K.print K.value
  | Sets_of_closures [set] ->
    Code_and_set_of_closures.print ppf set
  | Sets_of_closures sets ->
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        Code_and_set_of_closures.print)
      sets

let print_with_cache ~cache:_ ppf t = print ppf t

(* CR mshinwell: This should have an [invariant] function.  One thing to
   check is that the [closure_symbols] are all distinct. *)

let invariant _ _ = ()

let being_defined t =
  match t with
  | Singleton sym -> Symbol.Set.singleton sym
  | Sets_of_closures sets ->
    List.fold_left (fun being_defined set ->
        Symbol.Set.union (Code_and_set_of_closures.being_defined set)
          being_defined)
      Symbol.Set.empty
      sets

let closure_symbols_being_defined t =
  match t with
  | Singleton _sym -> Symbol.Set.empty
  | Sets_of_closures sets ->
    Symbol.Set.union_list
      (List.map Code_and_set_of_closures.closure_symbols_being_defined sets)

let code_being_defined t =
  match t with
  | Singleton _ -> Code_id.Set.empty
  | Sets_of_closures sets ->
    Code_id.Set.union_list
      (List.map Code_and_set_of_closures.code_being_defined sets)

let everything_being_defined t =
  let code =
    Code_id.Set.fold (fun code_id code ->
        Code_id_or_symbol.Set.add (Code_id code_id) code)
      (code_being_defined t)
      Code_id_or_symbol.Set.empty
  in
  let closure_symbols =
    Symbol.Set.fold (fun symbol closure_symbols ->
        Code_id_or_symbol.Set.add (Symbol symbol) closure_symbols)
      (being_defined t)
      Code_id_or_symbol.Set.empty
  in
  Code_id_or_symbol.Set.union code closure_symbols

let apply_name_permutation t _perm = t

let free_names t =
  match t with
  | Singleton sym -> Name_occurrences.singleton_symbol sym Name_mode.normal
  | Sets_of_closures sets ->
    Name_occurrences.union_list
      (List.map Code_and_set_of_closures.free_names sets)

let all_ids_for_export t =
  match t with
  | Singleton sym -> Ids_for_export.add_symbol (Ids_for_export.empty) sym
  | Sets_of_closures sets ->
    List.fold_left (fun ids set ->
        Ids_for_export.union ids
          (Code_and_set_of_closures.all_ids_for_export set))
      Ids_for_export.empty
      sets

let import import_map t =
  match t with
  | Singleton sym ->
    Singleton (Ids_for_export.Import_map.symbol import_map sym)
  | Sets_of_closures sets ->
    let sets = List.map (Code_and_set_of_closures.import import_map) sets in
    Sets_of_closures sets
