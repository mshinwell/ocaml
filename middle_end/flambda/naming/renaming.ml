(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Pierre Chambart and Guillaume Bury, OCamlPro                 *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42-55"]

(* CR mshinwell: Disabling warning 55 is required to satisfy Closure, work
   out something better... *)

module Continuations = (Permutation.Make [@inlined hint]) (Continuation)
module Variables = (Permutation.Make [@inlined hint]) (Variable)
module Code_ids = (Permutation.Make [@inlined hint]) (Code_id)
module Symbols = (Permutation.Make [@inlined hint]) (Symbol)

module Import_map = Ids_for_export.Import_map
module Simple = Reg_width_things.Simple

type t = {
  continuations : Continuations.t;
  variables : Variables.t;
  code_ids : Code_ids.t;
  symbols : Symbols.t;
  import_map : Import_map.t option;
}

let empty =
  { continuations = Continuations.empty;
    variables = Variables.empty;
    code_ids = Code_ids.empty;
    symbols = Symbols.empty;
    import_map = None;
  }

let of_import_map import_map =
  if Import_map.is_empty import_map then empty
  else { empty with import_map = Some import_map; }

let print ppf
      { continuations; variables; code_ids; symbols; import_map = _; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(variables@ %a)@])@ \
      @[<hov 1>(code_ids@ %a)@])@ \
      @[<hov 1>(symbols@ %a)@])\
      @]"
    Continuations.print continuations
    Variables.print variables
    Code_ids.print code_ids
    Symbols.print symbols

let has_no_action
      { continuations; variables; code_ids; symbols; import_map; } =
  Continuations.is_empty continuations
  && Variables.is_empty variables
  && Code_ids.is_empty code_ids
  && Symbols.is_empty symbols
  && match import_map with
     | None -> true
     | Some import_map -> Import_map.has_no_action import_map

let is_empty
      { continuations; variables; code_ids; symbols; import_map; } =
  Continuations.is_empty continuations
  && Variables.is_empty variables
  && Code_ids.is_empty code_ids
  && Symbols.is_empty symbols
  && match import_map with
     | None -> true
     | Some import_map -> Import_map.is_empty import_map

let compose0
      ~second:
        { continuations = continuations2;
          variables = variables2;
          code_ids = code_ids2;
          symbols = symbols2;
          import_map = import_map2;
        }
      ~first:
        { continuations = continuations1;
          variables = variables1;
          code_ids = code_ids1;
          symbols = symbols1;
          import_map = import_map1;
        } =
  { continuations =
      Continuations.compose ~second:continuations2 ~first:continuations1;
    variables = Variables.compose ~second:variables2 ~first:variables1;
    code_ids = Code_ids.compose ~second:code_ids2 ~first:code_ids1;
    symbols = Symbols.compose ~second:symbols2 ~first:symbols1;
    (* The import map substitution is always to fresh names, so this doesn't
       need a "proper" substitution composition operation. *)
    import_map =
      match import_map1, import_map2 with
      | None, None -> None
      | Some import_map, None | None, Some import_map ->
        Some import_map
      | Some import_map1, Some import_map2 ->
        Some (Import_map.union import_map1 import_map2);
  }

let compose ~second ~first =
  if is_empty second then first
  else if is_empty first then second
  else compose0 ~second ~first

let add_variable t var1 var2 =
  { t with
    variables = Variables.compose_one ~first:t.variables var1 var2;
  }

let add_fresh_variable t var1 ~guaranteed_fresh:var2 =
  { t with
    variables =
      Variables.compose_one_fresh t.variables var1 ~fresh:var2;
  }

let apply_variable t var =
  let var =
    match t.import_map with
    | None -> var
    | Some import_map -> Import_map.variable import_map var
  in
  Variables.apply t.variables var

let apply_variable_set t vars =
  Variable.Set.fold (fun var result ->
      let var = apply_variable t var in
      Variable.Set.add var result)
    vars
    Variable.Set.empty

let add_symbol t symbol1 symbol2 =
  { t with
    symbols = Symbols.compose_one ~first:t.symbols symbol1 symbol2;
  }

let add_fresh_symbol t symbol1 ~guaranteed_fresh:symbol2 =
  { t with
    symbols = Symbols.compose_one_fresh t.symbols symbol1 ~fresh:symbol2;
  }

let apply_symbol t symbol =
  let symbol =
    match t.import_map with
    | None -> symbol
    | Some import_map -> Import_map.symbol import_map symbol
  in
  Symbols.apply t.symbols symbol

let apply_symbol_set t symbols =
  Symbol.Set.fold (fun symbol result ->
    let symbol = apply_symbol t symbol in
    Symbol.Set.add symbol result)
    symbols
    Symbol.Set.empty

let apply_name t name =
  Name.pattern_match name
    ~var:(fun var -> Name.var (apply_variable t var))
    ~symbol:(fun symbol -> Name.symbol (apply_symbol t symbol))

let add_continuation t k1 k2 =
  { t with
    continuations = Continuations.compose_one ~first:t.continuations k1 k2;
  }

let add_fresh_continuation t k1 ~guaranteed_fresh:k2 =
  { t with
    continuations =
      Continuations.compose_one_fresh t.continuations k1 ~fresh:k2;
  }

let apply_continuation t k =
  let k =
    match t.import_map with
    | None -> k
    | Some import_map -> Import_map.continuation import_map k
  in
  Continuations.apply t.continuations k

let add_code_id t code_id1 code_id2 =
  { t with
    code_ids = Code_ids.compose_one ~first:t.code_ids code_id1 code_id2;
  }

let add_fresh_code_id t code_id1 ~guaranteed_fresh:code_id2 =
  { t with
    code_ids = Code_ids.compose_one_fresh t.code_ids code_id1 ~fresh:code_id2;
  }

let apply_code_id t code_id =
  let code_id =
    match t.import_map with
    | None -> code_id
    | Some import_map -> Import_map.code_id import_map code_id
  in
  Code_ids.apply t.code_ids code_id

let apply_simple t simple =
  let simple =
    match t.import_map with
    | None -> simple
    | Some import_map -> Import_map.simple import_map simple
  in
  let [@inline always] name old_name =
    let new_name = apply_name t old_name in
    if old_name == new_name then simple
    else
      match Simple.rec_info simple with
      | None -> Simple.name new_name
      | Some rec_info -> Simple.with_rec_info (Simple.name new_name) rec_info
  in
  (* Constants are never permuted, only freshened upon import. *)
  Simple.pattern_match simple ~const:(fun _ -> simple) ~name

let closure_var_is_used t closure_var =
  match t.import_map with
  | None -> true  (* N.B. not false! *)
  | Some import_map -> Import_map.closure_var_is_used import_map closure_var
