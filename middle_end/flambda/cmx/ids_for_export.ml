(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2020 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Coercion = Reg_width_things.Coercion
module Const = Reg_width_things.Const
module Simple = Reg_width_things.Simple

type t = {
  symbols : Symbol.Set.t;
  variables : Variable.Set.t;
  simples : Simple.Set.t;
  consts : Const.Set.t;
  code_ids : Code_id.Set.t;
  continuations : Continuation.Set.t;
}

let empty = {
  symbols = Symbol.Set.empty;
  variables = Variable.Set.empty;
  simples = Simple.Set.empty;
  consts = Const.Set.empty;
  code_ids = Code_id.Set.empty;
  continuations = Continuation.Set.empty;
}

let create
    ?(symbols = Symbol.Set.empty)
    ?(variables = Variable.Set.empty)
    ?(simples = Simple.Set.empty)
    ?(consts = Const.Set.empty)
    ?(code_ids = Code_id.Set.empty)
    ?(continuations = Continuation.Set.empty)
    () =
  { symbols;
    variables;
    simples;
    consts;
    code_ids;
    continuations;
  }

let singleton_code_id code_id =
  create ~code_ids:(Code_id.Set.singleton code_id) ()

let singleton_continuation cont =
  create ~continuations:(Continuation.Set.singleton cont) ()

let singleton_symbol symbol =
  create ~symbols:(Symbol.Set.singleton symbol) ()

let add_const t const =
  { t with consts = Const.Set.add const t.consts }

let add_variable t var =
  { t with variables = Variable.Set.add var t.variables }

let add_symbol t sym =
  { t with symbols = Symbol.Set.add sym t.symbols }

let add_name t name =
  Name.pattern_match name
    ~var:(add_variable t)
    ~symbol:(add_symbol t)

let add_simple t simple =
  let simples =
    if Coercion.is_id (Simple.coercion simple) then t.simples else
      Simple.Set.add simple t.simples
  in
  let t = { t with simples; } in
  Simple.pattern_match simple
    ~const:(add_const t)
    ~name:(fun name ~coercion:_ -> add_name t name)

let add_code_id t code_id =
  { t with code_ids = Code_id.Set.add code_id t.code_ids }

let add_continuation t continuation =
  { t with continuations = Continuation.Set.add continuation t.continuations }


let from_simple simple =
  let simples =
    if Coercion.is_id (Simple.coercion simple) then
      (* This simple will not be in the grand_table_of_simples *)
      Simple.Set.empty
    else
      Simple.Set.singleton simple
  in
  Simple.pattern_match simple
    ~const:(fun const ->
      create ~simples
        ~consts:(Const.Set.singleton const)
        ())
    ~name:(fun name ->
      Name.pattern_match name
        ~var:(fun var ~coercion:_ ->
          create ~simples
            ~variables:(Variable.Set.singleton var)
            ())
        ~symbol:(fun sym ~coercion:_ ->
          create ~simples
            ~symbols:(Symbol.Set.singleton sym)
            ()))

let union t1 t2 =
  { symbols = Symbol.Set.union t1.symbols t2.symbols;
    variables = Variable.Set.union t1.variables t2.variables;
    simples = Simple.Set.union t1.simples t2.simples;
    consts = Const.Set.union t1.consts t2.consts;
    code_ids = Code_id.Set.union t1.code_ids t2.code_ids;
    continuations = Continuation.Set.union t1.continuations t2.continuations;
  }

let rec union_list ts =
  match ts with
  | [] -> empty
  | t::ts -> union t (union_list ts)

module Table_data = struct
  type ids_for_export = t

  type t = {
    symbols : Symbol.exported Symbol.Map.t;
    variables : Variable.exported Variable.Map.t;
    simples : Simple.exported Simple.Map.t;
    consts : Const.exported Const.Map.t;
    code_ids : Code_id.exported Code_id.Map.t;
    continuations : Continuation.exported Continuation.Map.t;
  }

  let create (ids_for_export : ids_for_export) =
    let symbols =
      Symbol.Set.fold (fun symbol symbols ->
          Symbol.Map.add symbol (Symbol.export symbol) symbols)
        ids_for_export.symbols
        Symbol.Map.empty
    in
    let variables =
      Variable.Set.fold (fun variable variables ->
          Variable.Map.add variable (Variable.export variable) variables)
        ids_for_export.variables
        Variable.Map.empty
    in
    let simples =
      Reg_width_things.Simple.Set.fold (fun simple simples ->
          Simple.Map.add simple (Simple.export simple) simples)
        ids_for_export.simples
        Simple.Map.empty
    in
    let consts =
      Const.Set.fold (fun const consts ->
          Const.Map.add const (Const.export const) consts)
        ids_for_export.consts
        Const.Map.empty
    in
    let code_ids =
      Code_id.Set.fold (fun code_id code_ids ->
          Code_id.Map.add code_id (Code_id.export code_id) code_ids)
        ids_for_export.code_ids
        Code_id.Map.empty
    in
    let continuations =
      Continuation.Set.fold (fun continuation continuations ->
          Continuation.Map.add continuation (Continuation.export continuation)
            continuations)
        ids_for_export.continuations
        Continuation.Map.empty
    in
    { symbols;
      variables;
      simples;
      consts;
      code_ids;
      continuations;
    }

  let to_import_renaming t ~used_closure_vars =
    (* First create map for data that does not contain ids, i.e. everything
      except simples *)
    let filter import =
      fun key data ->
        let new_key = import data in
        if key == new_key then None else Some new_key
    in
    let symbols =
      Symbol.Map.filter_map (filter Symbol.import) t.symbols
    in
    let variables =
      Variable.Map.filter_map (filter Variable.import) t.variables
    in
    let simples =
      Simple.Map.filter_map (filter Simple.import) t.simples
    in
    let consts =
      Const.Map.filter_map (filter Const.import) t.consts
    in
    let code_ids =
      Code_id.Map.filter_map (filter Code_id.import) t.code_ids
    in
    let continuations =
      Continuation.Map.filter_map (filter Continuation.import)
        t.continuations
    in
    Renaming.create_import_map
      ~symbols
      ~variables
      ~simples
      ~consts
      ~code_ids
      ~continuations
      ~used_closure_vars

  let update_for_pack t ~pack_units ~pack =
    let update_cu unit =
      if Compilation_unit.Set.mem unit pack_units
      then pack
      else unit
    in
    let symbols =
      Symbol.Map.map (Symbol.map_compilation_unit update_cu)
        t.symbols
    in
    let variables =
      Variable.Map.map (Variable.map_compilation_unit update_cu)
        t.variables
    in
    let simples =
      Simple.Map.map (Simple.map_compilation_unit update_cu)
        t.simples
    in
    let consts =
      Const.Map.map (Const.map_compilation_unit update_cu)
        t.consts
    in
    let code_ids =
      Code_id.Map.map (Code_id.map_compilation_unit update_cu)
        t.code_ids
    in
    let continuations =
      Continuation.Map.map (Continuation.map_compilation_unit update_cu)
        t.continuations
    in
    { symbols;
      variables;
      simples;
      consts;
      code_ids;
      continuations;
    }
end
