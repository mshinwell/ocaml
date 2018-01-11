(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Var of Variable.t
  | Symbol of Symbol.t
  | Symbol_field of Symbol.t * Targetint.OCaml.t

let var v = Var v
let symbol s = Symbol s
let symbol_field s ~field = Symbol_field (s, field)

let map_var t ~f =
  match t with
  | Var var ->
    let var' = f var in
    if var == var' then t
    else Var var'
  | Symbol _ | Symbol_field _ -> t

let map_symbol t ~f =
  match t with
  | Var _ -> t
  | Symbol symbol ->
    let symbol' = f symbol in
    if symbol == symbol' then t
    else Symbol symbol'
  | Symbol_field (symbol, field) ->
    (* CR mshinwell: Should this be the semantics? *)
    let symbol' = f symbol in
    if symbol == symbol' then t
    else Symbol_field (symbol', field)

let to_var t =
  match t with
  | Var var -> Some var
  | Symbol _ | Symbol_field _ -> None

let to_symbol t =
  match t with
  | Var _ -> None
  | Symbol sym -> Some sym
  | Symbol_field _ -> None

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf t =
    match t with
    | Var var -> Variable.print ppf var
    | Symbol sym -> Symbol.print ppf sym
    | Symbol_field (sym, field) ->
      Format.fprintf ppf "%a.(%a)"
        Symbol.print sym
        Targetint.OCaml.print field

  let hash t =
    match t with
    | Var var -> Hashtbl.hash (0, Variable.hash var)
    | Symbol sym -> Hashtbl.hash (1, Symbol.hash sym)
    | Symbol_field (sym, field) ->
      Hashtbl.hash (2, Hashtbl.hash (Symbol.hash sym, Hashtbl.hash field))

  let compare t1 t2 =
    match t1, t2 with
    | Var var1, Var var2 -> Variable.compare var1 var2
    | Symbol sym1, Symbol sym2 -> Symbol.compare sym1 sym2
    | Symbol_field (sym1, field1), Symbol_field (sym2, field2) ->
      let c = Symbol.compare sym1 sym2 in
      if c <> 0 then c
      else Pervasives.compare field1 field2
    | Var _, Symbol _ -> -1
    | Symbol _, Var _ -> 1
    | Var _, Symbol_field _ -> -1
    | Symbol_field _, Var _ -> 1
    | Symbol _, Symbol_field _ -> -1
    | Symbol_field _, Symbol _ -> 1

  let equal t1 t2 =
    compare t1 t2 = 0
end)

let set_to_var_set t =
  Set.fold (fun name vars ->
      match to_var name with
      | None -> vars
      | Some var -> Variable.Set.add var vars)
    t
    Variable.Set.empty

let set_to_symbol_set t =
  Set.fold (fun name syms ->
      match to_symbol name with
      | None -> syms
      | Some sym -> Symbol.Set.add sym syms)
    t
    Symbol.Set.empty
