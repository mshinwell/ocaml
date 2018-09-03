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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t =
  | Var of Variable.t
  | Symbol of Symbol.t
  | Logical_var of Logical_variable.t
(*
  | Phantom_var of Variable.t
*)

let var v = Var v
let symbol s = Symbol s
let logical_var lv = Logical_var lv

let map_var t ~f =
  match t with
  | Var var ->
    let var' = f var in
    if var == var' then t
    else Var var'
  | Symbol _ | Logical_var _ -> t

let map_symbol t ~f =
  match t with
  | Var _ | Logical_var _ -> t
  | Symbol symbol ->
    let symbol' = f symbol in
    if symbol == symbol' then t
    else Symbol symbol'

let to_var t =
  match t with
  | Var var -> Some var
  | Symbol _ | Logical_var _ -> None

let to_symbol t =
  match t with
  | Var _ | Logical_var _ -> None
  | Symbol sym -> Some sym

module With_map =
  Hashtbl.Make_with_map (struct
    type nonrec t = t

    let print ppf t =
      match t with
      | Var var -> Variable.print ppf var
      | Symbol sym -> Symbol.print ppf sym
      | Logical_var lv -> Logical_variable.print ppf lv

    let hash t =
      match t with
      | Var var -> Hashtbl.hash (0, Variable.hash var)
      | Symbol sym -> Hashtbl.hash (1, Symbol.hash sym)
      | Logical_var lv -> Hashtbl.hash (2, Logical_variable.hash lv)

    let compare t1 t2 =
      match t1, t2 with
      | Var var1, Var var2 -> Variable.compare var1 var2
      | Symbol sym1, Symbol sym2 -> Symbol.compare sym1 sym2
      | Logical_var lv1, Logical_var lv2 -> Logical_variable.compare lv1 lv2
      | Var _, Symbol _ -> -1
      | Var _, Logical_var _ -> -1
      | Symbol _, Var _ -> 1
      | Symbol _, Logical_var _ -> -1
      | Logical_var _, Var _ -> 1
      | Logical_var _, Symbol _ -> 1
  end)

(* CR mshinwell: We need a better way of adding the colours to maps. *)

module T = With_map.T

let compare = T.compare
let equal t1 t2 = T.compare t1 t2 = 0
let print = T.print
let hash = T.hash

module Set = With_map.Set

module Map = struct
  include With_map.Map

  let print ?before_key ?after_key print_contents ppf t =
    ignore before_key;
    ignore after_key;
    print ~before_key:(Misc_color.bold_green ())
      ~after_key:(Misc_color.reset ())
      print_contents ppf t
end

module Tbl = With_map.Tbl

let variables_only t =
  Set.filter (fun name ->
      match name with
      | Var _ -> true
      | Symbol _ | Logical_var _ -> false)
    t

let symbols_only_map t =
  Map.filter (fun name _ ->
      match name with
      | Var _ | Logical_var _ -> false
      | Symbol _ -> true)
    t

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

let print_sexp ppf t =
  match t with
  | Var var -> Format.fprintf ppf "@[(Var %a)@]" Variable.print var
  | Symbol sym -> Format.fprintf ppf "@[(Symbol %a)@]" Symbol.print sym
  | Logical_var lv ->
    Format.fprintf ppf "@[(Logical_var %a)@]" Logical_variable.print lv

let is_predefined_exception t =
  match t with
  | Var _ -> false
  | Symbol sym -> Symbol.is_predefined_exception sym
  | Logical_var _ -> false

let rename t =
  match t with
  | Var var -> Var (Variable.rename var)
  | Symbol sym -> Symbol (Symbol.rename sym)
  | Logical_var lv -> Logical_var (Logical_variable.rename lv)

let in_compilation_unit t =
  match t with
  | Var var -> Variable.in_compilation_unit var
  | Symbol sym -> Symbol.in_compilation_unit sym
  | Logical_var lv -> Logical_variable.in_compilation_unit lv
