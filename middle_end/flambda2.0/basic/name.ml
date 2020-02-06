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

open Int_replace_polymorphic_compare

module Id = Table_by_int_id.Id

type t = Id.t

let var_flag = 0
let symbol_flag = 1

let () =
  assert (Id.flags_size_in_bits >= 1)

let var v = v
let symbol s = Id.with_flags v symbol_flag

let [@inline always] pattern_match t ~var ~symbol =
  let flags = Id.flags t in
  let var_or_symbol = Id.without_flags t in
  if flags = var_flag then var var_or_symbol
  else if flags = symbol_flag then symbol var_or_symbol
  else assert false

let is_var t = pattern_match t ~var:(fun _ -> true) ~symbol:(fun _ -> false)

let is_symbol t = pattern_match t ~var:(fun _ -> false) ~symbol:(fun _ -> true)

let map_var t ~f =
let map_var t ~f =
  pattern_match t ~var:(fun var -> ) ~symbol:(fun _ -> false)
  match t with
  | Var var ->
    let var' = f var in
    if var == var' then t
    else Var var'
  | Symbol _ -> t

let map_symbol t ~f =
  match t with
  | Var _ -> t
  | Symbol symbol ->
    let symbol' = f symbol in
    if symbol == symbol' then t
    else Symbol symbol'

let to_var t =
  match t with
  | Var var -> Some var
  | Symbol _ -> None

let to_symbol t =
  match t with
  | Var _ -> None
  | Symbol sym -> Some sym

module With_map =
  Identifiable.Make (struct
    type nonrec t = t

    let print ppf t =
      Format.fprintf ppf "@<0>%s" (Flambda_colours.name ());
      pattern_match t
        ~var:(fun var -> Variable.print ppf var)
        ~symbol:(fun symbol -> Symbol.print ppf symbol);
      Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

    let output chan t =
      print (Format.formatter_of_out_channel chan) t

    let hash = Id.hash
    let compare = Id.compare
    let equal = Id.equal
  end)

(* CR mshinwell: We need a better way of adding the colours to maps. *)

module T = With_map.T

module Set = With_map.Set

module Map = struct
  include With_map.Map

(* CR mshinwell: Fix this, to get map colours.
  let print ?before_key ?after_key print_contents ppf t =
    ignore before_key;
    ignore after_key;
    print ~before_key:(Misc_color.bold_green ())
      ~after_key:(Misc_color.reset ())
      print_contents ppf t
*)
end

module Tbl = With_map.Tbl

let variables_only set = Set.filter is_var set

let symbols_only_map map = Map.filter is_symbol map

let set_of_var_set vars =
  Variable.Set.fold (fun var t_set ->
      Set.add (Var var) t_set)
    vars
    Set.empty

let set_of_symbol_set symbols =
  Symbol.Set.fold (fun symbol t_set ->
      Set.add (Symbol symbol) t_set)
    symbols
    Set.empty

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

let is_predefined_exception t =
  match t with
  | Var _ -> false
  | Symbol sym -> Symbol.is_predefined_exception sym

let rename t =
  match t with
  | Var var -> Var (Variable.rename var)
  | Symbol sym -> Symbol (Symbol.rename sym)

module Pair = struct
  include Identifiable.Make_pair
    (struct type nonrec t = t include With_map end)
    (struct type nonrec t = t include With_map end)

  type nonrec t = t * t
end
