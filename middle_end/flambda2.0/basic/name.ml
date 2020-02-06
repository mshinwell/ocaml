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

open! Int_replace_polymorphic_compare

module Id = Table_by_int_id.Id

type t = int

let var_flag = 0
let symbol_flag = 1

let () =
  assert (Id.flags_size_in_bits >= 1)

(* CR mshinwell: Think about the use of [Obj].  It seems worse to have to
   remove the [private] in e.g. variable.mli.

   Maybe one option would be to turn the hierarchy around so Name is actually
   the thing doing the creation, rather than depending on Variable and
   Symbol.  There could just be Name, in fact, perhaps...

   Another option: put Variable, Symbol, Name (and in the future Simple) all
   in the same .ml file. *)

let var v = ((Obj.magic v) : t)
let symbol s = Id.with_flags ((Obj.magic s) : t) symbol_flag

let [@inline always] pattern_match t ~var ~symbol =
  let flags = Id.flags t in
  let var_or_symbol = Id.without_flags t in
  if flags = var_flag then var ((Obj.magic var_or_symbol) : Variable.t)
  else if flags = symbol_flag then symbol ((Obj.magic var_or_symbol) : Symbol.t)
  else assert false

let is_var t = pattern_match t ~var:(fun _ -> true) ~symbol:(fun _ -> false)

let is_symbol t = pattern_match t ~var:(fun _ -> false) ~symbol:(fun _ -> true)

let map_var t ~f =
  pattern_match t ~var:(fun v -> var (f v)) ~symbol:(fun _ -> t)

let map_symbol t ~f =
  pattern_match t ~var:(fun _ -> t) ~symbol:(fun s -> symbol (f s))

let to_var t =
  pattern_match t ~var:(fun var -> Some var) ~symbol:(fun _ -> None)

let to_symbol t =
  pattern_match t ~var:(fun _ -> None) ~symbol:(fun symbol -> Some symbol)

module With_map = Identifiable.Make (struct
  type nonrec t = t

  let print ppf t =
    Format.fprintf ppf "@<0>%s" (Flambda_colours.name ());
    pattern_match t
      ~var:(fun var -> Variable.print ppf var)
      ~symbol:(fun symbol -> Symbol.print ppf symbol);
    Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t

  let hash t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else if t1 < t2 then (-1)
    else 1

  let equal t1 t2 =
    t1 == t2
end)

(* CR mshinwell: We need a better way of adding the colours to maps. *)

let print = With_map.print
let output = With_map.output
let hash = With_map.hash
let equal = With_map.equal
let compare = With_map.compare

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

let symbols_only_map map = Map.filter (fun t _ -> is_symbol t) map

let set_of_var_set vars =
  Variable.Set.fold (fun v t_set ->
      Set.add (var v) t_set)
    vars
    Set.empty

let set_of_symbol_set symbols =
  Symbol.Set.fold (fun sym t_set ->
      Set.add (symbol sym) t_set)
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
  pattern_match t
    ~var:(fun var ->
      Format.fprintf ppf "@[(Var %a)@]" Variable.print var)
    ~symbol:(fun sym ->
      Format.fprintf ppf "@[(Symbol %a)@]" Symbol.print sym)

let is_predefined_exception t =
  pattern_match t
    ~var:(fun _ -> false)
    ~symbol:(fun sym -> Symbol.is_predefined_exception sym)

let rename t =
  (* CR mshinwell: It's odd that this doesn't rename symbols.  However things
     break if we do... *)
  pattern_match t
    ~var:(fun v -> var (Variable.rename v))
    ~symbol:(fun _ -> t)

let must_be_var_opt t =
  pattern_match t
    ~var:(fun var -> Some var)
    ~symbol:(fun _ -> None)

let must_be_symbol t =
  pattern_match t
    ~var:(fun _ -> Misc.fatal_errorf "Must be a symbol:@ %a" print t)
    ~symbol:(fun sym -> sym)

let must_be_symbol_opt t =
  pattern_match t
    ~var:(fun _ -> None)
    ~symbol:(fun sym -> Some sym)

module Pair = struct
  include Identifiable.Make_pair
    (struct type nonrec t = t include With_map end)
    (struct type nonrec t = t include With_map end)

  type nonrec t = t * t
end
