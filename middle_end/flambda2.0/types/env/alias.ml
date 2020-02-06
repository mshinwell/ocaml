(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* CR mshinwell: Since we're now baking in the assumption that symbols have
   kind [Value] and name mode [Normal], we can probably simplify some code
   elsewhere. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* This module relies upon the binding time of an alias being uniquely
   determined by the associated [Simple]. *)

type t = Simple.t
type elt = t

let compare = Simple.compare
let equal = Simple.equal
let hash = Simple.hash

module Set = Simple.Set
module Map = Simple.Map
module Tbl = Simple.Tbl

let grand_table_of_aliases = ref (Tbl.create 20_000)

let initialise () =
  grand_table_of_aliases := Tbl.create 20_000

let find_data t = Tbl.find !grand_table_of_aliases t

let create simple binding_time name_mode =
  if Simple.is_symbol simple || Simple.is_const simple then simple
  else
    let simple = Simple.without_rec_info simple in
    let data = Binding_time.With_name_mode.create binding_time name_mode in
    Tbl.add !grand_table_of_aliases simple data;
    simple

let create_symbol t = Simple.symbol t

let simple t = t

let binding_time t =
  if Simple.is_symbol t then Binding_time.symbols
  else if Simple.is_const t then Binding_time.consts_and_discriminants
  else Binding_time.With_name_mode.binding_time (find_data t)

let name_mode t =
  if Simple.is_symbol t || Simple.is_const t then Name_mode.normal
  else Binding_time.With_name_mode.name_mode (find_data t)

let print ppf t =
  Format.fprintf ppf "@[<hov 1>\
      @[<hov 1>(simple@ %a)@]@ \
      @[<hov 1>(binding_time@ %a)@]@ \
      @[<hov 1>(name_mode@ %a)@]\
      @]"
    Simple.print t
    Binding_time.print (binding_time t)
    Name_mode.print (name_mode t)

let output _ _ = Misc.fatal_error "Not implemented"

let defined_earlier t ~than =
  if Simple.equal t than then false
  else
    let time1 = binding_time t in
    let time2 = binding_time than in
    if Binding_time.equal time1 time2 then begin
      Misc.fatal_errorf "Unequal [Alias]es with same binding time: \
          %a and %a"
        print t
        print than
    end;
    Binding_time.strictly_earlier time1 ~than:time2

module Order_within_equiv_class = Name_mode

let order_within_equiv_class t = name_mode t

module Set_ordered_by_binding_time = Stdlib.Set.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if equal t1 t2 then 0
    else if defined_earlier t1 ~than:t2 then -1
    else 1
end)

module T0 = struct
  let compare = compare
  let equal = equal
  let hash = hash
  let print = print
  let output = output
end

include T0
module T = struct
  type nonrec t = t
  include T0
end

let set_to_simple_set set = set
