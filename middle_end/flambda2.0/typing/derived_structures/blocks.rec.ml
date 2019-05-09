(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Tag_and_targetint_ocaml =
  Hashtbl.Make_with_map_pair (Tag) (Targetint.OCaml)

module RL =
  Row_like.Make (Tag) (Targetint.OCaml) (Tag_and_targetint_ocaml) (Product)

type t = RL.t

type open_or_closed = Open | Closed of Tag.t

let create ~field_tys open_or_closed : t =
  let product = Product.create field_tys in
  let size = Targetint.OCaml.of_int (List.length field_tys) in
  match open_or_closed with
  | Open -> RL.create_at_least size product
  | Closed tag -> RL.create_exactly tag size product

let create_bottom = RL.create_bottom 

let _invariant _t = () (* CR mshinwell: RL.invariant *)

let print_with_cache = RL.print

let equal = RL.equal
let is_empty = RL.is_bottom

let meet env t1 t2 : _ Or_bottom.t =
  match RL.meet env t1 t2 with
  | Bottom -> Bottom
  | Ok (t, product) ->
    Ok (t, RP.standalone_extension product)

let join = RL.join

let free_names = RL.free_names
let apply_name_permutation = RL.apply_name_permutation

let classify = RL.classify
