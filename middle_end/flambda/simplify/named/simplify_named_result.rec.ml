(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

type descr =
  | Zero_terms
  | Single_term of Bindable_let_bound.t * Simplified_named.t
  | Multiple_bindings_to_symbols of Symbol.t Var_in_binding_pos.Map.t

type t = {
  dacc : DA.t;
  descr : descr;
}

let have_simplified_to_zero_terms dacc =
  { dacc;
    descr = Zero_terms;
  }

let have_simplified_to_single_term dacc bindable_let_bound defining_expr =
  { dacc;
    descr = Single_term (bindable_let_bound, defining_expr);
  }

let have_lifted_set_of_closures dacc bound_vars_to_symbols =
  { dacc;
    descr = Multiple_bindings_to_symbols bound_vars_to_symbols;
  }

let descr t = t.descr
let dacc t = t.dacc

let bindings_to_place_in_any_order t =
  match t.descr with
  | Zero_terms -> []
  | Single_term (bindable_let_bound, defining_expr) ->
    [bindable_let_bound, defining_expr]
  | Multiple_bindings_to_symbols bound_vars_to_symbols ->
    Var_in_binding_pos.Map.fold (fun bound_var symbol bindings ->
        let bindable_let_bound = Bindable_let_bound.singleton bound_var in
        let defining_expr =
          Simple.symbol symbol
          |> Named.create_simple
          |> Simplified_named.reachable
        in
        (bindable_let_bound, defining_expr) :: bindings)
      bound_vars_to_symbols
      []
