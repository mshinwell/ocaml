(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let run phantom_lets =
  let output = Ident.Tbl.create 42 in
  let rec resolve_phantom_let target_ident
        : Mach.phantom_defining_expr option =
    match Ident.Tbl.find output target_ident with
    | (_provenance, defining_expr) -> Some defining_expr
    | exception Not_found ->
      match Ident.Map.find target_ident phantom_lets with
      | exception Not_found -> None
      | ((provenance, defining_expr) :
          (Clambda.ulet_provenance * Clambda.uphantom_defining_expr)) ->
        let resolves_to defining_expr =
          Ident.Tbl.add output target_ident (provenance, defining_expr);
          Some defining_expr
        in
        match defining_expr with
        | Uphantom_const (Uconst_ref (symbol, _defining_expr)) ->
          (* It's not actually a "fun_name", but the mangling is the same.
             This should go away if we switch to [Symbol.t] everywhere. *)
          let symbol = Name_laundry.fun_name_to_symbol symbol in
          resolves_to (Mach.Iphantom_const_symbol symbol)
        | Uphantom_read_symbol_field (
            Uconst_ref (symbol, _defining_expr), field) ->
          let symbol = Name_laundry.fun_name_to_symbol symbol in
          resolves_to (Mach.Iphantom_read_symbol_field (symbol, field))
        | Uphantom_read_symbol_field _ ->
          Misc.fatal_errorf "Resolve_phantom_ranges: unknown Clambda \
            constant pattern for Uphantom_read_symbol_field"
        | Uphantom_const (Uconst_int i)
        | Uphantom_const (Uconst_ptr i) ->
          resolves_to (Mach.Iphantom_const_int i)
        | Uphantom_var defining_ident ->
          begin match resolve_phantom_let defining_ident with
          | Some defining_expr -> Some defining_expr
          | None ->
            (* In this case we assume that [defining_ident] is a non-phantom
               identifier.  An error will be produced later in the compiler's
               pipeline if that is found not to be the case. *)
            Some (Mach.Iphantom_var defining_ident)
          end
        | Uphantom_read_var_field (defining_ident, field) ->
          begin match resolve_phantom_let defining_ident with
          | None -> None
          | Some defining_expr ->
            resolves_to (Mach.Iphantom_read_var_field (defining_expr, field))
          end
        | Uphantom_offset_var_field (defining_ident, offset_in_words) ->
          begin match resolve_phantom_let defining_ident with
          | None -> None
          | Some defining_expr ->
            resolves_to
              (Mach.Iphantom_offset_var (defining_expr, offset_in_words))
          end
  in
  Ident.Map.iter (fun target_ident _provenance_and_defining_expr ->
      ignore ((resolve_phantom_let target_ident)
        : Mach.phantom_defining_expr option))
    phantom_lets;
  Ident.Tbl.to_map output
