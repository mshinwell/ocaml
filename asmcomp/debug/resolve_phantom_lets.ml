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
  let phantom_lets = Ident.Tbl.of_map phantom_lets in
  let output = Ident.Tbl.create 42 in
  let resolves_to ~target_ident ~provenance ~defining_expr =
    Ident.Tbl.add output target_ident (provenance, defining_expr);
    Some defining_expr
  in
  let rec resolve_phantom_let ~target_ident
        ~(provenance_and_defining_expr
           : (Clambda.ulet_provenance option
             * Clambda.uphantom_defining_expr option)) =
    let provenance, defining_expr = provenance_and_defining_expr in
    match Ident.Tbl.find output target_ident with
    | (_provenance, defining_expr) ->
      (* We've resolved this phantom let already. *)
      Some defining_expr
    | exception Not_found ->
      let resolves_to defining_expr =
        resolves_to ~target_ident ~provenance ~defining_expr
      in
      match defining_expr with
      | None ->
        (* The defining expression of this phantom let is never
            going to be available, perhaps because it was some expression
            that is not currently supported. *)
        None
      | Some (Uphantom_const (Uconst_ref (symbol, _defining_expr))) ->
        (* It's not actually a "fun_name", but the mangling is the same.
            This should go away if we switch to [Symbol.t] everywhere. *)
        let symbol = Name_laundry.fun_name_to_symbol symbol in
        resolves_to (Mach.Iphantom_const_symbol symbol)
      | Some (Uphantom_read_symbol_field (
          Uconst_ref (symbol, _defining_expr), field)) ->
        let symbol = Name_laundry.fun_name_to_symbol symbol in
        resolves_to (Mach.Iphantom_read_symbol_field (symbol, field))
      | Some Uphantom_read_symbol_field _ ->
        Misc.fatal_errorf "Resolve_phantom_ranges: unknown Clambda \
          constant pattern for Uphantom_read_symbol_field"
      | Some (Uphantom_const (Uconst_int i))
      | Some (Uphantom_const (Uconst_ptr i)) ->
        resolves_to (Mach.Iphantom_const_int i)
      | Some (Uphantom_var defining_ident) ->
        begin match resolve_variable ~defining_ident with
        | None -> None
        | Some defining_expr -> resolves_to defining_expr
        end
      | Some (Uphantom_read_var_field (defining_ident, field)) ->
        begin match resolve_variable ~defining_ident with
        | None -> None
        | Some defining_expr ->
          resolves_to (Mach.Iphantom_read_var_field (defining_expr, field))
        end
      | Some (Uphantom_offset_var_field (defining_ident,
          offset_in_words)) ->
        begin match resolve_variable ~defining_ident with
        | None -> None
        | Some defining_expr ->
          resolves_to (
            Mach.Iphantom_offset_var (defining_expr, offset_in_words))
        end
      | Some (Uphantom_block { tag; fields; }) ->
        let fields =
          List.map (fun field -> resolve_variable ~defining_ident:field)
            fields
        in
        resolves_to (Mach.Iphantom_block { tag; fields; })

  and resolve_variable ~defining_ident : Mach.phantom_defining_expr option =
    (* [defining_ident] is a variable that occurs within the definition of
       some phantom let.  Find out whether this identifier is actually bound
       by another phantom let; if not, it is expected to be a normal
       identifier. *)
    match Ident.Tbl.find phantom_lets defining_ident with
    | exception Not_found ->
      (* We expect [defining_ident] to be a normal identifier. *)
      Some (Mach.Iphantom_var defining_ident)
    | provenance_and_defining_expr ->
      resolve_phantom_let ~target_ident:defining_ident
        ~provenance_and_defining_expr
  in
  Ident.Tbl.iter (fun target_ident provenance_and_defining_expr ->
      ignore ((resolve_phantom_let ~target_ident
        ~provenance_and_defining_expr) : Mach.phantom_defining_expr option))
    phantom_lets;
  Ident.Tbl.to_map output
