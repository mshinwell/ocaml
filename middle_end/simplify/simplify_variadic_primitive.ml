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

module B = Inlining_cost.Benefit
module E = Simplify_env_and_result.Env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type

module Named = Flambda.Named
module Reachable = Flambda.Reachable

let simplify_make_block env r prim dbg
      ~(make_block_kind : Flambda_primitive.make_block_kind)
      ~(mutable_or_immutable : Flambda_primitive.mutable_or_immutable)
      args =
  let original_args = args in
  let args_with_tys = S.simplify_simples env args in
  let args, arg_tys = List.split args_with_tys in
  let original_term () : Named.t = Prim (Variadic (prim, args), dbg) in
  let invalid () =
    Reachable.invalid (), T.bottom (K.value ()),
      R.map_benefit r (B.remove_primitive (Variadic prim))
  in
  match make_block_kind with
  | Full_of_values (tag, value_kinds) ->
    if List.compare_lengths value_kinds args <> 0 then begin
      (* CR mshinwell: improve message *)
      Misc.fatal_errorf "GC value_kind indications in [Make_block] don't \
          match up 1:1 with arguments: %a"
        Simple.List.print original_args
    end;
    (* CR mshinwell: This could probably be done more neatly. *)
    let found_bottom = ref false in
    let arg_ty_values =
      assert (List.compare_lengths value_kinds args_with_tys = 0);
      List.map2 (fun ((arg : Simple.t), arg_ty) _value_kind ->
          if T.is_bottom (E.get_typing_environment env) arg_ty then begin
           found_bottom := true
          end;
          match arg with
          | Const _ -> T.force_to_kind_value arg_ty
          | Name name -> T.alias_type_of_as_ty_value name
          | Discriminant _ -> assert false  (* CR mshinwell: proper error! *) )
        args_with_tys value_kinds
    in
    if !found_bottom then begin
      invalid ()
    end else begin
      assert (List.compare_lengths arg_ty_values value_kinds = 0);
      let term : Named.t =
        Prim (Variadic (
          Make_block (Full_of_values (tag, value_kinds),
            mutable_or_immutable),
          args), dbg)
      in
      let ty =
        match mutable_or_immutable with
        | Immutable ->
          let fields =
            List.map (fun arg_ty_value : _ T.mutable_or_immutable ->
                Immutable arg_ty_value)
              arg_ty_values
          in
          (* CR mshinwell: eliminate gratuitous array allocations *)
          T.block_of_values tag
            ~fields:(Array.of_list fields)
        | Mutable ->
          let fields =
            List.map (fun _arg_ty : _ T.mutable_or_immutable -> Mutable)
              arg_ty_values
          in
          T.block_of_values tag ~fields:(Array.of_list fields)
      in
      Reachable.reachable term, ty, r
    end
  | Full_of_naked_floats ->
    let found_bottom = ref false in
    let arg_ty_naked_numbers =
      List.map (fun arg_ty ->
          if T.is_bottom (E.get_typing_environment env) arg_ty then begin
             found_bottom := true;
          end;
          T.prove_of_kind_naked_float arg_ty)
        arg_tys
    in
    if !found_bottom then begin
      invalid ()
    end else begin
      let ty =
        match mutable_or_immutable with
        | Immutable ->
          T.immutable_float_array (Array.of_list arg_ty_naked_numbers)
        | Mutable ->
          T.mutable_float_array
            ~size:(Targetint.OCaml.of_int (List.length arg_ty_naked_numbers))
      in
      Reachable.reachable (original_term ()), ty, r
    end
  | Generic_array _spec -> Misc.fatal_error "Not yet implemented"
  (* CR mshinwell: Finish off
    Simplify_generic_array.simplify_make_block env r prim dbg spec
      ~mutable_or_immutable args
  *)

(* CR mshinwell: Could use [unit or_invalid] rather than [bool] *)
(* XXX this should be "for all" not "exists".  Also, take care: when the list
   is empty, this should return false *)
let bigarray_indexes_are_invalid env
      (layout : Flambda_primitive.bigarray_layout) indexes =
  let index_proofs =
    List.map (fun index ->
        T.prove_tagged_immediate (E.get_typing_environment env) index)
      indexes
  in
  List.fold_left
    (fun invalid (index_proof : Immediate.Set.t T.proof) ->
      if invalid then true
      else
        match index_proof with
        | Proved indexes ->
          Immediate.Set.fold (fun index invalid ->
              if invalid then true
              else
                let index = Immediate.to_targetint index in
                match layout with
                | Unknown | C ->
                  Targetint.OCaml.compare index Targetint.OCaml.zero < 0
                | Fortran ->
                  Targetint.OCaml.compare index Targetint.OCaml.one < 0)
            indexes
            false
        | Unknown -> false
        | Invalid -> true)
    false
    index_proofs

(* XXX add "type_of_bigarray_kind" etc in this file. *)

let simplify_bigarray_set env r prim dbg ~num_dims ~kind ~layout ~args =
  let original_args = args in
  let args_with_tys = S.simplify_simples env args in
  let args, _arg_tys = List.split args_with_tys in
  let original_term () : Named.t = Prim (Variadic (prim, args), dbg) in
  let element_kind = Flambda_primitive.element_kind_of_bigarray_kind kind in
  let invalid () =
    Reachable.invalid (), T.unit_bottom (),
      R.map_benefit r (B.remove_primitive (Variadic prim))
  in
  let wrong_number_of_args () =
    Misc.fatal_errorf "Wrong number of arguments for [Bigarray_set]: %a"
      (Format.pp_print_list Simple.print) original_args
  in
  match args_with_tys with
  | (_bigarray, bigarray_ty)::args_with_tys ->
    begin match Misc.Stdlib.List.first_n args_with_tys num_dims with
    | Some (indexes_with_tys, args_with_tys) ->
      begin match args_with_tys with
      | [_new_value, new_value_ty] ->
        if T.is_bottom (E.get_typing_environment env) bigarray_ty
          || T.is_bottom (E.get_typing_environment env) new_value_ty
        then begin
          invalid ()
        end else begin
          ignore (T.force_to_kind_value bigarray_ty);
          let _indexes, index_tys = List.split indexes_with_tys in
          let indexes_are_invalid =
            bigarray_indexes_are_invalid env layout index_tys
          in
          if indexes_are_invalid then begin
            invalid ()
          end else begin
            T.check_of_kind new_value_ty element_kind;
            Reachable.reachable (original_term ()), T.unit (), r
          end
        end
      | _ -> wrong_number_of_args ()
      end
    | None -> wrong_number_of_args ()
    end
  | [] -> wrong_number_of_args ()

let simplify_bigarray_load env r prim dbg ~num_dims
      ~(kind : Flambda_primitive.bigarray_kind)
      ~(layout : Flambda_primitive.bigarray_layout)
      args =
  let original_args = args in
  let args_with_tys = S.simplify_simples env args in
  let args, _arg_tys = List.split args_with_tys in
  let original_term () : Named.t = Prim (Variadic (prim, args), dbg) in
  let element_kind = Flambda_primitive.element_kind_of_bigarray_kind kind in
  let invalid () =
    Reachable.invalid (), T.unit_bottom (),
      R.map_benefit r (B.remove_primitive (Variadic prim))
  in
  let wrong_number_of_args () =
    Misc.fatal_errorf "Wrong number of arguments for [Bigarray_load]: %a"
      (Format.pp_print_list Simple.print) original_args
  in
  match args_with_tys with
  | (_bigarray, bigarray_ty)::args_with_tys ->
    begin match Misc.Stdlib.List.first_n args_with_tys num_dims with
    | Some (indexes_with_tys, args_with_tys) ->
      begin match args_with_tys with
      | [] ->
        if T.is_bottom (E.get_typing_environment env) bigarray_ty then begin
          invalid ()
        end else begin
          ignore (T.prove_of_kind_value bigarray_ty);
          let _indexes, index_tys = List.split indexes_with_tys in
          let indexes_are_invalid =
            bigarray_indexes_are_invalid env layout index_tys
          in
          if indexes_are_invalid then begin
            invalid ()
          end else begin
            Reachable.reachable (original_term ()), T.unknown element_kind, r
          end
        end
      | _ -> wrong_number_of_args ()
      end
    | None -> wrong_number_of_args ()
    end
  | [] -> wrong_number_of_args ()

let simplify_variadic_primitive env r
      (prim : Flambda_primitive.variadic_primitive) args dbg =
  match prim with
  | Make_block (make_block_kind, mutable_or_immutable) ->
    simplify_make_block env r prim dbg ~make_block_kind ~mutable_or_immutable
      args
  | Bigarray_set (num_dims, kind, layout) ->
    simplify_bigarray_set env r prim dbg ~num_dims ~kind ~layout ~args
  | Bigarray_load (num_dims, kind, layout) ->
    simplify_bigarray_load env r prim dbg ~num_dims ~kind ~layout args
