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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

open! Flambda.Import

module B = Inlining_cost.Benefit
module E = Simplify_env_and_result.Env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module S = Simplify_simple
module T = Flambda_type

let simplify_make_block env r prim dbg
      ~(make_block_kind : Flambda_primitive.make_block_kind)
      ~(mutable_or_immutable : Flambda_primitive.mutable_or_immutable)
      args =
  let original_args = args in
  let args_with_tys = S.simplify_simples env args in
  let args, arg_tys = List.split args_with_tys in
  let original_term = Named.prim (Variadic (prim, args)) dbg in
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
      let term =
        Named.prim
          (Variadic (
            Make_block (Full_of_values (tag, value_kinds),
              mutable_or_immutable),
            args))
          dbg
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
          T.block_of_values tag ~fields:(Array.of_list fields)
        | Mutable -> T.any_value ()
      in
      Reachable.reachable term, ty, r
    end
  | Full_of_naked_floats ->
    Reachable.reachable original_term, T.any_value (), r
  | Generic_array _spec -> Misc.fatal_error "Not yet implemented"

let simplify_bigarray_set env r prim dbg ~num_dims ~kind ~layout ~args =
  let args_with_tys = S.simplify_simples_and_drop_types env args in
  let original_term = Named.prim (Variadic (prim, args)) dbg in
  let element_kind = Flambda_primitive.element_kind_of_bigarray_kind kind in
  let wrong_number_of_args () =
    Misc.fatal_errorf "Wrong number of arguments for [Bigarray_set]: %a"
      (Format.pp_print_list Simple.print) original_args
  in
  match args_with_tys with
  | (_bigarray, bigarray_ty)::args_with_tys ->
    begin match Misc.Stdlib.List.first_n args_with_tys num_dims with
    | Some (indexes, args) ->
      begin match args with
      | [_new_value] ->
        Reachable.reachable (original_term ()), T.unit (), r
      | _ -> wrong_number_of_args ()
      end
    | None -> wrong_number_of_args ()
    end
  | [] -> wrong_number_of_args ()

let simplify_bigarray_load env r prim dbg ~num_dims
      ~(kind : Flambda_primitive.bigarray_kind)
      ~(layout : Flambda_primitive.bigarray_layout)
      args =
  let args = S.simplify_simples_and_drop_types env args in
  let element_kind = Flambda_primitive.element_kind_of_bigarray_kind kind in
  let original_term = Named.prim (Variadic (prim, args)) dbg in
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
        Reachable.reachable (original_term ()), T.unknown element_kind, r
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
