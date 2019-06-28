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
(*   special exception on linking described in the file LICENSDE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module K = Flambda_kind
module R = Simplify_env_and_result.Result
module T = Flambda_type

let create_static_part (to_lift : T.to_lift)
    : K.value Flambda_static.Static_part.t =
  match to_lift with
  | Immutable_block (tag, symbols) ->
    let of_kind_values =
      List.map (fun sym : Flambda_static.Of_kind_value.t -> Symbol sym)
        symbols
    in
    Block (tag, Immutable, of_kind_values)
  | Boxed_float f -> Boxed_float (Const f)
  | Boxed_int32 i -> Boxed_int32 (Const i)
  | Boxed_int64 i -> Boxed_int64 (Const i)
  | Boxed_nativeint i -> Boxed_nativeint (Const i)

let lift dacc ty ~bound_to static_part =
(*
Format.eprintf "Lifting something bound to %a, type:@ %a@ backtrace:%s\n%!"
  Variable.print bound_to
  T.print ty
  (Printexc.raw_backtrace_to_string (Printexc.get_callstack 15));
*)
  let symbol =
    Symbol.create (Compilation_unit.get_current_exn ())
      (Linkage_name.create (Variable.unique_name bound_to))
  in
  if not (K.equal (T.kind ty) K.value) then begin
    (* Sets of closures may be lifted and are not of kind [Value], but they
       are dealt with directly in [Simplify_named]. *)
    Misc.fatal_errorf "Cannot lift non-[Value] variable: %a"
      Variable.print bound_to
  end;
  let lifted_constant =
    Lifted_constant.create (Symbol.Map.singleton symbol ty)
      (Singleton symbol)
      static_part
  in
  let dacc =
    DA.map_r dacc ~f:(fun r -> R.new_lifted_constant r lifted_constant)
  in
  let symbol' = Simple.symbol symbol in
  let term = Named.create_simple symbol' in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
      let denv = DE.add_symbol denv symbol ty in
      let ty = T.alias_type_of (T.kind ty) symbol' in
(*
Format.eprintf "Equation for lifted constant: %a = %a\n%!"
  Variable.print bound_to T.print ty;
*)
      DE.add_equation_on_variable denv bound_to ty)
  in
(*
Format.eprintf "New DA:@ %a\n%!" DA.print dacc;
*)
  Reachable.reachable term, dacc

let try_to_reify dacc (term : Reachable.t) ~bound_to ~cannot_lift =
  let occ_kind = Var_in_binding_pos.occurrence_kind bound_to in
  let bound_to = Var_in_binding_pos.var bound_to in
  if not (Name_occurrence_kind.is_normal occ_kind) then
    term, dacc
  else
    let denv = DA.denv dacc in
    let ty = DE.find_variable denv bound_to in
    match term with
    | Invalid _ -> 
      let ty = T.bottom_like ty in
      let denv = DE.add_equation_on_variable denv bound_to ty in
      term, (DA.with_denv dacc denv)
    | Reachable _ ->
      match T.reify (DE.typing_env denv) ty with
      | Lift to_lift ->
        if cannot_lift then term, dacc
        else
          let static_part = create_static_part to_lift in
          lift dacc ty ~bound_to static_part
      | Cannot_reify -> term, dacc
      | Invalid ->
        let ty = T.bottom_like ty in
        let denv = DE.add_equation_on_variable denv bound_to ty in
        Reachable.invalid (), DA.with_denv dacc denv
