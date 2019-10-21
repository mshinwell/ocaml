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
  | Immutable_block (tag, fields) ->
    let of_kind_values =
      List.map (fun (field : T.symbol_or_tagged_immediate)
              : Flambda_static.Of_kind_value.t ->
          match field with
          | Symbol sym -> Symbol sym
          | Tagged_immediate imm -> Tagged_immediate imm
          | Constructor imm -> Constructor imm)
        fields
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
    Lifted_constant.create (DE.typing_env (DA.denv dacc))
      (Symbol.Map.singleton symbol ty)
      (Singleton symbol)
      static_part
  in
  let dacc =
    DA.map_r dacc ~f:(fun r -> R.new_lifted_constant r lifted_constant)
  in
  let symbol' = Simple.symbol symbol in
  let term = Named.create_simple symbol' in
  let var_ty = T.alias_type_of (T.kind ty) symbol' in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
(*
Format.eprintf "Equation for lifted constant: %a = %a\n%!"
  Variable.print bound_to T.print ty;
*)
      let denv = DE.add_symbol denv symbol ty in
      DE.add_equation_on_variable denv bound_to var_ty)
  in
(*
Format.eprintf "New DA:@ %a\n%!" DA.print dacc;
*)
  Reachable.reachable term, dacc, var_ty

let try_to_reify dacc (term : Reachable.t) ~bound_to =
  let occ_kind = Var_in_binding_pos.occurrence_kind bound_to in
  let bound_to = Var_in_binding_pos.var bound_to in
  let denv = DA.denv dacc in
  let ty = DE.find_variable denv bound_to in
  match term with
  | Invalid _ ->
    let ty = T.bottom_like ty in
    let denv = DE.add_equation_on_variable denv bound_to ty in
    Reachable.invalid (), DA.with_denv dacc denv, ty
  | Reachable _ ->
    match T.reify (DE.typing_env denv) ~min_occurrence_kind:occ_kind ty with
    | Lift to_lift ->
      if Name_occurrence_kind.is_normal occ_kind then
        let static_part = create_static_part to_lift in
        lift dacc ty ~bound_to static_part
      else
        term, dacc, ty
    | Simple simple ->
      (* CR mshinwell: Think about whether this is the best way of handling
         this. *)
      (* It is possible that the only [Simple] that [reify] could return is
         in fact [bound_to] -- for example when all other aliases are of
         an unsuitable occurrence kind. *)
      if Simple.equal (Simple.var bound_to) simple then term, dacc, ty
      else Reachable.reachable (Named.create_simple simple), dacc, ty
    | Cannot_reify -> term, dacc, ty
    | Invalid ->
      let ty = T.bottom_like ty in
      let denv = DE.add_equation_on_variable denv bound_to ty in
      Reachable.invalid (), DA.with_denv dacc denv, ty
