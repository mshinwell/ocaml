(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module T = Flambda_type
module TE = Flambda_type.Typing_env
module TEE = Flambda_type.Typing_env_extension

module Use = struct
  module Kind = struct
    type t =
      | Not_inlinable_or_specialisable of T.t list
      | Inlinable_and_specialisable of (Simple.t * T.t) list
      | Only_specialisable of (Simple.t * T.t) list

    let not_inlinable_or_specialisable ~param_tys =
      Not_inlinable_or_specialisable param_tys

    let inlinable_and_specialisable ~args_with_tys =
      Inlinable_and_specialisable args_with_tys

    let only_specialisable ~args_with_tys =
      Only_specialisable args_with_tys

    let print ppf t =
      let print_arg_and_ty ppf (arg, ty) =
        Format.fprintf ppf "(%a %a)"
          Simple.print arg
          T.print ty
      in
      match t with
      | Not_inlinable_or_specialisable args_tys ->
        Format.fprintf ppf "(Not_inlinable_or_specialisable (%a))"
          (Format.pp_print_list T.print) args_tys
      | Inlinable_and_specialisable args_with_tys ->
        Format.fprintf ppf "(Inlinable_and_specialisable (%a))"
          (Format.pp_print_list print_arg_and_ty) args_with_tys
      | Only_specialisable args_with_tys ->
        Format.fprintf ppf "(Only_specialisable (%a))"
          (Format.pp_print_list print_arg_and_ty) args_with_tys

    let args_with_tys t =
      match t with
      | Not_inlinable_or_specialisable _ -> None
      | Inlinable_and_specialisable args_with_tys
      | Only_specialisable args_with_tys -> Some args_with_tys

(*
    let args t =
      match t with
      | Not_inlinable_or_specialisable _ -> []
      | Inlinable_and_specialisable args_with_tys
      | Only_specialisable args_with_tys ->
        List.map (fun (arg, _ty) -> arg) args_with_tys
*)

    let arg_tys t =
      match t with
      | Not_inlinable_or_specialisable args_tys -> args_tys
      | Inlinable_and_specialisable args_with_tys
      | Only_specialisable args_with_tys ->
        List.map (fun (_arg, ty) -> ty) args_with_tys

(*
    let args_with_tys t =
      match t with
      | Not_inlinable_or_specialisable args_tys ->
        List.map (fun ty -> None, ty) args_tys
      | Inlinable_and_specialisable args_with_tys
      | Only_specialisable args_with_tys ->
        List.map (fun (arg, ty) -> Some arg, ty) args_with_tys
*)

    let is_inlinable t =
      match t with
      | Not_inlinable_or_specialisable _ -> false
      | Inlinable_and_specialisable _ -> true
      | Only_specialisable _ -> false

(*
    let is_specialisable t =
      match t with
      | Not_inlinable_or_specialisable _ -> None
      | Inlinable_and_specialisable args_with_tys
      | Only_specialisable args_with_tys -> Some args_with_tys
*)
  end

  type t = {
    kind : Kind.t;
    params : T.Parameters.t;
  }

  let print ppf { kind; params; } =
    Format.fprintf ppf "@[((kind@ %a)@ (params@ %a))@]"
      Kind.print kind
      T.Parameters.print params

  let parameters t = t.params

(*
  let arg_tys t = Kind.arg_tys t.kind
  let args_with_tys t = Kind.args_with_tys t.kind
*)

  let is_inlinable t = Kind.is_inlinable t.kind

(*
  let is_specialisable t = Kind.is_specialisable t.kind
*)
end

type t = {
  continuation : Continuation.t;
  params : T.Parameters.t;
  definition_scope_level : Scope_level.t;
  uses : Use.t list;
}

let create ~continuation ~params ~definition_scope_level =
  { continuation;
    params;
    definition_scope_level;
    uses = [];
  }

let update_parameters t ~params =
  { t with
    params;
  }

let union t1 t2 =
  if not (Continuation.equal t1.continuation t2.continuation) then begin
    Misc.fatal_errorf "Cannot union [Continuation_uses.t] for two \
        different continuations (%a and %a)"
      Continuation.print t1.continuation
      Continuation.print t2.continuation
  end;
  { continuation = t1.continuation;
    params = t1.params;
    definition_scope_level = t1.definition_scope_level;
    uses = t1.uses @ t2.uses;
  }

let print ppf t =
  Format.fprintf ppf "(%a uses =@ (%a))"
    Continuation.print t.continuation
    (Format.pp_print_list Use.print) t.uses

let add_use t env kind =
  (* CR mshinwell: Instead, consider cutting further down. *)
  let cut_point = Scope_level.next t.definition_scope_level in
  let env_extension =
    TE.cut env ~existential_if_defined_at_or_later_than:cut_point
  in
  let arity = T.Parameters.arity t.params in
  let arg_tys = Use.Kind.arg_tys kind in
  if List.compare_lengths arity arg_tys <> 0 then begin
    Misc.fatal_errorf "Proposed use of continuation %a doesn't match arity \
        %a: %a"
      Continuation.print t.continuation
      Flambda_arity.print arity
      Use.Kind.print kind
  end;
  let fresh_kinded_params =
    List.mapi (fun index kind ->
        let name =
          Format.asprintf "%a_arg%d"
            Continuation.print t.continuation
            index
        in
        let param = Parameter.wrap (Variable.create name) in
        Kinded_parameter.create param kind)
      arity
  in
  let env_extension =
    match Use.Kind.args_with_tys kind with
    | None -> env_extension
    | Some args_with_tys ->
      List.fold_left
        (fun env_extension (fresh_kinded_param, (arg, ty)) ->
            let kind = Kinded_parameter.kind fresh_kinded_param in
            assert (Flambda_kind.equal kind (T.kind ty));
            TEE.add_equation env_extension
              (Kinded_parameter.name fresh_kinded_param)
              (T.alias_type_of kind arg))
        env_extension
        (List.combine fresh_kinded_params args_with_tys)
  in
  let params =
    T.Parameters.create_with_env_extension fresh_kinded_params env_extension
  in
  { t with
    uses = { Use. kind; params; } :: t.uses;
  }

let num_uses' t : Num_continuation_uses.t =
  match t.uses with
  | [] -> Zero
  | [_] -> One
  | _ -> Many

let unused t =
  match num_uses' t with
  | Zero -> true
  | One | Many -> false

let linearly_used t =
  match num_uses' t with
  | Zero -> false
  | One -> true
  | Many -> false

let num_uses t = List.length t.uses

let linearly_used_in_inlinable_position t =
  match t.uses with
  | [use] when Use.Kind.is_inlinable use.kind -> true
  | _ -> false

let continuation t = t.continuation
let uses t = t.uses
let params t = t.params
let definition_scope_level t = t.definition_scope_level
