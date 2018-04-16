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
      | Inlinable_and_specialisable args_and_tys ->
        Format.fprintf ppf "(Inlinable_and_specialisable (%a))"
          (Format.pp_print_list print_arg_and_ty) args_and_tys
      | Only_specialisable args_and_tys ->
        Format.fprintf ppf "(Only_specialisable (%a))"
          (Format.pp_print_list print_arg_and_ty) args_and_tys

    let args t =
      match t with
      | Not_inlinable_or_specialisable _ -> []
      | Inlinable_and_specialisable args_and_tys
      | Only_specialisable args_and_tys ->
        List.map (fun (arg, _ty) -> arg) args_and_tys

    let arg_tys t =
      match t with
      | Not_inlinable_or_specialisable args_tys -> args_tys
      | Inlinable_and_specialisable args_and_tys
      | Only_specialisable args_and_tys ->
        List.map (fun (_arg, ty) -> ty) args_and_tys

    let is_inlinable t =
      match t with
      | Not_inlinable_or_specialisable _ -> false
      | Inlinable_and_specialisable _ -> true
      | Only_specialisable _ -> false

    let is_specialisable t =
      match t with
      | Not_inlinable_or_specialisable _ -> None
      | Inlinable_and_specialisable args_and_tys
      | Only_specialisable args_and_tys -> Some args_and_tys
  end

  type t = {
    kind : Kind.t;
    env : TE.t;
  }

  let print ppf t =
    Format.fprintf ppf "@[((kind@ %a)@ (env@ %a))@]"
      Kind.print t.kind
      TE.print t.env

  let args t = Kind.args t.kind
  let arg_tys t = Kind.arg_tys t.kind
  let is_inlinable t = Kind.is_inlinable t.kind
  let is_specialisable t = Kind.is_specialisable t.kind
  let typing_env t = t.env
end

type t = {
  continuation : Continuation.t;
  params : Flambda.Typed_parameter.t list;
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
  { t with
    uses = { Use. env; kind; } :: t.uses;
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

let uses t = t.uses
