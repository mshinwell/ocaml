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

module KP = Kinded_parameter
module T = Flambda_type
module TE = Flambda_type.Typing_env
module TEE = Flambda_type.Typing_env_extension

module Use = struct
  type t = {
    id : Apply_cont_rewrite_id.t;
    arg_types : T.t list;
    typing_env : TE.t;
  }

  let create ~typing_env_at_use:typing_env id ~arg_types =
    { id;
      arg_types;
      typing_env;
    }

  let print ppf { typing_env = _; id = _; arg_types; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(arg_types@ %a)@]@ \
        )@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Flambda_type.print)
      arg_types

  let id t = t.id
  let arg_types t = t.arg_types
  let typing_env_at_use t = t.typing_env
end

type t = {
  continuation : Continuation.t;
  arity : Flambda_arity.t;
  uses : Use.t list;
}

let create continuation arity =
  { continuation;
    arity;
    uses = [];
  }

let print ppf { continuation; arity; uses; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuation@ %a)@]@ \
      @[<hov 1>(arity@ %a)@]@ \
      @[<hov 1>(uses@ %a)@]\
      )@]"
    Continuation.print continuation
    Flambda_arity.print arity
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Use.print) uses

let add_use t ~typing_env_at_use id ~arg_types =
  try
    let arity = T.arity_of_list arg_types in
    if not (Flambda_arity.equal arity t.arity) then begin
      Misc.fatal_errorf "Arity of use (%a) doesn't match continuation's \
          arity (%a)"
        Flambda_arity.print arity
        Flambda_arity.print t.arity
    end;
    let use = Use.create ~typing_env_at_use id ~arg_types in
    { t with
      uses = use :: t.uses;
    }
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sContext is:%s adding use of %a with arg types@ (%a);@ \
          existing uses:@ %a; environment:@ %a"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Continuation.print t.continuation
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) arg_types
      print t
      TE.print typing_env_at_use;
    raise Misc.Fatal_error
  end

let env_and_param_types t ~definition_typing_env =
  let definition_scope_level =
    T.Typing_env.current_scope definition_typing_env
  in
  let cut_point = (* Scope.next *) definition_scope_level in
Format.eprintf "Retrieving env + param types for %a; unknown >= level %a\n%!"
  Continuation.print t.continuation
  Scope.print cut_point;
  let cut_use_environment use =
    let env = Use.typing_env_at_use use in
    let env_extension, _vars_in_scope =
      TE.cut env ~unknown_if_defined_at_or_later_than:cut_point
    in
    env_extension
  in
(*
Format.eprintf "The definition TE is:@ %a\n%!" T.Typing_env.print definition_typing_env;
*)
  let process_use_arg_types use ~allowed =
    let env = Use.typing_env_at_use use in
    List.map (fun ty ->
        T.erase_aliases env ~bound_name:None ~allowed ty)
      (Use.arg_types use)
  in
  match t.uses with
  | [] ->
    definition_typing_env, List.map (fun kind -> T.unknown kind) t.arity,
      Continuation_extra_params_and_args.empty
  | (use :: uses) as all_uses ->
    let use_envs_with_ids_and_extensions =
      List.map (fun use ->
          let typing_env = Use.typing_env_at_use use in
          let id = Use.id use in
          let env_extension = cut_use_environment use in
          typing_env, id, env_extension)
        all_uses
    in
    (* CR mshinwell: Maybe cutting should return a level rather than an
       extension, to save opening all the extensions again? *)
    let joined_env_extension, extra_cse_bindings =
      TEE.n_way_join definition_typing_env use_envs_with_ids_and_extensions
    in
Format.eprintf "joined env extension:@ %a\n%!" TEE.print joined_env_extension;
    let env = TE.add_env_extension definition_typing_env joined_env_extension in
    let env =
      List.fold_left (fun env extra_param ->
          let name =
            Name_in_binding_pos.create (KP.name extra_param)
              Name_occurrence_kind.normal
          in
          TE.add_definition env name (KP.kind extra_param))
        env
        extra_cse_bindings.extra_params
    in
    let allowed = TE.var_domain env in
    let first_arg_types = process_use_arg_types use ~allowed in
    let arg_types =
      List.fold_left (fun joined_arg_types use ->
          List.map2 (fun arg_type arg_type' ->
              T.join env arg_type arg_type')
            joined_arg_types
            (process_use_arg_types use ~allowed))
        first_arg_types
        uses
    in
    env, arg_types, extra_cse_bindings

let number_of_uses t = List.length t.uses

let arity t = t.arity
