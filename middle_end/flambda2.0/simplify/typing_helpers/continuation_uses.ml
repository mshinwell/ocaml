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

module Use = struct
  type t = {
    id : Apply_cont_rewrite_id.t;
    kind : Continuation_use_kind.t;
    arg_types : T.t list;
    typing_env : TE.t;
  }

  let create kind ~typing_env_at_use:typing_env id ~arg_types =
    { id;
      kind;
      arg_types;
      typing_env;
    }

  let print ppf { typing_env = _; id = _; kind = _; arg_types; } =
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

let add_use t kind ~typing_env_at_use id ~arg_types =
  try
    let arity = T.arity_of_list arg_types in
    if not (Flambda_arity.equal arity t.arity) then begin
      Misc.fatal_errorf "Arity of use (%a) doesn't match continuation's \
          arity (%a)"
        Flambda_arity.print arity
        Flambda_arity.print t.arity
    end;
    let use = Use.create kind ~typing_env_at_use id ~arg_types in
(*
Format.eprintf "For %a, recording use:@ %a\n%!"
  Continuation.print t.continuation
  Use.print use;
*)
    { t with
      uses = use :: t.uses;
    }
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sContext is:%s adding use of %a with \
          arg types@ (%a);@ existing uses:@ %a; environment:@ %a"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Continuation.print t.continuation
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) arg_types
      print t
      TE.print typing_env_at_use;
    raise Misc.Fatal_error
  end

let env_and_param_types t ~definition_typing_env
      : Continuation_env_and_param_types.t =
  let definition_scope_level =
    T.Typing_env.current_scope definition_typing_env
  in
  let free_vars_at_definition_point =
    T.Typing_env.var_domain definition_typing_env
  in
  let process_use_arg_types use ~allowed perm = (* CR mshinwell: rename *)
    let env = Use.typing_env_at_use use in
    let arg_types_rev, perm =
      List.fold_left (fun (arg_types_rev, perm) arg_type ->
          let free_vars, arg_type =
            T.reduce_then_return_free_vars env ~allowed arg_type
          in
          let free_vars_to_black_hole =
            Variable.Set.diff free_vars free_vars_at_definition_point
          in
          let perm =
            Variable.Set.fold (fun var perm ->
                Name_permutation.add_black_hole perm var)
              free_vars_to_black_hole
              perm
          in
          arg_ty :: arg_types_rev, perm)
        ([], perm)
        (Use.arg_types use)
    in
    List.rev arg_types_rev, perm
  in
  match t.uses with
  | [] -> No_uses
  | (use :: uses) as all_uses ->
    let use_envs_with_ids =
      List.map (fun use -> Use.typing_env_at_use use, Use.id use) all_uses
    in
    let joined_env_extension, extra_params_and_args =
      TE.cut_and_n_way_join definition_typing_env use_envs_with_ids
        ~unknown_if_defined_at_or_later_than:definition_scope_level
    in
    let env = TE.add_env_extension definition_typing_env joined_env_extension in
    let env =
      List.fold_left (fun env extra_param ->
          let name =
            Name_in_binding_pos.create (KP.name extra_param)
              Name_occurrence_kind.normal
          in
          TE.add_definition env name (KP.kind extra_param))
        env
        extra_params_and_args.extra_params
    in
    let allowed = TE.var_domain env in
    let first_param_types = process_use_arg_types use ~allowed in
    (* XXX need to process all use arg types and collect perm. *)
    let param_types =
      List.fold_left (fun joined_arg_types use ->
          List.map2 (fun arg_type arg_type' -> T.join env arg_type arg_type')
            joined_arg_types
            (process_use_arg_types use ~allowed ~black_hole))
        first_param_types
        uses
    in
    let arg_types_by_use_id =
      List.fold_left (fun args use ->
          List.map2 (fun arg_map arg_type ->
              let env = Use.typing_env_at_use use in
              Apply_cont_rewrite_id.Map.add (Use.id use)
                (env, arg_type) arg_map)
            args
            (Use.arg_types use))
        (List.map (fun _ -> Apply_cont_rewrite_id.Map.empty) t.arity)
        all_uses
    in
    Uses {
      typing_env = env;
      arg_types_by_use_id;
      param_types;
      extra_params_and_args = extra_params_and_args;
    }

let number_of_uses t = List.length t.uses

let arity t = t.arity

(* CR mshinwell: Naming of this function still isn't ideal. *)
let cannot_change_continuation's_arity t =
  match Continuation.sort t.continuation with
  | Return -> true
  | Toplevel_return -> false
  | Exn -> true (* CR mshinwell: this should go to [false] *)
  | Normal -> false
  (* XXX Tidy up
    List.exists (fun use ->
        match Use.kind use with
        | Normal -> false
        | Fixed_arity -> true)
      t.uses *)
