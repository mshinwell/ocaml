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

open! Simplify_import

module Make (CHL : Continuation_handler_like_intf.S) = struct
  type 'body simplify_body = {
    simplify_body : 'a.
         Downwards_acc.t
      -> 'body
      -> (Continuation_uses_env.t
        -> Simplify_env_and_result.Result.t
        -> ('a * Upwards_acc.t))
      -> 'body * 'a * Upwards_acc.t;
  }

  let simplify_body_of_non_recursive_let_cont
        dacc cont cont_handler ~(simplify_body : _ simplify_body) ~body
        ~simplify_continuation_handler_like k =
    let definition_denv = DA.denv dacc in
    let body, (result, uenv', user_data), uacc =
      let original_cont_scope_level =
        DE.get_continuation_scope_level definition_denv
      in
      CHL.pattern_match cont_handler ~f:(fun handler ->
        let params = CHL.Opened.params handler in
        let dacc =
          DA.map_denv dacc ~f:(fun denv ->
            (* CR mshinwell: This parameter introduction stanza should be
               factored out of here and [Continuation_uses]. *)
            DE.map_typing_env denv ~f:(fun typing_env ->
              List.fold_left (fun typing_env param ->
                  let name =
                    Name_in_binding_pos.create (KP.name param)
                      Name_occurrence_kind.normal
                  in
                  TE.add_definition typing_env name (KP.kind param))
                typing_env
                params))
        in
        let dacc = DA.map_denv dacc ~f:DE.increment_continuation_scope_level in
        simplify_body.simplify_body dacc body (fun cont_uses_env r ->
          let definition_denv =
            (* CR mshinwell: Is this second increment needed?  Probably not *)
            DE.increment_continuation_scope_level definition_denv
          in
          let definition_denv =
            DE.add_lifted_constants definition_denv
              (R.get_lifted_constants r)
          in
          let num_uses = CUE.num_continuation_uses cont_uses_env cont in
          let cannot_change_arity =
            CUE.cannot_change_continuation's_arity cont_uses_env cont
          in
  (*
          Format.eprintf "Getting param types for %a\n%!"
            Continuation.print cont;
  *)
          let handler, user_data, uacc =
            match
              CUE.compute_handler_env cont_uses_env
                ~definition_typing_env_with_params_defined:
                  (DE.typing_env definition_denv)
                cont ~params
            with
            | No_uses ->
              (* Don't simplify the handler if there aren't any uses: otherwise,
                 its code will be deleted but any continuation usage information
                 collected during its simplification will remain. *)
              let user_data, uacc = k cont_uses_env r in
              cont_handler, user_data, uacc
            | Uses { handler_typing_env; arg_types_by_use_id;
                     extra_params_and_args; } ->
              let typing_env, extra_params_and_args =
                if cannot_change_arity then
                  handler_typing_env, Continuation_extra_params_and_args.empty
                else
                  let param_types =
                    List.map (fun param ->
                        TE.find handler_typing_env (KP.name param))
                      params
                  in
                  let typing_env, improved_param_types, extra_params_and_args =
                    Unbox_continuation_params.make_unboxing_decisions
                      handler_typing_env ~arg_types_by_use_id ~param_types
                      extra_params_and_args
                  in
                  let typing_env =
                    List.fold_left2 (fun typing_env param typ ->
                        TE.add_equation typing_env (KP.name param) typ)
                      typing_env
                      params improved_param_types
                  in
                  typing_env, extra_params_and_args
              in
              let dacc =
                DA.create
                  (DE.with_typing_environment definition_denv typing_env)
                  cont_uses_env r
              in
              try
                simplify_continuation_handler_like dacc
                  ~extra_params_and_args ~cannot_change_arity
                  cont handler k
              with Misc.Fatal_error -> begin
                Format.eprintf "\n%sContext is:%s simplifying continuation \
                    handler@ %a@ \
                    with [extra_params_and_args]@ %a@ \
                    with downwards accumulator:@ %a\n"
                  (Flambda_colours.error ())
                  (Flambda_colours.normal ())
                  CHL.print cont_handler
                  Continuation_extra_params_and_args.print extra_params_and_args
                  DA.print dacc;
                raise Misc.Fatal_error
              end
          in
          let uenv = UA.uenv uacc in
          let uenv_to_return = uenv in
          let uenv =
            if CHL.is_exn_handler handler then
              match CHL.behaviour handler with
              | Alias_for { arity; alias_for; } ->
                (* CR mshinwell: More checks here?  e.g. on the arity and
                   ensuring the aliased continuation is an exn handler too *)
                UE.add_continuation_alias uenv cont arity ~alias_for
              | Unreachable { arity; } | Unknown { arity; } ->
                UE.add_continuation uenv cont original_cont_scope_level arity
            else
              match CHL.behaviour handler with
              | Unreachable { arity; } ->
                UE.add_unreachable_continuation uenv cont
                  original_cont_scope_level arity
              | Alias_for { arity; alias_for; } ->
                UE.add_continuation_alias uenv cont arity ~alias_for
              | Unknown { arity; } ->
                let can_inline =
                  if num_uses <> 1 && not (CHL.stub handler)
                  then None
                  else CHL.real_handler handler
                in
                match can_inline with
                | None ->
                  UE.add_continuation uenv cont original_cont_scope_level arity
                | Some real_handler ->
                  UE.add_continuation_to_inline uenv cont
                    original_cont_scope_level arity
                    real_handler
          in
          let uacc = UA.with_uenv uacc uenv in
          (handler, uenv_to_return, user_data), uacc))
    in
    (* The upwards environment of [uacc] is replaced so that out-of-scope
       continuation bindings do not end up in the accumulator. *)
    let uacc = UA.with_uenv uacc uenv' in
    body, result, user_data, uacc
end
