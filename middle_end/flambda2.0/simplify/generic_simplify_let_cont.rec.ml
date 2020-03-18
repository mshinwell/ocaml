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

  let simplify_body_of_non_recursive_let_cont dacc cont cont_handler
        ~(simplify_body : _ simplify_body) ~body
        ~simplify_continuation_handler_like ~user_data k =
    let body, (result, uenv', user_data), uacc =
      let scope = DE.get_continuation_scope_level (DA.denv dacc) in
      let is_exn_handler = CHL.is_exn_handler cont_handler in
      CHL.pattern_match cont_handler ~f:(fun handler ->
        let params = CHL.Opened.params handler in
        let denv = DE.define_parameters (DA.denv dacc) ~params in
        let dacc =
          DA.with_denv dacc (DE.increment_continuation_scope_level denv)
        in
        simplify_body.simplify_body dacc body (fun cont_uses_env r ->
(*
          Format.eprintf "Parameters for %a: %a\n%!"
            Continuation.print cont
            KP.List.print params;
*)
          let denv =
            DE.add_lifted_constants denv ~lifted:(R.get_lifted_constants r)
          in
          let uses =
            CUE.compute_handler_env cont_uses_env cont ~params
              ~definition_typing_env_with_params_defined:(DE.typing_env denv)
          in
          let handler, user_data, uacc, is_single_inlinable_use =
            match uses with
            | No_uses ->
              (* Don't simplify the handler if there aren't any uses: otherwise,
                 its code will be deleted but any continuation usage information
                 collected during its simplification will remain. *)
              let user_data, uacc = k cont_uses_env r in
              cont_handler, user_data, uacc, false
            | Uses { handler_typing_env; arg_types_by_use_id;
                     extra_params_and_args; is_single_inlinable_use; } ->
              let typing_env, extra_params_and_args =
                match Continuation.sort cont with
                | Normal when is_single_inlinable_use ->
                  assert (not is_exn_handler);
                  handler_typing_env, extra_params_and_args
                | Normal | Toplevel_return ->
                  assert (not is_exn_handler);
                  let param_types = TE.find_params handler_typing_env params in
                  Unbox_continuation_params.make_unboxing_decisions
                    handler_typing_env ~arg_types_by_use_id ~params
                    ~param_types extra_params_and_args
                | Return ->
                  assert (not is_exn_handler);
                  handler_typing_env, extra_params_and_args
                | Exn ->
                  assert is_exn_handler;
                  handler_typing_env, extra_params_and_args
              in
              let dacc =
                DA.create (DE.with_typing_env denv typing_env) cont_uses_env r
              in
              try
                let handler, user_data, uacc =
                  simplify_continuation_handler_like dacc ~extra_params_and_args
                    cont handler ~user_data k
                in
                handler, user_data, uacc, is_single_inlinable_use
              with Misc.Fatal_error -> begin
                if !Clflags.flambda2_context_on_error then begin
                  Format.eprintf "\n%sContext is:%s simplifying continuation \
                      handler (inlinable? %b)@ %a@ with \
                      [extra_params_and_args]@ %a@ \
                      with downwards accumulator:@ %a\n"
                    (Flambda_colours.error ())
                    (Flambda_colours.normal ())
                    is_single_inlinable_use
                    CHL.print cont_handler
                    Continuation_extra_params_and_args.print
                    extra_params_and_args
                    DA.print dacc
                end;
                raise Misc.Fatal_error
              end
          in
          let uenv = UA.uenv uacc in
          let uenv_to_return = uenv in
          let uenv =
            match uses with
            | No_uses -> uenv
            | Uses _ ->
              match CHL.behaviour handler with
              | Unreachable { arity; } ->
                UE.add_unreachable_continuation uenv cont scope arity
              | Alias_for { arity; alias_for; } ->
                UE.add_continuation_alias uenv cont arity ~alias_for
              | Apply_cont_with_constant_arg
                  { cont = destination_cont; arg = destination_arg; arity; } ->
                UE.add_continuation_apply_cont_with_constant_arg uenv cont
                  scope arity ~destination_cont ~destination_arg
              | Unknown { arity; } ->
                let can_inline =
                  if is_single_inlinable_use && (not is_exn_handler) then
                    CHL.real_handler handler
                  else
                    None
                in
                match can_inline with
                | None -> UE.add_continuation uenv cont scope arity
                | Some handler ->
                  UE.add_continuation_to_inline uenv cont scope arity handler
          in
          let uacc = UA.with_uenv uacc uenv in
          (handler, uenv_to_return, user_data), uacc))
    in
    (* The upwards environment of [uacc] is replaced so that out-of-scope
       continuation bindings do not end up in the accumulator. *)
    let uacc = UA.with_uenv uacc uenv' in
    body, result, user_data, uacc
end
