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

module Make (Continuation_handler_like : sig
  type t

  val print : Format.formatter -> t -> unit

  val is_exn_handler : t -> bool

  val stub : t -> bool

  val arity : t -> Flambda_arity.t

  type behaviour = private
    | Unreachable of { arity : Flambda_arity.t; }
    | Alias_for of { arity : Flambda_arity.t; alias_for : Continuation.t; }
    | Unknown of { arity : Flambda_arity.t; }

  val behaviour : t -> behaviour

  val real_handler : t -> Continuation_handler.t option
end) = struct
  let make_unboxing_decision typing_env ~args_by_use_id ~param_type
        extra_params_and_args =
    match T.prove_tags_and_sizes typing_env param_type with
    | Invalid | Unknown -> typing_env, param_type, extra_params_and_args
    | Proved tags_to_sizes ->
      match Tag.Map.get_singleton tags_to_sizes with
      | None -> typing_env, param_type, extra_params_and_args
      | Some (tag, size) ->
        let fields_with_vars =
          List.init (Targetint.OCaml.to_int size) (fun _ ->
            let var = Variable.create "field" in
            let ty = T.alias_type_of K.value (Simple.var var) in
            ty, var)
        in
        let fields, vars = List.split fields_with_vars in
        let block_type = T.immutable_block tag ~fields in
        let typing_env =
          List.fold_left (fun typing_env var ->
              let name =
                Name_in_binding_pos.create (Name.var var)
                  Name_occurrence_kind.normal
              in
              TE.add_definition typing_env name K.value)
            typing_env vars
        in
        match T.meet typing_env block_type param_type with
        | Bottom ->
          Misc.fatal_errorf "[meet] between %a and %a should not have failed"
            T.print block_type
            T.print param_type
        | Ok (param_type, env_extension) ->
          let typing_env = TE.add_env_extension typing_env env_extension in
          let _index, extra_params_and_args =
            List.fold_left (fun (index, extra_params_and_args) param ->
                let extra_param = KP.create (Parameter.wrap param) K.value in
                let extra_args =
                  Apply_cont_rewrite_id.Map.map (fun arg : EA.t ->
                      let bound_to =
                        Var_in_binding_pos.create
                          (Variable.create (Printf.sprintf "unboxed%d" index))
                          Name_occurrence_kind.normal
                      in
                      let index =
                        Immediate.int (Targetint.OCaml.of_int index)
                      in
                      let prim =
                        P.Binary (
                          Block_load (Block (Value Anything), Immutable),
                          arg, Simple.const (Tagged_immediate index))
                      in
                      New_let_binding (bound_to, prim))
                    args_by_use_id
                in
                Format.printf "ARGS: %a@.EPA: %a@."
                  (Apply_cont_rewrite_id.Map.print Simple.print) args_by_use_id
                  EPA.print extra_params_and_args;
                let extra_params_and_args =
                  EPA.add extra_params_and_args ~extra_param ~extra_args
                in
                index + 1, extra_params_and_args)
              (0, extra_params_and_args)
              vars
          in
          typing_env, param_type, extra_params_and_args

  let make_unboxing_decisions typing_env ~args_by_use_id ~param_types
        extra_params_and_args =
    Format.printf "UNBOXLength %i@." (List.length param_types);
    let typing_env, param_types_rev, extra_params_and_args =
      List.fold_left (fun (typing_env, param_types_rev, extra_params_and_args)
                (args_by_use_id, param_type) ->
          let typing_env, param_type, extra_params_and_args =
            make_unboxing_decision typing_env ~args_by_use_id ~param_type
              extra_params_and_args
          in
          typing_env, param_type :: param_types_rev, extra_params_and_args)
        (typing_env, [], extra_params_and_args)
        (List.combine args_by_use_id param_types)
    in
    typing_env, List.rev param_types_rev, extra_params_and_args

  let simplify_body_of_non_recursive_let_cont
        dacc cont cont_handler ~body simplify_continuation_handler_like k =
    let definition_denv = DA.denv dacc in
    let body, (result, uenv', user_data), uacc =
      let original_cont_scope_level =
        DE.get_continuation_scope_level definition_denv
      in
      let dacc = DA.map_denv dacc ~f:DE.increment_continuation_scope_level in
      Simplify_expr.simplify_expr dacc body (fun cont_uses_env r ->
        (* The environment currently in [dacc] is not the correct environment
           for simplifying the handler. Instead, we must use the environment of
           the [Let_cont] definition itself, augmented with any lifted constants
           arising from simplification of the body. (These need to be present
           since the type(s) of the continuation's parameter(s) may involve the
           associated symbols.)

           The environment in [dacc] does, however, contain the usage
           information for the continuation. This will be used to compute the
           types of the continuation's parameter(s). *)
        let definition_denv =
          DE.increment_continuation_scope_level definition_denv
        in
        let definition_denv =
          DE.add_lifted_constants definition_denv
            (R.get_lifted_constants r)
        in
        let arity = Continuation_handler_like.arity cont_handler in
        (* CR mshinwell: rename arg_types -> param_types *)
        (* CR mshinwell: Return one list of args, types, etc *)
        let typing_env, args_by_use_id, arg_types, extra_params_and_args =
          CUE.continuation_env_and_param_types cont_uses_env
            ~definition_typing_env:(DE.typing_env definition_denv)
            cont arity
        in
        Format.printf "CUE: %a@." CUE.print cont_uses_env;
        let typing_env, param_types, extra_params_and_args =
          make_unboxing_decisions typing_env ~args_by_use_id
            ~param_types:arg_types extra_params_and_args
        in
        Format.printf "param typesXXX %i %i@.%a@." (List.length param_types) (List.length arg_types)
        Continuation_extra_params_and_args.print extra_params_and_args;
        let definition_denv =
          DE.with_typing_environment definition_denv typing_env
        in
        let dacc = DA.create definition_denv cont_uses_env r in
        let original_cont_num_uses = DA.num_continuation_uses dacc cont in
        let handler, user_data, uacc =
          (* Don't simplify the handler if there aren't any uses: otherwise,
             its code will be deleted but any continuation usage information
             collected during its simplification will remain. *)
          if original_cont_num_uses < 1 then
            let user_data, uacc =
              k (DA.continuation_uses_env dacc) (DA.r dacc)
            in
            cont_handler, user_data, uacc
          else
            try
              simplify_continuation_handler_like dacc ~arg_types:param_types
                ~extra_params_and_args cont cont_handler k
            with Misc.Fatal_error -> begin
              Format.eprintf "\n%sContext is:%s simplifying continuation \
                  handler@ %a@ \
                  with [extra_params_and_args]@ %a@ \
                  with downwards accumulator:@ %a\n"
                (Flambda_colours.error ())
                (Flambda_colours.normal ())
                Continuation_handler_like.print cont_handler
                Continuation_extra_params_and_args.print extra_params_and_args
                DA.print dacc;
              raise Misc.Fatal_error
            end
        in
        let uenv = UA.uenv uacc in
        let uenv_to_return = uenv in
        let uenv =
          if Continuation_handler_like.is_exn_handler handler then
            match Continuation_handler_like.behaviour handler with
            | Alias_for { arity; alias_for; } ->
              (* CR mshinwell: More checks here?  e.g. on the arity and
                 ensuring the aliased continuation is an exn handler too *)
              UE.add_continuation_alias uenv cont arity ~alias_for
            | Unreachable { arity; } | Unknown { arity; } ->
              UE.add_continuation uenv cont original_cont_scope_level arity
          else
            match Continuation_handler_like.behaviour handler with
            | Unreachable { arity; } ->
              UE.add_unreachable_continuation uenv cont
                original_cont_scope_level arity
            | Alias_for { arity; alias_for; } ->
              UE.add_continuation_alias uenv cont arity ~alias_for
            | Unknown { arity; } ->
              let can_inline =
                if original_cont_num_uses <> 1
                  && not (Continuation_handler_like.stub handler)
                then None
                else Continuation_handler_like.real_handler handler
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
        (handler, uenv_to_return, user_data), uacc)
    in
    (* The upwards environment of [uacc] is replaced so that out-of-scope
       continuation bindings do not end up in the accumulator. *)
    let uacc = UA.with_uenv uacc uenv' in
    body, result, user_data, uacc
end
