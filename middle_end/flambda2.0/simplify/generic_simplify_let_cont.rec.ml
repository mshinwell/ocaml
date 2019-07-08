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

open! Flambda.Import

module CUE = Continuation_uses_env
module DA = Downwards_acc
module DE = Simplify_env_and_result.Downwards_env
module R = Simplify_env_and_result.Result
module T = Flambda_type
module UA = Upwards_acc
module UE = Simplify_env_and_result.Upwards_env
module VB = Var_in_binding_pos

type 'a result =
  | No_wrapper of 'a
  | With_wrapper of {
      wrapper : Continuation_handler.t;
      renamed_original_cont : Continuation.t;
      original : 'a;
    }

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
  let simplify_body_of_non_recursive_let_cont
        dacc cont cont_handler ~body simplify_continuation_handler_like k =
    let definition_denv = DA.denv dacc in
    let body, (result, uenv', user_data), uacc =
      (* CR mshinwell: the following two names might be misleading *)
      let wrapper_scope_level =
        (* A gap in the levels is always left for a potential wrapper. *)
        DE.get_continuation_scope_level definition_denv
      in
      let original_cont_scope_level = Scope.next wrapper_scope_level in
      let dacc =
        DA.map_denv dacc ~f:(fun denv ->
          DE.increment_continuation_scope_level
            (DE.increment_continuation_scope_level denv))
      in
      Simplify_expr.simplify_expr dacc body (fun cont_uses_env r ->
        (* The environment currently in [dacc] is not the correct environment
           for simplifying the handler. Instead, we must use the environment
           of the [Let_cont] definition itself, augmented with any lifted
           constants arising from simplification of the body. (These need to
           be present since the type(s) of the continuation's parameter(s) may
           involve the associated symbols.)
           The environment in [dacc] does, however, contain the usage
           information for the continuation.  This will be used to
           compute the types of the continuation's parameter(s). *)
        let definition_denv =
          DE.increment_continuation_scope_level definition_denv
        in
        let definition_denv =
          DE.add_lifted_constants definition_denv
            (R.get_lifted_constants r)
        in
        let arity = Continuation_handler_like.arity cont_handler in
        let typing_env, arg_types =
          CUE.continuation_env_and_arg_types cont_uses_env
            ~definition_typing_env:(DE.typing_env definition_denv)
            cont arity
        in
        let definition_denv =
          DE.with_typing_environment definition_denv typing_env
        in
        let dacc = DA.create definition_denv cont_uses_env r in
        let original_cont_num_uses = DA.num_continuation_uses dacc cont in
        let result, user_data, uacc =
          (* Don't simplify the handler if there aren't any uses: otherwise,
             its code will be deleted but any continuation usage information
             collected during its simplification will remain. *)
          if original_cont_num_uses < 1 then
            let user_data, uacc =
              k (DA.continuation_uses_env dacc) (DA.r dacc)
            in
            No_wrapper cont_handler, user_data, uacc
          else
            try
              simplify_continuation_handler_like dacc ~arg_types cont
                cont_handler k
            with Misc.Fatal_error -> begin
              Format.eprintf "\n%sContext is:%s simplifying continuation \
                  handler@ %a@ \
                  with downwards accumulator:@ %a\n"
                (Flambda_colours.error ())
                (Flambda_colours.normal ())
                Continuation_handler_like.print cont_handler
                DA.print dacc;
              raise Misc.Fatal_error
            end
        in
        let uenv = UA.uenv uacc in
        let uenv' = uenv in
        let add_original_handler uenv cont cont_handler =
          if Continuation_handler_like.is_exn_handler cont_handler then
            match Continuation_handler_like.behaviour cont_handler with
            | Alias_for { arity; alias_for; } ->
              (* CR mshinwell: More checks here?  e.g. on the arity and
                 ensuring the aliased continuation is an exn handler too *)
              UE.add_continuation_alias uenv cont arity ~alias_for
            | Unreachable { arity; } | Unknown { arity; } ->
              UE.add_continuation uenv cont original_cont_scope_level arity
          else
            match Continuation_handler_like.behaviour cont_handler with
            | Unreachable { arity; } ->
              UE.add_unreachable_continuation uenv cont
                original_cont_scope_level arity
            | Alias_for { arity; alias_for; } ->
              UE.add_continuation_alias uenv cont arity ~alias_for
            | Unknown { arity; } ->
              let can_inline =
                if original_cont_num_uses <> 1
                  && not (Continuation_handler_like.stub cont_handler)
                then None
                else Continuation_handler_like.real_handler cont_handler
              in
              match can_inline with
              | None ->
                UE.add_continuation uenv cont original_cont_scope_level arity
              | Some real_handler ->
                UE.add_continuation_to_inline uenv cont
                  original_cont_scope_level arity
                  real_handler
        in
        let uenv =
          match result with
          | No_wrapper original ->
            let uenv = add_original_handler uenv cont original in
            uenv
          | With_wrapper { wrapper; renamed_original_cont; original; } ->
            let uenv =
              add_original_handler uenv renamed_original_cont original
            in
            let wrapper_arity = Continuation_handler.arity wrapper in
(*
Format.eprintf "%a --> renamed_original_cont %a, renamed original cont arity %a\n%!"
  Continuation.print cont
  Continuation.print renamed_original_cont
  Flambda_arity.print original_cont_arity;
*)
            UE.add_continuation_to_inline uenv cont
              wrapper_scope_level wrapper_arity wrapper
        in
        let uacc = UA.with_uenv uacc uenv in
        (result, uenv', user_data), uacc)
    in
    (* The upwards environment of [uacc] is replaced so that out-of-scope
       continuation bindings do not end up in the accumulator. *)
    let uacc = UA.with_uenv uacc uenv' in
    body, result, user_data, uacc
end
