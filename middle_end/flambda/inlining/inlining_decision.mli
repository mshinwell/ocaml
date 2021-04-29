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

module Function_declaration_decision : sig
  type t = private
    | Never_inline_attribute
    | Function_body_too_large of Code_size.t
    | Stub
    | Attribute_inline
    | Small_function of {
        size: Code_size.t;
        small_function_size: Code_size.t;
      }
    | Speculatively_inlinable of {
        size: Code_size.t;
        small_function_size: Code_size.t;
        large_function_size: Code_size.t;
      }

  val print : Format.formatter -> t -> unit

  val report : Format.formatter -> t -> unit

  type inlining_behaviour = private
    | Cannot_be_inlined
    | Must_be_inlined
    | Could_possibly_be_inlined

  val behaviour : t -> inlining_behaviour
end


type cost_metrics_source = From_denv | Metrics of Cost_metrics.t

(* If a cost_metrics is passed it will be used as the cost metrics for
   the function declaration instead of grabbing the cost metrics from the
   one stored on the code located in the env. *)
val make_decision_for_function_declaration
   : Downwards_env.t
  -> cost_metrics_source:cost_metrics_source
  -> Function_declaration.t
  -> Function_declaration_decision.t

module Call_site_decision : sig
  (* CR-someday mshinwell: Maybe have two types, one giving the reasons why
     something can be inlined, and one giving the reasons why something
     cannot be inlined. *)
  type t = private
    | Environment_says_never_inline
    | Unrolling_depth_exceeded
    | Max_inlining_depth_exceeded
    | Recursion_depth_exceeded
    | Never_inline_attribute
    | Speculatively_not_inline of {
        cost_metrics: Cost_metrics.t;
        evaluated_to: float;
        threshold: float;
      }
    | Attribute_always
    | Attribute_unroll of int
    | Definition_says_inline
    | Speculatively_inline of {
        cost_metrics: Cost_metrics.t;
        evaluated_to: float;
        threshold: float;
      }


  val print : Format.formatter -> t -> unit

  val report : Format.formatter -> t -> unit

  type can_inline = private
    | Do_not_inline
    | Inline of { unroll_to : int option; }

  val can_inline : t -> can_inline
end

val make_decision_for_call_site
   : Downwards_acc.t
  -> simplify_expr:Expr.t Simplify_common.expr_simplifier
  -> function_decl:Flambda_type.Function_declaration_type.Inlinable.t
  -> function_decl_rec_info:Rec_info.t
  -> apply:Apply.t
  -> return_arity:Flambda_arity.With_subkinds.t
  -> Call_site_decision.t
