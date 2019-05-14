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

type lifted_constants =
  (Flambda_type.t * Flambda_kind.t * Flambda_static.Static_part.t) Symbol.Map.t

module type Env = sig
  (** Environments, following the lexical scope of the program, used during
      simplification. *)
  type t

  type result

  val invariant : t -> unit

  (** Print a human-readable version of the given environment. *)
  val print : Format.formatter -> t -> unit

  (** Create a new environment. *)
  val create
     : never_inline:bool
    -> round:int
    -> backend:(module Backend_intf.S)
    -> scope_level_for_lifted_constants:Scope_level.t
    -> t

  (** Obtain the first-class module that gives information about the
      compiler backend being used for compilation. *)
  val backend : t -> (module Backend_intf.S)

  val resolver : t -> (Export_id.t -> Flambda_type.t option)

  (** Determine whether the inliner is currently inside a function body from
      the given set of closures.  This is used to detect whether a given
      function call refers to a function which exists somewhere on the current
      inlining stack. *)
  val inside_set_of_closures_declaration : t -> Set_of_closures_origin.t -> bool

  val entering_set_of_closures : t -> Set_of_closures_origin.t -> t

  (** Record that the simplifier is about to descend into [closure_id]. *)
  val enter_closure : t -> Closure_id.t -> t

  val add_lifted_constants : t -> lifted_constants -> t

  val increment_continuation_scope_level : t -> t

  val set_scope_level_for_lifted_constants : t -> Scope_level.t -> t

  val typing_env : t -> Flambda_type.Typing_env.t

  val add_variable : t -> Variable.t -> Flambda_type.t -> t

  val add_symbol : t -> Symbol.t -> Flambda_type.t -> t

  val add_parameters
     : t
    -> Kinded_parameter.t list
    -> arg_types:Flambda_type.t list
    -> t

  val add_parameters_with_unknown_types : t -> Kinded_parameter.t list -> t

  val add_continuation : t -> Continuation.t -> Flambda_arity.t -> t

  val add_unreachable_continuation
     : t
    -> Continuation.t
    -> Flambda_arity.t
    -> t

  val add_continuation_alias
     : t
    -> Continuation.t
    -> Flambda_arity.t
    -> alias_for:Continuation.t
    -> t

  val add_continuation_to_inline
     : t
    -> Continuation.t
    -> Flambda_arity.t
    -> Continuation_handler.t
    -> t

  val add_exn_continuation : t -> Exn_continuation.t -> t

  val mem_continuation : t -> Continuation.t -> bool

  val mem_exn_continuation : t -> Exn_continuation.t -> bool

  val find_continuation : t -> Continuation.t -> Continuation_in_env.t

  (** Appends the locations of inlined call-sites to the [~dbg] argument *)
  val add_inlined_debuginfo : t -> dbg:Debuginfo.t -> Debuginfo.t

   (** If collecting inlining statistics, record an inlining decision for the
       call at the top of the closure stack stored inside the given
       environment. *)
  val record_decision
     : t
    -> Inlining_stats_types.Decision.t
    -> unit

  (** The environment stores the call-site being inlined to produce
      precise location information. This function sets the current
      call-site being inlined.  *)
  val set_inline_debuginfo : t -> dbg:Debuginfo.t -> t
end

module type Result = sig
  (** The result structure used during simplification. *)

  type t

  type env

  val create : resolver:(Export_id.t -> Flambda_type.t option) -> t

  val add_continuation : t -> Continuation.t -> Flambda_arity.t -> t

  val record_continuation_use
     : t
    -> Continuation.t
    -> arg_types:Flambda_type.t list
    -> t

  val continuation_arg_types
     : t
    -> Continuation.t
    -> arity:Flambda_arity.t
    -> Flambda_type.t list

  (** Apply a transformation to the inlining benefit stored within the
      given result structure. *)
  val map_benefit
    : t
    -> (Inlining_cost.Benefit.t -> Inlining_cost.Benefit.t)
    -> t

  val new_lifted_constant
     : t
    -> name:string
    -> Flambda_type.t
    -> Flambda_static.Static_part.t
    -> Symbol.t * t

  val new_lifted_constants : t -> lifted_constants -> t

  val get_lifted_constants : t -> lifted_constants

  val clear_env_extension : t -> t
end
