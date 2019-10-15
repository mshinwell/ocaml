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

module type Downwards_env = sig
  type t

  type result

  val invariant : t -> unit

  (** Print a human-readable version of the given environment. *)
  val print : Format.formatter -> t -> unit

  (** Create a new environment. *)
  val create
     : round:int
    -> backend:(module Flambda2_backend_intf.S)
    -> t

  (** Obtain the first-class module that gives information about the
      compiler backend being used for compilation. *)
  val backend : t -> (module Flambda2_backend_intf.S)

  val resolver : t -> (Export_id.t -> Flambda_type.t option)

  val enter_closure : t -> t

  val increment_continuation_scope_level : t -> t

  val get_continuation_scope_level : t -> Scope.t

  val typing_env : t -> Flambda_type.Typing_env.t

  val add_variable : t -> Variable.t -> Flambda_type.t -> t

  val add_equation_on_variable : t -> Variable.t -> Flambda_type.t -> t

  val find_variable : t -> Variable.t -> Flambda_type.t

  val add_symbol : t -> Symbol.t -> Flambda_type.t -> t

  val define_symbol : t -> Symbol.t -> Flambda_kind.t -> t

  val add_equation_on_symbol : t -> Symbol.t -> Flambda_type.t -> t

  val add_parameters
     : t
    -> Kinded_parameter.t list
    -> arg_types:Flambda_type.t list
    -> t

  val add_parameters_with_unknown_types : t -> Kinded_parameter.t list -> t

  val extend_typing_environment : t -> Flambda_type.Typing_env_extension.t -> t

  val with_typing_environment : t -> Flambda_type.Typing_env.t -> t

  val check_variable_is_bound : t -> Variable.t -> unit

  val check_symbol_is_bound : t -> Symbol.t -> unit

  val check_name_is_bound : t -> Name.t -> unit

  val check_simple_is_bound : t -> Simple.t -> unit

  (** Appends the locations of inlined call-sites to the given debuginfo
      and sets the resulting debuginfo as the current one in the
      environment. *)
  val add_inlined_debuginfo : t -> Debuginfo.t -> t

  val round : t -> int

  (** Prevent function inlining from occurring in the given environment. *)
  val disable_function_inlining : t -> t

  (** Add the given lifted constants to the environment.  Symbols that are
      already defined in the environment are ignored. *)
  val add_lifted_constants : t -> Lifted_constant.t list -> t

  (** Like [add_lifted_constants], but takes the constants from the given
      result structure. *)
  val add_lifted_constants_from_r : t -> result -> t

  val can_inline : t -> bool
end

module type Upwards_env = sig
  type t

  val empty : t

  val invariant : t -> unit

  val print : Format.formatter -> t -> unit

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

  val find_continuation : t -> Continuation.t -> Continuation_in_env.t

  val resolve_continuation_aliases : t -> Continuation.t -> Continuation.t

  val continuation_arity : t -> Continuation.t -> Flambda_arity.t

  val check_continuation_is_bound : t -> Continuation.t -> unit

  val check_exn_continuation_is_bound : t -> Exn_continuation.t -> unit

  val continuation_scope_level : t -> Continuation.t -> Scope.t

  val exn_continuation_scope_level : t -> Exn_continuation.t -> Scope.t

  val record_continuation_use
     : t
    -> env
    -> Continuation.t
    -> arg_types:Flambda_type.t list
    -> t

  (* CR mshinwell: Add [record_exn_continuation_use]? *)

  val continuation_env_and_arg_types
     : t
    -> env
    -> Continuation.t
    -> Flambda_type.Typing_env.t * (Flambda_type.t list)
end

module type Result = sig
  type t

  type env

  val print : Format.formatter -> t -> unit

  val create : resolver:(Export_id.t -> Flambda_type.t option) -> t

  val new_lifted_constant : t -> Lifted_constant.t -> t

  val add_lifted_constants : t -> from:t -> t

  (** Retrieve constants lifted to toplevel.  The constants must be defined
      in the order returned (first element of the list defined first). *)
  (* CR mshinwell: Update name to reflect this *)
  val get_lifted_constants : t -> Lifted_constant.t list

  val imported_symbols : t -> Flambda_kind.t Symbol.Map.t
end
