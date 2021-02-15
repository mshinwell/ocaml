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

module DE = Downwards_env

type resolver = Compilation_unit.t -> Flambda_type.Typing_env.t option
type get_imported_names = unit -> Name.Set.t
type get_imported_code =
  unit -> Exported_code.t

module type Upwards_env = sig
  type t

  val empty : t

  val invariant : t -> unit

  val print : Format.formatter -> t -> unit

  val add_continuation
     : t
    -> Continuation.t
    -> Scope.t
    -> Flambda_arity.With_subkinds.t
    -> t

  val add_continuation_with_handler
     : t
    -> Continuation.t
    -> Scope.t
    -> Flambda_arity.With_subkinds.t
    -> Continuation_handler.t
    -> t

  val add_unreachable_continuation
     : t
    -> Continuation.t
    -> Scope.t
    -> Flambda_arity.With_subkinds.t
    -> t

  val add_continuation_alias
     : t
    -> Continuation.t
    -> Flambda_arity.With_subkinds.t
    -> alias_for:Continuation.t
    -> t

  val add_linearly_used_inlinable_continuation
     : t
    -> Continuation.t
    -> Scope.t
    -> Flambda_arity.With_subkinds.t  (* CR mshinwell: redundant *)
    -> params:Kinded_parameter.t list
    -> handler:Flambda.Expr.t
    -> free_names_of_handler:Name_occurrences.t
    -> t

  val add_exn_continuation
     : t
    -> Exn_continuation.t
    -> Scope.t
    -> t

  val find_continuation : t -> Continuation.t -> Continuation_in_env.t

  val mem_continuation : t -> Continuation.t -> bool

  val resolve_continuation_aliases : t -> Continuation.t -> Continuation.t

  val resolve_exn_continuation_aliases
     : t
    -> Exn_continuation.t
    -> Exn_continuation.t

  val continuation_arity : t -> Continuation.t -> Flambda_arity.With_subkinds.t

  val check_continuation_is_bound : t -> Continuation.t -> unit

  val check_exn_continuation_is_bound : t -> Exn_continuation.t -> unit

  val add_apply_cont_rewrite
     : t
    -> Continuation.t
    -> Apply_cont_rewrite.t
    -> t

  val find_apply_cont_rewrite
     : t
    -> Continuation.t
    -> Apply_cont_rewrite.t option

  val delete_apply_cont_rewrite
     : t
    -> Continuation.t
    -> t

  val will_inline_continuation : t -> Continuation.t -> bool
end

(* CR mshinwell: The name of this module is a bit misleading *)
module type Lifted_constant = sig
  (** Description of a group of statically-allocated constants discovered
      during simplification. *)

  module Definition : sig
    type descr = private
      | Code of Code_id.t
      | Set_of_closures of {
          denv : Downwards_env.t;
          closure_symbols_with_types
            : (Symbol.t * Flambda_type.t) Closure_id.Lmap.t;
          symbol_projections : Symbol_projection.t Variable.Map.t;
        }
      | Block_like of {
          symbol : Symbol.t;
          denv : Downwards_env.t;
          ty : Flambda_type.t;
          symbol_projections : Symbol_projection.t Variable.Map.t;
        }

    type t

    val descr : t -> descr

    val defining_expr : t -> Static_const_with_free_names.t

    val denv : t -> Downwards_env.t option

    val code : Code_id.t -> Static_const_with_free_names.t -> t

    val set_of_closures
       : Downwards_env.t
      -> closure_symbols_with_types
           : (Symbol.t * Flambda_type.t) Closure_id.Lmap.t
      -> symbol_projections:Symbol_projection.t Variable.Map.t
      -> Static_const_with_free_names.t
      -> t

    val block_like
       : Downwards_env.t
      -> Symbol.t
      -> Flambda_type.t
      -> symbol_projections:Symbol_projection.t Variable.Map.t
      -> Static_const_with_free_names.t
      -> t

    val bound_symbols : t -> Bound_symbols.t

    val free_names : t -> Name_occurrences.t

    val symbol_projections : t -> Symbol_projection.t Variable.Map.t
  end

  type t

  val print : Format.formatter -> t -> unit

  (** The creation functions take the types of symbols to avoid re-inferring
      them. *)
  val create_block_like
     : Symbol.t
    -> symbol_projections:Symbol_projection.t Variable.Map.t
    -> Static_const_with_free_names.t
    -> Downwards_env.t
    -> Flambda_type.t
    -> t

  val create_set_of_closures
     : Downwards_env.t
    -> closure_symbols_with_types:(Symbol.t * Flambda_type.t) Closure_id.Lmap.t
    -> symbol_projections:Symbol_projection.t Variable.Map.t
    -> Static_const_with_free_names.t
    -> t

  val create_code
     : Code_id.t
    -> Static_const_with_free_names.t
    -> t

  val definitions : t -> Definition.t list
  val bound_symbols : t -> Bound_symbols.t
  val defining_exprs : t -> Static_const_with_free_names.Group.t
  val types_of_symbols : t -> (Downwards_env.t * Flambda_type.t) Symbol.Map.t
  val symbol_projections : t -> Symbol_projection.t Variable.Map.t

  val concat : t list -> t

  val is_fully_static : t -> bool

  val all_defined_symbols : t -> Symbol.Set.t

  val free_names_of_defining_exprs : t -> Name_occurrences.t

  val apply_projection : t -> Symbol_projection.t -> Simple.t option
end

module type Lifted_constant_state = sig
  type lifted_constant
  type t

  val empty : t

  val is_empty : t -> bool

  val print : Format.formatter -> t -> unit

  val singleton : lifted_constant -> t

  (* Use if the order of constants doesn't matter. *)
  val add : t -> lifted_constant -> t

  val add_innermost : t -> lifted_constant -> t

  val add_outermost : t -> lifted_constant -> t

  val singleton_sorted_array_of_constants
     : innermost_first:lifted_constant array
     -> t

  (* Use if the order of constants doesn't matter. *)
  val union : t -> t -> t

  val union_ordered : innermost:t -> outermost:t -> t

  (* Use if the order of constants doesn't matter. *)
  val fold
     : t
    -> init:'a
    -> f:('a -> lifted_constant -> 'a)
    -> 'a

  val fold_outermost_first
     : t
    -> init:'a
    -> f:('a -> lifted_constant -> 'a)
    -> 'a

  val fold_innermost_first
     : t
    -> init:'a
    -> f:('a -> lifted_constant -> 'a)
    -> 'a

  val all_defined_symbols : t -> Symbol.Set.t

  val add_to_denv
     : ?maybe_already_defined:unit
    -> DE.t
    -> t
    -> DE.t

  val add_singleton_to_denv : DE.t -> lifted_constant -> DE.t

  val add_list_to_denv : DE.t -> lifted_constant list -> DE.t
end
