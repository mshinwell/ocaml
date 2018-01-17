(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Operations on Flambda statically-allocated code and data whose
    implementations cannot break invariants enforced by any private or
    abstract types. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Of_kind_value :
  module type of struct include Flambda_static0.Of_kind_value end

module Static_part :
  module type of struct include Flambda_static0.Static_part end

module Program_body :
  module type of struct include Flambda_static0.Program_body end

module Program : sig
  include module type of struct include Flambda_static0.Program end

  (** Perform well-formedness checks on the expression.  This is basically a
      simple type / kind checking algorithm. *)
  val invariant : t -> unit

  (** All symbols imported from other compilation units by the given program. *)
  val imported_symbols : t -> Flambda_kind.t Symbol.Map.t

  (** The module block symbol for the given program (the only symbol that
      can never be eliminated). *)
  val root_symbol : t -> Symbol.t

(*
  (** Creates a map from closure IDs to function declarations by iterating over
      all sets of closures in the given program. *)
  val make_closure_map : t -> Flambda.Function_declarations.t Closure_id.Map.t

  (** The definitions of all constants that have been lifted out to [Let_symbol]
      or [Let_rec_symbol] constructions. *)
  val all_lifted_constants : t -> (Symbol.t * Constant_defining_value.t) list

  (** Like [all_lifted_constant_symbols], but returns a map instead of a
      list. *)
  val all_lifted_constants_as_map : t -> Constant_defining_value.t Symbol.Map.t

  (** The identifiers of all constant sets of closures that have been lifted out
      to [Let_symbol] or [Let_rec_symbol] constructions. *)
  val all_lifted_constant_sets_of_closures : t -> Set_of_closures_id.Set.t

  (** All sets of closures in the given program (whether or not bound to a
      symbol.) *)
  val all_sets_of_closures : t -> Flambda.Set_of_closures.t list

  val all_sets_of_closures_map
     : t
    -> Flambda.Set_of_closures.t Set_of_closures_id.Map.t

  val all_function_decls_indexed_by_set_of_closures_id
     : t
    -> Flambda.Function_declarations.t Set_of_closures_id.Map.t

  val all_function_decls_indexed_by_closure_id
     : t
    -> Flambda.Function_declarations.t Closure_id.Map.t
*)
  module Iterators : sig
    (* CR mshinwell: give comment defining semantics. *)
    val iter_sets_of_closures
       : t
      -> f:(Flambda.Set_of_closures.t -> unit)
      -> unit

    (** Iterate over all toplevel expressions in the program, including
        subfunctions.
        Note the difference in semantics between this and
        [Toplevel_only.iter_exprs]. *)
    val iter_toplevel_exprs
       : t
      -> f:(continuation_arity:Flambda_arity.t
        -> Continuation.t
        -> Flambda.Expr.t
        -> unit)
      -> unit

    (** A specialised version of [iter_toplevel_exprs] that only passes
        [Named.t] values to the given [f]. *)
    val iter_named
       : t
      -> f:(Flambda.Named.t -> unit)
      -> unit
    
    (** A specialised version of [iter_toplevel_exprs] that only passes
        [Apply] nodes to the given [f]. *)
    val iter_apply
       : t
      -> f:(Flambda.Apply.t -> unit)
      -> unit

(*
    val iter_constant_defining_values
       : t
      -> f:(Constant_defining_value.t -> unit)
      -> unit

    module Toplevel_only : sig
      (** Iterate over all expressions occurring directly at the toplevel of the
          program. Note that the only function bodies iterated over are those
          bound directly to a symbol. *)
      val iter_exprs
         : t
        -> f:(continuation_arity:Flambda_arity.t
          -> Continuation.t
          -> Flambda.Expr.t
          -> unit)
        -> unit
    end
*)
  end

  module Mappers : sig
    (** Apply the given [f] to every expression at toplevel or forming the
        body of a function, including nested functions, within the given [t].
        That is to say:
        (a) the expressions associated with [computation]s at toplevel;
        (b) the bodies of functions found within the expressions from
            part (a), together with (recursively) the bodies of any functions
            nested therein;
        (c) the bodies of functions, together with (recursively) the bodies
            of any functions nested within such functions, found in
            the [static_structure]. *)
    val map_toplevel_exprs
       : t
      -> f:(continuation_arity:Flambda_arity.t
        -> Continuation.t
        -> Flambda.Expr.t
        -> Flambda.Expr.t)
      -> t
  end

(*
  module Mappers : sig    
    (* CR mshinwell: Define semantics -- does it recurse? *)
    val map_set_of_closures
       : t
      -> f:(Flambda0.Set_of_closures.t -> Flambda0.Set_of_closures.t)
      -> t

    (* CR mshinwell: check naming.
       Change terminology to explicitly distinguish between toplevel expressions
       such as [Effect]s and closure bodies at toplevel? *)
    val map_toplevel_exprs
       : t
      -> f:(Flambda.Expr.t -> Flambda.Expr.t)
      -> t

    (** Maps over the expressions iterated over by [iter_named], above. *)    
    val map_named
       : t
      -> f:(Variable.t -> Flambda.Named.t -> Flambda.Named.t)
      -> t
  end
*)
end

(*



  val declare_boxed_float : t -> float -> t * Symbol.t
  val declare_boxed_int32 : t -> Int32.t -> t * Symbol.t
  val declare_boxed_int64 : t -> Int64.t -> t * Symbol.t
  val declare_boxed_nativeint : t -> Targetint.t -> t * Symbol.t
  val declare_immutable_string : t -> string -> t * Symbol.t
  val declare_mutable_string : t -> initial_value:string -> t * Symbol.t
  val declare_float_array : t -> float list -> t * Symbol.t
  val declare_block : t -> Tag.Scannable.t -> Symbol.t list -> t * Symbol.t

  val declare_single_field_pointing_at : t -> Variable.t -> Flambda_kind.t -> t
*)
