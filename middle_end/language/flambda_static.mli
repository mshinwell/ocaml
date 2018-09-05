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

(** Flambda language terms that represent statically-allocated values. *)

module Of_kind_value : sig
  type t =
    | Symbol of Symbol.t
    | Tagged_immediate of Immediate.t
    | Dynamically_computed of Variable.t

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit
end

module Static_part : sig
  type 'a or_variable =
    | Const of 'a
    | Var of Variable.t

  type mutable_or_immutable = Mutable | Immutable

  (** The static structure of a symbol, possibly with holes, ready to be
      filled with values computed at runtime. *)
  type t =
    | Block of Tag.Scannable.t * mutable_or_immutable * (Of_kind_value.t list)
    | Fabricated_block of Variable.t
    | Set_of_closures of Flambda0.Set_of_closures.t
    | Closure of Symbol.t * Closure_id.t
    | Boxed_float of Numbers.Float_by_bit_pattern.t or_variable
    | Boxed_int32 of Int32.t or_variable
    | Boxed_int64 of Int64.t or_variable
    | Boxed_nativeint of Targetint.t or_variable
    (* CR mshinwell: Unify arrays and blocks, as in the type language? *)
    | Mutable_float_array of
        { initial_value : Numbers.Float_by_bit_pattern.t or_variable list; }
    | Immutable_float_array of Numbers.Float_by_bit_pattern.t or_variable list
    | Mutable_string of { initial_value : string or_variable; }
    | Immutable_string of string or_variable

  val print : Format.formatter -> t -> unit

  (** All names free in the given static part.  (Note that this will
      descend into function bodies to find symbols.) *)
  val free_names : t -> Name_occurrences.t
end

module Program_body : sig
  type computation = {
    expr : Flambda.Expr.t;
    (** The expression that is to be evaluated.  It must have no free
        variables and call [return_cont] with its results. *)
    return_cont : Continuation.t;
    (** The return continuation of [expr]. *)
    exception_cont : Continuation.t;
    (** The uncaught exception continuation of [expr]. *)
    computed_values : (Variable.t * Flambda_kind.t) list;
    (** Variables, with their kinds, used to reference results of the
        computation [expr] inside the [static_structure] (see below).  This
        list of variables must be in bijection with the parameters of the
        [return_cont]. *)
  }

  type static_structure = (Symbol.t * Flambda_kind.t * Static_part.t) list

  type definition = {
    computation : computation option;
    (** A computation which provides values to fill in parts of the
        statically-declared structure of one or more symbols.
        [computation] may not reference the symbols bound by the same
        definition's [static_structure]. *)
    static_structure : static_structure;
    (** The statically-declared structure of the symbols being declared.
        Bindings of symbols in [static_structure] are simultaneous, not
        ordered. *)
  }

  type t =
    | Define_symbol of definition * t
      (** Define the given symbol(s).  No symbol defined by the
          [definition] may be referenced by the same definition, only by
          subsequent [Define_symbol] or [Define_symbol_rec] constructs. *)
    | Define_symbol_rec of definition * t
      (** As for [Define_symbol] except that recursive uses of the symbols
          being defined in the given [definition] may be used in the static
          part of that [definition]. *)
    | Root of Symbol.t
      (** The module block symbol for the compilation unit. *)

  val print : Format.formatter -> t -> unit
end

(** A "program" is the contents of one compilation unit.  It describes the
    various values that are assigned to symbols in the object file. *)
module Program : sig
  type t = {
    imported_symbols : Flambda_kind.t Symbol.Map.t;
    body : Program_body.t;
  }

  val print : Format.formatter -> t -> unit

  (** All symbols from the given program which must be registered as roots
      with the GC.  (This does not count any imported symbols.) *)
  val gc_roots : t -> Symbol.Set.t

  (** All free symbols in the given program.  Imported symbols are not treated
      as free. *)
  val free_symbols : t -> Symbol.Set.t
end


(********)



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
