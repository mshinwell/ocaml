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

(** Language terms that represent statically-allocated values. *)

module Of_kind_value : sig
  (** The various possible inhabitants of fields of statically-allocated
      blocks. *)
  type t =
    | Symbol of Symbol.t
      (** A field containing the address of the given symbol. *)
    | Tagged_immediate of Immediate.t
      (** A field containing the given tagged immediate. *)
    | Dynamically_computed of Variable.t
      (** A field containing the value of the variable that arose from
          a previous [computation] (see below). *)

  (** Printing, total ordering, etc. *)
  include Identifiable.S with type t := t
end

module Static_part : sig
  type 'a or_variable =
    | Const of 'a
    | Var of Variable.t

  (** The mutability status of a block field. *)
  type mutable_or_immutable = Mutable | Immutable

  (** A piece of code, comprising of the parameters and body of a function,
      together with a field indicating whether the piece of code is a newer
      version of one that existed previously (and may still exist), for
      example after a round of simplification. *)
  type code = {
    params_and_body : Flambda.Function_params_and_body.t or_deleted;
    newer_version_of : Code_id.t option;
  }
  and 'a or_deleted =
    | Present of 'a
    | Deleted

  (** The possibly-recursive declaration of pieces of code and any associated
      set of closures. *)
  type code_and_set_of_closures = {
    code : code Code_id.Map.t;
    (* CR mshinwell: Check the free names of the set of closures *)
    set_of_closures : Flambda.Set_of_closures.t option;
  }

  (** The static structure of a symbol, possibly with holes, ready to be
      filled with values computed at runtime. *)
  type 'k t =
    | Block : Tag.Scannable.t * mutable_or_immutable
        * (Of_kind_value.t list) -> Flambda_kind.value t
    | Fabricated_block : Variable.t -> Flambda_kind.value t
    | Code_and_set_of_closures : code_and_set_of_closures
        -> Flambda_kind.fabricated t
    | Boxed_float : Numbers.Float_by_bit_pattern.t or_variable
        -> Flambda_kind.value t
    | Boxed_int32 : Int32.t or_variable -> Flambda_kind.value t
    | Boxed_int64 : Int64.t or_variable -> Flambda_kind.value t
    | Boxed_nativeint : Targetint.t or_variable -> Flambda_kind.value t
    | Immutable_float_array : Numbers.Float_by_bit_pattern.t or_variable list
        -> Flambda_kind.value t
    | Mutable_string : { initial_value : string or_variable; }
        -> Flambda_kind.value t
    | Immutable_string : string or_variable -> Flambda_kind.value t

  (** Print a static structure definition to a formatter. *)
  val print : Format.formatter -> _ t -> unit

  (** All names free in the given static part.  (Note that this will
      descend into function bodies to find symbols.) *)
  val free_names : _ t -> Name_occurrences.t

  val apply_name_permutation : 'k t -> Name_permutation.t -> 'k t
end

type static_part_iterator = {
  f : 'k. ('k Static_part.t -> unit);
}

type static_part_mapper = {
  f : 'k. ('k Static_part.t -> 'k Static_part.t);
}

module Program_body : sig
  module Computation : sig
    type t = {
      expr : Flambda.Expr.t;
      (** The expression that is to be evaluated.  It must have no free
          variables and call [return_cont] with its results. *)
      return_continuation : Continuation.t;
      (** The return continuation of [expr]. *)
      exn_continuation : Exn_continuation.t;
      (** The uncaught exception continuation of [expr]. *)
      computed_values : Kinded_parameter.t list;
      (** Variables, with their kinds, used to reference results of the
          computation [expr] inside the [static_structure] (see below).  This
          list of variables must be in bijection with the parameters of the
          [return_cont].
          Since we don't really do any transformations on these structures, the
          [computed_values] variables are not treated up to alpha conversion. *)
    }

    val print : Format.formatter -> t -> unit

    val iter_expr : t -> f:(Flambda.Expr.t -> unit) -> unit

    val map_expr : t -> f:(Flambda.Expr.t -> Flambda.Expr.t) -> t
  end

  module Bound_symbols : sig
    type 'k t =
      | Singleton : Symbol.t -> Flambda_kind.value t
        (** A binding of a single symbol of kind [Value]. *)
      | Code_and_set_of_closures : {
          code_ids : Code_id.Set.t;
          closure_symbols : Symbol.t Closure_id.Map.t;
        } -> Flambda_kind.fabricated t
        (** A recursive binding of possibly multiple symbols to the individual
            closures within a set of closures; and/or bindings of code to
            code IDs. *)

    val print : Format.formatter -> _ t -> unit

    val being_defined : _ t -> Symbol.Set.t
  end

  module Static_structure : sig
    (** The bindings in a [Static_structure] are ordered: symbols bound later
        in the list cannot be referred to by [Static_part]s earlier in the
        list.
        Allowing multiple bindings here enables several static parts to use
        the same results from a given [computation]. *)
    type t0 =
      | S : 'k Bound_symbols.t * 'k Static_part.t -> t0

    type t

    val print : Format.formatter -> t -> unit

    val create
       : t0 list
      -> symbol_placeholders:Variable.t Symbol.Map.t
      -> t

    val empty : t

    val free_names : t -> Name_occurrences.t

    val being_defined : t -> Symbol.Set.t

    val has_no_bindings : t -> bool

    (* CR mshinwell: Maybe call "pieces"? *)
    val bindings : t -> t0 list

    val symbol_placeholders : t -> Variable.t Symbol.Map.t

    (** If [newer_versions_of] maps [id1] to [id2] then [id1] is a newer
        version of [id2]. *)
    val pieces_of_code
       : ?newer_versions_of:Code_id.t Code_id.Map.t
      -> ?set_of_closures:
           (Symbol.t Closure_id.Map.t * Flambda.Set_of_closures.t)
      -> Flambda.Function_params_and_body.t Code_id.Map.t
      -> t0

    val prepend_bindings : t -> prepend:t0 list -> t

    val delete_bindings
       : t
      -> free_names_after:Name_occurrences.t
      -> Code_age_relation.t
      -> t
  end

  module Definition : sig
    type t = {
      computation : Computation.t option;
      (** A computation which provides values to fill in parts of the
          statically-declared structure of one or more symbols.
          [computation] may not reference the symbols bound by the same
          definition's [static_structure]. *)
      static_structure : Static_structure.t;
      (** The statically-declared structure of the symbols being declared.
          Bindings of symbols in each element of a list comprising a
          [static_structure] are simultaneous, not ordered, or recursive. *)
    }

    val print : Format.formatter -> t -> unit

    val singleton_symbol : Symbol.t -> Flambda_kind.value Static_part.t -> t

    (** If [newer_versions_of] maps [id1] to [id2] then [id1] is a newer
        version of [id2]. *)
    val pieces_of_code
       : ?newer_versions_of:Code_id.t Code_id.Map.t
      -> Flambda.Function_params_and_body.t Code_id.Map.t
      -> t

    val get_pieces_of_code
       : t
      -> (Flambda.Function_params_and_body.t * (Code_id.t option)) Code_id.Map.t

    val being_defined : t -> Symbol.Set.t

    val code_being_defined : t -> Code_id.Set.t

    val iter_computation : t -> f:(Computation.t -> unit) -> unit

    val map_computation : t -> f:(Computation.t -> Computation.t) -> t

    val iter_static_parts : t -> static_part_iterator -> unit

    val map_static_parts : t -> static_part_mapper -> t
  end

  type t

  (** Print a list of symbol definitions to a formatter. *)
  val print : Format.formatter -> t -> unit

  (** Define the given symbol(s).  No symbol defined by the
      [definition] may be referenced by the same definition, only by
      subsequent [define_symbol] constructs. *)
  val define_symbol : Definition.t -> body:t -> Code_age_relation.t -> t

  (** The module block symbol for the compilation unit. *)
  val root : Symbol.t -> t

  val free_names : t -> Name_occurrences.t

  val iter_definitions : t -> f:(Definition.t -> unit) -> unit

  type descr = private
    (* CR mshinwell: Rename [Definition] to [Definition].  It doesn't
       always define a symbol now. *)
    | Definition of Definition.t * t
    | Root of Symbol.t

  val descr : t -> descr
end

(** A "program" is the contents of one compilation unit.  It describes the
    various values that are assigned to symbols in the object file. *)
module Program : sig
  type t = {
    imported_symbols : Flambda_kind.t Symbol.Map.t;
    body : Program_body.t;
  }

  (** Perform well-formedness checks on the program. *)
  val invariant : t -> unit

  (** Print a program to a formatter. *)
  val print : Format.formatter -> t -> unit

  (** All free names in the given program.  Imported symbols are not treated
      as free. *)
  val free_names : t -> Name_occurrences.t

  val used_closure_vars : t -> Var_within_closure.Set.t

  (** All symbols imported from other compilation units by the given program. *)
  val imported_symbols : t -> Flambda_kind.t Symbol.Map.t

  (** The module block symbol for the given program (the only symbol that
      can never be eliminated). *)
  val root_symbol : t -> Symbol.t

  val iter_body : t -> f:(Program_body.t -> unit) -> unit

  val map_body : t -> f:(Program_body.t -> Program_body.t) -> t
end
