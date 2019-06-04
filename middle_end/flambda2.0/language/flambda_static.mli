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

  (** The static structure of a symbol, possibly with holes, ready to be
      filled with values computed at runtime. *)
  type 'k t =
    | Block : Tag.Scannable.t * mutable_or_immutable
        * (Of_kind_value.t list) -> Flambda_kind.value t
    | Fabricated_block : Variable.t -> Flambda_kind.value t
    | Set_of_closures : Flambda.Set_of_closures.t -> Flambda_kind.fabricated t
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
end

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
      computed_values : (Variable.t * Flambda_kind.t) list;
      (** Variables, with their kinds, used to reference results of the
          computation [expr] inside the [static_structure] (see below).  This
          list of variables must be in bijection with the parameters of the
          [return_cont].
          Since we don't really do any transformations on these structures, the
          [computed_values] variables are not treated up to alpha conversion. *)
    }
  end

  module Bound_symbols : sig
    type 'k t =
      | Singleton : Symbol.t -> Flambda_kind.value t
        (** A binding of a single symbol of kind [Value]. *)
      | Set_of_closures : {
          set_of_closures_symbol : Symbol.t;
          closure_symbols : Symbol.t Closure_id.Map.t;
        } -> Flambda_kind.fabricated t
        (** A binding of a single symbol to a set of closures together with
            the binding of possibly multiple symbols to the individual closures
            within such set of closures. *)

    val print : Format.formatter -> _ t -> unit

    val being_defined : _ t -> Symbol.Set.t
  end

  module Static_structure : sig
    type t =
      | S : ('k Bound_symbols.t * 'k Static_part.t) list -> t
      [@@unboxed]
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
  end

  type t =
    | Define_symbol of Definition.t * t
      (** Define the given symbol(s).  No symbol defined by the
          [definition] may be referenced by the same definition, only by
          subsequent [Define_symbol] constructs. *)
    | Root of Symbol.t
      (** The module block symbol for the compilation unit. *)

  (** Print a list of symbol definitions to a formatter. *)
  val print : Format.formatter -> t -> unit
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

  (** All symbols from the given program which must be registered as roots
      with the GC.  (This does not count any imported symbols.) *)
  val gc_roots : t -> Symbol.Set.t

  (** All free symbols in the given program.  Imported symbols are not treated
      as free. *)
  val free_symbols : t -> Symbol.Set.t

  (** All symbols imported from other compilation units by the given program. *)
  val imported_symbols : t -> Flambda_kind.t Symbol.Map.t

  (** The module block symbol for the given program (the only symbol that
      can never be eliminated). *)
  val root_symbol : t -> Symbol.t
end
