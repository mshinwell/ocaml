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

  (** The static structure of a symbol, possibly with holes, ready to be
      filled with values computed at runtime.  As might be expected, this is
      isomorphic to a subset of [Flambda_type.t]. *)
  type t =
    | Block of Tag.Scannable.t * Flambda.mutable_or_immutable
        * (Of_kind_value.t list)
    | Set_of_closures of Flambda0.Set_of_closures.t
    | Closure of Symbol.t * Closure_id.t
    | Boxed_float of Numbers.Float_by_bit_pattern.t or_variable
    | Boxed_int32 of Int32.t or_variable
    | Boxed_int64 of Int64.t or_variable
    | Boxed_nativeint of Targetint.t or_variable
    | Mutable_float_array of
        { initial_value : Numbers.Float_by_bit_pattern.t or_variable list; }
    | Immutable_float_array of Numbers.Float_by_bit_pattern.t or_variable list
    | Mutable_string of { initial_value : string or_variable; }
    | Immutable_string of string or_variable

  val print : Format.formatter -> t -> unit

  (** All names free in the given static part.  (Note that this will
      descend into function bodies to find symbols.) *)
  val free_names : t -> Name.Set.t
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

  type static_structure = (Symbol.t * Static_part.t) list

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
    imported_symbols : Symbol.Set.t;
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
