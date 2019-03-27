(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* A variant of the "lambda" code with direct / indirect calls explicit
   and closures explicit too *)

open Asttypes
open Lambda

type function_label = Symbol.t

type ustructured_constant =
  | Uconst_float of float
  | Uconst_int32 of int32
  | Uconst_int64 of int64
  | Uconst_nativeint of nativeint
  | Uconst_block of int * uconstant list
  | Uconst_float_array of float list
  | Uconst_string of string
  | Uconst_closure of ufunction list * Symbol.t * uconstant list

and uconstant =
  | Uconst_ref of Symbol.t * ustructured_constant option
  | Uconst_int of int
  | Uconst_ptr of int

and uphantom_defining_expr =
  | Uphantom_const of uconstant
  (** The phantom-let-bound variable is a constant. *)
  | Uphantom_var of Variable.t
  (** The phantom-let-bound variable is an alias for another variable. *)
  | Uphantom_offset_var of { var : Variable.t; offset_in_words : int; }
  (** The phantom-let-bound-variable's value is defined by adding the given
      number of words to the pointer contained in the given identifier. *)
  | Uphantom_read_field of { var : Variable.t; field : int; }
  (** The phantom-let-bound-variable's value is found by adding the given
      number of words to the pointer contained in the given identifier, then
      dereferencing. *)
  | Uphantom_read_symbol_field of { sym : Symbol.t; field : int; }
  (** As for [Uphantom_read_var_field], but with the pointer specified by
      a symbol. *)
  | Uphantom_block of { tag : int; fields : Variable.t list; }
  (** The phantom-let-bound variable points at a block with the given
      structure. *)

and ulambda =
    Uvar of Variable.t
  | Uconst of uconstant
  | Udirect_apply of function_label * ulambda list * Debuginfo.t
  | Ugeneric_apply of ulambda * ulambda list * Debuginfo.t
  | Uclosure of ufunction list * ulambda list
  | Uoffset of ulambda * int
  | Ulet of mutable_flag * value_kind * Variable.With_provenance.t
      * ulambda * ulambda
  | Uphantom_let of Variable.With_provenance.t
      * uphantom_defining_expr option * ulambda
  | Uletrec of (Variable.With_provenance.t * ulambda) list * ulambda
  | Uprim of Clambda_primitives.primitive * ulambda list * Debuginfo.t
  | Uswitch of ulambda * ulambda_switch * Debuginfo.t
  | Ustringswitch of ulambda * (string * ulambda) list * ulambda option
  | Ustaticfail of int * ulambda list
  | Ucatch of
      int *
      (Variable.With_provenance.t * value_kind) list *
      ulambda *
      ulambda
  | Utrywith of ulambda * Variable.With_provenance.t * ulambda
  | Uifthenelse of ulambda * ulambda * ulambda
  | Usequence of ulambda * ulambda
  | Uwhile of ulambda * ulambda
  | Ufor of Variable.With_provenance.t * ulambda * ulambda
      * direction_flag * ulambda
  | Uassign of Variable.t * ulambda
  | Usend of meth_kind * ulambda * ulambda * ulambda list * Debuginfo.t
  | Uunreachable

and ufunction = {
  label  : function_label;
  arity  : int;
  params : (Variable.With_provenance.t * value_kind) list;
  return : value_kind;
  body   : ulambda;
  dbg    : Debuginfo.t;
  env    : Variable.t option;
}

and ulambda_switch =
  { us_index_consts: int array;
    us_actions_consts: ulambda array;
    us_index_blocks: int array;
    us_actions_blocks: ulambda array}

(* Description of known functions *)

type function_description =
  { fun_label: function_label;          (* Label of direct entry point *)
    fun_arity: int;                     (* Number of arguments *)
    mutable fun_closed: bool;           (* True if environment not used *)
    mutable fun_inline: (Variable.With_provenance.t list * ulambda) option;
    mutable fun_float_const_prop: bool  (* Can propagate FP consts *)
  }

(* Approximation of values *)

type value_approximation =
    Value_closure of function_description * value_approximation
  | Value_tuple of value_approximation array
  | Value_unknown
  | Value_const of uconstant
  | Value_global_field of Symbol.t * int

(* Comparison functions for constants *)

val compare_structured_constants:
        ustructured_constant -> ustructured_constant -> int
val compare_constants:
        uconstant -> uconstant -> int

type usymbol_provenance = {
  original_idents : Ident.t list;
  module_path : Path.t;
}

type uconstant_block_field =
  | Uconst_field_ref of Symbol.t
  | Uconst_field_int of int

type preallocated_block = {
  symbol : Symbol.t;
  exported : bool;
  tag : int;
  fields : uconstant_block_field option list;
  provenance : usymbol_provenance option;
}

type preallocated_constant = {
  symbol : Symbol.t;
  exported : bool;
  definition : ustructured_constant;
  provenance : usymbol_provenance option;
}

type with_constants =
  ulambda * preallocated_block list * preallocated_constant list
