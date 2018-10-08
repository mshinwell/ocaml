(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Symbols in the assembly stream. Unlike labels, symbols are named entities
    that are accessible in an object file. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Create an assembly symbol from a backend symbol. *)
val create : Backend_sym.t -> t

(** Create an assembly symbol from a name as found in an object file. *)
val of_external_name : string -> t

(** Convert a symbol to the corresponding textual form, suitable for direct
    emission into an assembly file.  This may be useful e.g. when emitting
    an instruction referencing a symbol.
    
    The optional [reloc] parameter will be appended to the encoded symbol (with
    no escaping applied to [reloc]) if provided. *)
val encode : ?reloc:string -> t -> string

(** Detection of functions that can be duplicated between a DLL and the main
    program (PR#4690). *)
val is_generic_function : t -> bool

include Identifiable.S with type t := t

(* CR mshinwell: delete unused names from here and [Backend_sym] *)
module Names : sig
  (** Nothing in particular. *)
  val __dummy__ : t

  (** Distinguished identifier referencing the global offset table. *)
  val _GLOBAL_OFFSET_TABLE_ : t

  (** External variables from the C library. *)
  val mcount : t
  val _mcount : t
  val __gnu_mcount_nc : t
  val sqrt : t

  (** Global variables in the OCaml runtime accessed by OCaml code. *)
  val caml_young_ptr : t
  val caml_young_limit : t
  val caml_exception_pointer : t
  val caml_negf_mask : t
  val caml_absf_mask : t
  val caml_backtrace_pos : t

  (** Entry points to the OCaml runtime from OCaml code.  (Note that this is
      not an exhaustive list of such entry points.) *)
  val caml_call_gc : t
  val caml_c_call : t
  val caml_modify : t
  val caml_initialize : t
  val caml_alloc : t
  val caml_allocN : t
  val caml_alloc1 : t
  val caml_alloc2 : t
  val caml_alloc3 : t
  val caml_ml_array_bound_error : t
  val caml_raise_exn : t

  (** Main OCaml entry point related functions. *)
  val caml_program : t
  val caml_startup : t

  (** Header of a dynamically-loaded library. *)
  val caml_plugin_header : t

  (** Master table of globals. *)
  val caml_globals : t
  val caml_globals_map : t

  (** Master table of module data and code segments. *)
  val caml_code_segments : t
  val caml_data_segments : t

  (** Standard OCaml auxiliary data structures. *)
  val caml_frametable : t
  val caml_spacetime_shapes : t
end
