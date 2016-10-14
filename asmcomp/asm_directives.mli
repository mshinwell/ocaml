(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Abstraction over backends for the emission of assembler directives. *)

type constant =
  | Const of int64
  | This
  | Label of string
  | Numeric_label of Linearize.label
  | Add of constant * constant
  | Sub of constant * constant

module Directive : sig
  type t = private
    | Align of bool * int
    | Byte of constant
    | Bytes of string
    | Comment of string
    | Global of string
    | Long of constant
    | NewLabel of string
    | Quad of constant
    | Section of string list * string option * string list
    | Space of int
    | Word of constant
    (* gas only (the masm emitter will fail on them): *)
    | Cfi_adjust_cfa_offset of int
    | Cfi_endproc
    | Cfi_startproc
    | File of { file_num : int; filename : int; }
    | Indirect_symbol of string
    | Loc of { file_num : int; line : int; col : int; }
    | Private_extern of string
    | Set of string * constant
    | Size of string * constant
    | Sleb128 of constant
    | Type of string * string
    | Uleb128 of constant
    (* Mac OS X only: *)
    | Direct_assignment of string * constant

  val print : Buffer.t -> t -> unit
end

(** To be called by the emitter at the very start of code generation.
    Calling the functions below will cause directives to be passed to the
    given [emit] function. *)
val initialize : emit:(Directive.t -> unit) -> unit

(** Reinitialize the emitter before compiling a different source file. *)
val reset : unit -> unit

(** A good explanation of relocatability in this context is given in the Apple
    documentation: https://developer.apple.com/library/content/\
    documentation/DeveloperTools/Reference/Assembler/\
    030-Assembly_Language_Statements/asm_language.html *)
type constant_evaluated_at =
  | Compile_time of constant
  (** The operands are to be evaluated when the assembler runs at compile
      time---in other words, an absolute expression. *)
  | Link_time of constant
  (** The operands are to be evaluated at link time---in other words, a
      relocatable expression. *)

type width =
  | Thirty_two
  | Sixty_four

type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_loc
  | Debug_str
  | Debug_line

type section =
  | Text
  | Data
  | Dwarf of dwarf_section

(** Retrieve the label that [switch_to_section] (below) will put at the start
    of the given section.  This function may be called before
    [switch_to_section] for the section concerned. *)
val label_for_section : section -> Linearize.label

(** Emit subsequent directives to the given section.  If this function
    has not been called before on the particular section, a label
    declaration will be emitted after declaring the section.
    Such labels may seem strange, but they are necessary so that
    references (e.g. DW_FORM_ref_addr / DW_FORM_sec_offset when emitting
    DWARF) to places that are currently at the start of these sections
    get relocated correctly when those places become not at the start
    (e.g. during linking). *)
val switch_to_section : section -> unit

(** Leave as much space as is required to achieve the given alignment. *)
val align : bytes:int -> unit

val byte : constant -> unit

(** Adjust the current frame address offset by the given number of bytes. *)
val cfi_adjust_cfa_offset : bytes:int -> unit

(** Mark the beginning of a function, for CFI purposes. *)
val cfi_startproc : unit -> unit

(** Mark the end of a function, for CFI purposes. *)
val cfi_endproc : unit -> unit

(** Emit a comment. *)
val comment : string -> unit

val file : file_num:int -> file_name:string -> unit

val global : string -> unit

val indirect_symbol : string -> unit

(** Emit an 8-bit integer (not padded). *)
val int8 : Numbers.Int8.t -> unit

(** Emit a 16-bit integer (not padded). *)
val int16 : Numbers.Int16.t -> unit

(** Emit a 32-bit integer (not padded). *)
val int32 : Int32.t -> unit

(** Emit a 64-bit integer. *)
val int64 : Int64.t -> unit

(** Mark the source location of the current assembly position. *)
val loc : file_num:int -> line:int -> col:int -> unit

val long : constant -> unit

val private_extern : string -> unit

val qword : constant -> unit

(** Set the given variable to the given expression.  (On Mac OS X this
    translates to .set or direct assignment depending on the relocatability of
    the expression.) *)
val set_variable : string * constant_evaluated_at -> unit

val size : string -> constant -> unit

val space : int -> unit

(** Emit a string (directly into the current section).  This function
    does not write a terminating null. *)
val string : string -> unit

val type_ : string -> string -> unit

val word : constant -> unit

(** Emit a machine-width reference to the given symbol. *)
val symbol : Symbol.t -> unit

(** Define a symbol at the current output position. *)
val define_symbol : Symbol.t -> unit

(** Emit a machine-width reference giving the displacement between the
    two given symbols.  To obtain a positive result the symbol at the
    lower address should be the second argument (just like subtraction). *)
val between_symbols : upper:Symbol.t -> lower:Symbol.t -> unit

(** Emit a 32-bit number giving the displacement between the two given
    labels.  To obtain a positive result the symbol at the lower address
    should be the second argument (just like subtraction).
    The arguments are not relocated before the subtraction: in other words,
    the displacement in the linked executable will equal the displacement
    in the unlinked object file. *)
val between_labels_32bit : upper:Cmm.label -> lower:Cmm.label -> unit

(** Emit a machine-width reference giving the displacement between the
    lower symbol and the sum of the address of the upper label plus
    [offset_upper]. *)
val between_symbol_and_label_offset
   : upper:Linearize.label
  -> lower:Symbol.t
  -> offset_upper:Target_system.Address.t
  -> unit

(** Emit a machine-width reference to the address formed by adding the
    given byte offset to the address of the given symbol. *)
val symbol_plus_offset
   : Symbol.t
  -> offset_in_bytes:Target_system.Address.t
  -> unit

(** Emit a machine-width reference to the given label. *)
val label : Linearize.label -> unit

(** Define a label at the current position in the current section. *)
val label_declaration : label_name:Linearize.label -> unit

(** Like [label_declaration], but using a textual name for the label. *)
val label_declaration' : string -> unit

(** Emit an integer whose width is that of an address on the target
    machine. *)
val target_address : Target_system.Address.t -> unit

(** Emit a 64-bit integer in unsigned LEB128 variable-length encoding
    (cf. DWARF debugging information standard). *)
val uleb128 : Int64.t -> unit

(** Emit a 64-bit integer in signed LEB128 variable-length encoding. *)
val sleb128 : Int64.t -> unit

(** Cache a string for later emission.  The returned label may be used to
    obtain the address of the string in the section.  This function does
    not emit anything.  (See [emit_cached_strings], below.)
    If a string is supplied to this function that is already in the cache
    then the previously-assigned label is returned, not a new one. *)
val cache_string : string -> Linearize.label

(** Emit the sequence of:
      label definition:
        <string><null terminator>
    pairs as per previous calls to [cache_string].  This function clears
    the cache. *)
val emit_cached_strings : unit -> unit

(** Emit an integer giving the distance obtained by subtracting the
    address of [base] from the address of [label].  [width] specifies the
    size of the integer.  The subtraction is performed after relocation: in
    other words, the displacement in the linked executable may differ from
    the displacement in the object file. *)
val offset_into_section_label
   : section:section
  -> label:Linearize.label
  -> width:width
  -> unit

(** As for [offset_into_section_label], but using a symbol instead of
    a label as one end of the measurement. *)
val offset_into_section_symbol
   : section:section
  -> symbol:Symbol.t
  -> width:width
  -> unit
