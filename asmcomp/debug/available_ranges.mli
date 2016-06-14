(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Given a value identifier [x] and a function, an "available subrange" is
   in the normal case a contiguous subset of that function's code paired with
   a register [r], such that at all times during the block's execution the
   value of [x] is available in [r].  ([r] may end up being a hard register
   or a location on the stack.)

   An available subrange may instead be associated with a phantom identifier.
   Phantom identifiers correspond to identifiers that once bound computations
   that have now been lifted to toplevel and whose values are given by
   symbols or some sequence of indirections through a symbol.  (In the future
   phantom identifiers may be extended to also describe relations between
   other identifiers, enabling the reconstruction of (possibly partial)
   values in the debugger given some of all of their sub-parts.)

   Available subranges associated with normal value identifiers are computed
   by this pass based on the information from the dataflow analysis in
   [Available_regs].  (The linearized code is updated so that it contains the
   necessary labels for delimiting such ranges.)  Those associated with
   phantom identifiers, however, are tracked explicitly from the
   [Uphantom_let] Clambda expression onwards.

   An "available range" is then a set of available subranges that do not
   overlap in code space, again for a single identifier (normal or phantom)
   and function.
*)

module Available_subrange : sig
  type t

  type 'a location =
    | Reg of Reg.t * 'a
    | Phantom of Clambda.ulet_provenance * 'a phantom

  and 'a phantom =
    | Const_int of int
    | Const_symbol of Symbol.t
    | Read_symbol_field of { symbol : Symbol.t; field : int; }
    | Read_field of { address : 'a location; field : int; }
    | Offset_pointer of { address : 'a location; offset_in_words : int; }

  val start_pos : t -> Linearize.label
  val end_pos : t -> Linearize.label
  val end_pos_offset : t -> int option

  val location : t -> unit location

  (** [offset_from_stack_ptr_in_bytes] returns [Some] only when [location]
      is [Reg] and the contained register is assigned to the stack. *)
  val offset_from_stack_ptr_in_bytes : t -> int option
end

module Available_range : sig
  type t

  val is_parameter : t -> int option  (* viz. reg.mli. *)
  val extremities : t -> Linearize.label * Linearize.label

  val iter
     : t
    -> f:(available_subrange:Available_subrange.t -> unit)
    -> unit

  val fold
     : t
    -> init:'a
    -> f:('a -> available_subrange:Available_subrange.t -> 'a)
    -> 'a
end

type t

(* [create ~fundecl] may change [fundecl].  It may change the first
   instruction, even, which is why a new declaration is returned. *)
val create
   : fundecl:Linearize.fundecl
  -> t * Linearize.fundecl

val find : t -> ident:Ident.t -> Available_range.t option

type label_classification =
  | Start of {
      end_pos : Linearize.label;
      location : unit Available_subrange.location;
    }
  | End

val classify_label
   : t
  -> Linearize.label
  -> (label_classification * Ident.t * Available_subrange.t) list

val fold
   : t
  -> init:'a
  (* XXX fix [is_unique] stuff *)
  (* [is_unique] is [true] if there is no other value identifier with the
     same (unstamped) name as [ident] in [t].  (It follows that using the
     unstamped name is sufficient to uniquely identify the identifier amongst
     a list of, say, local variables in a debugger.) *)
  -> f:('a -> ident:Ident.t -> is_unique:bool -> range:Available_range.t -> 'a)
  -> 'a

val rewrite_labels : t -> env:int Numbers.Int.Map.t -> t
