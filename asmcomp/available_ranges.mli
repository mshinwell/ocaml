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
   that have now been lifted to toplevel and whose values must be obtained by
   reading the contents of a symbol.  (In the future phantom identifiers are
   likely to be extended to also describe relations between other identifiers,
   to enable the reconstruction of values in the debugger given some of all of
   their sub-parts.)

   Available subranges associated with normal value identifiers are computed
   by this pass based on the information from the dataflow analysis in
   [Available_regs].  (The linearized code is updated so that it contains the
   necessary labels for delimiting such ranges.)  Those associated with
   phantom identifiers, however, are tracked explicitly from the
   [Uphantom_let] Clambda expression onwards.

   An "available range" is then a set of available subranges that do not
   overlap in code space, again for a single identifier (normal or phantom)
   and function.  For phantom identifiers, available ranges and subranges
   coincide, since the relevant value is expected to be permanently accessible.
*)

type phantom_defining_expr =
  | Symbol of Symbol.t
  | Int of int

module Available_subrange : sig
  type t

  type location =
    | Reg of Reg.t
    | Phantom of phantom_defining_expr

  val start_pos : t -> Linearize.label
  val end_pos : t -> Linearize.label

  val location : t -> location
  val ident : t -> Ident.t

  (* [offset_from_stack_ptr] returns [Some] only when [location] is [Reg] and
     the contained register is assigned to the stack. *)
  val offset_from_stack_ptr : t -> int option
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
  -> phantom_ranges:Linearize.phantom_let_range Ident.tbl
  -> t * Linearize.fundecl

val find : t -> ident:Ident.t -> Available_range.t option

type label_classification = Start | End

val classify_label
   : t
  -> Linearize.label
  -> (label_classification * Available_subrange.t) option

val fold
   : ?exclude:Ident.t
  -> t
  -> init:'a
  (* XXX fix [is_unique] stuff *)
  (* [is_unique] is [true] if there is no other value identifier with the
     same (unstamped) name as [ident] in [t].  (It follows that using the
     unstamped name is sufficient to uniquely identify the identifier amongst
     a list of, say, local variables in a debugger.) *)
  -> f:('a -> ident:Ident.t -> is_unique:bool -> range:Available_range.t -> 'a)
  -> 'a

val rewrite_labels : t -> env:int Numbers.Int.Map.t -> t
