(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Given a variable [x] and a function, an "available subrange" is in the
    normal case a contiguous subset of that function's code paired with a
    register [r], such that at all times during the block's execution the value
    of [x] is available in [r]. ([r] may end up being a hard register or a
    location on the stack.)
 
    An available subrange may instead be associated with a phantom variable.
    Phantom variables correspond to variables that once bound computations that
    have now been optimised out.
 
    Available subranges associated with normal variables are computed by this
    pass based on the information from the dataflow analysis in
    [Available_regs]. (The linearized code is updated so that it contains the
    necessary labels for delimiting such ranges.) Those associated with phantom
    variables, however, are tracked explicitly from the [Uphantom_let] Clambda
    expression onwards.
 
    An "available range" is then a set of available subranges that do not
    overlap in code space, again for a single variable (normal or phantom) and
    function.

    Available ranges are associated with scopes.  These correspond to the
    structure of let-expressions and blocks.  They are used to form DWARF
    lexical blocks.
*)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Scope : sig
  type t

  val start_pos : t -> Linearize.label
  val end_pos : t -> Linearize.label
  val parent : t -> t option

  include Identifiable.S with type t := t
end

module Available_subrange : sig
  type t

  type 'a location = private
    | Reg of Reg.t * 'a
    | Phantom

  val start_pos : t -> Linearize.label
  val end_pos : t -> Linearize.label
  val end_pos_offset : t -> int option

  val location : t -> unit location

  (** [offset_from_stack_ptr_in_bytes] must be called only when [location]
      is [Reg] and the contained register is assigned to the stack. *)
  val offset_from_stack_ptr_in_bytes : t -> int
end

type type_info = private
  | From_cmt_file of Backend_var.Provenance.t option
  | Phantom of
      Backend_var.Provenance.t option * Mach.phantom_defining_expr option

type is_parameter = private
  | Local
  | Parameter of { index : int; }

module Available_range : sig
  type t

  val scope : t -> Scope.t option
  val type_info : t -> type_info
  val is_parameter : t -> is_parameter
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

(** [create ~fundecl] may change [fundecl].  It may change the first
    instruction, even, which is why a new declaration is returned. *)
val create
   : fundecl:Linearize.fundecl
  -> t * Linearize.fundecl

val find : t -> var:Backend_var.t -> Available_range.t option

type label_classification =
  | Start of {
      end_pos : Linearize.label;
      location : unit Available_subrange.location;
    }
  | End

val classify_label
   : t
  -> Linearize.label
  -> (label_classification * Backend_var.t * Available_subrange.t) list

val fold
   : t
  -> init:'a
  -> f:('a
    -> var:Backend_var.t
    -> name_is_unique:bool
    -> location_is_unique:bool
    -> range:Available_range.t
    -> 'a)
  -> 'a

(** It is guaranteed that the [parent]s of scopes will always occur before
    such scopes in the returned list. *)
val scopes : t -> Scope.t list
