(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Name : sig
  (** The name of a compilation unit, without any "-for-pack" prefix. *)

  type t

  (** Printing, comparison, sets, maps, etc. *)
  include Identifiable.S with type t := t

  (** Create a compilation unit name from a string. *)
  val of_string : string -> t

  (** Convert a compilation unit name to a string. *)
  val to_string : t -> string
end

(** The name of a "compilation unit" along with any "-for-pack" prefix that
    was specified when the unit was compiled.  By "compilation unit" we
    mean the code and data associated with the compilation of a single .ml
    source file: that is to say, file-level entities having OCaml semantics.
    The notion neither includes the special "startup" files nor external
    libraries.

    The [for_pack_prefix] is specified with the oldest pack at the tail of
    the list. *)
type t

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Print only the name of the given compilation unit. *)
val print_name : Format.formatter -> t -> unit

(** Create a compilation unit with the given [name] (which is not encoded or
    mangled in any way) and an optional "-for-pack" prefix. *)
val create : ?for_pack_prefix:Name.t list -> Name.t -> t

(** A distinguished compilation unit for initialisation of mutable state. *)
val none : t

(** Whether the specified compilation unit is that of the current unit
    being compiled.  An exception will be raised if [set_current] has not
    previously been called. *)
val is_current_exn : t -> bool

(** Record that the given value of type [t] is that of the current unit being
    compiled. *)
val set_current : t -> unit

(** Get the value of type [t] corresponding to the current unit being
    compiled.  An exception will be raised if [set_current] has not
    previously been called. *)
val get_current_exn : unit -> t

(** Get the persistent identifier corresponding to the given compilation
    unit. *)
val get_current_id_exn : unit -> Ident.t

(** Like [get_current_exn], but returns an option instead of raising an
    exception. *)
val get_current : unit -> t option

(** The name of the compilation unit, excluding any [for_pack_prefix]. *)
val name : t -> Name.t

(** The [for_pack_prefix] specified to [create] for the given compilation
    unit. *)
val for_pack_prefix : t -> Name.t list

(** If an empty [for_pack_prefix] was specified to [create] for the given
    compilation unit, this function returns [None].  Otherwise it returns the
    textual version of the [for_pack_prefix] (i.e. the components separated
    by dots). *)
val for_pack_prefix_as_string : t -> string option

(** Returns [true] iff the two given compilation units have ... *)

(** Like [for_pack_prefix], except with the [name] of the compilation unit
    appended. *)
val full_path : t -> Name.t list

(** Like [full_path], except with the result returned as a string, with
    a dot between each pair of components. *)
val full_path_as_string : t -> string

(** Returns [true] iff the given compilation unit has a non-empty
    [for_pack_prefix]. *)
val is_packed : t -> bool

(** Convert a compilation unit to a path. *)
val path : t -> Path.t
