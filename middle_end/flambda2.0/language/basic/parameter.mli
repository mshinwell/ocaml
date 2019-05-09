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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** [Parameter.t] carries a unique [Variable.t] used as function parameter. *)

type t
type parameter = t

(** Make a parameter from a variable with default attributes *)
(* CR-soon mshinwell: This whole thing is a bit of a mess.  This wrap
   function is going to lead to attributes being dropped by accident.  We
   should think about how to improve this.
   There should for a start be a proper creation function with "with
   default attributes" in the name, or similar. *)
val wrap : Variable.t -> t

val create : Name.t -> t

val var : t -> Variable.t

val name : t -> Name.t

(** Rename the inner variable of the parameter *)
val rename
   : ?current_compilation_unit:Compilation_unit.t
  -> ?append:string
  -> t
  -> t

val map_var : (Variable.t -> Variable.t) -> t -> t

module T : Identifiable.Thing with type t = t

module Set : sig
  include Identifiable.Set with module T := T

  val vars : parameter list -> Variable.Set.t

  val wrap : Variable.Set.t -> t
end

include Identifiable.S with type t := t
                       and module T := T
                       and module Set := Set

val print_list : Format.formatter -> t list -> unit

module List : sig
  (** extract variables from a list of parameters, preserving the order *)
  val vars : t list -> Variable.t list

  val wrap : Variable.t list -> t list

  val equal_vars : t list -> Variable.t list -> bool

  val rename
     : ?current_compilation_unit:Compilation_unit.t
    -> ?append:string
    -> t list
    -> t list

  val print : Format.formatter -> t list -> unit

  val compare : t list -> t list -> int
end
