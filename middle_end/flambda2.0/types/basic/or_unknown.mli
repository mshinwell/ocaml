(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type 'a t =
  | Known of 'a
  | Unknown

val print
   : (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val map : 'a t -> f:('a -> 'b) -> 'b t

val map_sharing : 'a t -> f:('a -> 'a) -> 'a t

val free_names : ('a -> Name_occurrences.t) -> 'a t -> Name_occurrences.t

module Lift (I : Identifiable.S) : sig
  type nonrec t = I.t t

  include Identifiable.S with type t := t
end
(*
module Make_meet_or_join
  (Known : functor (E1 : Lattice_ops_intf.S)
    -> sig
      val meet : meet_env -> t -> t -> (t * typing_env_extension) Or_bottom.t
    end

  end)
  (E : Lattice_ops_intf.S
    with type meet_env := E1.meet_env
    with type typing_env := E1.typing_env
    with type typing_env_extension := E1.typing_env_extension)
: sig

end
*)