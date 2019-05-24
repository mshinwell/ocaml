(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Module signature used for abbreviating .mli files that correspond to
    meet and join operations on naked numbers. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  type flambda_type
  type 'a ty
  type 'a of_kind_naked_number
  type typing_env
  type meet_env
  type typing_env_extension

  module Naked_number : sig
    type t
    module Set : Set.S with type elt = t
  end

  module Make
    (E : Lattice_ops_intf.S
      with type typing_env := typing_env
      with type meet_env := meet_env
      with type typing_env_extension := typing_env_extension) :
  sig
    include Meet_and_join_spec_intf.S
      with type flambda_type := flambda_type
      with type 'a ty := 'a ty
      with type 'a of_kind_naked_number := 'a of_kind_naked_number
      with type meet_env := meet_env
      with type typing_env_extension := typing_env_extension
      with type of_kind_foo = Naked_number.Set.t of_kind_naked_number
  end
end
