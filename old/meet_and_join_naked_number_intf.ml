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

module Make (Naked_number : sig
  type t
  module Set : Set.S with type elt = t
end) = struct
  module type S = sig
    module T : Typing_world_abstract.S

    module Make (E : Either_meet_or_join_intf.S) : sig
      include Meet_and_join_spec_intf.S
        with module T := T
        with type of_kind_foo =
          Naked_number.Set.t T.Flambda_types.of_kind_naked_number
    end
  end
end
