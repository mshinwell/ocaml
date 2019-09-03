(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell type Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Blocks_and_tagged_immediates of {
      immediates : Immediates.t Or_unknown.t;
      blocks : Blocks.t Or_unknown.t;
    }
  | Boxed_number of Type_of_kind_naked_number.t
  | Closures of {
      by_closure_id : Closures_entry_by_set_of_closures_contents.t;
    }
  | String of String_info.Set.t
  | Array of { length : Type_of_kind_value.t; }

include Contains_names.S

module Make_meet_or_join (E : Lattice_ops_intf.S
  with type meet_env = Meet_env.t
  with type typing_env_extension = Typing_env_extension.t)
: sig
  val meet_or_join
     : Meet_env.t
    -> t
    -> t
    -> (t * Typing_env_extension.t) Or_bottom_or_absorbing.t
end

val erase_aliases : t -> allowed:Variable.Set.t -> t

val apply_rec_info : t -> Rec_info.t -> t Or_bottom.t
