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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module KI = Kind_independent_meet_or_join
module T = Type_grammar
module TEE = Typing_env_extension

module Make
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  module Of_kind_naked_immediate = Meet_and_join_naked_immediate.Make (E)
  module Of_kind_naked_float = Meet_and_join_naked_float.Make (E)
  module Of_kind_naked_int32 = Meet_and_join_naked_int32.Make (E)
  module Of_kind_naked_int64 = Meet_and_join_naked_int64.Make (E)
  module Of_kind_naked_nativeint = Meet_and_join_naked_nativeint.Make (E)
  module Of_kind_fabricated = Meet_and_join_fabricated.Make (E)

  module Naked_immediate = KI.Make (E) (Of_kind_naked_immediate)
  module Naked_float = KI.Make (E) (Of_kind_naked_float)
  module Naked_int32 = KI.Make (E) (Of_kind_naked_int32)
  module Naked_int64 = KI.Make (E) (Of_kind_naked_int64)
  module Naked_nativeint = KI.Make (E) (Of_kind_naked_nativeint)
  module Fabricated = KI.Make (E) (Of_kind_fabricated)

  type of_kind_foo = T.of_kind_value

  let kind = K.value
  let to_type ty : T.t = Value ty
  let force_to_kind = Basic_type_ops.force_to_kind_value
  let print_ty = Type_printers.print_ty_value_with_cache
  let apply_rec_info = Basic_type_ops.apply_rec_info_of_kind_value

  (* CR mshinwell: These next two could go in a separate file. *)


end
