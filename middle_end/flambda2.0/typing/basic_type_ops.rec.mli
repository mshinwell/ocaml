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

(** For documentation on this module please see [Type_system_intf]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val apply_rec_info_of_kind_value
   : Type_grammar.of_kind_value
  -> Rec_info.t
  -> Type_grammar.of_kind_value Or_bottom.t

val apply_rec_info_of_kind_naked_number
   : 'a Type_grammar.of_kind_naked_number
  -> Rec_info.t
  -> 'a Type_grammar.of_kind_naked_number Or_bottom.t

val apply_rec_info_of_kind_fabricated
   : Type_grammar.of_kind_fabricated
  -> Rec_info.t
  -> Type_grammar.of_kind_fabricated Or_bottom.t
