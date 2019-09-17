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
  | Boxed_float of Type_of_kind_naked_float.t
  | Boxed_int32 of Type_of_kind_naked_int32.t
  | Boxed_int64 of Type_of_kind_naked_int64.t
  | Boxed_nativeint of Type_of_kind_naked_nativeint.t
  | Closures of {
      by_closure_id : Closures_entry_by_set_of_closures_contents.t;
    }
  | String of String_info.Set.t
  | Array of { length : Type_of_kind_value.t; }

include Type_head_intf.S
  with type t := t
  with type type_grammar := Type_grammar.t
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
  with type meet_env := Meet_env.t
