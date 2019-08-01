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

module Int_indexed_product
  : Product_intf.S
    with module Index := Numbers.Int
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type type_equality_env := Type_equality_env.t
    with type typing_env_extension := Typing_env_extension.t

type t

type open_or_closed = Open | Closed of Tag.t

val create : field_tys:Type_grammar.t list -> open_or_closed -> t

val create_bottom : unit -> t

val all_tags_and_sizes : t -> Targetint.OCaml.t Tag.Map.t Or_unknown.t

val get_singleton : t -> (Tag_and_size.t * Int_indexed_product.t) option

val is_bottom : t -> bool

include Type_structure_intf.S
  with type t := t
  with type flambda_type := Type_grammar.t
  with type meet_env := Meet_env.t
  with type type_equality_env := Type_equality_env.t
  with type typing_env := Typing_env.t
  with type typing_env_extension := Typing_env_extension.t
