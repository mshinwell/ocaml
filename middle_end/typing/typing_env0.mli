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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Make (T : sig
  include Flambda_type0_internal_intf.S

  val print : Format.formatter -> t -> unit

  val print_typing_environment
     : Format.formatter
    -> typing_environment
    -> unit

  val free_names : flambda_type -> Name_occurrences.t

  val free_names_set : flambda_type -> Name.Set.t

  val unknown : Flambda_kind.t -> t

  val force_to_kind_value : t -> of_kind_value ty

  val force_to_kind_naked_number
     : 'a Flambda_kind.Naked_number.t
    -> t
    -> 'a of_kind_naked_number ty

  val force_to_kind_fabricated : t -> of_kind_fabricated ty

  val kind : flambda_type -> Flambda_kind.t

  module Typing_env_extension : Typing_env_extension_intf.S
    with type env_extension := env_extension
    with type typing_environment := typing_environment
    with type flambda_type := flambda_type

  val join : t_in_context -> t_in_context -> t

  val meet
     : bias_towards:t_in_context
    -> t_in_context
    -> t * Typing_env_extension.t

  val is_empty_typing_environment : typing_environment -> bool
end) : sig
  include Typing_env0_intf.S
    with type typing_environment = T.typing_environment
    with type env_extension = T.Typing_env_extension.t
    with type flambda_type = T.flambda_type
    with type t_in_context = T.t_in_context
    with type 'a ty = 'a T.ty
    with type 'a unknown_or_join = 'a T.unknown_or_join
end
