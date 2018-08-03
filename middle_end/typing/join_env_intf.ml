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

module type S = sig
  module Flambda_type0_core : sig type t end
  module Meet_env : sig type t end
  module Typing_env : sig type t end
  module Typing_env_extension : sig type t end

  type t

  (** Perform various invariant checks upon the given join environment. *)
  val invariant : t -> unit

  val create : Meet_env.t -> t

  val add_definition_central_environment
     : t
    -> Name.t
    -> Flambda_type0_core.t
    -> t

  val add_extensions
     : t
    -> holds_on_left:Typing_env_extension.t
    -> holds_on_right:Typing_env_extension.t
    -> t

  val add_extensions_and_extend_central_environment
     : t
    -> holds_on_left:Typing_env_extension.t
    -> holds_on_right:Typing_env_extension.t
    -> central_extension:Typing_env_extension.t
    -> t

  val central_environment : t -> Meet_env.t

  val environment_on_left : t -> Typing_env.t

  val environment_on_right : t -> Typing_env.t

  val holds_on_left : t -> Typing_env_extension.t

  val holds_on_right : t -> Typing_env_extension.t

  val shortcut_precondition : t -> bool

  val perm_left : t -> Name_permutation.t

  val perm_right : t -> Name_permutation.t

  val clear_name_permutations : t -> t
end
