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

module type S_for_types = sig
  module T : sig
    type flambda_type
    type typing_environment
    type env_extension
    type join_env
  end

  val meet_or_join
     : T.join_env
    -> T.flambda_type
    -> T.flambda_type
    -> T.flambda_type * T.env_extension
end

module type S_both = sig
  module T : sig
    type env_extension
    type join_env
    type typing_environment
    type flambda_type
  end

  val meet
     : T.typing_environment
    -> T.flambda_type
    -> T.flambda_type
    -> T.flambda_type * T.env_extension

  val join
     : T.join_env
    -> T.flambda_type
    -> T.flambda_type
    -> T.flambda_type

  val as_or_more_precise
     : T.typing_environment
    -> T.flambda_type
    -> than:T.flambda_type
    -> bool

  val strictly_more_precise
     : T.typing_environment
    -> T.flambda_type
    -> than:T.flambda_type
    -> bool
end
