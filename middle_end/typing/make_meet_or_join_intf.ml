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

module type S = sig
  module T : Typing_world_abstract.S

  module Make
    (E : Either_meet_or_join_intf.S)
    (S : Meet_and_join_spec_intf.S with module T := T) :
  sig
    val meet_or_join_ty
       : T.Join_env.t
      -> S.of_kind_foo T.Flambda_types.ty
      -> S.of_kind_foo T.Flambda_types.ty
      -> S.of_kind_foo T.Flambda_types.ty * T.Typing_env_extension.t
  end
end
