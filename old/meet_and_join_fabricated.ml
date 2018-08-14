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

module K = Flambda_kind

(* CR mshinwell: Delete >= 4.08 *)
[@@@ocaml.warning "-60"]
module Closure_ids = struct end
module Discriminants = struct end
module Flambda_type0_core = struct end
module Types_by_closure_id = struct end
module Typing_env_extension = struct end 

module Make (W : Typing_world.S) = struct
  open! W

  module Make
    (E : Either_meet_or_join_intf.S
      with module Join_env := W.Join_env
      with module Meet_env := W.Meet_env
      with module Typing_env := W.Typing_env
      with module Typing_env_extension := W.Typing_env_extension) =
  struct
    type of_kind_foo = Flambda_types.of_kind_fabricated

    let kind = K.fabricated ()

    let to_type ty : Flambda_types.t = Fabricated ty

    let force_to_kind = Flambda_type0_core.force_to_kind_fabricated
    let print_ty = Type_printers.print_ty_fabricated_with_cache

    let meet_or_join_set_of_closures_entry env
            (({ by_closure_id = by_closure_id1; }
              : Flambda_types.set_of_closures_entry)
              as set_of_closures_entry1)
            (({ by_closure_id = by_closure_id2; }
              : Flambda_types.set_of_closures_entry)
              as set_of_closures_entry2) : _ Or_absorbing.t =
      if Join_env.shortcut_precondition env
        && set_of_closures_entry1 == set_of_closures_entry2
      then
        Ok (set_of_closures_entry1, Typing_env_extension.empty)
      else
        let meet_or_join =
          E.switch Types_by_closure_id.meet Types_by_closure_id.join
            env by_closure_id1 by_closure_id2
        in
        match meet_or_join with
        | Bottom -> Absorbing
        | Ok (by_closure_id, env_extension) ->
          let set_of_closures_entry : Flambda_types.set_of_closures_entry =
            { by_closure_id; }
          in
          Ok (set_of_closures_entry, env_extension)

    let meet_or_join_of_kind_foo env
          (of_kind1 : Flambda_types.of_kind_fabricated)
          (of_kind2 : Flambda_types.of_kind_fabricated)
          : (Flambda_types.of_kind_fabricated * Typing_env_extension.t)
              Or_absorbing.t =
      if Join_env.shortcut_precondition env
        && of_kind1 == of_kind2
      then
        Ok (of_kind1, Typing_env_extension.empty)
      else
        match of_kind1, of_kind2 with
        | Discriminants discriminants1, Discriminants discriminants2 ->
          let discriminants =
            E.switch Discriminants.meet Discriminants.join env
              discriminants1 discriminants2
          in
          begin match discriminants with
          | Bottom -> Absorbing
          | Ok (discriminants, env_extension) ->
            Ok (Discriminants discriminants, env_extension)
          end
        | Set_of_closures { closures = closures1 },
            Set_of_closures { closures = closures2 } ->
          let closures =
            E.switch Closure_ids.meet Closure_ids.join env closures1 closures2
          in
          begin match closures with
          | Bottom -> Absorbing
          | Ok (closures, env_extension) ->
            Ok (Set_of_closures { closures; }, env_extension)
          end
        | (Discriminants _ | Set_of_closures _), _ -> Absorbing

    module Closure_ids = W.Closure_ids
    module Discriminants = W.Discriminants
    module Flambda_type0_core = W.Flambda_type0_core
    module Types_by_closure_id = W.Types_by_closure_id
    module Typing_env_extension = W.Typing_env_extension
  end

  module Closure_ids = W.Closure_ids
  module Discriminants = W.Discriminants
  module Flambda_type0_core = W.Flambda_type0_core
  module Types_by_closure_id = W.Types_by_closure_id
  module Typing_env_extension = W.Typing_env_extension
end
