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

module Make (W : Typing_world.S)
    (E : Either_meet_or_join_intf.S with module W := W) =
struct
  open! W

  type of_kind_foo = Flambda_type0_core.of_kind_fabricated

  let kind = K.fabricated ()

  let to_type ty : Flambda_type0_core.t =
    { descr = Fabricated ty;
    }

  let force_to_kind = Flambda_type0_core.force_to_kind_fabricated
  let print_ty = Flambda_type0_core.print_ty_fabricated

  let meet_or_join_set_of_closures_entry env
          (({ by_closure_id = by_closure_id1; } : set_of_closures_entry)
            as set_of_closures_entry1)
          (({ by_closure_id = by_closure_id2; } : set_of_closures_entry)
            as set_of_closures_entry2) : _ Or_absorbing.t =
    if JE.fast_check_extensions_same_both_sides env
      && set_of_closures_entry1 == set_of_closures_empty2
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
        Ok ({ by_closure_id; }, env_extension)

  let meet_or_join_of_kind_foo env
        (of_kind1 : of_kind_fabricated) (of_kind2 : of_kind_fabricated)
        : (of_kind_fabricated * env_extension) Or_absorbing.t =
    if JE.fast_check_extensions_same_both_sides env
      && of_kind1 == of_kind2
    then
      Ok (of_kind1, Typing_env_extension.empty)
    else
      match of_kind1, of_kind2 with
      | Discriminants discriminants1, Discriminants discriminants2 ->
        let discriminants, env_extension1 =
          E.switch Discriminants.meet Discriminants.join env perm1 perm2
            discriminants1 discriminants2
        in
        begin match discriminants with
        | Bottom -> Absorbing
        | Ok discriminants ->
          let env_extension2 =
            match Discriminants.get_singleton discriminants with
            | None -> Typing_env_extension.empty
            | Some product -> Discriminants.standalone_extension product
          in
          begin match
            Typing_env_extension.meet env env_extension1 env_extension2
          with
          | Bottom -> Absorbing
          | Ok discriminants ->
            Ok (Discriminants discriminants, env_extension)
          end
        end
      | Set_of_closures { closures = closures1 },
          Set_of_closures { closures = closures2 } ->
        let closures =
          E.switch Closure_ids.meet Closure_ids.join perm1 perm2
            closures1 closures2
        in
        begin match closures with
        | Bottom -> Absorbing
        | Ok closures -> Ok (Set_of_closures { closures; })
        end
      | (Discriminant _ | Set_of_closures _), _ -> Absorbing

  module Closure_ids = W.Closure_ids
  module Discriminants = W.Discriminants
  module Flambda_type0_core = W.Flambda_type0_core
  module Types_by_closure_id = W.Types_by_closure_id
  module Typing_env_extension = W.Typing_env_extension
end
