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

module Make (W : Typing_world.S)
    (E : Either_meet_or_join_intf.S with module W := W) =
struct
  module JE = W.Join_env
  module TEE = W.Typing_env_extension
  module T = W.Flambda_type

  type of_kind_foo = T.of_kind_fabricated

  let kind = K.fabricated ()

  let to_type ty : T.t =
    { descr = Fabricated ty;
    }

  let force_to_kind = T.force_to_kind_fabricated
  let print_ty = T.print_ty_fabricated

  let meet_or_join_of_kind_foo env perm1 perm2
        (of_kind1 : of_kind_fabricated) (of_kind2 : of_kind_fabricated)
        : (of_kind_fabricated * env_extension) Or_absorbing.t =
    if JE.fast_check_extensions_same_both_sides env
      && perm1 == perm2
      && of_kind1 == of_kind2
    then
      Ok (of_kind1, TEE.empty)
    else
      match of_kind1, of_kind2 with
      | Discriminants discriminants1, Discriminants discriminants2 ->
        let discriminants, env_extension1 =
          E.switch W.Discriminants.meet W.Discriminants.join env perm1 perm2
            discriminants1 discriminants2
        in
        begin match discriminants with
        | Bottom -> Absorbing
        | Ok discriminants ->
          let env_extension2 =
            match W.Discriminants.get_singleton discriminants with
            | None -> TEE.empty
            | Some product -> W.Discriminants.RP.standalone_extension product
          in
          let env_extension = TEE.meet env env_extension1 env_extension2 in
          Ok (Discriminants discriminants, env_extension)
        end
      | Set_of_closures { closures = closures1 },
          Set_of_closures { closures = closures2 } ->
        let closures =
          E.switch W.Closure_ids.meet W.Closure_ids.join perm1 perm2
            closures1 closures2
        in
        begin match closures with
        | Bottom -> Absorbing
        | Ok closures -> Ok (Set_of_closures { closures; })
        end
      | (Discriminant _ | Set_of_closures _), _ -> Absorbing
end
