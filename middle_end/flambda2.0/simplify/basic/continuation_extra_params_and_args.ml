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

type t = {
  extra_params : Kinded_parameter.t list;
  extra_args : Simple.t list Apply_cont_rewrite_id.Map.t;
}

let empty = {
  extra_params = [];
  extra_args = Apply_cont_rewrite_id.Map.empty;
}

let add t ~extra_param ~extra_args =
  let extra_args =
    if Apply_cont_rewrite_id.Map.is_empty t.extra_args then
      Apply_cont_rewrite_id.Map.map (fun extra_args -> [extra_args]) extra_args
    else
      Apply_cont_rewrite_id.Map.merge (fun _id already_extra_args extra_args ->
          match already_extra_args, extra_args with
          | None, None -> None
          | Some _, None | None, Some _ ->
            Misc.fatal_error "Cannot change domain"
          | Some already_extra_args, Some extra_args ->
            Some (extra_args :: already_extra_args))
        t.extra_args
        extra_args
  in
  { extra_params = extra_param :: t.extra_params;
    extra_args;
  }
