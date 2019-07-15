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

open! Simplify_import

type t = {
  original_params : KP.t list;
  used_params : KP.t list;
  used_extra_params : (KP.t * Simple.t) list;
}

let create ~original_params ~used_params ~used_extra_params =
  if List.compare_lengths original_params used_params < 0 then begin
    Misc.fatal_error "Must have at least as many [original_params] (%a) as \
        [used_params] (%a)"
      KP.List.print original_params
      KP.List.print used_params
  end;
  { original_params;
    used_params = KP.Set.of_list used_params_set;
    used_extra_params;
  }

let apply t apply_cont =
  let args = Apply_cont.args apply_cont in
  if List.compare_lengths args t.original_params <> 0 then begin
    Misc.fatal_errorf "Arguments to this [Apply_cont] do not match \
        [original_params] (%a):@ %a"
      KP.List.print t.original_params
      Simple.List.print args
  end;
  let original_params_with_args = List.combine t.original_params args in
  let args =
    List.filter_map (fun (original_param, arg) ->
        if KP.Set.mem original_param t.used_params then Some arg
        else None)
      original_params_with_args
  in
  let extra_args =
    List.map (fun (_extra_param, bound_to) -> bound_to) t.used_extra_params
  in
  Apply_cont.with_args apply_cont ~args

let extra_params t = List.map fst t.used_extra_params

let extra_args t = List.map snd t.used_extra_params
