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

module KP = Kinded_parameter

module Id = struct
  include Numbers.Int

  let next = ref 0

  let create () =
    let t = !next in
    incr next;
    t
end

type t = {
  original_params : KP.t list;
  used_params : KP.Set.t;
  used_extra_params : KP.Set.t;
  extra_args : Simple.t list Id.Map.t;
}

let create ~original_params ~used_params ~extra_params ~extra_args
      ~used_extra_params =
  (* CR mshinwell: check there weren't any duplicates in the param lists too *)
  if List.length original_params < KP.Set.cardinal used_params then begin
    Misc.fatal_errorf "Must have at least as many [original_params] (%a)@ as \
        [used_params] (%a)"
      KP.List.print original_params
      KP.Set.print used_params
  end;
  if List.length extra_params < KP.Set.cardinal used_extra_params then begin
    Misc.fatal_errorf "Must have at least as many [extra_params] (%a)@ as \
        [used_extra_params] (%a)"
      KP.List.print extra_params
      KP.Set.print used_extra_params
  end;
  let extra_args =
    Id.Map.map (fun extra_args ->
        if List.compare_lengths extra_params extra_args <> 0 then begin
          Misc.fatal_errorf "Lengths of [extra_params] (%a)@ and all \
              [extra_args] (e.g. %a) should be equal"
            KP.List.print extra_params
            Simple.List.print extra_args
        end;
        let extra_params_and_args = List.combine extra_params extra_args in
        List.filter_map (fun (extra_param, extra_arg) ->
            if KP.Set.mem extra_param used_extra_params then Some extra_arg
            else None)
          extra_params_and_args)
      extra_args
  in
  { original_params;
    used_params;
    used_extra_params;
    extra_args;
  }

let extra_params t = t.used_extra_params

let extra_args t id =
  match Id.Map.find id t.extra_args with
  | exception Not_found ->
    Misc.fatal_errorf "Apply cont rewrite ID %a not found"
      Id.print id
  | extra_args -> extra_args

let rewrite_use t id apply_cont =
  let args = Flambda.Apply_cont.args apply_cont in
  if List.compare_lengths args t.original_params <> 0 then begin
    Misc.fatal_errorf "Arguments to this [Apply_cont] do not match@ \
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
  let extra_args = extra_args t id in
  Flambda.Apply_cont.update_args apply_cont ~args:(args @ extra_args)
