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

module Extra_arg = struct
  type t =
    | Already_in_scope of Simple.t
    | New_binding of Flambda_primitive.t

  let print ppf t =
    match t with
    | Already_in_scope simple ->
      Format.fprintf ppf "@[<hov 1>(Already_in_scope@ %a)@]"
        Simple.print simple
    | New_binding prim ->
      Format.fprintf ppf "@[<hov 1>(New_binding@ %a)@]"
        Flambda_primitive.print prim

  module List = struct
    type nonrec t = t list

    let print ppf t =
      Format.fprintf ppf "(%a)"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space print) t
  end
end

type t = {
  extra_params : Kinded_parameter.t list;
  extra_args : Extra_arg.t list Apply_cont_rewrite_id.Map.t;
  extra_args_recursive_uses : Extra_arg.t list;
}

let print ppf { extra_params; extra_args; extra_args_recursive_uses; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(extra_params@ %a)@]@ \
      @[<hov 1>(extra_args_recursive_uses@ %a)@]@ \
      @[<hov 1>(extra_args@ %a)@]\
      )@]"
    Kinded_parameter.List.print extra_params
    (Apply_cont_rewrite_id.Map.print Extra_arg.List.print) extra_args
    Extra_arg.List.print extra_args_recursive_uses

let empty = {
  extra_params = [];
  extra_args = Apply_cont_rewrite_id.Map.empty;
  extra_args_recursive_uses = [];
}

let is_empty t =
  match t.extra_params with
  | [] ->
    assert (Apply_cont_rewrite_id.Map.is_empty t.extra_args);
    assert (List.length t.extra_args_recursive_uses = 0);
    true
  | _::_ -> false

let add t ~extra_param ~extra_arg ~extra_arg_recursive_uses =
  let extra_args =
    if Apply_cont_rewrite_id.Map.is_empty t.extra_args then
      Apply_cont_rewrite_id.Map.map (fun extra_arg -> [extra_arg]) extra_arg
    else
      Apply_cont_rewrite_id.Map.merge (fun id already_extra_args extra_arg ->
          match already_extra_args, extra_arg with
          | None, None -> None
          | Some l, None ->
            Misc.fatal_errorf "Cannot change domain (1) %a %i"
              Apply_cont_rewrite_id.print id
              (List.length l)
          | None, Some _ ->
            Misc.fatal_error "Cannot change domain"
          | Some already_extra_args, Some extra_arg ->
            Some (extra_arg :: already_extra_args))
        t.extra_args
        extra_arg
  in
  { extra_params = extra_param :: t.extra_params;
    extra_args;
    extra_args_recursive_uses =
      extra_arg_recursive_uses :: t.extra_args_recursive_uses;
  }
