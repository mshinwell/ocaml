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
  exn_handler : Continuation.t;
  extra_args : (Simple.t * Flambda_kind.t) list;
}

let create ~exn_handler ~extra_args =
  { exn_handler;
    extra_args;
  }

let exn_handler t = t.exn_handler

let extra_args t = t.extra_args

let invariant _env _t = ()

let print ppf { exn_handler; extra_args; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(exn_handler@ %a)@]@ \
      @[<hov 1>(extra_args@ %a)@])@]"
    Continuation.print exn_handler
    Simple.List.print extra_args

let print_with_cache ~cache:_ ppf t = print ppf t

let free_names { exn_handler; extra_args; } =
  Name_occurrences.union
    (Name_occurrences.singleton_in_terms (Continuation exn_handler))
    (Simple.List.free_names extra_args)

let continuation_counts { exn_handler; extra_args = _; } =
  Continuation_counts.create_singleton exn_handler

let apply_name_permutation ({ exn_handler; extra_args; } as t) perm =
  let exn_handler' = Name_permutation.apply_continuation perm exn_handler in
  let extra_args' = Simple.List.apply_name_permutation extra_args perm in
  if exn_handler == exn_handler' && extra_args == extra_args' then t
  else { exn_handler = exn_handler'; extra_args = extra_args'; }
