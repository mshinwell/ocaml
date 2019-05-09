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
  k : Continuation.t;
  args : Simple.t list;
  raise_kind : Raise_kind.Option.t;
}

let print ppf { k; args; raise_kind; } =
  match args with
  | [] ->
    Format.fprintf ppf "@[<2>%a%sgoto%s@ %a@]"
      Raise_kind.Option.print raise_kind
      (Misc.Color.bold_cyan ())
      (Misc.Color.reset ())
      Continuation.print k
  | _ ->
    Format.fprintf ppf "@[<2>%a%sapply_cont%s@ %a@ %a@]"
      Raise_kind.Option.print raise_kind
      (Misc.Color.bold_cyan ())
      (Misc.Color.reset ())
      Continuation.print k
      Simple.List.print args

let print_with_cache ~cache:_ ppf t = print ppf t

let invariant _env _t = ()
let create ?raise_kind k ~args = { k; args; raise_kind; }

let goto k =
  { k;
    args = [];
    raise_kind = None;
  }

let continuation t = t.k
let args t = t.args
let raise_kind t = t.raise_kind

let free_names { k; args; raise_kind; } =
  let raise_kind_free_names =
    match raise_kind with
    | None -> Name_occurrences.empty
    | Some raise_kind -> Raise_kind.free_names raise_kind
  in
  Name_occurrences.union_list [
    Name_occurrences.singleton_continuation k;
    Simple.List.free_names args;
    raise_kind_free_names;
  ]

let apply_name_permutation ({ k; args; raise_kind; } as t) perm =
  let k' = Name_permutation.apply_continuation perm k in
  let args' = Simple.List.apply_name_permutation args perm in
  let raise_kind' =
    match raise_kind with
    | None -> None
    | Some raise_kind' ->
      let new_raise_kind' =
        Raise_kind.apply_name_permutation raise_kind' perm
      in
      if new_raise_kind' == raise_kind' then raise_kind
      else Some new_raise_kind'
  in
  if k == k' && args == args' && raise_kind == raise_kind' then t
  else { k = k'; args = args'; raise_kind = raise_kind'; }

let update_args t ~args =
  if args == t.args then t
  else { t with args; }
