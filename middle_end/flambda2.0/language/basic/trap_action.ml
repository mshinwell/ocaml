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

type raise_kind =
  | Regular
  | Reraise
  | No_trace

type t =
  | Push of { exn_handler : Continuation.t; }
  | Pop of {
      exn_handler : Continuation.t;
      raise_kind : raise_kind option;
    }

let raise_kind_option_to_string = function
  | None -> ""
  | Some Regular -> " (raise-regular)"
  | Some Reraise -> " (reraise)"
  | Some No_trace -> " (notrace)"

let print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | Push { exn_handler; } ->
    fprintf ppf "%spush%s %a %sthen%s "
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      Continuation.print exn_handler
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
  | Pop { exn_handler; raise_kind; } ->
    fprintf ppf "%spop%s%s %a %sthen%s "
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      (raise_kind_option_to_string raise_kind)
      Continuation.print exn_handler
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())

let print_with_cache ~cache:_ ppf t = print ppf t

let invariant _env _t = ()

let free_names = function
  | Push { exn_handler; }
  | Pop { exn_handler; raise_kind = _; } ->
    Name_occurrences.singleton_continuation exn_handler

let apply_name_permutation t perm =
  match t with
  | Push { exn_handler; } ->
    let exn_handler' = Name_permutation.apply_continuation perm exn_handler in
    if exn_handler == exn_handler' then t
    else Push { exn_handler = exn_handler'; }
  | Pop { exn_handler; raise_kind; } ->
    let exn_handler' = Name_permutation.apply_continuation perm exn_handler in
    if exn_handler == exn_handler' then t
    else Pop { exn_handler = exn_handler'; raise_kind; }

module Option = struct
  type nonrec t = t option

  let print ppf = function
    | None -> ()
    | Some t -> print ppf t

(*
  let free_names = function
    | None -> Name_occurrences.empty
    | Some trap_action -> free_names trap_action

  let apply_name_permutation t perm =
    match t with
    | None -> None
    | Some trap_action ->
      let trap_action' = apply_name_permutation trap_action perm in
      if trap_action == trap_action' then t
      else Some trap_action'
*)
end
