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

type t = Expr.continuation_handlers

let invariant _env _t = ()

let print_with_cache ~cache:_ _ppf _t =
  Misc.fatal_error "Continuation_handlers.print_with_cache not yet implemented"

let print _ppf _t =
  Misc.fatal_error "Continuation_handlers.print not yet implemented"

let to_map t = t

let free_names = Expr.free_names_continuation_handlers
let apply_renaming = Expr.apply_renaming_continuation_handlers
let all_ids_for_export = Expr.all_ids_for_export_continuation_handlers

let domain t = Continuation.Map.keys t

let contains_exn_handler t =
  Continuation.Map.exists (fun _cont handler ->
      Continuation_handler.is_exn_handler handler)
    t
