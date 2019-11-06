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

module Continuation_and_body =
  Name_abstraction.Make (Bindable_continuation) (Expr)

type t = {
  continuation_and_body : Continuation_and_body.t;
  handler : Continuation_handler.t;
  size : Inlining_size.t;
}

let invariant _env _t = ()

let print _ppf _t = Misc.fatal_error "Not yet implemented"

let print_with_cache ~cache:_ _ppf _t = Misc.fatal_error "Not yet implemented"

let create continuation ~body handler =
  let continuation_and_body =
    Continuation_and_body.create continuation body
  in
  let size =
    Inlining_size.(+) (Expr.size body) (Continuation_handler.size handler)
  in
  { continuation_and_body;
    handler;
    size;
  }

let pattern_match t ~f =
  Continuation_and_body.pattern_match t.continuation_and_body
    ~f:(fun continuation body -> f continuation ~body)

let handler t = t.handler

let free_names { continuation_and_body; handler; size = _; } =
  Name_occurrences.union
    (Continuation_and_body.free_names continuation_and_body)
    (Continuation_handler.free_names handler)

let apply_name_permutation { continuation_and_body; handler; size; } perm =
  let continuation_and_body' =
    Continuation_and_body.apply_name_permutation continuation_and_body perm
  in
  let handler' =
    Continuation_handler.apply_name_permutation handler perm
  in
  { handler = handler';
    continuation_and_body = continuation_and_body';
    size;
  }

let size t = t.size
