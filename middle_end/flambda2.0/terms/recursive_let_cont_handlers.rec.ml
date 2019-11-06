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

module T0 = struct
  type t = {
    handlers : Continuation_handlers.t;
    body : Expr.t;
  }

  let invariant _env _t = ()

  let print _ppf _t = Misc.fatal_error "Not yet implemented"
  let print_with_cache ~cache:_ _ppf _t = Misc.fatal_error "Not yet implemented"

  let create ~body handlers =
    { handlers;
      body;
    }

  let handlers t = t.handlers
  let body t = t.body

  let free_names { handlers; body; } =
    Name_occurrences.union (Continuation_handlers.free_names handlers)
      (Expr.free_names body)

  let apply_name_permutation { handlers; body; } perm =
    let handlers' =
      Continuation_handlers.apply_name_permutation handlers perm
    in
    let body' =
      Expr.apply_name_permutation body perm
    in
    { handlers = handlers';
      body = body';
    }
end

module A = Name_abstraction.Make_list (Bindable_continuation) (T0)

type t = {
  abst : A.t;
  size : Inlining_size.t;
}

let print ppf t : unit = A.print ppf t.abst

let print_with_cache ~cache ppf t : unit = A.print_with_cache ~cache ppf t.abst

let invariant _env _t = ()

let create ~body handlers =
  let bound = Continuation_handlers.domain handlers in
  let handlers0 = T0.create ~body handlers in
  let size =
    Inlining_size.(+) (Expr.size body)
      (Continuation_handlers.size handlers)
  in
  let abst = A.create (Continuation.Set.elements bound) handlers0 in
  { abst;
    size;
  }

let pattern_match t ~f =
  A.pattern_match t.abst ~f:(fun _bound handlers0 ->
    let body = T0.body handlers0 in
    let handlers = T0.handlers handlers0 in
    f ~body handlers)

let size t = t.size

let apply_name_permutation ({ abst; size; } as t) perm =
  let abst' = A.apply_name_permutation abst perm in
  if abst == abst' then t
  else { abst = abst'; size; }

let free_names { abst; size = _; } = A.free_names abst
