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
    handler : Expr.t;
  }

  let print_with_cache ~cache ppf { handler; } =
    fprintf ppf "@[<hov 1>(\
        @[<hov 1>(handler@ %a)@]\
        )@]"
      (Expr.print_with_cache ~cache) handler

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names { handler; } =
    Expr.free_names handler

  let apply_name_permutation ({ handler; } as t) perm =
    let handler' =
      Expr.apply_name_permutation handler perm
    in
    if handler == handler' then t
    else { handler = handler'; }
end

module A = Name_abstraction.Make_list (Kinded_parameter) (T0)

type t = {
  abst : A.t;
  size : Inlining_size.t;
}

let invariant _env _t = ()

let print ppf t : unit = A.print ppf t.abst

let print_with_cache ~cache ppf t : unit = A.print_with_cache ~cache ppf t.abst

let create params ~handler =
  let t0 : T0.t =
    { handler;
    }
  in
  let abst = A.create params t0 in
  { abst;
    size = Expr.size handler;
  }

let pattern_match t ~f =
  A.pattern_match t.abst ~f:(fun params { handler; } ->
    f params ~handler)

let size t = t.size

let apply_name_permutation ({ abst; size; } as t) perm =
  let abst' = A.apply_name_permutation abst perm in
  if abst == abst' then t
  else { abst = abst'; size; }

let free_names { abst; size = _; } = A.free_names abst
