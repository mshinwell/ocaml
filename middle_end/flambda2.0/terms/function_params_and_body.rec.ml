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
    body : Expr.t;
  }

  let print_with_cache ~cache:_ ppf { body; } =
    fprintf ppf "@[<hov 1>(\
        @[<hov 1>(body %a)@]\
        )@]"
      Expr.print body

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names { body; } =
    Expr.free_names body

  let apply_name_permutation ({ body;} as t) perm =
    let body' =
      Expr.apply_name_permutation body perm
    in
    if body == body' then t
    else { body = body'; }
end
module T1 = Name_abstraction.Make_list (Kinded_parameter) (T0)
module T2 = Name_abstraction.Make (Bindable_exn_continuation) (T1)
module A = Name_abstraction.Make (Bindable_continuation) (T2)

type t = {
  abst : A.t;
  params_arity : Flambda_arity.t;
}

let invariant _env _t = ()

let print ppf t : unit = A.print ppf t.abst

let print_with_cache ~cache ppf t : unit = A.print_with_cache ~cache ppf t.abst

let create ~return_continuation exn_continuation params ~body ~my_closure =
  let t0 : T0.t =
    { body;
    }
  in
  let my_closure =
    Kinded_parameter.create (Parameter.wrap my_closure) K.value
  in
  let t1 = T1.create (params @ [my_closure]) t0 in
  let t2 = T2.create exn_continuation t1 in
  let abst = A.create return_continuation t2 in
  { abst;
    params_arity = Kinded_parameter.List.arity params;
  }

let pattern_match t ~f =
  A.pattern_match t.abst ~f:(fun return_continuation t2 ->
    T2.pattern_match t2 ~f:(fun exn_continuation t1 ->
      T1.pattern_match t1 ~f:(fun params_and_my_closure t0 ->
        let params, my_closure =
          match List.rev params_and_my_closure with
          | my_closure::params_rev ->
            List.rev params_rev, Kinded_parameter.var my_closure
          | [] -> assert false  (* see [create], above. *)
        in
        f ~return_continuation exn_continuation params ~body:t0.body
          ~my_closure)))

let params_arity t = t.params_arity

let apply_name_permutation ({ abst; params_arity; } as t) perm =
  let abst' = A.apply_name_permutation abst perm in
  if abst == abst' then t
  else { abst = abst'; params_arity; }

let free_names { abst; params_arity = _; } = A.free_names abst