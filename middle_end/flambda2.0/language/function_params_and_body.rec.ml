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

module My_closure_and_rec_info_var = struct
  type t = {
    my_closure : Variable.t;
    rec_info_var : Variable.t;
  }

  let print ppf { my_closure; rec_info_var; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[(my_closure@ %a)@]@ \
        @[(rec_info_var@ %a)@]\
        )@]"
      Variable.print my_closure
      Variable.print rec_info_var

  let free_names { my_closure; rec_info_var; } =
    Name_occurrences.add_variable
      (Name_occurrences.singleton_variable my_closure
        Name_occurrence_kind.normal)
      rec_info_var
      Name_occurrence_kind.in_types

  let apply_name_permutation ({ my_closure; rec_info_var; } as t) perm =
    let my_closure' = Name_permutation.apply_variable perm my_closure in
    let rec_info_var' = Name_permutation.apply_variable perm rec_info_var in
    if my_closure == my_closure' && rec_info_var == rec_info_var' then t
    else
      { my_closure = my_closure';
        rec_info_var = rec_info_var';
      }
end

module Closure_id_and_irrelevant_var = struct
  type t = Closure_id.t * Variable.t

  include Identifiable.Make_pair (Closure_id) (Variable)

  let free_names (_closure_id, var) =
    Name_occurrences.singleton_variable var Name_occurrence_kind.in_types

  let apply_name_permutation ((closure_id, var) as t) perm =
    let var' = Name_permutation.apply_variable perm var in
    if var == var' then t
    else (closure_id, var')
end

module T0 = Name_abstraction.Make (My_closure_and_rec_info_var) (Expr)
module T1 = Name_abstraction.Make_list (Closure_id_and_irrelevant_var) (T0)
module T2 = Name_abstraction.Make_list (Kinded_parameter) (T1)
module T3 = Name_abstraction.Make (Bindable_exn_continuation) (T2)
include Name_abstraction.Make (Bindable_continuation) (T3)

let invariant _env _t = ()

let print ppf t : unit = print ppf t

let print_with_cache ~cache ppf t : unit = print_with_cache ~cache ppf t

let create ~return_continuation exn_continuation params ~body ~my_closure
      ~irrelevant_closure_vars ~rec_info_var =
  let my_closure =
    Kinded_parameter.create (Parameter.wrap my_closure) K.value
  in
  let rec_info_var =
    Kinded_parameter.create (Parameter.wrap rec_info_var) K.fabricated
  in
  let my_closure_and_rec_info_var =
    { my_closure;
      rec_info_var;
    }
  in
  let irrelevant_closure_vars =
    Closure_id.Map.bindings irrelevant_closure_vars
  in
  let t0 = T0.create (my_closure_and_rec_info_var) body in
  let t1 = T1.create irrelevant_closure_vars t0 in
  let t2 = T2.create params t1 in
  let t3 = T3.create exn_continuation t2 in
  create return_continuation t3

let pattern_match t ~f =
  pattern_match t ~f:(fun return_continuation t3 ->
    T3.pattern_match t3 ~f:(fun exn_continuation t2 ->
      T2.pattern_match t2 ~f:(fun params t1 ->
        T1.pattern_match t1 ~f:(fun irrelevant_closure_vars t0 ->
          T0.pattern_match t0 ~f:(fun my_closure_and_rec_info_var body ->
            let irrelevant_closure_vars =
              Closure_id.Map.of_list irrelevant_closure_vars
            in
            f ~return_continuation exn_continuation params ~body:t0.body
              ~my_closure:my_closure_and_rec_info_var.my_closure
              ~irrelevant_closure_vars
              ~rec_info_var:my_closure_and_rec_info_var.rec_info_var)))))
