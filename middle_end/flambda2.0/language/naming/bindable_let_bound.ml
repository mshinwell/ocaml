(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Singleton of Var_in_binding_pos.t
  | Set_of_closures of {
      closure_vars : Var_in_binding_pos.t Closure_id.Map.t;
    }

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf t =
    match t with
    | Singleton var -> Var_in_binding_pos.print ppf var
    | Set_of_closures { closure_vars; } ->
      Closure_id.Map.print Var_in_binding_pos.print ppf closure_vars

  let compare t1 t2 =
    match t1, t2 with
    | Singleton var1, Singleton var2 -> Var_in_binding_pos.compare var1 var2
    | Singleton _, Set_of_closures _ -> -1
    | Set_of_closures _, Singleton _ -> 1
    | Set_of_closures { closure_vars = closure_vars1; },
        Set_of_closures { closure_vars = closure_vars2; } ->
      Closure_id.Map.compare Var_in_binding_pos.compare
        closure_vars1 closure_vars2

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash _ = Misc.fatal_error "Not yet implemented"

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let print_with_cache ~cache:_ ppf t = print ppf t

let free_names t =
  match t with
  | Singleton var ->
    let var = Var_in_binding_pos.var var in
    Name_occurrences.singleton_variable var Name_occurrence_kind.normal
  | Set_of_closures { closure_vars; } ->
    Closure_id.Map.fold (fun _closure_id var free_names ->
        let var = Var_in_binding_pos.var var in
        Name_occurrences.add_variable free_names var
          Name_occurrence_kind.normal)
      closure_vars
      Name_occurrences.empty

let apply_name_permutation t perm =
  match t with
  | Singleton var ->
    let var' = Var_in_binding_pos.apply_name_permutation var perm in
    if var == var' then t
    else Singleton var'
  | Set_of_closures { closure_vars; } ->
    let closure_vars' =
      Closure_id.Map.map_sharing (fun var ->
          Var_in_binding_pos.apply_name_permutation var perm)
        closure_vars
    in
    if closure_vars == closure_vars' then t
    else Set_of_closures { closure_vars = closure_vars'; }

let rename t =
  match t with
  | Singleton var -> Singleton (Var_in_binding_pos.rename var)
  | Set_of_closures { closure_vars; } ->
    let closure_vars =
      Closure_id.Map.map (fun var -> Var_in_binding_pos.rename var) closure_vars
    in
    Set_of_closures { closure_vars; }

let add_to_name_permutation t1 t2 perm =
  match t1, t2 with
  | Singleton var1, Singleton var2 ->
    Name_permutation.add_variable perm
      (Var_in_binding_pos.var var1)
      (Var_in_binding_pos.var var2)
  | Set_of_closures { closure_vars = closure_vars1; },
      Set_of_closures { closure_vars = closure_vars2; } ->
    let perm =
      Closure_id.Map.fold2_stop_on_key_mismatch
        (fun _closure_var var1 var2 perm ->
          Name_permutation.add_variable perm
            (Var_in_binding_pos.var var1)
            (Var_in_binding_pos.var var2))
        closure_vars1
        closure_vars2
        perm
    in
    begin match perm with
    | Some perm -> perm
    | None ->
      Misc.fatal_errorf "Mismatching closure vars:@ %a@ and@ %a"
        print t1
        print t2
    end
  | (Singleton _ | Set_of_closures _), _ ->
    Misc.fatal_errorf "Kind mismatch:@ %a@ and@ %a"
      print t1
      print t2

let name_permutation t1 t2 =
  add_to_name_permutation t1 t2 Name_permutation.empty

let singleton_occurrence_in_terms t = free_names t

let add_occurrence_in_terms t occs =
  Name_occurrences.union (free_names t) occs
