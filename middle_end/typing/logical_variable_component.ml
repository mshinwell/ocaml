(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Logical_variable

let free_names t =
  Name_occurrences.singleton_in_types (Name (Name.logical_var t))

(* CR mshinwell: This is strange.  Should logical variables not be in [Name]
   and instead separately in [Bindable_name]? *)
let apply_name_permutation t perm =
  match Name_permutation.apply_name perm (Name.logical_var t) with
  | Logical_var var -> var
  | _ ->
    Misc.fatal_errorf "Illegal name permutation on logical variables: %a"
      Name_permutation.print perm

let name t = Name.logical_var t

let equal env t1 t2 =
  let t1 = apply_name_permutation t1 (Type_equality_env.perm_left env) in
  let t2 = apply_name_permutation t2 (Type_equality_env.perm_right env) in
  equal t1 t2
