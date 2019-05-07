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
