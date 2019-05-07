include Relational_product.Make (Int_index) (Logical_variable_component)

let create tys =
  let lvs =
    List.map (fun ty ->
      let kind = Flambda_type0_core.kind ty in
      Logical_variable.create kind) tys
  in
  let indexes_to_lvs =
    Targetint.OCaml.Map.of_list (
      List.mapi (fun index lv ->
          Targetint.OCaml.of_int index, lv)
        lvs)
  in
  let env_extension =
    List.fold_left2 (fun env_extension lv ty ->
        let name = Name.logical_var lv in
        Typing_env_extension.add_equation env_extension name ty)
      (Typing_env_extension.empty ())
      lvs tys
  in
  create indexes_to_lvs env_extension
