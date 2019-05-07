
(* CR mshinwell: Share with closures_entry_by_closure_id.ml *)
module Closure_id = struct
  include Closure_id

  let free_names _t = Name_occurrences.create ()
  let apply_name_permutation t _perm = t
  let rename t = t
end

include
  Relational_product.Make (Closure_id) (Logical_variable_component)

(* CR mshinwell: Any line of the following form should be removed *)
let print = print_with_cache

let create closure_ids_to_tys =
  let closure_ids_to_logical_variables =
    Closure_id.Map.map (fun _ty ->
        Logical_variable.create (Flambda_kind.value ()))
      closure_ids_to_tys
  in
  let env_extension =
    Closure_id.Map.fold (fun closure_id ty env_extension ->
        let logical_var =
          Closure_id.Map.find closure_id closure_ids_to_logical_variables
        in
        Typing_env_extension.add_equation env_extension
          (Name.logical_var logical_var) ty)
      closure_ids_to_tys
      (Typing_env_extension.empty ())
  in
  create closure_ids_to_logical_variables env_extension
