module Var_within_closure = struct
  include Var_within_closure

  let free_names _t = Name_occurrences.create ()

  let apply_name_permutation t _perm = t

  (* CR mshinwell: Add [Var_within_closure.rename] *)
  let rename t =
    Var_within_closure.wrap (Variable.rename (Var_within_closure.unwrap t))
end

module RP =
  Relational_product.Make (Var_within_closure)
    (Logical_variable_component)

type t = RP.t

let create closure_elements_to_tys =
  let closure_elements_to_logical_variables =
    Var_within_closure.Map.map (fun _ty ->
        Logical_variable.create (Flambda_kind.value ()))
      closure_elements_to_tys
  in
  let env_extension =
    Var_within_closure.Map.fold (fun var ty env_extension ->
        let logical_var =
          Var_within_closure.Map.find var
            closure_elements_to_logical_variables
        in
        Typing_env_extension.add_equation env_extension
          (Name.logical_var logical_var) ty)
      closure_elements_to_tys
      (Typing_env_extension.empty ())
  in
  RP.create closure_elements_to_logical_variables env_extension

let create_bottom = RP.create_bottom

let print ~cache:_ ppf t = RP.print ppf t

let meet = RP.meet
let join = RP.join

let equal = RP.equal
let free_names = RP.free_names
let apply_name_permutation = RP.apply_name_permutation
