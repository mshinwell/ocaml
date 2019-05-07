module RP = struct
  include Relational_product.Make
    (Int_index) (Logical_variable_component)

  let bottom () = create_bottom ()
end

module Tag_and_targetint_ocaml =
  Hashtbl.Make_with_map_pair (Tag) (Targetint.OCaml)

module RL =
  Row_like.Make (Tag) (Targetint.OCaml) (Tag_and_targetint_ocaml) (RP)

type t = RL.t

type open_or_closed = Open | Closed of Tag.t

let create ~field_tys open_or_closed : t =
  (* CR mshinwell: This code is very similar to some in [Function_type]. *)
  let indexes_to_vars =
    Targetint.OCaml.Map.of_list (
      List.mapi (fun index _field_ty ->
          let index = Targetint.OCaml.of_int index in
          let logical_var =
            Logical_variable.create (Flambda_kind.value ())
          in
          index, logical_var)
        field_tys)
  in
  let env_extension, _index =
    List.fold_left (fun (env_extension, index) field_ty ->
        let logical_var = Targetint.OCaml.Map.find index indexes_to_vars in
        let env_extension =
          Typing_env_extension.add_equation env_extension
            (Name.logical_var logical_var) field_ty
        in
        let next_index = Targetint.OCaml.add index Targetint.OCaml.one in
        env_extension, next_index)
      (Typing_env_extension.empty (), Targetint.OCaml.zero)
      field_tys
  in
  let product = RP.create indexes_to_vars env_extension in
  let size = Targetint.OCaml.of_int (List.length field_tys) in
  match open_or_closed with
  | Open -> RL.create_at_least size product
  | Closed tag -> RL.create_exactly tag size product

let create_bottom = RL.create_bottom 

let _invariant _t = () (* CR mshinwell: RL.invariant *)
(* CR mshinwell: Here is an invariant: the number of variables in the name
   abstraction for the relational product (= number of indexes in the RP)
   must be equal to the number of fields in the block (for a known size)
   or the minimum number of fields in the block (for "at least size of") *)
let print_with_cache = RL.print

let equal = RL.equal
let is_empty = RL.is_bottom

let meet env t1 t2 : _ Or_bottom.t =
  match RL.meet env t1 t2 with
  | Bottom -> Bottom
  | Ok (t, product) ->
    Ok (t, RP.standalone_extension product)

let join = RL.join

let free_names = RL.free_names
let apply_name_permutation = RL.apply_name_permutation

let classify = RL.classify
