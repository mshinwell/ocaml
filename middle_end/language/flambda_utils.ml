(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let make_closure_map' input =
  let map = ref Closure_id.Map.empty in
  let add_set_of_closures _ (function_decls : Flambda.Function_declarations.t) =
    Closure_id.Map.iter (fun closure_id _ ->
        map := Closure_id.Map.add closure_id function_decls !map)
      function_decls.funs
  in
  Set_of_closures_id.Map.iter add_set_of_closures input;
  !map

let make_variable_symbol var =
  Symbol.create (Compilation_unit.get_current_exn ())
    (Linkage_name.create
       (Variable.unique_name (Variable.rename var)))

let make_variables_symbol vars =
  let name =
    String.concat "_and_"
      (List.map (fun var -> Variable.unique_name (Variable.rename var)) vars)
  in
  Symbol.create (Compilation_unit.get_current_exn ()) (Linkage_name.create name)

(*
type sharing_key = Continuation.t
let make_key cont = Some cont
let compare_key = Continuation.compare

module Switch_storer =
  Switch.Store
    (struct
      (* CR mshinwell: Check if this thing uses polymorphic comparison.
         Should be ok if so, at the moment, but should be fixed.
         vlaviron: the addition of a compare function to the signature should
         fix the problem. *)
      type t = Continuation.t
      type key = sharing_key
      let make_key = make_key
      let compare_key = compare_key
    end)
*)
(*
type specialised_to_same_as =
  | Not_specialised
  | Specialised_and_aliased_to of Variable.Set.t

let parameters_specialised_to_the_same_variable
      ~(function_decls : Flambda.Function_declarations.t)
      ~(specialised_args : Flambda.specialised_to Variable.Map.t) =
  let specialised_arg_aliasing =
    (* For each external variable involved in a specialisation, which
       internal variable(s) it maps to via that specialisation. *)
    Variable.Map.transpose_keys_and_data_set
      (Variable.Map.filter_map specialised_args
        ~f:(fun _param ({ var; _ } : Flambda.specialised_to) -> var))
  in
  Variable.Map.map (fun ({ params; _ } : Flambda.Function_declaration.t) ->
      List.map (fun param ->
          match Variable.Map.find (Parameter.var param) specialised_args with
          | exception Not_found -> Not_specialised
          | { var; _ } ->
            match var with
            | None -> Not_specialised
            | Some var ->
              Specialised_and_aliased_to
                (Variable.Map.find var specialised_arg_aliasing))
        params)
    function_decls.funs
*)

(*
let create_wrapper_params ~params ~specialised_args
      ~freshening_already_assigned =
  let renaming =
    List.map (fun param ->
        match Parameter.Map.find param freshening_already_assigned with
        | exception Not_found -> param, Parameter.rename param
        | renamed_param -> param, renamed_param)
      params
  in
  let renaming_map = Parameter.Map.of_list renaming in
  let freshen_param param =
    match Parameter.Map.find param renaming_map with
    | exception Not_found -> assert false
    | param -> param
  in
  let wrapper_params = List.map freshen_param params in
  (* CR mshinwell: We need to sort out this nonsense.  Should specialised
     args be Parameter.Map.t, for example? *)
  let renaming_map' =
    Variable.Map.of_list (
      List.map (fun (param, renamed_param) ->
          Parameter.var param, Parameter.var renamed_param)
        renaming)
  in
  let freshen_param param =
    match Variable.Map.find param renaming_map' with
    | exception Not_found -> assert false
    | param -> param
  in
  let params = Parameter.Set.vars params in
  let wrapper_specialised_args =
    Variable.Map.fold (fun param (spec_to : Flambda.specialised_to)
            wrapper_specialised_args ->
        if not (Variable.Set.mem param params) then begin
          (* A specialised argument of one of the other functions in the set
             of closures / continuations. *)
          wrapper_specialised_args
        end else begin
          let param = freshen_param param in
          let projection =
            match spec_to.projection with
            | None -> None
            | Some projection ->
              Some (Projection.map_projecting_from projection
                ~f:(fun param -> freshen_param param))
          in
          let spec_to : Flambda.specialised_to =
            { var = spec_to.var;
              projection;
            }
          in
          Variable.Map.add param spec_to wrapper_specialised_args
        end)
      specialised_args
      Variable.Map.empty
  in
  renaming_map, wrapper_params, wrapper_specialised_args
*)

let make_let_cont_alias ~importer ~type_of_name ~name ~alias_of
      ~parameter_types : Flambda.Let_cont_handlers.t =
  let handler_params, apply_params =
    let param_and_var_for ty =
      let ty = Flambda_type.unknown_like ~importer ~type_of_name ty in
      let var = Variable.create "let_cont_alias" in
      let param = Parameter.wrap var in
      let typed_param =
        Flambda.Typed_parameter.create ~importer ~type_of_name param ty
      in
      typed_param, Simple.var var
    in
    List.split (List.map param_and_var_for parameter_types)
  in
  Non_recursive {
    name;
    handler = {
      params = handler_params;
      stub = true;
      is_exn_handler = false;
      handler = Apply_cont (alias_of, None, apply_params);
    };
  }
