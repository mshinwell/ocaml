(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
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

let create_wrapper_params ~params ~freshening_already_assigned =
  let module Typed_parameter = Flambda.Typed_parameter in
  let renaming =
    List.map (fun typed_param ->
        let param = Typed_parameter.param typed_param in
        match Parameter.Map.find param freshening_already_assigned with
        | exception Not_found ->
          param, Typed_parameter.rename typed_param
        | renamed_param -> param, renamed_param)
      params
  in
  let renaming_map = Parameter.Map.of_list renaming in
  let freshen_typed_param typed_param =
    let param = Typed_parameter.param typed_param in
    match Parameter.Map.find param renaming_map with
    | exception Not_found -> assert false
    | param -> param
  in
  let wrapper_params = List.map freshen_typed_param params in
  renaming_map, wrapper_params

let make_let_cont_alias ~name ~alias_of
      ~parameter_types : Flambda.Let_cont_handlers.t =
  let handler_params, apply_params =
    let param_and_var_for ty =
      let ty = Flambda_type.unknown_like ty in
      let var = Variable.create "let_cont_alias" in
      let param = Parameter.wrap var in
      let typed_param = Flambda.Typed_parameter.create param ty in
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
