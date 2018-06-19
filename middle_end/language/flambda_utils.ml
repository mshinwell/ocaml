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

let create_wrapper_params ~params:_ ~freshening_already_assigned:_ = (* XXX *)
  assert false
(*
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
*)

let make_let_cont_alias ~name ~alias_of ~params : Flambda.Let_cont_handlers.t =
  let args =
    List.mapi (fun index _kinded_param ->
        let name = Format.sprintf "let_cont_alias_%d" index in
        Simple.var (Variable.create name))
      (Parameters.kinded_params params)
  in
  Non_recursive {
    name;
    handler = {
      params = Parameters.freshen parameters;
      stub = true;
      is_exn_handler = false;
      handler = Apply_cont (alias_of, None, args);
    };
  }
