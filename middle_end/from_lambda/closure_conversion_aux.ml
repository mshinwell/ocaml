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

module Env = struct
  type t = {
    variables : Variable.t Ident.Map.t;
    mutable_variables : Mutable_variable.t Ident.Map.t;
    globals : Symbol.t Numbers.Int.Map.t;
    at_toplevel : bool;
    administrative_redexes : (Ident.t list * Ilambda.t) Continuation.Map.t;
  }

  let empty = {
    variables = Ident.Map.empty;
    mutable_variables = Ident.Map.empty;
    globals = Numbers.Int.Map.empty;
    at_toplevel = true;
    administrative_redexes = Continuation.Map.empty;
  }

  let clear_local_bindings env =
    { empty with globals = env.globals }

  let add_var t id var = { t with variables = Ident.Map.add id var t.variables }
  let add_vars t ids vars = List.fold_left2 add_var t ids vars
  let add_var_map t map =
    { t with variables = Ident.Map.union_right t.variables map }

  let add_var_like t id =
    let var = Variable.create_with_same_name_as_ident id in
    add_var t id var, var

  let add_vars_like t ids =
    let vars = List.map Variable.create_with_same_name_as_ident ids in
    add_vars t ids vars, vars

  let find_var t id =
    try Ident.Map.find id t.variables
    with Not_found ->
      Misc.fatal_errorf "Closure_conversion.Env.find_var: %s@ %s"
        (Ident.unique_name id)
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack 42))

  let find_var_exn t id =
    Ident.Map.find id t.variables

  let find_name t id = Name.var (find_var t id)
  let find_name_exn t id = Name.var (find_var_exn t id)

  let find_simple t id = Simple.var (find_var t id)
  let find_simple_exn t id = Simple.var (find_var_exn t id)

  let find_vars t ids =
    List.map (fun id -> find_var t id) ids

  let find_simples t ids =
    List.map (fun id -> Simple.var (find_var t id)) ids

  let add_mutable_var t id mutable_var =
    { t with
      mutable_variables = Ident.Map.add id mutable_var t.mutable_variables;
    }

  let find_mutable_var t id =
    try Ident.Map.find id t.mutable_variables
    with Not_found ->
      Misc.fatal_errorf "Closure_conversion.Env.find_mutable_var: %s@ %s"
        (Ident.unique_name id)
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack 42))

  let add_global t pos symbol =
    { t with globals = Numbers.Int.Map.add pos symbol t.globals }

  let find_global t pos =
    try Numbers.Int.Map.find pos t.globals
    with Not_found ->
      Misc.fatal_error ("Closure_conversion.Env.find_global: global "
        ^ string_of_int pos)

  let at_toplevel t = t.at_toplevel

  let not_at_toplevel t = { t with at_toplevel = false; }

  let add_administrative_redex t continuation ~params ~handler =
    assert (not (Continuation.Map.mem continuation t.administrative_redexes));
    { t with
      administrative_redexes =
        Continuation.Map.add continuation (params, handler)
          t.administrative_redexes;
    }

  let find_administrative_redex t continuation =
    match Continuation.Map.find continuation t.administrative_redexes with
    | exception Not_found -> None
    | handler -> Some handler
end

module Function_decls = struct
  module Function_decl = struct
    type t = {
      let_rec_ident : Ident.t;
      closure_bound_var : Closure_id.t;
      kind : Lambda.function_kind;
      params : (Ident.t * Lambda.value_kind) list;
      continuation_param : Continuation.t;
      exn_continuation_param : Continuation.t;
      body : Ilambda.t;
      free_idents_of_body : Ident.Set.t;
      attr : Lambda.function_attribute;
      loc : Location.t;
      stub : bool;
    }

    let create ~let_rec_ident ~closure_bound_var ~kind ~params
        ~continuation_param ~exn_continuation_param ~body ~attr
        ~loc ~free_idents_of_body ~stub =
      let let_rec_ident =
        match let_rec_ident with
        | None -> Ident.create "unnamed_function"
        | Some let_rec_ident -> let_rec_ident
      in
      { let_rec_ident;
        closure_bound_var;
        kind;
        params;
        continuation_param;
        exn_continuation_param;
        body;
        free_idents_of_body;
        attr;
        loc;
        stub;
      }

    let let_rec_ident t = t.let_rec_ident
    let closure_bound_var t = t.closure_bound_var
    let kind t = t.kind
    let params t = t.params
    let continuation_param t = t.continuation_param
    let exn_continuation_param t = t.exn_continuation_param
    let body t = t.body
    let free_idents t = t.free_idents_of_body
    let inline t = t.attr.inline
    let specialise t = t.attr.specialise
    let is_a_functor t = t.attr.is_a_functor
    let stub t = t.attr.stub
    let loc t = t.loc
  end

  type t = {
    function_decls : Function_decl.t list;
    all_free_idents : Ident.Set.t;
  }

  (* All identifiers free in the bodies of the given function declarations,
     indexed by the identifiers corresponding to the functions themselves. *)
  let free_idents_by_function function_decls =
    List.fold_right (fun decl map ->
        Closure_id.Map.add (Function_decl.closure_bound_var decl)
          (Function_decl.free_idents decl) map)
      function_decls Closure_id.Map.empty

  let all_free_idents function_decls =
    Closure_id.Map.fold (fun _ -> Ident.Set.union)
      (free_idents_by_function function_decls) Ident.Set.empty

  (* All identifiers of simultaneously-defined functions in [ts]. *)
  let let_rec_idents function_decls =
    List.map Function_decl.let_rec_ident function_decls

  (* All parameters of functions in [ts]. *)
  let all_params function_decls =
    List.concat (List.map Function_decl.params function_decls)

  let set_diff (from : Ident.Set.t) (idents : Ident.t list) =
    List.fold_right Ident.Set.remove idents from

  (* CR-someday lwhite: use a different name from above or explain the
     difference *)
  let all_free_idents function_decls =
    set_diff (set_diff (all_free_idents function_decls)
        (List.map fst (all_params function_decls)))
      (let_rec_idents function_decls)

  let create function_decls =
    { function_decls;
      all_free_idents = all_free_idents function_decls;
    }

  let to_list t = t.function_decls

  let all_free_idents t = t.all_free_idents

  (* let closure_env_without_parameters external_env t = *)
  (*   let closure_env = *)
  (*     (\* For "let rec"-bound functions. *\) *)
  (*     List.fold_right (fun function_decl env -> *)
  (*         Env.add_var env (Function_decl.let_rec_ident function_decl) *)
  (*           (Function_decl.closure_bound_var function_decl)) *)
  (*       t.function_decls (Env.clear_local_bindings external_env) *)
  (*   in *)
  (*   (\* For free variables. *\) *)
  (*   Ident.Set.fold (fun id env -> *)
  (*       Env.add_var env id (Variable.create (Ident.name id))) *)
  (*     t.all_free_idents closure_env *)
end
