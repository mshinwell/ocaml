(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015 Institut National de Recherche en Informatique et     *)
(*   en Automatique.  All rights reserved.  This file is distributed      *)
(*   under the terms of the Q Public License version 1.0.                 *)
(*                                                                        *)
(**************************************************************************)

let rec lift_constants expr =
  let inconstants =
    Inconstant_idents.inconstants ~for_clambda:false
      ~compilation_unit:(Compilation_unit.get_current_exn ())
  in
  let var_is_constant var = not (Variable.Set.mem var inconstants.id) in
  let set_of_closures_id_is_constant set_of_closures_id =
    not (Variable.Set.mem set_of_closures_id inconstants.closure)
  in
  let constants = Symbol.Tbl.create 42 in
  let assign_to_symbol ~defining_expr =
    let symbol = Compilenv.new_const_symbol' () in
    assert (not (Symbol.Tbl.mem constants symbol));
    Symbol.Tbl.add constants symbol defining_expr;
    Fsymbol (symbol, ())
  in
  let maybe_assign_var_to_symbol ~var ~defining_expr =
    if var_is_constant var then assign_to_symbol ~defining_expr
    else defining_expr
  in
  let rec lift (expr : _ Flambda.t) =
    match expr with
    | Fconst (Fconst_float_array _)
    | Fconst (Fconst_string _)
    | Fconst (Fconst_immstring _) -> assign_to_symbol ~defining_expr:expr
    | Flet (kind, var, defining_expr, body, _) ->
      Flet (kind, var, maybe_assign_var_to_symbol ~var ~defining_expr,
        body, ())
    | Fletrec (bindings, body, _) ->
      let lift_binding (var, defining_expr) =
        maybe_assign_var_to_symbol ~var ~defining_expr
      in
      Fletrec (List.map lift_binding bindings, body, ())
    | Fset_of_closures (set_of_closures, _) ->
      (* CR mshinwell: need to erase specialised argument information.
         Anything else? *)
      if set_of_closures_id_is_constant
        set_of_closures.function_decls.set_of_closures_id
      then
        assign_to_symbol ~defining_expr:expr
      else
        expr
    | _ -> expr
  in
  lift expr, constants
