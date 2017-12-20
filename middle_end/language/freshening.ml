(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type tbl = {
  sb_var : Variable.t Variable.Map.t;
  sb_mutable_var : Mutable_variable.t Mutable_variable.Map.t;
  sb_exn : Continuation.t Continuation.Map.t;
  sb_trap : Trap_id.t Trap_id.Map.t;
  (* Used to handle substitution sequences: we cannot call the substitution
     recursively because there can be name clashes. *)
  back_var : Variable.t list Variable.Map.t;
  back_mutable_var : Mutable_variable.t list Mutable_variable.Map.t;
}

type t =
  | Inactive
  | Active of tbl

type subst = t

let empty_tbl = {
  sb_var = Variable.Map.empty;
  sb_mutable_var = Mutable_variable.Map.empty;
  sb_exn = Continuation.Map.empty;
  sb_trap = Trap_id.Map.empty;
  back_var = Variable.Map.empty;
  back_mutable_var = Mutable_variable.Map.empty;
}

let print ppf = function
  | Inactive -> Format.fprintf ppf "Inactive"
  | Active tbl ->
    Format.fprintf ppf "Active:@ ";
    Variable.Map.iter (fun var1 var2 ->
        Format.fprintf ppf "%a -> %a@ "
          Variable.print var1
          Variable.print var2)
      tbl.sb_var;
    Mutable_variable.Map.iter (fun mut_var1 mut_var2 ->
        Format.fprintf ppf "(mutable) %a -> %a@ "
          Mutable_variable.print mut_var1
          Mutable_variable.print mut_var2)
      tbl.sb_mutable_var;
    Variable.Map.iter (fun var vars ->
        Format.fprintf ppf "%a -> %a@ "
          Variable.print var
          Variable.Set.print (Variable.Set.of_list vars))
      tbl.back_var;
    Mutable_variable.Map.iter (fun mut_var mut_vars ->
        Format.fprintf ppf "(mutable) %a -> %a@ "
          Mutable_variable.print mut_var
          Mutable_variable.Set.print (Mutable_variable.Set.of_list mut_vars))
      tbl.back_mutable_var;
    Continuation.Map.iter (fun cont1 cont2 ->
        Format.fprintf ppf "(cont) %a -> %a@ "
          Continuation.print cont1
          Continuation.print cont2)
      tbl.sb_exn

let empty = Inactive

let empty_preserving_activation_state = function
  | Inactive -> Inactive
  | Active _ -> Active empty_tbl

let activate = function
  | Inactive -> Active empty_tbl
  | Active _ as t -> t

let rec add_sb_var sb id id' =
  let sb = { sb with sb_var = Variable.Map.add id id' sb.sb_var } in
  let sb =
    try let pre_vars = Variable.Map.find id sb.back_var in
      List.fold_left (fun sb pre_id -> add_sb_var sb pre_id id') sb pre_vars
    with Not_found -> sb in
  let back_var =
    let l = try Variable.Map.find id' sb.back_var with Not_found -> [] in
    Variable.Map.add id' (id :: l) sb.back_var in
  { sb with back_var }

let rec add_sb_mutable_var sb id id' =
  let sb =
    { sb with
      sb_mutable_var = Mutable_variable.Map.add id id' sb.sb_mutable_var;
    }
  in
  let sb =
    try
      let pre_vars = Mutable_variable.Map.find id sb.back_mutable_var in
      List.fold_left (fun sb pre_id -> add_sb_mutable_var sb pre_id id')
        sb pre_vars
    with Not_found -> sb in
  let back_mutable_var =
    let l =
      try Mutable_variable.Map.find id' sb.back_mutable_var
      with Not_found -> []
    in
    Mutable_variable.Map.add id' (id :: l) sb.back_mutable_var
  in
  { sb with back_mutable_var }

let apply_continuation t i =
  match t with
  | Inactive ->
    i
  | Active t ->
    try Continuation.Map.find i t.sb_exn
    with Not_found -> i

let add_continuation t i =
  match t with
  | Inactive -> i, t
  | Active t ->
    let i' = Continuation.create () in
(*
Format.eprintf "Freshening %a -> %a.  Is %a in the map? %s\nBacktrace:\n%s\n%!"
  Continuation.print i
  Continuation.print i'
  Continuation.print i
  (if Continuation.Map.mem i t.sb_exn then "yes" else "no")
  (Printexc.raw_backtrace_to_string (Printexc.get_callstack 10));
*)
    let sb_exn =
      Continuation.Map.add i i' t.sb_exn
    in
    i', Active { t with sb_exn; }

let apply_trap t trap =
  match t with
  | Inactive ->
    trap
  | Active t ->
    try Trap_id.Map.find trap t.sb_trap
    with Not_found -> trap

let add_trap t trap =
  match t with
  | Inactive -> trap, t
  | Active t ->
    let trap' = Trap_id.create () in
    let sb_trap =
      Trap_id.Map.add trap trap' t.sb_trap
    in
    trap', Active { t with sb_trap; }

let active_add_variable t id =
  let id' = Variable.rename id in
  let t = add_sb_var t id id' in
  id', t

let add_variable t id =
  match t with
  | Inactive -> id, t
  | Active t ->
     let id', t = active_add_variable t id in
     id', Active t

let add_variables t defs =
  List.fold_right (fun (id, data) (defs, t) ->
      let id', t = add_variable t id in
      (id', data) :: defs, t) defs ([], t)

let add_variables' t ids =
  List.fold_right (fun id (ids, t) ->
      let id', t = add_variable t id in
      id' :: ids, t) ids ([], t)

let active_add_mutable_variable t id =
  let id' = Mutable_variable.freshen id in
  let t = add_sb_mutable_var t id id' in
  id', t

let add_mutable_variable t id =
  match t with
  | Inactive -> id, t
  | Active t ->
     let id', t = active_add_mutable_variable t id in
     id', Active t

let apply_variable t var =
  match t with
  | Inactive -> var
  | Active t ->
   try Variable.Map.find var t.sb_var with
   | Not_found -> var

let apply_mutable_variable t mut_var =
  match t with
  | Inactive -> mut_var
  | Active t ->
   try Mutable_variable.Map.find mut_var t.sb_mutable_var with
   | Not_found -> mut_var

let rewrite_recursive_calls_with_symbols _t _ ~make_closure_symbol:_ =
  assert false

(* XXX pchambart to fix
      (function_declarations : Flambda.Function_declarations.t)
      ~make_closure_symbol =
  match t with
  | Inactive -> function_declarations
  | Active _ ->
    let all_free_symbols =
      Flambda.Function_declarations.all_free_symbols function_declarations
    in
    let closure_symbols_used = ref false in
    let closure_symbols =
      Variable.Map.fold (fun var _ map ->
        let closure_id = Closure_id.wrap var in
        let sym = make_closure_symbol closure_id in
        if Symbol.Set.mem sym all_free_symbols then begin
          closure_symbols_used := true;
          Symbol.Map.add sym var map
        end else begin
          map
        end)
      function_declarations.funs Symbol.Map.empty
    in
    if not !closure_symbols_used then begin
      (* Don't waste time rewriting the function declaration(s) if there
         are no occurrences of any of the closure symbols. *)
      function_declarations
    end else begin
      let funs =
        Variable.Map.map (fun (func_decl : Flambda.Function_declaration.t) ->
          let body =
            Flambda.Expr.Mappers.Toplevel_only.map_named
              (* CR-someday pchambart: This may be worth deep substituting
                 below the closures, but that means that we need to take care
                 of functions' free variables. *)
              (function
                | Symbol sym when Symbol.Map.mem sym closure_symbols ->
                  Var (Symbol.Map.find sym closure_symbols)
                | e -> e)
              func_decl.body
          in
          Flambda.Function_declaration.update_body func_decl ~body)
        function_declarations.funs
      in
      Flambda.Function_declarations.update function_declarations ~funs
    end
*)

let does_not_freshen t vars =
  match t with
  | Inactive -> true
  | Active subst ->
    not (List.exists (fun var -> Variable.Map.mem var subst.sb_var) vars)

(*
let freshen_projection (projection : Projection.t) ~freshening : Projection.t =
  match projection with
  | Project_var project_var ->
    Project_var {
      var = project_var.var;
      closure = apply_variable freshening project_var.closure;
    }
  | Project_closure { set_of_closures; closure_id; } ->
    Project_closure {
      set_of_closures = apply_variable freshening set_of_closures;
      closure_id;
    }
  | Move_within_set_of_closures { closure; move } ->
    Move_within_set_of_closures {
      closure = apply_variable freshening closure;
      move;
    }
  | Pure_primitive (prim, args) ->
    Pure_primitive (prim,
      List.map (fun arg -> apply_variable freshening arg) args)
  | Field (field, block) -> Field (field, apply_variable freshening block)
  | Switch var ->
    Switch (apply_variable freshening var)

let freshen_free_vars_projection_relation relation ~freshening =
  Variable.Map.map (fun (spec_to : Flambda.Free_var.t) ->
      let projection =
        match spec_to.projection with
        | None -> None
        | Some projection ->
          Some (freshen_projection projection ~freshening)
      in
      { spec_to with projection; })
    relation

let freshen_free_vars_projection_relation' relation ~freshening =
  Variable.Map.map (fun ((spec_to : Flambda.Free_var.t), data) ->
      let projection =
        match spec_to.projection with
        | None -> None
        | Some projection ->
          Some (freshen_projection projection ~freshening)
      in
      { spec_to with projection; }, data)
    relation
*)

let range_of_continuation_freshening t =
  match t with
  | Inactive -> Continuation.Set.empty
  | Active tbl ->
    Continuation.Set.of_list (Continuation.Map.data tbl.sb_exn)
