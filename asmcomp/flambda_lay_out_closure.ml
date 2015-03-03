open Abstract_identifiers

module Storer =
  Switch.Store (struct
    type t = Expr_id.t Flambda.t
    type key = Flambdautils.sharing_key
    let make_key = Flambdautils.make_key
  end)

(* [fun_offset_table] associates a function label to its offset inside a
   closure *)
let fun_offset_table = ref Closure_id.Map.empty
(* [fv_offset_table] associates a free variable to its offset inside a
   closure *)
let fv_offset_table = ref Var_within_closure.Map.empty

let rec iter (expr : _ Flambda.t) =
  match expr with
  | Fset_of_closures({cl_fun = funct; cl_free_var = fv}, _) ->
    iter_closure funct fv
  | _ -> ()

and iter_closure functs fv =
  let funct = Variable.Map.bindings functs.funs in
  let fv = Variable.Map.bindings fv in

  (* build the table mapping the function to the offset of its code
     pointer inside the closure value *)
  let aux_fun_offset (map,env_pos) (id, func) =
    let pos = env_pos + 1 in
    let arity = Flambdautils.function_arity func in
    let env_pos = env_pos + 1 +
                  (if arity <> 1 then 3 else 2) in
    let map = Closure_id.Map.add (Closure_id.wrap id) pos map in
    (map,env_pos)
  in
  let fun_offset, fv_pos =
    List.fold_left aux_fun_offset (!fun_offset_table, -1) funct in

  (* Adds the mapping of free variables to their offset. It is not
     used inside the body of the function: it is directly
     substituted here. But if the function is inlined, it is
     possible that the closure is accessed from outside its body. *)
  let aux_fv_offset (map,pos) (id, _) =
    let off = Var_within_closure.wrap id in
    assert(not (Var_within_closure.Map.mem off map));
    let map = Var_within_closure.Map.add off pos map in
    (map, pos + 1)
  in
  let fv_offset, _ = List.fold_left aux_fv_offset
      (!fv_offset_table, fv_pos) fv in

  fun_offset_table := fun_offset;
  fv_offset_table := fv_offset;

  List.iter (fun (_, { Flambda. body }) ->
      Flambdaiter.iter_toplevel iter body)
    funct

let assign_offsets ~expr ~constants =
  let run flam = Flambdaiter.iter_toplevel iter flam in
  run expr;
  Symbol.Map.iter (fun _ -> run) constants;
  let result = !fun_offset_table, !fv_offset_table in
  fun_offset_table := Closure_id.Map.empty;
  fv_offset_table := Var_within_closure.Map.empty;
  result

let reexported_offsets ~extern_fun_offset_table ~extern_fv_offset_table ~expr =
  let set_fun = ref Closure_id.Set.empty in
  let set_fv = ref Var_within_closure.Set.empty in
  let aux (expr : _ Flambda.t) =
    match expr with
    | Fvariable_in_closure({vc_var = env_var; vc_fun = env_fun_id}, _) ->
        set_fun := Closure_id.Set.add env_fun_id !set_fun;
        set_fv := Var_within_closure.Set.add env_var !set_fv;
    | Fclosure({fu_fun = id; fu_relative_to = rel}, _) ->
        let set = match rel with
          | None -> !set_fun
          | Some rel -> Closure_id.Set.add rel !set_fun in
        set_fun := Closure_id.Set.add id set;
    | e -> ()
  in
  Flambdaiter.iter aux expr;
  let f extern_map closure_id new_map =
    try
      Closure_id.Map.add closure_id
        (Closure_id.Map.find closure_id extern_map) new_map
    with Not_found -> new_map  (* it is a closure in the current unit *)
  in
  let f' extern_map var_within_closure new_map =
    try
      Var_within_closure.Map.add var_within_closure
        (Var_within_closure.Map.find var_within_closure extern_map) new_map
    with Not_found -> new_map  (* it accesses a closure in the current unit *)
  in
  let fun_map = Closure_id.Set.fold (f extern_fun_offset_table) !set_fun in
  let fv_map =
    Var_within_closure.Set.fold (f' extern_fv_offset_table) !set_fv
  in
  fun_map, fv_map
