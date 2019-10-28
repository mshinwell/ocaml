(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Continuation use. A continuation can be translated one of two ways:
   - by a static jump (Cmm jump, using a unique integer)
   - by inlining the continuation's body at the call site. *)

type cont =
  | Jump of Cmm.machtype list * int
  | Inline of Kinded_parameter.t list * Flambda.Expr.t

(* Delayed let-bindings. Let bindings are delayed in stages in order
   to allow for potential reordering and inlining of variables that are bound
   and used exactly once, (without changing semantics), in order to optimize
   the generated cmm code. There are two main optimizations that are
   targeted : arithmetic optimization of nested expressions (mainly
   tagging/untagging), and potential optimizations performed later on
   function applications which work better when arguments are not
   let-bound. Non-linear let bindings are also delayed to allow linear
   let-bound vars to be permuted with non-linear let-bound vars.

   Let-bound variables can be one of three kinds: pure, coeffect and effect
   (effectful variables can also have coeffects).
   Pure let-bound variables are simply added to the environment usual map.
   Effectful and coeffectful variables are organised into stages:
   a stage is either:
   - a serie of consecutive bindings with only coeffects
   - a single effectful binding
   Whenever a new binding that doesn't match the current stage is added,
   the current stage is archived, and replaced by a new stage.
   Only bindings in the current stage are candidates to inlining.
   When inlined, a binding is removed from its stage (as only linear
   bindings are supposed to be inlined), and if the current stage becomes
   empty, the last archived stage is "un-archived".
*)

type kind =
  | Pure
  | Coeff
  | Effect

type binding = {
  order : int;
  inline : bool;
  effs : Effects_and_coeffects.t;
  cmm_var : Backend_var.With_provenance.t;
  cmm_expr : Cmm.expression;
}

type stage = {
  kind : kind;
  order : int;
  map : binding Variable.Map.t;
}

(* Translation environment *)

type t = {
  k     : Continuation.t;
  (* The continuation of the current context
       (used to determine which calls are tail-calls) *)
  k_exn : Continuation.t;
  (* The exception continuation of the current context
     (used to determine where to insert try-with blocks) *)
  vars  : Cmm.expression Variable.Map.t;
  (* Map from flambda2 variables to backend_variables *)
  conts : cont Continuation.Map.t;
  (* Map from continuations to handlers (i.e variables bound by the
     continuation and expression of the continuation handler). *)
  offsets : Un_cps_closure.env;
  (* Offsets for closure_ids and var_within_closures. *)

  archived : stage list;
  (* archived stages, in reverse chronological order. *)
  current : stage;
  (* current stage of bound variables *)
}

let empty_stage =
  { kind = Pure; order = 0; map = Variable.Map.empty; }

let mk offsets k k_exn = {
  k; k_exn; offsets;
  archived = [];
  current = empty_stage;
  vars = Variable.Map.empty;
  conts = Continuation.Map.empty;
}

let dummy offsets =
  mk
    offsets
    (Continuation.create ())
    (Continuation.create ())

let return_cont env = env.k
let exn_cont env = env.k_exn

(* Variables *)

let gen_variable v =
  let name = Variable.unique_name v in
  let v = Backend_var.create_local name in
  let v = Backend_var.With_provenance.create v in
  v

let add_variable env v v' =
  let v'' = Backend_var.With_provenance.var v' in
  let vars = Variable.Map.add v (Un_cps_helper.var v'') env.vars in
  { env with vars }

let create_variable env v =
  assert (not (Variable.Map.mem v env.vars));
  let v' = gen_variable v in
  let env = add_variable env v v' in
  env, v'

let create_variables env l =
  let env, l' =
    List.fold_left (fun (env, l) v ->
        let env', v' = create_variable env v in
        env', v' :: l) (env, []) l
  in
  env, List.rev l'

let get_variable env v =
  try Variable.Map.find v env.vars
  with Not_found ->
    Misc.fatal_errorf "Variable %a not found in env" Variable.print v


(* Continuations *)

let get_jump_id env k =
  match Continuation.Map.find k env.conts with
  | Jump (_, id) -> id
  | Inline _ ->
      Misc.fatal_errorf "Expected continuation %a to be bound to a jump"
        Continuation.print k
  | exception Not_found ->
      Misc.fatal_errorf "Continuation %a not found in env"
        Continuation.print k

let get_k env k =
  match Continuation.Map.find k env.conts with
  | exception Not_found ->
      Misc.fatal_errorf
        "Could not find continuation %a in env during un_cps"
        Continuation.print k
  | res -> res

let new_jump_id = Lambda.next_raise_count

let add_jump_cont env tys k =
  let id = new_jump_id () in
  let conts = Continuation.Map.add k (Jump (tys, id)) env.conts in
  id, { env with conts }

let add_inline_cont env k vars e =
  let conts = Continuation.Map.add k (Inline (vars, e)) env.conts in
  { env with conts }

(* Offsets *)

let closure_offset env closure =
  Un_cps_closure.closure_offset env.offsets closure

let env_var_offset env env_var =
  Un_cps_closure.env_var_offset env.offsets env_var

let layout env closures env_vars =
  Un_cps_closure.layout env.offsets closures env_vars


(* Printing

let print_binding fmt b =
  Format.fprintf fmt "@[<hv>[%a : %a ->@ %a@ (%a)@,]@]"
    Variable.print b.var
    Backend_var.With_provenance.print b.cmm_var
    Printcmm.expression b.cmm_expr
    Effects_and_coeffects.print b.effs

let print_binding_list fmt l =
  Format.fprintf fmt "@[<v>";
  List.iter (fun b ->
      Format.fprintf fmt "%a@," print_binding b
    ) l;
  Format.fprintf fmt "@]"
*)

(* Inlining of let-bindings *)

let classify effs =
  if Effects_and_coeffects.has_commuting_effects effs then
    Effect
  else if Effects_and_coeffects.has_commuting_coeffects effs then
    Coeff
  else
    Pure

let add_binding_aux env var effs inline cmm_expr =
  let map = env.current.map in
  let order = env.current.order in
  let cmm_var = gen_variable var in
  let binding = { order; inline; effs; cmm_var; cmm_expr; } in
  let map = Variable.Map.add var binding map in
  let v = Backend_var.With_provenance.var cmm_var in
  let e = Un_cps_helper.var v in
  let env = { env with vars = Variable.Map.add var e env.vars; } in
  env, order + 1, map

let add_binding kind env var effs inline cmm_expr =
  let env, order, map = add_binding_aux env var effs inline cmm_expr in
  { env with current = { kind; order; map; }; }

let archive_then_bind kind env var effs inline cmm_expr =
  let env = {
    env with
    current = empty_stage;
    archived = env.current :: env.archived;
  } in
  add_binding kind env var effs inline cmm_expr

let bind_variable env var effs inline cmm_expr =
  (* shorthand to respecetively:
     - bind the variable in the current stage, and change the stage kind
     - archive the current stage, then bind the variable in a new stage
       with the given kind. *)
  let bind kind = add_binding kind env var effs inline cmm_expr in
  let archive kind = archive_then_bind kind env var effs inline cmm_expr in

  match classify effs, env.current.kind with
  | Pure, _ when inline ->
      { env with vars = Variable.Map.add var cmm_expr env.vars }
  (* Pure bindings can be mixed with anything (since they commute with everything) *)
  | Pure, (Pure as kind)
  | Pure, (Coeff | Effect as kind)
  | (Coeff | Effect as kind), Pure -> bind kind
  (* Coeffects commute with themselves *)
  | Coeff, Coeff -> bind Coeff
  (* Effects do not commute with anything, themselves included ! *)
  | Effect, Coeff
  | Coeff, Effect
  | Effect, Effect -> archive Effect



let pop_stage env =
  match env.archived with
  | [] ->
      { env with current = empty_stage; }
  | s :: r ->
      { env with current = s; archived = r; }

let remove_binding env v =
  let map = Variable.Map.remove v env.current.map in
  if Variable.Map.is_empty map then
    pop_stage env
  else
    { env with current = { env.current with map } }

let inline_variable env v =
  match Variable.Map.find v env.current.map with
  | exception Not_found ->
      begin match Variable.Map.find v env.vars with
      | exception Not_found ->
          Misc.fatal_errorf "Variable %a not found in env" Variable.print v
      | e -> e, env, Effects_and_coeffects.pure
      end
  | b ->
      if b.inline then begin
        let env = remove_binding env v in
        b.cmm_expr, env, b.effs
      end else begin
        let v = Backend_var.With_provenance.var b.cmm_var in
        Un_cps_helper.var v, env, Effects_and_coeffects.pure
      end


(* Map on integers in descending order *)
module M = Map.Make(struct
    type t = int
    let compare x y = compare y x
  end)

let flush_delayed_lets env =
  (* generate a wrapper function to introduce the delayed let-bindings. *)
  let wrap_aux acc stage =
    let order_map = Variable.Map.fold (fun _ (b: binding) acc ->
        M.add b.order b acc
      ) stage.map M.empty in
    M.fold (fun _ b acc ->
        Un_cps_helper.letin b.cmm_var b.cmm_expr acc
      ) order_map acc
  in
  let wrap e = List.fold_left wrap_aux e (env.current :: env.archived) in
  (* Return a wrapper and a cleared env *)
  wrap, { env with current = empty_stage; archived = []; }

