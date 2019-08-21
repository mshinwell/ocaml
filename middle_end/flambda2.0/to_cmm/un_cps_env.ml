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

(* Delayed let-bindings. Let bindings are delayed in a stack in order
   to allow for potentiel reordering of variables that are bound and
   use exactly once, (without changing semantics), in order to optimize
   the generated cmm code. There are tow main optimizations that are
   targeted : arithmetic optimization of nested expressions (mainly
   tagging/untagging), and potential optimizations performed later on
   funciton applications which work better when arguments are not
   let-bound.
   Non-linear let bindings are also delayed to allow linear let-bound vars
   to be permuted with non-linear let-bound vars.
*)

type binding = {
  var : Variable.t;
  effs : Effects_and_coeffects.t;
  inline : bool;
  cmm_var : Backend_var.With_provenance.t;
  cmm_expr : Cmm.expression;
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
  bindings : binding list;
  (* A list of let-bindings (with information about effects and coeffects),
     in reverse order of encounter (i.e. the first element of the list is
     the most recently seen). *)
}


let mk offsets k k_exn = {
  k; k_exn; offsets;
  bindings = [];
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

let new_jump_id =
  let i = ref 0 in
  (fun () -> incr i; !i)

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

let bind_variable env var effs inline cmm_expr =
  if inline && Effects_and_coeffects.is_pure effs then begin
    { env with vars = Variable.Map.add var cmm_expr env.vars }
  end else begin
    let cmm_var = gen_variable var in
    let binding = { var; effs; inline; cmm_var; cmm_expr; } in
    { env with bindings = binding :: env.bindings }
  end

let inline_variable env v =
  let skip_inlining b =
    let v = Backend_var.With_provenance.var b.cmm_var in
    Un_cps_helper.var v, env
  in
  let rec commute b new_stack = function
    | [] ->
        b.cmm_expr, { env with bindings = new_stack }
    | b' :: r ->
        if Effects_and_coeffects.commute b.effs b'.effs then
          commute b (b' :: new_stack) r
        else
          skip_inlining b
  in
  let rec find v acc = function
    | [] ->
        Misc.fatal_errorf "Variable %a not found in env" Variable.print v
    | b :: r ->
        if Variable.compare v b.var = 0 then
          if b.inline then begin
            commute b r acc
          end else begin
            skip_inlining b
          end
        else
          find v (b :: acc) r
  in
  match Variable.Map.find v env.vars with
  | e ->
      e, env
  | exception Not_found ->
      find v [] env.bindings

let flush_delayed_lets env =
  (* Add the generated variable to the vars bindings, so that the
     variables bound by the flushed let-bindings can be used. *)
  let env_aux acc b = add_variable acc b.var b.cmm_var in
  let env = List.fold_left env_aux env env.bindings in
  (* generate a wrapper function to introduce the delayed let-bindings. *)
  let wrap_aux acc b = Un_cps_helper.letin b.cmm_var b.cmm_expr acc in
  let wrap e = List.fold_left wrap_aux e env.bindings in
  wrap, { env with bindings = [] }

