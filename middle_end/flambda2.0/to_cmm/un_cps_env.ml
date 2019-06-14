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
  | Jump of int
  | Inline of Backend_var.With_provenance.t list * Cmm.expression


(* Translation environment *)

type t = {
  k     : Continuation.t;
  (* The continuation of the current context
       (used to determine which calls are tail-calls) *)
  k_exn : Continuation.t;
  (* The exception continuation of the current context
     (used to determine where to insert try-with blocks) *)
  vars  : Backend_var.t Variable.Map.t;
  (* Map from flambda2 variables to backend_variables *)
  conts : cont Continuation.Map.t;
  (* Map from continuations to handlers (i.e variables bound by the
     continuation and expression of the continuation handler). *)
  offsets : Un_cps_closure.env;
  (* Offsets for closure_ids and var_within_closures. *)
}


let mk offsets k k_exn = {
  k; k_exn; offsets;
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

let create_variable env v =
  assert (not (Variable.Map.mem v env.vars));
  let name = Variable.unique_name v in
  let v' = Backend_var.create_local name in
  let vars = Variable.Map.add v v' env.vars in
  let v'' = Backend_var.With_provenance.create v' in
  { env with vars }, v''

let create_variables env l =
  let env, l' =
    List.fold_left (fun (env, l) v ->
        let env', v' = create_variable env v in
        env', v' :: l) (env, []) l
  in
  env, List.rev l'

let get_variable env v =
  try Variable.Map.find v env.vars
  with Not_found -> assert false


(* Continuations *)

let get_jump_id env k =
  match Continuation.Map.find k env.conts with
  | Jump id -> id
  | Inline _
  | exception Not_found -> assert false

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

let add_jump_cont env k =
  let id = new_jump_id () in
  let conts = Continuation.Map.add k (Jump id) env.conts in
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

