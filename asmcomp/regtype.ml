(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Guillaume Bury, OCamlPro                         *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Cmm
open Reg
open Mach

type reason =
  | Lower_bound of Cmm.machtype_component
  | Constraint of Reg.t * Mach.instruction

let print_reason fmt = function
  | Lower_bound ty -> Format.fprintf fmt " > %a" Printcmm.machtype_component ty
  | Constraint (r, i) -> Format.fprintf fmt " > %a || %a" Printmach.reg r Printmach.instr i

type environment = {
  constraints : reason Reg.Map.t Reg.Map.t;
  (* map a register [r] to a set of registers whose type needs to be greater
     or equal to that of [r] *)
}

let empty_env = {
  constraints = Reg.Map.empty;
}

let get_constraint env r =
  try Reg.Map.find r env.constraints
  with Not_found -> Reg.Map.empty


(* enforce a constant constraint reg.typ >= b *)
let rec set_lower_bound_aux env stack =
  match Stack.pop stack with
  | (r, reason, bound) ->
      if Cmm.ge_component r.typ bound then ()
      else begin
        let typ = Cmm.lub_component r.typ bound in
        assert (typ <> r.typ);
        Format.ifprintf Format.err_formatter
          "WARNING ! adjusted reg %a: %a -> %a@\n  because of %a@."
          Printmach.reg r Printcmm.machtype_component r.typ Printcmm.machtype_component typ
        print_reason reason;
        r.typ <- typ;
        let s = get_constraint env r in
        Reg.Map.iter (fun r' reason -> Stack.push (r', reason, typ) stack) s;
        set_lower_bound_aux env stack
      end
  | exception Stack.Empty ->
      ()

let set_lower_bound env reg b =
  let stack = Stack.create () in
  Stack.push (reg, Lower_bound b, b) stack;
  set_lower_bound_aux env stack

let set_lower_bounds env regs b =
  let stack = Stack.create () in
  Array.iter (fun r -> Stack.push (r, Lower_bound b, b) stack) regs;
  set_lower_bound_aux env stack


(* add a constraint specifying that r1.typ >= r2.typ *)
let add_constraint env i r1 r2 =
  let s = get_constraint env r2 in
  let reason = Constraint (r2, { i with next = end_instr ()}) in
  let s' = Reg.Map.add r1 reason s in
  let constraints = Reg.Map.add r2 s' env.constraints in
  let env = { (* env with *) constraints } in
  let stack = Stack.create () in
  Stack.push (r1, reason, r2.typ) stack;
  set_lower_bound_aux env stack;
  env

let add_constraints env i a1 a2 =
  let cur = ref env in
  Array.iter2 (fun r1 r2 -> cur := add_constraint !cur i r1 r2) a1 a2;
  !cur


let type_of_chunk = function
  | Word_val -> Val
  | Single | Double | Double_u -> Float
  | _ -> Int

let specific env i arg res op =
  let aux acc = function
    | `Constraint (r1, r2) -> add_constraint acc i r1 r2
    | `Bound (r, b) -> set_lower_bound acc r b; acc
  in
  let l = Arch.infer_specific arg res op in
  List.fold_left aux env l

let infer_op env i arg res = function
  | Imove -> add_constraints env i res arg
  | Ispill
  | Ireload -> assert false
  | Iconst_int _ -> set_lower_bound env res.(0) Int; env
  | Iconst_float _-> set_lower_bound env res.(0) Float; env
  | Iconst_symbol _ -> set_lower_bound env res.(0) Val; env
  | Icall_ind _ | Icall_imm _
  | Itailcall_ind _ | Itailcall_imm _
  | Iextcall _ ->
      (* result registers types are already set during selectgen *)
      env
  | Istackoffset _ -> env
  | Iload (chunk, _) -> set_lower_bound env res.(0) (type_of_chunk chunk); env
  | Istore (chunk, _, _) -> set_lower_bound env arg.(0) (type_of_chunk chunk); env
  | Ialloc _ -> set_lower_bound env res.(0) Val; env
  | Iintop _
  | Iintop_imm _ ->
      set_lower_bounds env arg Int;
      begin match res with
      | [| |] -> env
      | [| r |] ->
          Array.fold_left (fun acc r' -> add_constraint acc i r r') env arg
      | _ -> assert false
      end
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf ->
      set_lower_bounds env arg Float;
      set_lower_bounds env res Float;
      env
  | Ifloatofint ->
      set_lower_bound env arg.(0) Int;
      set_lower_bound env res.(0) Float;
      env
  | Iintoffloat ->
      set_lower_bound env arg.(0) Float;
      set_lower_bound env res.(0) Int;
      env
  | Ispecific op -> specific env i arg res op
  | Iname_for_debugger _ -> env

let test env arg _res = function
  | Itruetest | Ifalsetest -> env
  | Ioddtest | Ieventest
  | Iinttest _ | Iinttest_imm _ -> set_lower_bounds env arg Int; env
  | Ifloattest _ -> set_lower_bounds env arg Float; env

let rec instruction_desc env i args res = function
  | Iend -> assert false
  | Iop op -> infer_op env i args res op
  | Ireturn -> env
  | Iifthenelse (t, then_branch, else_branch) ->
      let env = test env args res t in
      let env = instruction env then_branch in
      let env = instruction env else_branch in
      env
  | Iswitch (_, a) ->
      Array.fold_left instruction env a
  | Icatch (_, l, body) ->
      List.fold_left instruction (instruction env body) (List.map snd l)
  | Iexit _ ->
      env
  | Itrywith (e, f) ->
      instruction (instruction env e) f
  | Iraise _ ->
      env

and instruction env i =
  match i.desc with
  | Iend -> env
  | _ ->
      let env' = instruction_desc env i i.arg i.res i.desc in
      instruction env' i.next

let fundecl decl =
  let env = empty_env in
  let _ : environment = instruction env decl.fun_body in
  decl
