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


(* Reasons for lower bounds on registers, can be either:
   - a statically known lower-bound
   - a constraint speciying a register should have a greater type
     then another register *)
type reason =
  | Lower_bound of Cmm.machtype_component * Mach.instruction
  | Constraint of Reg.t * Mach.instruction

let print_reason fmt = function
  | Lower_bound (ty, i) ->
      Format.fprintf fmt " > %a || %a"
        Printcmm.machtype_component ty Printmach.instr i
  | Constraint (r, i) ->
      Format.fprintf fmt " > %a || %a"
        Printmach.reg r Printmach.instr i

let print_reasons fmt reasons =
  let pp_sep fmt () = Format.fprintf fmt "@ || " in
  Format.pp_print_list ~pp_sep Printmach.instr fmt reasons

(* Statically known upper bound on register types. These arise from stores, which
   set a static upper bounds on the contents of a register. *)
type upper_bound = {
  ty : Cmm.machtype_component;
  reasons : Mach.instruction list;
}

let print_upper_bound fmt { ty; reasons; } =
  Format.fprintf fmt " <= %a @[<hv>|| %a@]"
    Printcmm.machtype_component ty
    print_reasons reasons


(* Register typing environment.
   The environment accumulates constraints on register types (lowetr and upper bounds).
   The algorithm in this module assumes that most registers already have an adequate type,
   and there will be only a few register type updates to perform (and even fewer cycles
   of constraints). As such, the env registers constrinats between register types,
   while static lower bounds trigger a walk of the constraint graph to upgrade the
   registers' types as needed (which should be fairly quick and limited given the
   assumptions above). Upper bounds on types are stored in the env and checked
   once at the end. *)
type environment = {
  constraints : reason Reg.Map.t Reg.Map.t;
  (* map a register [r] to a set of registers whose type needs to be greater
     or equal to that of [r] *)
  upper_bounds : upper_bound Reg.Map.t;
}

let empty_env = {
  constraints = Reg.Map.empty;
  upper_bounds = Reg.Map.empty;
}

let get_constraint env r =
  try Reg.Map.find r env.constraints
  with Not_found -> Reg.Map.empty

let set_upper_bound env i r ty =
  let instr = { i with next = end_instr (); } in
  let b =
    match Reg.Map.find r env.upper_bounds with
    | exception Not_found -> { ty; reasons = [instr]; }
    | b' ->
        let reasons = instr :: b'.reasons in
        begin try { ty = Cmm.glb_component ty b'.ty; reasons; }
        with Cmm.Incompatible_types (c1, c2) ->
          Misc.fatal_errorf
            ("Register %a should have a type <= %a@\n  because of %a@\n"
             ^^ "but the upper bound of %a and %a does not exists")
            Printmach.reg r Printcmm.machtype_component ty
            print_reasons reasons Printcmm.machtype_component c1 Printcmm.machtype_component c2
        end
  in
  { env with upper_bounds = Reg.Map.add r b env.upper_bounds; }

(* enforce a constant constraint reg.typ >= b *)
let rec set_lower_bound_aux env stack =
  match Stack.pop stack with
  | (r, reason, bound) ->
      begin try
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
      with Cmm.Incompatible_types (c1, c2) ->
        Misc.fatal_errorf
          ("Register %a should have a type >= %a@\n  because of %a@\n"
           ^^ "but the lower bound of %a and %a does not exists")
          Printmach.reg r Printcmm.machtype_component bound
          print_reason reason Printcmm.machtype_component c1 Printcmm.machtype_component c2
      end
  | exception Stack.Empty ->
      ()

let set_lower_bound env i reg b =
  let stack = Stack.create () in
  let instr = { i with next = end_instr () } in
  Stack.push (reg, Lower_bound (b, instr), b) stack;
  set_lower_bound_aux env stack;
  env

let set_lower_bounds env i regs b =
  let stack = Stack.create () in
  let instr = { i with next = end_instr () } in
  Array.iter (fun r -> Stack.push (r, Lower_bound (b, instr), b) stack) regs;
  set_lower_bound_aux env stack;
  env

(* add a constraint specifying that r1.typ >= r2.typ *)
let add_constraint env i r1 r2 =
  let s = get_constraint env r2 in
  let reason = Constraint (r2, { i with next = end_instr ()}) in
  let s' = Reg.Map.add r1 reason s in
  let constraints = Reg.Map.add r2 s' env.constraints in
  let env = { env with constraints } in
  let stack = Stack.create () in
  Stack.push (r1, reason, r2.typ) stack;
  set_lower_bound_aux env stack;
  env


let chunk_lower_type = function
  | Word_val -> Val
  | Single | Double | Double_u -> Float
  | _ -> Int

let chunk_upper_type = function
  | Single | Double | Double_u -> Float
  | _ -> Val

let specific env i arg res op =
  let aux acc = function
    | `Constraint (r1, r2) -> add_constraint acc i r1 r2
    | `Bound (r, b) -> set_lower_bound acc i r b
  in
  let l = Arch.infer_specific arg res op in
  List.fold_left aux env l

let infer_op env i arg res = function

  | Imove -> add_constraint env i res.(0) arg.(0)
  | Iconst_int _ -> set_lower_bound env i res.(0) Int
  | Iconst_float _-> set_lower_bound env i res.(0) Float
  | Iconst_symbol _ -> set_lower_bound env i res.(0) Val

  | Ispill
  | Ireload -> env
  (* result registers types are already set during selectgen *)
  | Icall_ind _ | Icall_imm _
  | Itailcall_ind _ | Itailcall_imm _
  | Iextcall _ -> env
  | Istackoffset _ -> env

  (* TODO: cover more exhaustively the various addressing modes to also
           infer type for the address registers ? *)
  | Iload (chunk, _) -> set_lower_bound env i res.(0) (chunk_lower_type chunk)
  | Istore (chunk, _, _) -> set_upper_bound env i arg.(0) (chunk_upper_type chunk)
  | Ialloc _ ->
      (* CR Gbury: should we also set Val to be the upper bound on res.(0) ? *)
      set_lower_bound env i res.(0) Val

  (* Adequate register types have been set in selectegn based on the cmm operation
     used (e.g. Caddi vs Cadda). *)
  | Iintop _
  | Iintop_imm _ ->
      let env = set_lower_bounds env i arg Int in
      set_lower_bounds env i res Int
  | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf ->
      let env = set_lower_bounds env i arg Float in
      set_lower_bounds env i res Float
  | Ifloatofint ->
      let env = set_lower_bound env i arg.(0) Int in
      set_lower_bound env i res.(0) Float
  | Iintoffloat ->
      let env = set_lower_bound env i arg.(0) Float in
      set_lower_bound env i res.(0) Int
  | Ispecific op -> specific env i arg res op
  | Iname_for_debugger _ -> env

let test env i arg _res = function
  | Itruetest | Ifalsetest -> env
  | Ioddtest | Ieventest
  | Iinttest _ | Iinttest_imm _ -> set_lower_bounds env i arg Int
  | Ifloattest _ -> set_lower_bounds env i arg Float

let rec instruction_desc env i args res = function
  | Iend -> assert false
  | Iop op -> infer_op env i args res op
  | Ireturn _ -> env
  | Iifthenelse (t, then_branch, else_branch) ->
      let env = test env i args res t in
      let env = instruction env then_branch in
      let env = instruction env else_branch in
      env
  | Iswitch (_, a) ->
      Array.fold_left instruction env a
  | Icatch (_, l, body) ->
      let h (_n, _ts, handler) = handler in
      List.fold_left instruction (instruction env body) (List.map h l)
  | Iexit _ ->
      env
  | Itrywith (e, _, f) ->
      instruction (instruction env e) f
  | Iraise _ ->
      env

and instruction env i =
  match i.desc with
  | Iend -> env
  | _ ->
      let env' = instruction_desc env i i.arg i.res i.desc in
      instruction env' i.next

let check_upper_bounds env =
  Reg.Map.iter (fun r b ->
      let error =
        try not (Cmm.ge_component b.ty r.typ)
        with Cmm.Incompatible_types _ -> true
      in
      if error then
        Misc.fatal_errorf "Invalid register typ %a for %a@\n  because of %a"
          Printcmm.machtype_component r.typ Printmach.reg r
          print_upper_bound b
    ) env.upper_bounds

let fundecl decl =
  let env = empty_env in
  let env = instruction env decl.fun_body in
  let () = check_upper_bounds env in
  decl
