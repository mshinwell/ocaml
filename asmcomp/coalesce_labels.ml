(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Int = Numbers.Int
module L = Linearize
module Option = Misc.Stdlib.Option

let rewrite_label env label =
  match Int.Map.find label env with
  | exception Not_found ->
    Misc.fatal_errorf "Coalesce_labels: label %d undefined" label
  | label -> label

let rewrite_or_add_label env label =
  match Int.Map.find label env with
  | exception Not_found ->
    let new_label = Linearize.new_label () in
    let env = Int.Map.add label new_label env in
    env, new_label
  | label -> env, label

let rec coalesce env (insn : L.instruction) ~last_insn_was_label =
  if insn == L.end_instr then
    env, insn
  else
    let env, desc, this_insn_is_label =
      match insn.desc with
      | Lprologue
      | Lend
      | Lop _
      | Lreloadretaddr
      | Lreturn
      | Lpushtrap
      | Lpoptrap
      | Lraise _
      | Lavailable_subrange _ -> env, Some insn.desc, None
      | Llabel label ->
        begin match last_insn_was_label with
        | Some existing_label ->
          let env = Int.Map.add label existing_label env in
          env, None, last_insn_was_label
        | None ->
          let env, new_label =
            match Int.Map.find label env with
            | exception Not_found ->
              let new_label = Linearize.new_label () in
              let env = Int.Map.add label new_label env in
              env, new_label
            | label_used_prior_to_decl -> env, label_used_prior_to_decl
          in
          env, Some (L.Llabel new_label), Some new_label
        end
      | Lbranch label ->
        let env, label = rewrite_or_add_label env label in
        env, Some (L.Lbranch label), None
      | Lcondbranch (test, label) ->
        let env, label = rewrite_or_add_label env label in
        env, Some (L.Lcondbranch (test, label)), None
      | Lcondbranch3 (label1_opt, label2_opt, label3_opt) ->
        let rewrite_opt env label_opt =
          match label_opt with
          | None -> env, None
          | Some label ->
            let env, label = rewrite_or_add_label env label in
            env, Some label
        in
        let env, label1_opt = rewrite_opt env label1_opt in
        let env, label2_opt = rewrite_opt env label2_opt in
        let env, label3_opt = rewrite_opt env label3_opt in
        env, Some (L.Lcondbranch3 (label1_opt, label2_opt, label3_opt)),
          None
      | Lswitch labels ->
        let env, labels =
          Array.fold_left (fun (env, labels) label ->
              let env, label = rewrite_or_add_label env label in
              env, label::labels)
            (env, [])
            labels
        in
        env, Some (L.Lswitch (Array.of_list (List.rev labels))), None
      | Lsetuptrap label ->
        let env, label = rewrite_or_add_label env label in
        env, Some (L.Lsetuptrap label), None
    in
    let env, next =
      coalesce env insn.next ~last_insn_was_label:this_insn_is_label
    in
    let insn =
      match desc with
      | None -> next
      | Some desc ->
        { insn with
          desc;
          next;
        }
    in
    env, insn

let fundecl (decl : L.fundecl) : int Int.Map.t * L.fundecl =
  if not !Clflags.debug then Int.Map.empty, decl
  else begin
    let env, fun_body =
      coalesce Int.Map.empty decl.fun_body ~last_insn_was_label:None
    in
    let fun_phantom_let_ranges =
      Ident.fold_all (fun ident (range : L.phantom_let_range) ranges ->
          let range =
            { range with
              starting_label = rewrite_label env range.starting_label;
              ending_label = rewrite_label env range.ending_label;
            }
          in
          Ident.add ident range ranges)
        decl.fun_phantom_let_ranges
        Ident.empty
    in
    let decl =
      { decl with
        fun_body;
        fun_phantom_let_ranges;
      }
    in
    env, decl
  end
