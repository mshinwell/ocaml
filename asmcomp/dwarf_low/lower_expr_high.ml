(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Holding                                *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

module E = Expr

type lowering_state = {
  frame_size : int;
  var_to_frame_offsets : (Var.t, int) Hashtbl.t;
}

let lower ~functions ~emit =
  let rec lower e ~state =
    match e with
    | Unit ->
      emit E.DW_OP_nop
    | Var var ->
      begin match Hashtbl.find state.var_to_frame_offsets var with
      | exception Not_found ->
        failwith (Printf.sprintf "undefined variable '%s'"
          (Var.to_string var))
      | offset ->
        (* Turn the offset from frame base into the offset back from the
           top of stack. *)
        let offset = (state.frame_size - 1) - offset in
        assert (offset >= 0);
        if offset >= 255 then failwith "frame size exceeds maximum (255)"
        else begin
          state.frame_size <- state.frame_size + 1;
          if offset = 1 then
            emit E.DW_OP_over
          else
            emit (E.DW_OP_pick offset)
        end
      end
    | Let (var, binding, body) ->
      begin match Hashtbl.find state.var_to_frame_offsets var with
      | _offset ->
        failwith (Printf.sprintf "shadowing of variables is unsupported"
          (Var.to_string var))
      | exception Not_found ->
        let old_frame_size = state.frame_size in
        let old_var_to_frame_offsets = state.var_to_frame_offsets;
        lower binding;
        assert (state.frame_size >= old_frame_size + 1);
        state.frame_size <- old_frame_size;
        state.var_to_frame_offsets <- old_var_to_frame_offsets;
        Hashtbl.replace state.var_to_frame_offsets var state.frame_size;
        lower body
      end
    | Address addr ->
      state.frame_size <- state.frame_size + 1;
      emit (E.DW_OP_addr addr)
    | Deref (`Whole_word, e) ->
      state.frame_size <- state.frame_size + 1;
      lower e;
      emit E.DW_OP_deref
    | Deref (`This_many_bytes bytes, e) ->
      state.frame_size <- state.frame_size + 1;
      lower e;
      emit (E.DW_OP_deref_size bytes)
    | Signed_int c ->
      state.frame_size <- state.frame_size + 1;
      begin match E.literal_for_int64 c with
      | Some literal -> emit literal
      | None ->
        if Int8.int64_within_signed_bounds c then
          emit (E.DW_OP_const1s (Int8.of_int64_exn c))
        else if Int16.int64_within_signed_bounds c then
          emit (E.DW_OP_const2s (Int16.of_int64_exn c))
        else if Int32.int64_within_signed_bounds c then
          emit (E.DW_OP_const4s (Int32.of_int64_exn c))
        else
          emit (E.DW_OP_const8s c)
      end
    | Unsigned_int c ->
      state.frame_size <- state.frame_size + 1;
      begin match E.literal_for_int64 c with
      | Some literal -> emit literal
      | None ->
        if Int8.int64_within_unsigned_bounds c then
          emit (E.DW_OP_const1u (Int8.of_int64_exn c))
        else if Int16.int64_within_unsigned_bounds c then
          emit (E.DW_OP_const2u (Int16.of_int64_exn c))
        else if Int32.int64_within_unsigned_bounds c then
          emit (E.DW_OP_const4u (Int32.of_int64_exn c))
        else
          emit (E.DW_OP_const8u c)
      end
    | Canonical_frame_address ->
      state.frame_size <- state.frame_size + 1;
      emit E.DW_OP_cfa
    | Neg e ->
      lower e;
      emit E.DW_OP_neg
    | Plus (e1, e2) -> lower_binop DW_OP_plus e1 e2
    | Minus (e1, e2) -> lower_binop DW_OP_minus e1 e2
    | Mul (e1, e2) -> lower_binop DW_OP_mul e1 e2
    | Div (e1, e2) -> lower_binop DW_OP_div e1 e2
    | Mod (e1, e2) -> lower_binop DW_OP_mod e1 e2
    | Abs e ->
      lower e ~emit;
      emit E.DW_OP_abs
    | Not e ->
      lower e ~emit;
      emit E.DW_OP_not
    | And (e1, e2) -> lower_binop DW_OP_and e1 e2
    | Or (e1, e2) -> lower_binop DW_OP_or e1 e2
    | Xor (e1, e2) -> lower_binop DW_OP_xor e1 e2
    | Lsl (e1, `by shift) -> lower_shift DW_OP_shl e1 ~by
    | Lsr (e1, `by shift) -> lower_shift DW_OP_shr e1 ~by
    | Asr (e1, `by shift) -> lower_shift DW_OP_shra e1 ~by
    | Call func_name ->
      (* need to emit the offset of the DIE for [func_name] *)
      emit E.DW_OP_call_ref



    | Cond (cond, t) ->


    | If (cond, if_true, if_false) ->
      lower 

  and lower_binop expr left right =
    lower left;
    lower right;
    state.frame_size <- state.frame_size - 1;
    emit expr
  and lower_shift expr arg ~by =
    lower arg;
    lower by;
    state.frame_size <- state.frame_size - 1;
    emit expr
  in
  List.map functions ~f:(fun (func_name : Proto_DIE.t, body) ->
    func_name, lower body ~state:...)
