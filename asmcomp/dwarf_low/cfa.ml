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

type t =
  | Advance_loc of Int6.t
  | Offset of Register.Six_bit.t * Uleb128.t
  | Restore of Register.Six_bit.t
  | Nop
  | Set_loc of Target_addr.t
  | Advance_loc1 of Int8.t
  | Advance_loc2 of Int16.t
  | Advance_loc4 of Int32.t
  | Offset_extended of Register.t * Uleb128.t
  | Restore_extended of Register.t
  | Undefined of Register.t
  | Same_value of Register.t
  | Register of Register.t * Register.t
  | Remember_state
  | Restore_state
  | Def_cfa of Register.t * Uleb128.t
  | Def_cfa_register of Register.t
  | Def_cfa_offset of Uleb128.t
  | Def_cfa_expression of Block.t
  | Expression of Register.t * Block.t
  | Offset_extended_sf of Register.t * Sleb128.t
  | Def_cfa_sf of Register.t * Sleb128.t
  | Def_cfa_offset_sf of Sleb128.t
  | Val_offset of Uleb128.t * Uleb128.t
  | Val_offset_sf of Uleb128.t * Sleb128.t
  | Val_expression of Uleb128.t * Block.t

let dw_cfa_lo_user_high_2_bits = 0
let dw_cfa_lo_user_low_6_bits = 0x1c
let dw_cfa_hi_user_high_2_bits = 0
let dw_cfa_hi_user_low_6_bits = 0x3f

(* DWARF-4 specification figure 40, pages 181--182. *)
let high_2_bits = function
  | Advance_loc _ -> 0x1
  | Offset _ ->  0x2
  | Restore _ -> 0x3
  | Nop -> 0
  | Set_loc _ -> 0
  | Advance_loc1 _ -> 0
  | Advance_loc2 _ -> 0
  | Advance_loc4 _ -> 0
  | Offset_extended _ -> 0
  | Restore_extended _ -> 0
  | Undefined _ -> 0
  | Same_value _ -> 0
  | Register _ -> 0
  | Remember_state -> 0
  | Restore_state -> 0
  | Def_cfa _ -> 0
  | Def_cfa_register _ -> 0
  | Def_cfa_offset _ -> 0
  | Def_cfa_expression _ -> 0
  | Expression _ -> 0
  | Offset_extended_sf _ -> 0
  | Def_cfa_sf _ -> 0
  | Def_cfa_offset_sf _ -> 0
  | Val_offset _ -> 0
  | Val_offset_sf _ -> 0
  | Val_expression _ -> 0

(* DWARF-4 specification figure 40, pages 181--182. *)
let low_6_bits = function
  | Advance_loc delta -> delta
  | Offset reg ->  Register.Six_bit.to_int6 reg
  | Restore reg -> Register.Six_bit.to_int6 reg
  | Nop -> Int6.of_int_exn 0
  | Set_loc -> Int6.of_int_exn 0x01
  | Advance_loc1 -> Int6.of_int_exn 0x02
  | Advance_loc2 -> Int6.of_int_exn 0x03
  | Advance_loc4 -> Int6.of_int_exn 0x04
  | Offset_extended -> Int6.of_int_exn 0x05
  | Restore_extended -> Int6.of_int_exn 0x06
  | Undefined -> Int6.of_int_exn 0x07
  | Same_value -> Int6.of_int_exn 0x08
  | Register -> Int6.of_int_exn 0x09
  | Remember_state -> Int6.of_int_exn 0x0a
  | Restore_state -> Int6.of_int_exn 0x0b
  | Def_cfa -> Int6.of_int_exn 0x0c
  | Def_cfa_register -> Int6.of_int_exn 0x0d
  | Def_cfa_offset -> Int6.of_int_exn 0x0e
  | Def_cfa_expression -> Int6.of_int_exn 0x0f
  | Expression -> Int6.of_int_exn 0x10
  | Offset_extended_sf -> Int6.of_int_exn 0x11
  | Def_cfa_sf -> Int6.of_int_exn 0x12
  | Def_cfa_offset_sf -> Int6.of_int_exn 0x13
  | Val_offset -> Int6.of_int_exn 0x14
  | Val_offset_sf -> Int6.of_int_exn 0x15
  | Val_expression -> Int6.of_int_exn 0x16

let operands = function
  | Advance_loc _ -> []
  | Offset (_, offset) -> [Value.of_uleb128 offset]
  | Restore _ -> []
  | Nop -> []
  | Set_loc addr -> [Value.of_target_addr addr]
  | Advance_loc1 delta -> [Value.of_int8 delta]
  | Advance_loc2 delta -> [Value.of_int16 delta]
  | Advance_loc4 delta -> [Value.of_int32 delta]
  | Offset_extended (reg, offset) ->
    [Register.to_value reg; Value.of_uleb128 offset]
  | Restore_extended reg -> [Register.to_value reg]
  | Undefined reg -> [Register.to_value reg]
  | Same_value reg -> [Register.to_value reg]
  | Register (reg1, reg2) -> [Register.to_value reg1; Register.to_value reg2]
  | Remember_state -> 0
  | Restore_state -> 0
  | Def_cfa (reg, offset) ->
    [Register.to_value reg; Value.of_uleb128 offset]
  | Def_cfa_register reg -> [Register.to_value reg]
  | Def_cfa_offset offset -> [Value.of_uleb128 offset]
  | Def_cfa_expression block -> [Block.to_value block]
  | Expression (reg, block) ->
    [Register.to_value reg; Block.to_value block]
  | Offset_extended_sf (reg, offset) ->
    [Register.to_value reg; Value.of_sleb128 offset]
  | Def_cfa_sf (reg, offset) ->
    [Register.to_value reg; Value.of_sleb128 offset]
  | Def_cfa_offset_sf offset -> [Value.of_sleb128 offset]
  | Val_offset (offset1, offset2) ->
    [Value.of_uleb128 offset1; Value.of_uleb128 offset2]
  | Val_offset_sf (offset1, offset2) ->
    [Value.of_uleb128 offset1; Value.of_sleb128 offset2]
  | Val_expression (offset, block) ->
    [Value.of_uleb128 offset; Block.to_value block]

let parse ~stream =
  let open Stream.Monad_infix in
  Stream.read_int8 stream
  >>= fun primary_opcode ->
  let high_2_bits = primary_opcode lor 0xc0 in
  let low_6_bits = primary_opcode lor 0x3f in
  match high_2_bits with
  | 0x1 -> Advance_loc (Int6.of_int_exn low_6_bits)
  | 0x2 ->
    let reg = Register.Six_bit.of_int_exn low_6_bits in
    Stream.read_uleb128 stream
    >>= fun offset ->
    Offset (reg, offset)
  | 0x3 -> Restore (Register.Six_bit.of_int_exn low_6_bits)
  | 0 ->
    begin match low_6_bits with
    | 0 -> Nop
    | 0x01 ->
      Stream.parse_target_addr stream
      >>= fun addr ->
      Set_loc addr
    | 0x02 ->
      Stream.parse_int8 stream
      >>= fun delta ->
      Advance_loc1 delta
    | 0x03 ->
      Stream.parse_int16 stream
      >>= fun delta ->
      Advance_loc2 delta
    | 0x04 ->
      Stream.parse_int32 stream
      >>= fun delta ->
      Advance_loc4 delta
    | 0x05 ->
      Register.parse stream
      >>= fun reg ->
      Stream.parse_uleb128 stream
      >>= fun offset ->
      Offset_extended (reg, offset)
    | 0x06 ->
      Register.parse stream
      >>= fun reg ->
      Restore_extended reg
    | 0x07 ->
      Register.parse stream
      >>= fun reg ->
      Undefined
    | 0x08 ->
      Register.parse stream
      >>= fun reg ->
      Same_value reg
    | 0x09 ->
      Register.parse stream
      >>= fun reg1 ->
      Register.parse stream
      >>= fun reg2 ->
      Register (reg1, reg2)
    | 0x0a -> Remember_state
    | 0x0b -> Restore_state
    | 0x0c ->
      Register.parse stream
      >>= fun reg ->
      Stream.parse_uleb128 stream
      >>= fun offset ->
      Def_cfa (reg, offset)
    | 0x0d ->
      Register.parse stream
      >>= fun reg ->
      Def_cfa_register reg
    | 0x0e ->
      Stream.parse_uleb128 stream
      >>= fun offset ->
      Def_cfa_offset offset
    | 0x0f ->
      Block.parse stream
      >>= fun block ->
      Def_cfa_expression block
    | 0x10 ->
      Register.parse stream
      >>= fun reg ->
      Block.parse stream
      >>= fun block ->
      Expression (reg, block)
    | 0x11 ->
      Register.parse stream
      >>= fun reg ->
      Stream.parse_sleb128 stream
      >>= fun offset ->
      Offset_extended_sf (reg, offset)
    | 0x12 ->
      Register.parse stream
      >>= fun reg ->
      Stream.parse_sleb128 stream
      >>= fun offset ->
      Def_cfa_sf (reg, offset)
    | 0x13 ->
      Stream.parse_sleb128 stream
      >>= fun offset ->
      Def_cfa_offset_sf offset
    | 0x14 ->
      Stream.parse_uleb128 stream
      >>= fun offset1 ->
      Stream.parse_uleb128 stream
      >>= fun offset2 ->
      Val_offset (offset1, offset2)
    | 0x15 ->
      Stream.parse_uleb128 stream
      >>= fun offset1 ->
      Stream.parse_sleb128 stream
      >>= fun offset2 ->
      Val_offset_sf (offset1, offset2)
    | 0x16 ->
      Stream.parse_uleb128 stream
      >>= fun offset ->
      Block.parse stream
      >>= fun block ->
      Val_expression (offset, block)
    | _ ->
      Error (Printf.sprintf "invalid low 6 bits in primary CFA opcode %d"
          primary_opcode)
    end
  | _ ->
    Error (Printf.sprintf "invalid high 2 bits in primary CFA opcode %d"
        primary_opcode)

let values t =
  let primary_opcode =
    Int8.of_int_exn (((high_2_bits t) lsl 6) lor (Int6.to_int (low_6_bits t)))
  in
  (Value.of_int8 primary_opcode)::(operands t)

let emit t ~emitter =
  List.iter (Value.emit ~emitter) (values t)

let size t =
  Value.total_size (values t)
