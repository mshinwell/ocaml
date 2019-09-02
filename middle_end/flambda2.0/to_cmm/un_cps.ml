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

(* XXX Remove this once the file has been updated to cope with the
   change in type of Let patterns *)
[@@@ocaml.warning "-32"]

open! Flambda.Import

module Env = Un_cps_env

(* Notes:
   - an int64 on a 32-bit host is represented across two registers,
     hence most operations on them will actually need to call C primitive
      that can handle them.
   - int32 on 64 bits are represented as an int64 in the range of
     32-bit integers. Currently we trust flambda2 to insert
     double shifts to clear the higher order 32-bits between operations.
     Once the small_arith PR comes, we can use dedicated 32-bits
     cmm arithmetic operations.
*)

(* TODO: remove all uses of this, ^^ *)
let todo () = failwith "Not yet implemented"

(* Cmm helpers *)
module C = struct
  include Cmm_helpers
  include Un_cps_helper
end

(* Shortcuts for useful cmm machtypes *)
let typ_int = Cmm.typ_int
let typ_val = Cmm.typ_val
let typ_float = Cmm.typ_float
let typ_int64 = C.typ_int64


(* Result for translating a program,
   named R instead of Result to avoid shadowing *)

module R = struct

  type t = {
    init : Cmm.expression;
    current_data : Cmm.data_item list;
    other_data : Cmm.data_item list list;
  }

  let empty = {
    init = C.void;
    current_data = [];
    other_data = [];
  }

  let add_if_not_empty x l =
    match x with
    | [] -> l
    | _ :: _ -> x :: l

  let combine r t = {
    init = C.sequence r.init t.init;
    current_data = [];
    other_data =
      add_if_not_empty r.current_data (
        add_if_not_empty t.current_data (
          (r.other_data @ t.other_data)));
  }

  let wrap_init f r =
    { r with init = f r.init; }

  let add_data d r =
    { r with current_data = d @ r.current_data; }

  let update_data f r =
    { r with current_data = f r.current_data; }

  let to_cmm r =
    let entry =
      let dbg = Debuginfo.none in
      let fun_name = Compilenv.make_symbol (Some "entry") in
      let fun_codegen =
        if Config.flambda then
          [ Cmm.Reduce_code_size;
            Cmm.No_CSE ]
        else
          [ Cmm.Reduce_code_size ]
      in
      let init = C.sequence r.init (C.unit ~dbg) in
      C.cfunction (C.fundecl fun_name [] init fun_codegen dbg)
    in
    let data_list = add_if_not_empty r.current_data r.other_data in
    let data = List.map C.cdata data_list in
    data, entry

end

(* CR gbury: this conversion is potentially unsafe when cross-compiling
             for a 64-bit machine on a 32-bit host *)
let nativeint_of_targetint t =
  match Targetint.repr t with
  | Int32 i -> Nativeint.of_int32 i
  | Int64 i -> Int64.to_nativeint i

(* Name expressions *)

let symbol s =
  Linkage_name.to_string (Symbol.linkage_name s)

let name env = function
  | Name.Var v -> Env.inline_variable env v
  | Name.Symbol s -> C.symbol (symbol s), env

let name_static _env = function
  | Name.Var v -> `Var v
  | Name.Symbol s -> `Data [C.symbol_address (symbol s)]

(* Constants *)

let tag_targetint t = Targetint.(add (shift_left t 1) one)
let targetint_of_imm i = Targetint.OCaml.to_targetint i.Immediate.value

let const _env c =
  match (c : Simple.Const.t) with
  | Naked_immediate i ->
      C.targetint (targetint_of_imm i)
  | Tagged_immediate i ->
      C.targetint (tag_targetint (targetint_of_imm i))
  | Naked_float f ->
      C.float (Numbers.Float_by_bit_pattern.to_float f)
  | Naked_int32 i -> C.int32 i
  | Naked_int64 i -> C.int64 i
  | Naked_nativeint t -> C.targetint t

let const_static _env c =
  match (c : Simple.Const.t) with
  | Naked_immediate i ->
      [C.cint (nativeint_of_targetint (targetint_of_imm i))]
  | Tagged_immediate i ->
      [C.cint (nativeint_of_targetint (tag_targetint (targetint_of_imm i)))]
  | Naked_float f ->
      [C.cfloat (Numbers.Float_by_bit_pattern.to_float f)]
  | Naked_int32 i ->
      [C.cint (Nativeint.of_int32 i)]
  | Naked_int64 i ->
      if C.arch32 then todo() (* split int64 on 32-bit archs *)
      else [C.cint (Int64.to_nativeint i)]
  | Naked_nativeint t ->
      [C.cint (nativeint_of_targetint t)]

(* Discriminants *)

let discriminant _env d =
  C.targetint (Targetint.OCaml.to_targetint (Discriminant.to_int d))

let discriminant_static _env d =
  C.cint (nativeint_of_targetint
            (Targetint.OCaml.to_targetint (Discriminant.to_int d)))

(* Function symbol *)

let function_name s =
  match (Simple.descr s : Simple.descr) with
  | Name Symbol s -> symbol s
  | _ ->
      Misc.fatal_errorf
        "Expected a function symbol, instead of@ %a" Simple.print s

(* 'Simple' expression *)

let simple env s =
  match (Simple.descr s : Simple.descr) with
  | Name n -> name env n
  | Const c -> const env c, env
  | Discriminant d -> discriminant env d, env

let simple_static env s =
  match (Simple.descr s : Simple.descr) with
  | Name n -> name_static env n
  | Const c -> `Data (const_static env c)
  | Discriminant d -> `Data [discriminant_static env d]

(* Arithmetic primitives *)

let primitive_boxed_int_of_standard_int x =
  match (x : Flambda_kind.Standard_int.t) with
  | Naked_int32 -> Primitive.Pint32
  | Naked_int64 -> Primitive.Pint64
  | Naked_nativeint -> Primitive.Pnativeint
  | Tagged_immediate -> assert false

let unary_int_arith_primitive _env dbg kind op arg =
  match (kind : Flambda_kind.Standard_int.t),
        (op : Flambda_primitive.unary_int_arith_op) with
  | Tagged_immediate, Neg -> C.negint arg dbg
  | Tagged_immediate, Swap_byte_endianness ->
      let untagged = C.untag_int arg dbg in
      let swapped = C.bswap16 untagged dbg in
      C.tag_int swapped dbg
  (* Special case for manipulating int64 on 32-bit hosts *)
  | Naked_int64, Neg when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_neg_native" typ_int64 [arg]
  (* General case (including byte swap for 64-bit on 32-bit archi) *)
  | _, Neg -> C.sub_int (C.int 0) arg dbg
  | _, Swap_byte_endianness ->
      let primitive_kind = primitive_boxed_int_of_standard_int kind in
      C.bbswap primitive_kind arg dbg

let unary_float_arith_primitive _env dbg op arg =
  match (op : Flambda_primitive.unary_float_arith_op) with
  | Abs -> C.float_abs ~dbg arg
  | Neg -> C.float_neg ~dbg arg

let arithmetic_conversion dbg src dst arg =
  let open Flambda_kind.Standard_int_or_float in
  match src, dst with
  (* 64-bit on 32-bit host specific cases *)
  | Naked_int64, Tagged_immediate when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_to_int" typ_int [arg]
  | Naked_int64, Naked_int32 when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_to_int32" typ_int [arg]
  | Naked_int64, Naked_nativeint when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_to_nativeint" typ_int [arg]
  | Naked_int64, Naked_float when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_to_float_unboxed" typ_float [arg]
  | Tagged_immediate, Naked_int64 when C.arch32 ->
      C.extcall ~alloc:true "caml_int64_of_int" typ_val [arg]
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | Naked_int32, Naked_int64 when C.arch32 ->
      C.extcall ~alloc:true "caml_int64_of_int32" typ_val [arg]
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | Naked_nativeint, Naked_int64 when C.arch32 ->
      C.extcall ~alloc:true "caml_int64_of_nativeint" typ_val [arg]
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | Naked_float, Naked_int64 when C.arch32 ->
      C.extcall ~alloc:true "caml_int64_of_float_unboxed" typ_val [arg]
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  (* Identity on floats *)
  | Naked_float, Naked_float -> arg
  (* general cases between integers *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Tagged_immediate ->
      C.force_tag_int arg dbg
  | Tagged_immediate, (Naked_int32 | Naked_int64 | Naked_nativeint) ->
      C.untag_int arg dbg
  (* TODO: insert shifts to zero-out higher-order bits during
           the 64 to 32 bit conversion ? *)
  | Tagged_immediate, Tagged_immediate
  | Naked_int32, (Naked_int32 | Naked_int64 | Naked_nativeint)
  | Naked_int64, (Naked_int32 | Naked_int64 | Naked_nativeint)
  | Naked_nativeint, (Naked_int32 | Naked_int64 | Naked_nativeint) ->
      arg
  (* Int-Float conversions *)
  | (Tagged_immediate | Naked_int32 | Naked_int64 | Naked_nativeint),
    Naked_float ->
      C.float_of_int ~dbg arg
  | Naked_float,
    (Tagged_immediate | Naked_int32 | Naked_int64 | Naked_nativeint) ->
      C.int_of_float ~dbg arg

let binary_phys_comparison _env dbg kind op x y =
  match (kind : Flambda_kind.t),
        (op : Flambda_primitive.equality_comparison) with
  (* int64 special case *)
  | Naked_number Naked_int64, Eq when C.arch32 ->
      C.extcall ~alloc:true "caml_equal" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_number Naked_int64, Neq when C.arch32 ->
      C.extcall ~alloc:true "caml_notequal" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  (* General case *)
  | _, Eq -> C.tag_int (C.eq ~dbg x y) dbg
  | _, Neq -> C.tag_int (C.neq ~dbg x y) dbg

let binary_int_arith_primitive _env dbg kind op x y =
  match (kind : Flambda_kind.Standard_int.t),
        (op : Flambda_primitive.binary_int_arith_op) with
  (* Int64 bits ints on 32-bit archs *)
  | Naked_int64, Add when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_add_native" typ_int64 [x; y]
  | Naked_int64, Sub when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_sub_native" typ_int64 [x; y]
  | Naked_int64, Mul when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_mul_native" typ_int64 [x; y]
  | Naked_int64, Div when C.arch32 ->
      C.extcall ~alloc:true "caml_int64_div_native" typ_int64 [x; y]
  | Naked_int64, Mod when C.arch32 ->
      C.extcall ~alloc:true "caml_int64_mod_native" typ_int64 [x; y]
  | Naked_int64, And when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_and_native" typ_int64 [x; y]
  | Naked_int64, Or when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_or_native" typ_int64 [x; y]
  | Naked_int64, Xor when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_xor_native" typ_int64 [x; y]
  (* Tagged integers *)
  | Tagged_immediate, Add -> C.add_int_caml x y dbg
  | Tagged_immediate, Sub -> C.sub_int_caml x y dbg
  | Tagged_immediate, Mul -> C.mul_int_caml x y dbg
  | Tagged_immediate, Div -> C.div_int_caml Lambda.Unsafe x y dbg
  | Tagged_immediate, Mod -> C.mod_int_caml Lambda.Unsafe x y dbg
  | Tagged_immediate, And -> C.and_int_caml x y dbg
  | Tagged_immediate, Or  -> C.or_int_caml x y dbg
  | Tagged_immediate, Xor -> C.xor_int_caml x y dbg
  (* Naked ints *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Add ->
      C.add_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Sub ->
      C.sub_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Mul ->
      C.mul_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Div ->
      C.div_int x y Lambda.Unsafe dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Mod ->
      C.mod_int x y Lambda.Unsafe dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), And ->
      C.and_ ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Or ->
      C.or_ ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Xor ->
      C.xor_ ~dbg x y

let binary_int_shift_primitive _env dbg kind op x y =
  match (kind : Flambda_kind.Standard_int.t),
        (op : Flambda_primitive.int_shift_op) with
  (* Int64 special case *)
  | Naked_int64, Lsl when C.arch32 ->
      todo() (* caml primitives for these have no native/unboxed version *)
  | Naked_int64, Lsr when C.arch32 ->
      todo() (* caml primitives for these have no native/unboxed version *)
  | Naked_int64, Asr when C.arch32 ->
      todo() (* caml primitives for these have no native/unboxed version *)
  (* Tagged integers *)
  | Tagged_immediate, Lsl -> C.lsl_int_caml x y dbg
  | Tagged_immediate, Lsr -> C.lsr_int_caml x y dbg
  | Tagged_immediate, Asr -> C.asr_int_caml x y dbg
  (* Naked ints *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Lsl -> C.lsl_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Lsr -> C.lsr_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint), Asr -> C.asr_int x y dbg

let binary_int_comp_primitive _env dbg kind signed cmp x y =
  match (kind : Flambda_kind.Standard_int.t),
        (signed : Flambda_primitive.signed_or_unsigned),
        (cmp : Flambda_primitive.ordered_comparison) with
  | Naked_int64, Signed, Lt when C.arch32 ->
      C.extcall ~alloc:true "caml_lessthan" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Signed, Le when C.arch32 ->
      C.extcall ~alloc:true "caml_lessequal" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Signed, Gt when C.arch32 ->
      C.extcall ~alloc:true "caml_greaterthan" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Signed, Ge when C.arch32 ->
      C.extcall ~alloc:true "caml_greaterequal" typ_int
        [C.box_int64 ~dbg x; C.box_int64 ~dbg y]
  | Naked_int64, Unsigned, (Lt | Le | Gt | Ge) when C.arch32 ->
      todo() (* There are no runtime C functions to do that afaict *)
  (* Tagged integers *)
  | Tagged_immediate, Signed, Lt -> C.tag_int (C.lt ~dbg x y) dbg
  | Tagged_immediate, Signed, Le -> C.tag_int (C.le ~dbg x y) dbg
  | Tagged_immediate, Signed, Gt -> C.tag_int (C.gt ~dbg x y) dbg
  | Tagged_immediate, Signed, Ge -> C.tag_int (C.ge ~dbg x y) dbg
  | Tagged_immediate, Unsigned, Lt -> C.tag_int (C.ult ~dbg x y) dbg
  | Tagged_immediate, Unsigned, Le -> C.tag_int (C.ule ~dbg x y) dbg
  | Tagged_immediate, Unsigned, Gt -> C.tag_int (C.ugt ~dbg x y) dbg
  | Tagged_immediate, Unsigned, Ge -> C.tag_int (C.uge ~dbg x y) dbg
  (* Naked integers. *)
  | (Naked_int32|Naked_int64|Naked_nativeint), Signed, Lt ->
      C.tag_int (C.lt ~dbg x y) dbg
  | (Naked_int32|Naked_int64|Naked_nativeint), Signed, Le ->
      C.tag_int (C.le ~dbg x y) dbg
  | (Naked_int32|Naked_int64|Naked_nativeint), Signed, Gt ->
      C.tag_int (C.gt ~dbg x y) dbg
  | (Naked_int32|Naked_int64|Naked_nativeint), Signed, Ge ->
      C.tag_int (C.ge ~dbg x y) dbg
  | (Naked_int32|Naked_int64|Naked_nativeint), Unsigned, Lt ->
      C.tag_int (C.ult ~dbg x y) dbg
  | (Naked_int32|Naked_int64|Naked_nativeint), Unsigned, Le ->
      C.tag_int (C.ule ~dbg x y) dbg
  | (Naked_int32|Naked_int64|Naked_nativeint), Unsigned, Gt ->
      C.tag_int (C.ugt ~dbg x y) dbg
  | (Naked_int32|Naked_int64|Naked_nativeint), Unsigned, Ge ->
      C.tag_int (C.uge ~dbg x y) dbg

let binary_float_arith_primitive _env dbg op x y =
  match (op : Flambda_primitive.binary_float_arith_op) with
  | Add -> C.float_add ~dbg x y
  | Sub -> C.float_sub ~dbg x y
  | Mul -> C.float_mul ~dbg x y
  | Div -> C.float_div ~dbg x y

let binary_float_comp_primitive _env dbg op x y =
  match (op : Flambda_primitive.comparison) with
  | Eq -> C.tag_int (C.float_eq ~dbg x y) dbg
  | Neq -> C.tag_int (C.float_neq ~dbg x y) dbg
  | Lt -> C.tag_int (C.float_lt ~dbg x y) dbg
  | Gt -> C.tag_int (C.float_gt ~dbg x y) dbg
  | Le -> C.tag_int (C.float_le ~dbg x y) dbg
  | Ge -> C.tag_int (C.float_ge ~dbg x y) dbg

(* Primitives *)

let ba_dimension_offset layout total_dim dim =
  match (layout : Lambda.bigarray_layout) with
  | Pbigarray_fortran_layout -> 4 + dim
  | Pbigarray_c_layout -> 5 + total_dim - dim
  | Pbigarray_unknown_layout ->
      Misc.fatal_errorf
        "Unknown bigarray layout, cannot compute dimension offset"

let unary_primitive env dbg f arg =
  match (f : Flambda_primitive.unary_primitive) with
  | Duplicate_block _ ->
      C.extcall ~alloc:true "caml_obj_dup" typ_val [arg]
  | Is_int ->
      C.and_ ~dbg arg (C.int ~dbg 1)
  | Get_tag ->
      C.get_tag arg dbg
  | Array_length block_access_kind ->
      C.block_length ~dbg block_access_kind arg
  | Bigarray_length { dimension } ->
      (* TODO: need the bigarray layout here !! + check the dimension offset computation *)
      let dim_ofs = ba_dimension_offset (todo()) (todo()) dimension in
      C.load ~dbg Cmm.Word_int Asttypes.Mutable (C.field_address arg dim_ofs dbg)
  | String_length _ ->
      C.string_length arg dbg
  | Int_as_pointer ->
      C.int_as_pointer arg dbg
  | Opaque_identity ->
      arg
  | Int_arith (kind, op) ->
      unary_int_arith_primitive env dbg kind op arg
  | Float_arith op ->
      unary_float_arith_primitive env dbg op arg
  | Num_conv { src; dst; } ->
      arithmetic_conversion dbg src dst arg
  | Boolean_not ->
      C.mk_not dbg arg
  | Unbox_number kind ->
      C.unbox_number ~dbg kind arg
  | Box_number kind ->
      C.box_number ~dbg kind arg
  | Select_closure { move_from = c1; move_to = c2} ->
      let diff = (Env.closure_offset env c2) - (Env.closure_offset env c1) in
      C.infix_field_address ~dbg arg diff
  | Project_var v ->
      C.get_field_gen Asttypes.Immutable arg (Env.env_var_offset env v) dbg

let binary_primitive env dbg f x y =
  match (f : Flambda_primitive.binary_primitive) with
  | Block_load (kind, _) ->
      C.block_load ~dbg kind x y
  | String_or_bigstring_load (kind, width) ->
      C.string_like_load ~dbg kind width x y
  | Phys_equal (kind, op) ->
      binary_phys_comparison env dbg kind op x y
  | Int_arith (kind, op) ->
      binary_int_arith_primitive env dbg kind op x y
  | Int_shift (kind, op) ->
      binary_int_shift_primitive env dbg kind op x y
  | Int_comp (kind, signed, cmp) ->
      binary_int_comp_primitive env dbg kind signed cmp x y
  | Float_arith op ->
      binary_float_arith_primitive env dbg op x y
  | Float_comp cmp ->
      binary_float_comp_primitive env dbg cmp x y

let ternary_primitive _env dbg f x y z =
  match (f : Flambda_primitive.ternary_primitive) with
  | Block_set (block_access, init) ->
      C.block_set ~dbg block_access init x y z
  | Bytes_or_bigstring_set (kind, width) ->
      C.bytes_like_set ~dbg kind width x y z

let variadic_primitive _env dbg f args =
  match (f : Flambda_primitive.variadic_primitive) with
  | Make_block (kind, _mut) ->
      C.make_block ~dbg kind args
  | Bigarray_load (dimensions, kind, layout) ->
      C.bigarray_load ~dbg dimensions kind layout args
  | Bigarray_set (dimensions, kind, layout) ->
      C.bigarray_store ~dbg dimensions kind layout args

let arg_list env l =
  let aux (acc, env') x = let y, env'' = simple env' x in (y :: acc, env'') in
  let args, env = List.fold_left aux ([], env) l in
  List.rev args, env

let prim env dbg p =
  match (p : Flambda_primitive.t) with
  | Unary (f, x) ->
      let x, env = simple env x in
      unary_primitive env dbg f x, env
  | Binary (f, x, y) ->
      let x, env = simple env x in
      let y, env = simple env y in
      binary_primitive env dbg f x y, env
  | Ternary (f, x, y, z) ->
      let x, env = simple env x in
      let y, env = simple env y in
      let z, env = simple env z in
      ternary_primitive env dbg f x y z, env
  | Variadic (f, l) ->
      let args, env = arg_list env l in
      variadic_primitive env dbg f args, env

(* Kinds and types *)

let machtype_of_kind k =
  match (k  : Flambda_kind.t) with
  | Value -> typ_val
  | Fabricated -> typ_val (* TODO: correct ? *)
  | Naked_number Naked_float -> typ_float
  | Naked_number Naked_int64 -> typ_int64
  | Naked_number (Naked_immediate | Naked_int32 | Naked_nativeint) ->
      typ_int

let machtype_of_kinded_parameter p =
  machtype_of_kind (Kinded_parameter.kind p)

let machtype_of_return_arity = function
  | [k] -> machtype_of_kind k
  | _ ->
      (* TODO: update when unboxed tuples are used *)
      Misc.fatal_errorf "Functions are currently limited to a single return value"

let meth_kind k =
  match (k : Call_kind.method_kind) with
  | Self -> (Self : Lambda.meth_kind)
  | Public -> (Public : Lambda.meth_kind)
  | Cached -> (Cached : Lambda.meth_kind)

(* Function calls and continuations *)

let var_list env l =
  let flambda_vars = List.map Kinded_parameter.var l in
  let env, cmm_vars = Env.create_variables env flambda_vars in
  let vars = List.map2 (fun v v' ->
      (v, machtype_of_kinded_parameter v')
    ) cmm_vars l in
  env, vars

(* effects and co-effects *)

let named_effs n =
  match (n : Named.t) with
  | Simple _ -> Effects_and_coeffects.pure
  | Prim (p, _) -> Flambda_primitive.effects_and_coeffects p
  | Set_of_closures _ -> Effects_and_coeffects.pure

let cont_has_one_occurrence k num =
  match (num : Name_occurrences.Num_occurrences.t) with
  | One -> true
  | More_than_one -> false
  | Zero ->
      Misc.fatal_errorf
        "Found unused let-bound continuation %a, this should not happen"
        Continuation.print k

type inlining_decision =
  | Skip (* no use, the bound variable can be skipped/ignored *)
  | Inline (* the variable is used once, we can try and inline its use *)
  | Regular (* the variable is used multiple times, do not try and inline it. *)

let decide_inline_let effs v body =
  let free_names = Expr.free_names body in
  match Name_occurrences.count_variable free_names v with
  | Zero ->
      if Effects_and_coeffects.has_commuting_effects effs
      then Regular (* Could be Inline technically, but it's not clear the
                      code would be better (nor more readable). *)
      else Skip
  | One -> Inline
  | More_than_one -> Regular

(* Expressions *)

let rec expr env e =
  match (Expr.descr e : Expr.descr) with
  | Let e' -> let_expr env e'
  | Let_cont e' -> let_cont env e'
  | Apply e' -> apply_expr env e'
  | Apply_cont e' -> apply_cont env e'
  | Switch e' -> switch env e'
  | Invalid e' -> invalid env e'

and named env n =
  match (n : Named.t) with
  | Simple s -> simple env s
  | Prim (p, dbg) -> prim env dbg p
  | Set_of_closures s -> set_of_closures env s

and let_expr env t =
  Let.pattern_match t ~f:(fun ~bound_vars ~body ->
      let e = Let.defining_expr t in
      match bound_vars, e with
      | Singleton v, _ ->
          let v = Var_in_binding_pos.var v in
          let_expr_aux env v e body
      | Set_of_closures { closure_vars; _ }, Set_of_closures soc ->
          (* First translate the set of closures, and bind it in the env *)
          let csoc, env = set_of_closures env soc in
          let soc_var = Variable.create "*set_of_closures*" in
          let effs = Effects_and_coeffects.all in
          let env = Env.bind_variable env soc_var effs false csoc in
          (* Then get from the env the cmm variable that was create and bound
             to the compiled set of closures. *)
          let soc_cmm_var, env = Env.inline_variable env soc_var in
          (* Add to the env bindingsd for all the closure variables. *)
          let effs = Effects_and_coeffects.pure in
          let env =
            Closure_id.Map.fold (fun cid v acc ->
                let v = Var_in_binding_pos.var v in
                let e =
                  C.infix_field_address ~dbg: Debuginfo.none
                    soc_cmm_var (Env.closure_offset env cid)
                in
                let_expr_bind body acc v e effs
              ) closure_vars env in
          (* The set of closures, as well as the individual closures variables
             are correctly set in the env, go on translating the body. *)
          expr env body
      | Set_of_closures _, (Simple _ | Prim _) ->
          Misc.fatal_errorf
            "Set_of_closures binding a non-Set_of_closures:@ %a"
            Let.print t
    )

and let_expr_bind body env v cmm_expr effs =
  match decide_inline_let effs v body with
  | Skip -> env
  | Inline -> Env.bind_variable env v effs true cmm_expr
  | Regular -> Env.bind_variable env v effs false cmm_expr

and let_expr_env body env v e =
  let effs = named_effs e in
  match decide_inline_let effs v body with
  | Skip ->
      env
  | Inline ->
      let cmm_expr, env = named env e in
      Env.bind_variable env v effs true cmm_expr
  | Regular ->
      let cmm_expr, env = named env e in
      Env.bind_variable env v effs false cmm_expr

and let_expr_aux env v e body =
  let env = let_expr_env body env v e in
  expr env body

and decide_inline_cont h k num_free_occurrences =
  Continuation_handler.stub h || cont_has_one_occurrence k num_free_occurrences

and let_cont env = function
  | Let_cont.Non_recursive { handler; num_free_occurrences; } ->
      Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
          let h = Non_recursive_let_cont_handler.handler handler in
          if decide_inline_cont h k num_free_occurrences then begin
            let_cont_inline env k h body
          end else
            let_cont_jump env k h body
        )
  | Let_cont.Recursive handlers ->
      Recursive_let_cont_handlers.pattern_match handlers ~f:(fun ~body conts ->
          assert (not (Continuation_handlers.contains_exn_handler conts));
          let_cont_rec env conts body
        )

and let_cont_inline env k h body =
  let args, handler = continuation_handler_split h in
  let env = Env.add_inline_cont env k args handler in
  expr env body

and let_cont_jump env k h body =
  let vars, handle = continuation_handler env h in
  let id, env = Env.add_jump_cont env (List.map snd vars) k in
  C.ccatch
    ~rec_flag:false
    ~body:(expr env body)
    ~handlers:[C.handler id vars handle]

and let_cont_rec env conts body =
  let wrap, env = Env.flush_delayed_lets env in
  (* Compute the environment for jump ids *)
  let map = Continuation_handlers.to_map conts in
  let env = Continuation.Map.fold (fun k h acc ->
      snd (Env.add_jump_cont acc (continuation_arg_tys h) k)
    ) map env in
  (* Translate each continuation handler *)
  let map = Continuation.Map.map (continuation_handler env) map in
  (* Setup the cmm handlers for the static catch *)
  let handlers = Continuation.Map.fold (fun k (vars, handle) acc ->
      let id = Env.get_jump_id env k in
      C.handler id vars handle :: acc
    ) map [] in
  wrap (C.ccatch
          ~rec_flag:true
          ~body:(expr env body)
          ~handlers
       )

and continuation_handler_split h =
  let h = Continuation_handler.params_and_handler h in
  Continuation_params_and_handler.pattern_match h ~f:(fun args ~handler ->
      args, handler
    )

and continuation_arg_tys h =
  let args, _ = continuation_handler_split h in
  List.map machtype_of_kinded_parameter args

and continuation_handler env h =
  let args, handler = continuation_handler_split h in
  let env, vars = var_list env args in
  vars, expr env handler

and apply_expr env e =
  let call, env = apply_call env e in
  let wrap, env = Env.flush_delayed_lets env in
  wrap (wrap_cont env (wrap_exn env call e) e)

and apply_call env e =
  let f = Apply_expr.callee e in
  let dbg = Apply_expr.dbg e in
  match Apply_expr.call_kind e with
  | Call_kind.Function
      Call_kind.Function_call.Direct { closure_id; return_arity; } ->
      let f = Un_cps_closure.(closure_code (closure_name closure_id)) in
      let args, env = arg_list env (Apply_expr.args e) in
      let ty = machtype_of_return_arity return_arity in
      C.direct_call ~dbg ty (C.symbol f) args, env
  | Call_kind.Function
      Call_kind.Function_call.Indirect_unknown_arity ->
      let f, env = simple env f in
      let args, env = arg_list env (Apply_expr.args e) in
      C.indirect_call ~dbg typ_val f args, env
  | Call_kind.Function
      Call_kind.Function_call.Indirect_known_arity { return_arity; _ } ->
      let f, env = simple env f in
      let args, env = arg_list env (Apply_expr.args e) in
      let ty = machtype_of_return_arity return_arity in
      C.indirect_call ~dbg ty f args, env
  | Call_kind.C_call { alloc; return_arity; _ } ->
      let f = function_name f in
      (* CR vlaviron: temporary hack to recover the right symbol *)
      let len = String.length f in
      assert (len >= 9);
      assert (String.sub f 0 9 = ".extern__");
      let f = String.sub f 9 (len - 9) in
      let args, env = arg_list env (Apply_expr.args e) in
      let ty = machtype_of_return_arity return_arity in
      C.extcall ~dbg ~alloc f ty args, env
  | Call_kind.Method { kind; obj; } ->
      let obj, env = simple env obj in
      let meth, env = simple env f in
      let kind = meth_kind kind in
      let args, env = arg_list env (Apply_expr.args e) in
      C.send kind meth obj args dbg, env

and wrap_cont env res e =
  let k = Apply_expr.continuation e in
  if Continuation.equal (Env.return_cont env) k then
    res
  else begin
    match Env.get_k env k with
    | Jump ([], id) -> C.sequence res (C.cexit id [])
    | Jump ([_], id) -> C.cexit id [res]
    | Inline ([], body) ->
        C.sequence res (expr env body)
    | Inline ([v], body) ->
        let var = Kinded_parameter.var v in
        (* Function calls can have any effects and coeffects by default.
           CR Gbury: maybe annotate calls with effects ? *)
        let effs = Effects_and_coeffects.all in
        let env = let_expr_bind body env var res effs in
        expr env body
    | Jump _
    | Inline _ ->
        (* TODO: add support using unboxed tuples *)
        Misc.fatal_errorf
          "Continuation %a should not handle multiple return values in@\n%a@\n%s"
          Continuation.print k Apply_expr.print e
          "Multi-arguments continuation across function calls are not yet supported"
  end

and wrap_exn env res e =
  let k_exn = Apply_expr.exn_continuation e in
  let k_exn = Exn_continuation.exn_handler k_exn in
  if Continuation.equal (Env.exn_cont env) k_exn then
    res
  else begin
    match Env.get_k env k_exn with
    | Jump ([_], id) ->
        let v = Backend_var.create_local "exn_var" in
        let exn_var = Backend_var.With_provenance.create v in
        C.trywith
          ~dbg:Debuginfo.none
          ~body:res ~exn_var
          ~handler:(C.cexit id [C.var v])
    (* CR mshinwell: This will need to support exception continuations
       with any number of parameters (including zero).  For the moment I've
       stopped the simplifier from deleting parameters of exception
       continuations, but this will still go wrong if it adds some (e.g. as
       a result of mutable variable conversion). *)
    | Inline ([v], h) ->
        let var = Kinded_parameter.var v in
        let env, exn_var = Env.create_variable env var in
        let handler = expr env h in
        C.trywith ~dbg:Debuginfo.none ~body:res ~exn_var ~handler
    | Jump _
    | Inline _ ->
        Misc.fatal_errorf
          "Exception continuation %a should take exactly one argument:@ %a"
          Continuation.print k_exn
          Apply_expr.print e
  end

and apply_cont env e =
  let k = Apply_cont_expr.continuation e in
  let args = Apply_cont_expr.args e in
  if Continuation.equal (Env.exn_cont env) k then begin
    match args with
    | [res] ->
        let exn, env = simple env res in
        let wrap, _ = Env.flush_delayed_lets env in
        wrap (C.raise_regular Debuginfo.none exn)
    | _ ->
        Misc.fatal_errorf
          "Continuation %a (exn cont) should be applied to a single argument in@\n%a@\n%s"
          Continuation.print k Apply_cont_expr.print e
          "Exception continuations should only applied to a single argument"
  end else if Continuation.equal (Env.return_cont env) k then begin
    match args with
    | [] -> C.void
    | [res] ->
        let res, env = simple env res in
        let wrap, _ = Env.flush_delayed_lets env in
        wrap res
    | _ ->
        (* TODO: add support using unboxed tuples *)
        Misc.fatal_errorf
          "Continuation %a (return cont) should be applied to a single argument in@\n%a@\n%s"
          Continuation.print k Apply_cont_expr.print e
          "Multi-arguments continuation across function calls are not yet supported"
  end else begin
    match Env.get_k env k with
    | Jump (tys, id) ->
        (* The provided args should match the types in tys *)
        assert (List.compare_lengths tys args = 0);
        let args, env = arg_list env args in
        let wrap, _ = Env.flush_delayed_lets env in
        wrap (C.cexit id args)
    | Inline (l, body) ->
        if not (List.length args = List.length l) then
          Misc.fatal_errorf
            "Continuation %a in@\n%a@\nExpected %d arguments but got %a."
            Continuation.print k Apply_cont_expr.print e
            (List.length l) Apply_cont_expr.print e;
        let vars = List.map Kinded_parameter.var l in
        let args = List.map Named.create_simple args in
        let env = List.fold_left2 (let_expr_env body) env vars args in
        expr env body
  end

and switch env s =
  let e, env = simple env (Switch.scrutinee s) in
  let wrap, env = Env.flush_delayed_lets env in
  let ints, exprs =
    Discriminant.Map.fold (fun d k (ints, exprs) ->
      let i = Targetint.OCaml.to_int (Discriminant.to_int d) in
      let e = match Env.get_k env k with
        | Jump ([], id) -> C.cexit id []
        | Inline ([], body) -> expr env body
        | Jump _
        | Inline _ ->
            Misc.fatal_errorf
              "In@\n%a@\nSwitch branches should be goto (zero arguments) continuations"
              Switch.print s
      in
      (i :: ints, e :: exprs)
      ) (Switch.arms s) ([], [])
  in
  let ints = Array.of_list (List.rev ints) in
  let exprs = Array.of_list (List.rev exprs) in
  assert (Array.length ints = Array.length exprs);
  match ints, exprs with
  | [| 0; 1 |], [| else_; then_ |] ->
      (* This switch is actually an if-then-else.
         On such switches, transl_switch_clambda will actually generate
         code that compare the scrutinee with 0 (or 1), whereas directly
         generating an if-then-else on the scrutinee is better
         (avoid a comparison, and even let selectgen/emit optimize away
         the move from the condition register to a regular register). *)
      wrap (C.ite e ~then_ ~else_)
  | _ ->
      if Misc.Stdlib.Array.for_alli (=) ints then
        wrap (C.transl_switch_clambda Location.none e ints exprs)
      else begin
        (* Add an unreachable case to the cases array *)
        let c = Array.length exprs in
        let cases = Array.append exprs [| C.unreachable |] in
        (* The transl_switch_clambda expects an index array such that
           index.(i) is the index in [cases] of the expression to
           execute when [e] matches [i]. *)
        let d, _ = Discriminant.Map.max_binding (Switch.arms s) in
        let n = Targetint.OCaml.to_int (Discriminant.to_int d) in
        let index = Array.make (n + 2) c in
        Array.iteri (fun i j -> index.(j) <- i) ints;
        wrap (C.transl_switch_clambda Location.none e index cases)
      end

and invalid _env _e =
  C.unreachable

and set_of_closures env s =
  let fun_decls = Set_of_closures.function_decls s in
  let decls = Function_declarations.funs fun_decls in
  let elts = Set_of_closures.closure_elements s in
  let layout = Env.layout env
      (List.map fst (Closure_id.Map.bindings decls))
      (List.map fst (Var_within_closure.Map.bindings elts))
  in
  let l, env = fill_layout decls elts env [] 0 layout in
  C.make_closure_block l, env

and fill_layout decls elts env acc i = function
  | [] -> List.rev acc, env
  | (j, slot) :: r ->
      let acc = fill_up_to j acc i in
      let acc, offset, env = fill_slot decls elts env acc j slot in
      fill_layout decls elts env acc offset r

and fill_slot decls elts env acc offset slot =
  match (slot : Un_cps_closure.layout_slot) with
  | Infix_header ->
      let field = C.alloc_infix_header offset Debuginfo.none in
      field :: acc, offset + 1, env
  | Env_var v ->
      let field, env = simple env (Var_within_closure.Map.find v elts) in
      field :: acc, offset + 1, env
  | Closure (c : Closure_id.t) ->
      let c : Closure_id.t = c in
      let decl = Closure_id.Map.find c decls in
      let dbg = Function_declaration.dbg decl in
      let arity = List.length (Function_declaration.params_arity decl) in
      let name = Un_cps_closure.(closure_code (closure_name c)) in
      (* We build here the **reverse** list of fields for the closure *)
      if arity = 1 || arity = 0 then begin
        let acc =
          C.int_const dbg arity ::
          C.symbol ~dbg name ::
          acc
        in
        acc, offset + 2, env
      end else begin
        let acc =
          C.symbol ~dbg name ::
          C.int_const dbg arity ::
          C.symbol ~dbg (C.curry_function_sym arity) ::
          acc
        in
        acc, offset + 3, env
      end

and fill_up_to j acc i =
  assert (i <= j);
  if i = j then acc
  else fill_up_to j (C.int 1 :: acc) (i + 1)

(* Static structures *)

let static_value _env v =
  match (v : Flambda_static.Of_kind_value.t) with
  | Symbol s -> C.symbol_address (symbol s)
  | Dynamically_computed _ -> C.cint 1n
  | Tagged_immediate i ->
      C.cint (nativeint_of_targetint (tag_targetint (targetint_of_imm i)))

let or_variable f default v cont =
  match (v : _ Flambda_static.Static_part.or_variable) with
  | Const c -> f c cont
  | Var _ -> f default cont

let map_or_variable f default v =
  match (v : _ Flambda_static.Static_part.or_variable) with
  | Const c -> f c
  | Var _ -> default

let make_update env kind symb var i =
  let e = Env.get_variable env var in
  let address = C.field_address symb i Debuginfo.none in
  C.store kind Lambda.Root_initialization address e

let rec static_block_updates symb env acc i = function
  | [] -> List.fold_left C.sequence C.void acc
  | sv :: r ->
      begin match (sv : Flambda_static.Of_kind_value.t) with
      | Symbol _
      | Tagged_immediate _ ->
          static_block_updates symb env acc (i + 1) r
      | Dynamically_computed var ->
          let update = make_update env Cmm.Word_val symb var i in
          static_block_updates symb env (update :: acc) (i + 1) r
      end

let rec static_float_array_updates symb env acc i = function
  | [] -> List.fold_left C.sequence C.void acc
  | sv :: r ->
      begin match (sv : _ Flambda_static.Static_part.or_variable) with
      | Const _ ->
          static_float_array_updates symb env acc (i + 1) r
      | Var var ->
          let update = make_update env Cmm.Double_u symb var i in
          static_float_array_updates symb env (update :: acc) (i + 1) r
      end

let static_boxed_number kind env s default emit transl v r =
  let name = symbol s in
  let aux x cont = emit (name, Cmmgen_state.Global) (transl x) cont in
  let wrapper =
    match (v : _ Flambda_static.Static_part.or_variable) with
    | Const _ -> Fun.id
    | Var v ->
        let update = make_update env kind (C.symbol name) v 0 in
        C.sequence update
  in
  R.wrap_init wrapper (R.update_data (or_variable aux default v) r)

let rec static_set_of_closures env symbs set =
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs fun_decls in
  let elts = Set_of_closures.closure_elements set in
  let layout = Env.layout env
      (List.map fst (Closure_id.Map.bindings decls))
      (List.map fst (Var_within_closure.Map.bindings elts))
  in
  let l, updates, length =
    fill_static_layout symbs decls elts env [] C.void 0 layout
  in
  C.cint (C.black_closure_header length) :: l, updates

and fill_static_layout symbs decls elts env acc updates i = function
  | [] -> List.rev acc, updates, i
  | (j, slot) :: r ->
      let acc = fill_static_up_to j acc i in
      let acc, offset, updates =
        fill_static_slot symbs decls elts env acc j updates slot
      in
      fill_static_layout symbs decls elts env acc updates offset r

and fill_static_slot symbs decls elts env acc offset updates slot =
  match (slot : Un_cps_closure.layout_slot) with
  | Infix_header ->
      let field = C.cint (C.infix_header offset) in
      field :: acc, offset + 1, updates
  | Env_var v ->
      let fields, updates =
        match simple_static env (Var_within_closure.Map.find v elts) with
        | `Data fields -> fields, updates
        | `Var _v -> todo ()
      in
      List.rev fields @ acc, offset + 1, updates
  | Closure c ->
      let decl = Closure_id.Map.find c decls in
      let symb = Closure_id.Map.find c symbs in
      let name = symbol symb in
      let acc = List.rev (C.define_symbol ~global:true name) @ acc in
      let arity = List.length (Function_declaration.params_arity decl) in
      let tagged_arity = arity * 2 + 1 in
      (* We build here the **reverse** list of fields for the closure *)
      if arity = 1 || arity = 0 then begin
        let acc =
          C.cint (Nativeint.of_int tagged_arity) ::
          C.symbol_address (Un_cps_closure.closure_code name) ::
          acc
        in
        acc, offset + 2, updates
      end else begin
        let acc =
          C.symbol_address (Un_cps_closure.closure_code name) ::
          C.cint (Nativeint.of_int tagged_arity) ::
          C.symbol_address (C.curry_function_sym arity) ::
          acc
        in
        acc, offset + 3, updates
      end

and fill_static_up_to j acc i =
  if i = j then acc
  else fill_static_up_to j (C.cint 1n :: acc) (i + 1)

let static_structure_item (type a) env r (symb, st) =
  match (symb : a Flambda_static.Program_body.Bound_symbols.t),
        (st : a Flambda_static.Static_part.t) with
  | Singleton s, Block (tag, _mut, fields) ->
      let name = symbol s in
      let tag = Tag.Scannable.to_int tag in
      let block_name = name, Cmmgen_state.Global in
      let header = C.block_header tag (List.length fields) in
      let static_fields = List.map (static_value env) fields in
      let block = C.emit_block block_name header static_fields in
      let e = static_block_updates (C.symbol name) env [] 0 fields in
      R.wrap_init (C.sequence e) (R.add_data block r)
  | Singleton _, Fabricated_block _ ->
      todo()
  | Set_of_closures s, Set_of_closures set ->
      let data, updates =
        (* CR mshinwell: Which symbol should be chosen instead of
           the set of closures symbol, which now doesn't exist? *)
        (* CR vlaviron: The symbol is only needed if we need to update some
           fields after allocation. For now, I'll assume it never happens. *)
        static_set_of_closures env s.closure_symbols set
      in
      R.wrap_init (C.sequence updates) (R.add_data data r)
  | Singleton s, Boxed_float v ->
      let default = Numbers.Float_by_bit_pattern.zero in
      let transl = Numbers.Float_by_bit_pattern.to_float in
      static_boxed_number
        Cmm.Double_u env s default C.emit_float_constant transl v r
  | Singleton s, Boxed_int32 v ->
      static_boxed_number
        Cmm.Word_int env s 0l C.emit_int32_constant Fun.id v r
  | Singleton s, Boxed_int64 v ->
      static_boxed_number
        Cmm.Word_int env s 0L C.emit_int64_constant Fun.id v r
  | Singleton s, Boxed_nativeint v ->
      let default = Targetint.zero in
      let transl = nativeint_of_targetint in
      static_boxed_number
        Cmm.Word_int env s default C.emit_nativeint_constant transl v r
  | Singleton s, Immutable_float_array fields ->
      let name = symbol s in
      let aux = map_or_variable Numbers.Float_by_bit_pattern.to_float 0. in
      let static_fields = List.map aux fields in
      let float_array =
        C.emit_float_array_constant (name, Cmmgen_state.Global) static_fields
      in
      let e = static_float_array_updates (C.symbol name) env [] 0 fields in
      R.wrap_init (C.sequence e) (R.update_data float_array r)
  | Singleton s, Mutable_string { initial_value; }
  | Singleton s, Immutable_string initial_value ->
      let name = symbol s in
      begin match initial_value with
      | Var _ ->
          (* CR vlaviron: this doesn't make sense, strings
             can't be initialized without knowing their length *)
          Misc.fatal_errorf "Trying to initialize a string of unknown length"
      | Const str ->
          let data = C.emit_string_constant (name, Cmmgen_state.Global) str in
          R.update_data data r
      end

let static_structure env s =
  let S l = (s : Flambda_static.Program_body.Static_structure.t) in
  List.fold_left (static_structure_item env) R.empty l

(* Definition *)

let computation_wrapper offsets c =
  match c with
  | None ->
      Env.dummy offsets, (fun x -> x)
  | Some (c : Flambda_static.Program_body.Computation.t) ->
      (* The env for the computation is given a dummy continuation,
         since the return continuation will be bound in the env. *)
      let dummy_k = Continuation.create () in
      let k_exn = Exn_continuation.exn_handler c.exn_continuation in
      let c_env = Env.mk offsets dummy_k k_exn in
      (* The environment for the static structure update must contain the
         variables produced by the computation *)
      let s_env = Env.mk offsets dummy_k dummy_k in
      let s_env, vars = var_list s_env c.computed_values in
      (* Wrap the static structure update expression [e] by manually
         translating the computation return continuation by a jump to
         [e]. In the case of a single-use continuation, using a jump
         instead of inlining [e] at the continuation call site does not
         change much, since - assuming the continuation is called at the
         end of the body - the jump will be erased in linearize. *)
      let wrap (e : Cmm.expression) =
        let k = c.return_continuation in
        let tys = List.map snd vars in
        let id, env = Env.add_jump_cont c_env tys k in
        let body = expr env c.expr in
        C.ccatch
          ~rec_flag:false ~body
          ~handlers:[C.handler id vars e]
      in
      (* CR gbury: for the future, try and rearrange the generated cmm
         code to move assignments closer to the variable definitions
         Or better: add traps to the env to insert assignemnts after
         the variable definitions. *)
      s_env, wrap

let definition offsets (d : Flambda_static.Program_body.Definition.t) =
  let env, wrapper = computation_wrapper offsets d.computation in
  let r = static_structure env d.static_structure in
  R.wrap_init wrapper r


(* Translate a function declaration. *)

let is_var_used v e =
  let free_names = Expr.free_names e in
  let occurrence = Name_occurrences.greatest_occurrence_kind_var free_names v in
  match (occurrence : Name_occurrence_kind.Or_absent.t) with
  | Absent -> false
  | Present k -> Name_occurrence_kind.is_normal k

let function_args vars my_closure body =
  if is_var_used my_closure body then begin
    let param = Parameter.wrap my_closure in
    let last_arg = Kinded_parameter.create param Flambda_kind.value in
    vars @ [last_arg]
  end else
    vars

let function_flags () =
  if !Clflags.optimize_for_speed then
    []
  else
    [ Cmm.Reduce_code_size ]

let function_decl offsets fun_name _ d =
  let fun_dbg = Function_declaration.dbg d in
  let p = Function_declaration.params_and_body d in
  Function_params_and_body.pattern_match p
    ~f:(fun ~return_continuation:k k_exn vars ~body ~my_closure ->
        try
          let args = function_args vars my_closure body in
          let k_exn = Exn_continuation.exn_handler k_exn in
          let env = Env.mk offsets k k_exn in
          let env, fun_args = var_list env args in
          let fun_body = expr env body in
          let fun_flags = function_flags () in
          C.fundecl fun_name fun_args fun_body fun_flags fun_dbg
        with Misc.Fatal_error ->
          Format.eprintf "\n%sContext is:%s translating function %s to Cmm \
              with body@ %a\n"
            (Flambda_colours.error ())
            (Flambda_colours.normal ())
            fun_name
            Expr.print body;
          raise Misc.Fatal_error)

(* Programs *)

let rec program_body offsets acc body =
  match Flambda_static.Program_body.descr body with
  | Flambda_static.Program_body.Root sym ->
      sym, List.fold_left (fun acc r -> R.combine r acc) R.empty acc
  | Flambda_static.Program_body.Define_symbol (def, rest) ->
      let r = definition offsets def in
      program_body offsets (r :: acc) rest

let program_functions offsets p =
  let fmap = Un_cps_closure.map_on_function_decl (function_decl offsets) p in
  let all_functions = Closure_id.Map.fold (fun _ x acc -> x :: acc) fmap [] in
  (* This is to keep the current cmmgen behaviour which sorts functions by
     debuginfo (and thus keeps the order of declaration). *)
  let sorted = List.sort
      (fun f f' -> Debuginfo.compare f.Cmm.fun_dbg f'.Cmm.fun_dbg) all_functions
  in
  List.map (fun decl -> C.cfunction decl) sorted

let program (p : Flambda_static.Program.t) =
  let offsets = Un_cps_closure.compute_offsets p in
  let functions = program_functions offsets p in
  let sym, res = program_body offsets [] p.body in
  let data, entry = R.to_cmm res in
  let cmm_data = C.flush_cmmgen_state () in
  (C.gc_root_table [symbol sym]) :: data @ cmm_data @ functions @ [entry]

