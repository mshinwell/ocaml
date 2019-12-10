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

open! Flambda.Import

module Env = Un_cps_env
module Ece = Effects_and_coeffects

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
    gc_roots : Symbol.t list;
  }

  let empty = {
    init = C.void;
    current_data = [];
    other_data = [];
    gc_roots = [];
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
    gc_roots = r.gc_roots @ t.gc_roots;
  }

  let archive_data r =
    { r with current_data = [];
             other_data = add_if_not_empty r.current_data r.other_data; }

  let wrap_init f r =
    { r with init = f r.init; }

  let add_data d r =
    { r with current_data = d @ r.current_data; }

  let update_data f r =
    { r with current_data = f r.current_data; }

  let add_gc_roots l r =
    { r with gc_roots = l @ r.gc_roots; }

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
    data, entry, r.gc_roots

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
  | Name.Symbol s -> C.symbol (symbol s), env, Ece.pure

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

let default_of_kind (k : Flambda_kind.t) =
  match k with
  | Value -> C.int 1
  | Naked_number Naked_immediate -> C.int 0
  | Naked_number Naked_float -> C.float 0.
  | Naked_number Naked_int32 -> C.int 0
  | Naked_number Naked_int64 when C.arch32 -> todo ()
  | Naked_number Naked_int64 -> C.int 0
  | Naked_number Naked_nativeint -> C.int 0
  | Fabricated -> Misc.fatal_error "Fabricated_kind have no default value"

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
  | Const c -> const env c, env, Ece.pure

let simple_static env s =
  match (Simple.descr s : Simple.descr) with
  | Name n -> name_static env n
  | Const c -> `Data (const_static env c)

(* Arithmetic primitives *)

let primitive_boxed_int_of_standard_int x =
  match (x : Flambda_kind.Standard_int.t) with
  | Naked_int32 -> Primitive.Pint32
  | Naked_int64 -> Primitive.Pint64
  | Naked_nativeint -> Primitive.Pnativeint
  | Tagged_immediate -> assert false
  | Naked_immediate -> assert false

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
  | Naked_int64, (Naked_nativeint | Naked_immediate) when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_to_nativeint" typ_int [arg]
  | Naked_int64, Naked_float when C.arch32 ->
      C.extcall ~alloc:false "caml_int64_to_float_unboxed" typ_float [arg]
  | Tagged_immediate, Naked_int64 when C.arch32 ->
      C.extcall ~alloc:true "caml_int64_of_int" typ_val [arg]
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | Naked_int32, Naked_int64 when C.arch32 ->
      C.extcall ~alloc:true "caml_int64_of_int32" typ_val [arg]
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | (Naked_nativeint | Naked_immediate), Naked_int64 when C.arch32 ->
      C.extcall ~alloc:true "caml_int64_of_nativeint" typ_val [arg]
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  | Naked_float, Naked_int64 when C.arch32 ->
      C.extcall ~alloc:true "caml_int64_of_float_unboxed" typ_val [arg]
      |> C.unbox_number ~dbg Flambda_kind.Boxable_number.Naked_int64
  (* Identity on floats *)
  | Naked_float, Naked_float -> arg
  (* general cases between integers *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate),
    Tagged_immediate ->
      C.tag_int arg dbg
  | Tagged_immediate,
    (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate) ->
      C.untag_int arg dbg
  (* TODO: insert shifts to zero-out higher-order bits during
           the 64 to 32 bit conversion ? *)
  | Tagged_immediate, Tagged_immediate
  | Naked_int32, (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_int64, (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_nativeint,
    (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate)
  | Naked_immediate,
    (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate) ->
      arg
  (* Int-Float conversions *)
  | Tagged_immediate, Naked_float ->
      C.float_of_int ~dbg (C.untag_int arg dbg)
  | (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint),
    Naked_float ->
      C.float_of_int ~dbg arg
  | Naked_float, Tagged_immediate ->
      C.tag_int (C.int_of_float ~dbg arg) dbg
  | Naked_float,
    (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint) ->
      C.int_of_float ~dbg arg

let binary_phys_comparison _env dbg kind op x y =
  match (kind : Flambda_kind.t),
        (op : Flambda_primitive.equality_comparison) with
  (* int64 special case *)
  | Naked_number Naked_int64, Eq when C.arch32 ->
      C.untag_int
        (C.extcall ~alloc:true "caml_equal" typ_int
          [C.box_int64 ~dbg x; C.box_int64 ~dbg y])
        dbg
  | Naked_number Naked_int64, Neq when C.arch32 ->
      C.untag_int
        (C.extcall ~alloc:true "caml_notequal" typ_int
          [C.box_int64 ~dbg x; C.box_int64 ~dbg y])
        dbg
  (* General case *)
  | _, Eq -> C.eq ~dbg x y
  | _, Neq -> C.neq ~dbg x y

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
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Add ->
      C.add_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Sub ->
      C.sub_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Mul ->
      C.mul_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), And ->
      C.and_ ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Or ->
      C.or_ ~dbg x y
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Xor ->
      C.xor_ ~dbg x y
  (* Division and modulo need some extra care *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Div ->
      let bi = C.primitive_boxed_int_of_standard_int kind in
      C.safe_div_bi Lambda.Unsafe x y bi dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Mod ->
      let bi = C.primitive_boxed_int_of_standard_int kind in
      C.safe_mod_bi Lambda.Unsafe x y bi dbg

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
  (* Int32 special case *)
  | Naked_int32, Lsr when C.arch64 ->
      C.asr_int (C.zero_extend_32 dbg x) y dbg
  (* Tagged integers *)
  | Tagged_immediate, Lsl -> C.lsl_int_caml_raw ~dbg x y
  | Tagged_immediate, Lsr -> C.lsr_int_caml_raw ~dbg x y
  | Tagged_immediate, Asr -> C.asr_int_caml_raw ~dbg x y
  (* Naked ints *)
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Lsl ->
      C.lsl_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Lsr ->
      C.lsr_int x y dbg
  | (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate), Asr ->
      C.asr_int x y dbg

let binary_int_comp_primitive _env dbg kind signed cmp x y =
  match (kind : Flambda_kind.Standard_int.t),
        (signed : Flambda_primitive.signed_or_unsigned),
        (cmp : Flambda_primitive.ordered_comparison) with
  (* XXX arch32 cases need [untag_int] now. *)
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
  | Tagged_immediate, Signed, Lt -> C.lt ~dbg x y
  | Tagged_immediate, Signed, Le -> C.le ~dbg x y
  | Tagged_immediate, Signed, Gt -> C.gt ~dbg x y
  | Tagged_immediate, Signed, Ge -> C.ge ~dbg x y
  | Tagged_immediate, Unsigned, Lt -> C.ult ~dbg x y
  | Tagged_immediate, Unsigned, Le -> C.ule ~dbg x y
  | Tagged_immediate, Unsigned, Gt -> C.ugt ~dbg x y
  | Tagged_immediate, Unsigned, Ge -> C.uge ~dbg x y
  (* Naked integers. *)
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Lt ->
      C.lt ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Le ->
      C.le ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Gt ->
      C.gt ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Signed, Ge ->
      C.ge ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Lt ->
      C.ult ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Le ->
      C.ule ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Gt ->
      C.ugt ~dbg x y
  | (Naked_int32|Naked_int64|Naked_nativeint|Naked_immediate), Unsigned, Ge ->
      C.uge ~dbg x y

let binary_float_arith_primitive _env dbg op x y =
  match (op : Flambda_primitive.binary_float_arith_op) with
  | Add -> C.float_add ~dbg x y
  | Sub -> C.float_sub ~dbg x y
  | Mul -> C.float_mul ~dbg x y
  | Div -> C.float_div ~dbg x y

let binary_float_comp_primitive _env dbg op x y =
  match (op : Flambda_primitive.comparison) with
  | Eq -> C.float_eq ~dbg x y
  | Neq -> C.float_neq ~dbg x y
  | Lt -> C.float_lt ~dbg x y
  | Gt -> C.float_gt ~dbg x y
  | Le -> C.float_le ~dbg x y
  | Ge -> C.float_ge ~dbg x y

(* Primitives *)

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
      C.load ~dbg Cmm.Word_int Asttypes.Mutable
        (C.field_address arg (4 + dimension) dbg)
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
  | Project_var { project_from; var; } ->
      let offset = Env.env_var_offset env var in
      let base = Env.closure_offset env project_from in
      C.get_field_gen Asttypes.Immutable arg (offset - base) dbg

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
  | Bigarray_load (_is_safe, dimensions, Unknown, _)
  | Bigarray_load (_is_safe, dimensions, _, Unknown) ->
      let f = "caml_ba_get_" ^ string_of_int dimensions in
      C.extcall ~dbg ~alloc:true f typ_val args
  | Bigarray_set (_is_safe, dimensions, Unknown, _)
  | Bigarray_set (_is_safe, dimensions, _, Unknown) ->
      let f = "caml_ba_set_" ^ string_of_int dimensions in
      C.extcall ~dbg ~alloc:true f typ_val args
  | Bigarray_load (is_safe, dimensions, kind, layout) ->
      C.bigarray_load ~dbg is_safe dimensions kind layout args
  | Bigarray_set (is_safe, dimensions, kind, layout) ->
      C.bigarray_store ~dbg is_safe dimensions kind layout args

let arg_list env l =
  let aux (list, env, effs) x =
    let y, env, eff = simple env x in
    (y :: list, env, Ece.join eff effs)
  in
  let args, env, effs = List.fold_left aux ([], env, Ece.pure) l in
  List.rev args, env, effs

(* CR Gbury: check the order in which the primitive arguments are
             given to [Env.inline_variable]. *)
let prim env dbg p =
  match (p : Flambda_primitive.t) with
  | Unary (f, x) ->
      let x, env, eff = simple env x in
      unary_primitive env dbg f x, env, eff
  | Binary (f, x, y) ->
      let x, env, effx = simple env x in
      let y, env, effy = simple env y in
      let effs = Ece.join effx effy in
      binary_primitive env dbg f x y, env, effs
  | Ternary (f, x, y, z) ->
      let x, env, effx = simple env x in
      let y, env, effy = simple env y in
      let z, env, effz = simple env z in
      let effs = Ece.join (Ece.join effx effy) effz in
      ternary_primitive env dbg f x y z, env, effs
  | Variadic (f, l) ->
      let args, env, effs = arg_list env l in
      variadic_primitive env dbg f args, env, effs

(* Kinds and types *)

let machtype_of_kind k =
  match (k  : Flambda_kind.t) with
  | Value -> typ_val
  | Naked_number Naked_float -> typ_float
  | Naked_number Naked_int64 -> typ_int64
  | Naked_number (Naked_immediate | Naked_int32 | Naked_nativeint) ->
      typ_int
  | Fabricated -> assert false

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

(* Closure variables *)

let filter_closure_vars env s =
  let used_closure_vars = Env.used_closure_vars env in
  let aux clos_var _bound_to =
    Var_within_closure.Set.mem clos_var used_closure_vars
  in
  Var_within_closure.Map.filter aux s


(* Function calls and continuations *)

let var_list env l =
  let flambda_vars = List.map Kinded_parameter.var l in
  let env, cmm_vars = Env.create_variables env flambda_vars in
  let vars = List.map2 (fun v v' ->
      (v, machtype_of_kinded_parameter v')
    ) cmm_vars l in
  env, vars

let split_exn_cont_args k = function
  | (v, _) :: rest ->
      v, rest
  | [] ->
      Misc.fatal_errorf
        "Exception continuation %a should have at least one argument"
        Continuation.print k

(* effects and co-effects *)

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
  | Set_of_closures s -> set_of_closures env s
  | Prim (p, dbg) ->
      let prim_eff = Flambda_primitive.effects_and_coeffects p in
      let t, env, effs = prim env dbg p in
      t, env, Ece.join effs prim_eff

and let_expr env t =
  Let.pattern_match t ~f:(fun ~bound_vars ~body ->
      let e = Let.defining_expr t in
      match bound_vars, e with
      | Singleton v, _ ->
          let v = Var_in_binding_pos.var v in
          let_expr_aux env v e body
      | Set_of_closures { closure_vars; _ }, Set_of_closures soc ->
          let_set_of_closures env body closure_vars soc
      | Set_of_closures _, (Simple _ | Prim _) ->
          Misc.fatal_errorf
            "Set_of_closures binding a non-Set_of_closures:@ %a"
            Let.print t
    )

and let_set_of_closures env body closure_vars soc =
  (* First translate the set of closures, and bind it in the env *)
  let csoc, env, effs = set_of_closures env soc in
  let soc_var = Variable.create "*set_of_closures*" in
  let env = Env.bind_variable env soc_var effs false csoc in
  (* Then get from the env the cmm variable that was created and bound
     to the compiled set of closures. *)
  let soc_cmm_var, env, peff = Env.inline_variable env soc_var in
  assert (Ece.is_pure peff);
  (* Add env bindings for all the closure variables. *)
  let effs = Effects_and_coeffects.read in
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

and let_expr_bind body env v cmm_expr effs =
  match decide_inline_let effs v body with
  | Skip -> env
  | Inline -> Env.bind_variable env v effs true cmm_expr
  | Regular -> Env.bind_variable env v effs false cmm_expr

and let_expr_env body env v e =
  let cmm_expr, env, effs = named env e in
  match decide_inline_let effs v body with
  | Skip -> env
  | Inline -> Env.bind_variable env v effs true cmm_expr
  | Regular -> Env.bind_variable env v effs false cmm_expr

and let_expr_aux env v e body =
  let env = let_expr_env body env v e in
  expr env body

and decide_inline_cont h k num_free_occurrences =
  not (Continuation_handler.is_exn_handler h)
  && (Continuation_handler.stub h
      || cont_has_one_occurrence k num_free_occurrences)

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

(* The bound continuation [k] will try to be inlined. However, if [k] occurs in a
   trap action, it is not possible to inline it. In those cases, we need to translate
   [k] as a jump. For that purpose, a reference is used to record whether [k] is used
   in a trap action, or rather, if [k] is used as a jump continuation, in which case,
   a handler and a ccatch must be computed. *)
and let_cont_inline env k h body =
  let args, handler = continuation_handler_split h in
  let env = Env.add_inline_cont env k args handler in
  expr env body

(* Continuations that are not inlined are translated using a jump:
   - exceptions continuations use "dynamic" jumps using the
     raise/trywith cmm mechanism
   - regular continuations use static jumps, through the
     exit/catch cmm mechanism *)
(* CR Gbury: "split" the environment according to which variables the
             handler and the body uses, to allow for inlining to proceed
             within each expression. *)
and let_cont_jump env k h body =
  let wrap, env = Env.flush_delayed_lets env in
  let vars, handle = continuation_handler env h in
  let id, env = Env.add_jump_cont env (List.map snd vars) k in
  if Continuation_handler.is_exn_handler h then
    wrap (let_cont_exn env k h body vars handle id)
  else
    wrap (C.ccatch
            ~rec_flag:false
            ~body:(expr env body)
            ~handlers:[C.handler id vars handle]
         )

(* Exception continuations, translated using delayed trywith blocks.
   Additionally, exn continuations in flambda2 can have extra args, which
   are passed through the trywith using mutable cmm variables. Thus the
   exn handler must first read the contents of thos extra args (eagerly
   in order to minmize the lifetime of the mutable variables) *)
and let_cont_exn env k h body vars handle id =
  let exn_var, extra_params = split_exn_cont_args k vars in
  let env_body, extra_vars = Env.add_exn_handler env k h in
  let handler = exn_handler handle extra_vars extra_params in
  let trywith =
    C.trywith
      ~dbg:Debuginfo.none
      ~kind:(Delayed id)
      ~body:(expr env_body body)
      ~exn_var
      ~handler:handler
  in
  wrap_let_cont_exn_body trywith extra_vars

(* wrap a exn handler with read of the mutable variables *)
and exn_handler handle extra_vars extra_params =
  List.fold_left2
    (fun handler (v, _) (p, _) -> C.letin p (C.var v) handler)
    handle extra_vars extra_params

(* define and initialize the mutable cmm variables used by an exn extra args *)
and wrap_let_cont_exn_body handler extra_vars =
  List.fold_left (fun expr (v, k) ->
      let v = Backend_var.With_provenance.create v in
      C.letin v (default_of_kind k) expr
    ) handler extra_vars

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

(* Function calls: besides the function calls, there are a few things to do:
   - setup the mutable variables for the exn cont extra args if needed
   - Flush the delayed let-bindings (this is not stricly necessary)
   - translate the call continuation (either through a jump, or inlining). *)
and apply_expr env e =
  let call, env, effs = apply_call env e in
  let k_exn = Apply_expr.exn_continuation e in
  let call, env = wrap_call_exn env e call k_exn in
  let wrap, env = Env.flush_delayed_lets env in
  wrap (wrap_cont env effs call e)

(* Bare function calls *)
and apply_call env e =
  let f = Apply_expr.callee e in
  let dbg = Apply_expr.dbg e in
  let effs = Ece.all in
  match Apply_expr.call_kind e with
  (* Effects from arguments are ignored since a function call will always be
     given arbitrary effects and coeffects. *)
  | Call_kind.Function
      Call_kind.Function_call.Direct { closure_id; return_arity; } ->
      let f_code = Un_cps_closure.(closure_code (closure_name closure_id)) in
      let f, env, _ = simple env f in
      let args, env, _ = arg_list env (Apply_expr.args e) in
      let ty = machtype_of_return_arity return_arity in
      C.direct_call ~dbg ty (C.symbol f_code) args f, env, effs
  | Call_kind.Function
      Call_kind.Function_call.Indirect_unknown_arity ->
      let f, env, _ = simple env f in
      let args, env, _ = arg_list env (Apply_expr.args e) in
      C.indirect_call ~dbg typ_val f args, env, effs
  | Call_kind.Function
      Call_kind.Function_call.Indirect_known_arity { return_arity; _ } ->
      let f, env, _ = simple env f in
      let args, env, _ = arg_list env (Apply_expr.args e) in
      let ty = machtype_of_return_arity return_arity in
      C.indirect_call ~dbg ty f args, env, effs
  | Call_kind.C_call { alloc; return_arity; _ } ->
      let f = function_name f in
      (* CR vlaviron: temporary hack to recover the right symbol *)
      let len = String.length f in
      assert (len >= 9);
      assert (String.sub f 0 9 = ".extern__");
      let f = String.sub f 9 (len - 9) in
      let args, env, _ = arg_list env (Apply_expr.args e) in
      let ty = machtype_of_return_arity return_arity in
      C.extcall ~dbg ~alloc f ty args, env, effs
  | Call_kind.Method { kind; obj; } ->
      let obj, env, _ = simple env obj in
      let meth, env, _ = simple env f in
      let kind = meth_kind kind in
      let args, env, _ = arg_list env (Apply_expr.args e) in
      C.send kind meth obj args dbg, env, effs

(* function calls that have an exn continuation with extra arguments
   must be wrapped with assignments for the mutable variables used
   to pass the extra arguments. *)
and wrap_call_exn env e call k_exn =
  let h_exn = Exn_continuation.exn_handler k_exn in
  let mut_vars = Env.get_exn_extra_args env h_exn in
  let extra_args = Exn_continuation.extra_args k_exn in
  if List.compare_lengths extra_args mut_vars = 0 then begin
    let aux (call, env) (arg, _k) v =
      let arg, env, _ = simple env arg in
      C.sequence (C.assign v arg) call, env
    in
    List.fold_left2 aux (call, env) extra_args mut_vars
  end else
    Misc.fatal_errorf "Length of [extra_args] in exception continuation %a@ \
                       does not match those in the environment (%a)@ for application \
                       expression:@ %a"
      Exn_continuation.print k_exn
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Ident.print)
      mut_vars
      Apply_expr.print e

(* Wrap a function call to honour its continuation *)
and wrap_cont env effs res e =
  let k = Apply_expr.continuation e in
  if Continuation.equal (Env.return_cont env) k then
    res
  else begin
    match Env.get_k env k with
    | Jump { types = []; cont; } ->
        let wrap, _ = Env.flush_delayed_lets env in
        wrap (C.sequence res (C.cexit cont [] []))
    | Jump { types = [_]; cont; } ->
        let wrap, _ = Env.flush_delayed_lets env in
        wrap (C.cexit cont [res] [])
    | Inline { handler_params = []; handler_body = body; _ } ->
        let var = Variable.create "*apply_res*" in
        let env = let_expr_bind body env var res effs in
        expr env body
    | Inline { handler_params = [v]; handler_body = body; _ } ->
        let var = Kinded_parameter.var v in
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

and apply_cont env e =
  let k = Apply_cont_expr.continuation e in
  let args = Apply_cont_expr.args e in
  if Continuation.is_exn k then
    apply_cont_exn env e k args
  else if Continuation.equal (Env.return_cont env) k
       && Apply_cont_expr.trap_action e = None then
    apply_cont_ret env e k args
  else
    apply_cont_regular env e k args

(* Exception Continuations always raise their first argument (which is
   supposed to be an exception). Additionally, they may have extra arguments
   that are passed to the handler via mutables variables (which are expected to be
   spilled on the stack). *)
and apply_cont_exn env e k = function
  | res :: extra ->
      let raise_kind =
        match Apply_cont_expr.trap_action e with
        | Some Pop {raise_kind; _} -> C.raise_kind raise_kind
        | _ ->
            Misc.fatal_errorf
              "Apply cont %a calls an exception cont without a Pop trap action"
              Apply_cont.print e
      in
      let exn, env, _ = simple env res in
      let extra, env, _ = arg_list env extra in
      let mut_vars = Env.get_exn_extra_args env k in
      let wrap, _ = Env.flush_delayed_lets env in
      let expr =
        C.raise_prim raise_kind exn
          (Apply_cont_expr.debuginfo e)
      in
      let expr =
        List.fold_left2
          (fun expr arg v -> C.sequence (C.assign v arg) expr)
          expr extra mut_vars
      in
      wrap expr
  | [] ->
      Misc.fatal_errorf "Exception continuation %a has no arguments in@\n%a"
        Continuation.print k Apply_cont.print e

(* A call to the return continuation of the current block simply is the return value
   for the current block being translated. *)
and apply_cont_ret env e k = function
  | [] ->
      assert (Apply_cont_expr.trap_action e = None);
      let wrap, _ = Env.flush_delayed_lets env in
      wrap C.void
  | [res] ->
      assert (Apply_cont_expr.trap_action e = None);
      let res, env, _ = simple env res in
      let wrap, _ = Env.flush_delayed_lets env in
      wrap res
  | _ ->
      (* TODO: add support using unboxed tuples *)
      Misc.fatal_errorf
        "Continuation %a (return cont) should be applied to a single argument in@\n%a@\n%s"
        Continuation.print k Apply_cont_expr.print e
        "Multi-arguments continuation across function calls are not yet supported"

and apply_cont_regular env e k args =
  match Env.get_k env k with
  | Inline { handler_params; handler_body; } ->
      assert (Apply_cont_expr.trap_action e = None);
      apply_cont_inline env e k args handler_body handler_params
  | Jump { types; cont; } ->
      apply_cont_jump env e types cont args

(* Inlining a continuation call simply needs to bind the arguments to the
   variables that the continuation's body expects. The delayed lets in the
   environment enables that translation to be tail-rec. *)
and apply_cont_inline env e k args handler_body handler_params =
  if List.compare_lengths args handler_params = 0 then begin
    let vars = List.map Kinded_parameter.var handler_params in
    let args = List.map Named.create_simple args in
    let env = List.fold_left2 (let_expr_env handler_body) env vars args in
    expr env handler_body
  end else
    Misc.fatal_errorf
      "Continuation %a in@\n%a@\nExpected %d arguments but got %a."
      Continuation.print k Apply_cont_expr.print e
      (List.length handler_params) Apply_cont_expr.print e;

(* Continuation calls need to also translate the associated trap actions. *)
and apply_cont_jump env e types cont args =
  if List.compare_lengths types args = 0 then begin
    let trap_actions = apply_cont_trap_actions env e in
    let args, env, _ = arg_list env args in
    let wrap, _ = Env.flush_delayed_lets env in
    wrap (C.cexit cont args trap_actions)
  end else
    Misc.fatal_errorf "Types (%a) do not match arguments of@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         Printcmm.machtype) types
      Apply_cont_expr.print e

and apply_cont_trap_actions env e =
  match Apply_cont_expr.trap_action e with
  | None -> []
  | Some (Pop _) -> [Cmm.Pop]
  | Some (Push { exn_handler; }) ->
      let cont = Env.get_jump_id env exn_handler in
      [Cmm.Push cont]

and switch env s =
  let e, env, _ = simple env (Switch.scrutinee s) in
  let wrap, env = Env.flush_delayed_lets env in
  let ints, exprs =
    Immediate.Map.fold (fun d k (ints, exprs) ->
      let i = Targetint.OCaml.to_int (Immediate.to_targetint d) in
      let e = match Env.get_k env k with
        | Jump { types = []; cont; } -> C.cexit cont [] []
        | Inline { handler_params = []; handler_body; _ } ->
            expr env handler_body
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
        let d, _ = Immediate.Map.max_binding (Switch.arms s) in
        let n = Targetint.OCaml.to_int (Immediate.to_targetint d) in
        let index = Array.make (n + 2) c in
        Array.iteri (fun i j -> index.(j) <- i) ints;
        wrap (C.transl_switch_clambda Location.none e index cases)
      end

and invalid _env _e =
  C.unreachable

and set_of_closures env s =
  let fun_decls = Set_of_closures.function_decls s in
  let decls = Function_declarations.funs fun_decls in
  let elts = filter_closure_vars env (Set_of_closures.closure_elements s) in
  let layout = Env.layout env
      (List.map fst (Closure_id.Map.bindings decls))
      (List.map fst (Var_within_closure.Map.bindings elts))
  in
  let l, env, effs = fill_layout decls elts env Ece.pure [] 0 layout in
  C.make_closure_block l, env, effs

and fill_layout decls elts env effs acc i = function
  | [] -> List.rev acc, env, effs
  | (j, slot) :: r ->
      let acc = fill_up_to j acc i in
      let acc, offset, env, eff = fill_slot decls elts env acc j slot in
      let effs = Ece.join eff effs in
      fill_layout decls elts env effs acc offset r

and fill_slot decls elts env acc offset slot =
  match (slot : Un_cps_closure.layout_slot) with
  | Infix_header ->
      let field = C.alloc_infix_header (offset + 1) Debuginfo.none in
      field :: acc, offset + 1, env, Ece.pure
  | Env_var v ->
      let field, env, eff = simple env (Var_within_closure.Map.find v elts) in
      field :: acc, offset + 1, env, eff
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
        acc, offset + 2, env, Ece.pure
      end else begin
        let acc =
          C.symbol ~dbg name ::
          C.int_const dbg arity ::
          C.symbol ~dbg (C.curry_function_sym arity) ::
          acc
        in
        acc, offset + 3, env, Ece.pure
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

let get_whole_closure_symbol =
  let whole_closure_symb_count = ref 0 in
  (fun r ->
     match !r with
     | Some s -> s
     | None ->
         incr whole_closure_symb_count;
         let comp_unit = Compilation_unit.get_current_exn () in
         let linkage_name =
           Linkage_name.create @@
           Printf.sprintf ".clos_%d" !whole_closure_symb_count
         in
         let s = Symbol.create comp_unit linkage_name in
         r := Some s;
         s
  )

let rec static_set_of_closures env symbs set =
  let clos_symb = ref None in
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs fun_decls in
  let elts = filter_closure_vars env (Set_of_closures.closure_elements set) in
  let layout = Env.layout env
      (List.map fst (Closure_id.Map.bindings decls))
      (List.map fst (Var_within_closure.Map.bindings elts))
  in
  let l, updates, length =
    fill_static_layout clos_symb symbs decls elts env [] C.void 0 layout
  in
  let header = C.cint (C.black_closure_header length) in
  let sdef = match !clos_symb with
    | None -> []
    | Some s -> C.define_symbol ~global:false (symbol s)
  in
  header :: sdef @ l, updates

and fill_static_layout s symbs decls elts env acc updates i = function
  | [] -> List.rev acc, updates, i
  | (j, slot) :: r ->
      let acc = fill_static_up_to j acc i in
      let acc, offset, updates =
        fill_static_slot s symbs decls elts env acc j updates slot
      in
      fill_static_layout s symbs decls elts env acc updates offset r

and fill_static_slot s symbs decls elts env acc offset updates slot =
  match (slot : Un_cps_closure.layout_slot) with
  | Infix_header ->
      let field = C.cint (C.infix_header (offset + 1)) in
      field :: acc, offset + 1, updates
  | Env_var v ->
      let fields, updates =
        match simple_static env (Var_within_closure.Map.find v elts) with
        | `Data fields -> fields, updates
        | `Var v ->
            let s = get_whole_closure_symbol s in
            let update =
              make_update env Cmm.Word_val (C.symbol (symbol s)) v offset
            in
            [C.cint 1n], C.sequence update updates
      in
      List.rev fields @ acc, offset + 1, updates
  | Closure c ->
      let decl = Closure_id.Map.find c decls in
      let symb = Closure_id.Map.find c symbs in
      let external_name = symbol symb in
      let code_name =
        Un_cps_closure.closure_code (Un_cps_closure.closure_name c)
      in
      let acc = List.rev (C.define_symbol ~global:true external_name) @ acc in
      let arity = List.length (Function_declaration.params_arity decl) in
      let tagged_arity = arity * 2 + 1 in
      (* We build here the **reverse** list of fields for the closure *)
      if arity = 1 || arity = 0 then begin
        let acc =
          C.cint (Nativeint.of_int tagged_arity) ::
          C.symbol_address code_name ::
          acc
        in
        acc, offset + 2, updates
      end else begin
        let acc =
          C.symbol_address code_name ::
          C.cint (Nativeint.of_int tagged_arity) ::
          C.symbol_address (C.curry_function_sym arity) ::
          acc
        in
        acc, offset + 3, updates
      end

and fill_static_up_to j acc i =
  if i = j then acc
  else fill_static_up_to j (C.cint 1n :: acc) (i + 1)

let static_structure_item env r
      ((S (symb, st)) : Flambda_static.Program_body.Static_structure.t0) =
  match symb, st with
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
      (* CR Gbury: What are those ? *)
      todo()
  | Set_of_closures s, Set_of_closures set ->
      let data, updates =
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

let static_structure env is_fully_static s =
  (* Gc roots: statically allocated blocks themselves do not need to be scanned,
     however if statically allocated blocks contain dynamically allocated contents,
     then that block has to be registered as Gc roots for the Gc to correctly patch
     it if/when it moves some of the dynamically allocated blocks. As a safe
     over-approximation, we thus register as gc_roots all symbols who have an
     associated computation (and thus are not fully_static). *)
  let roots =
    if is_fully_static then []
    else Symbol.Set.elements
        (Flambda_static.Program_body.Static_structure.being_defined s)
  in
  let r = R.add_gc_roots roots R.empty in
  List.fold_left (fun acc item ->
      (* Archive_data helps keep definitions of separate symbols in different
         data_item lists and this increases readability of the generated cmm. *)
      R.archive_data (static_structure_item env acc item)
    ) r s

(* Definition *)

let computation_wrapper offsets used_closure_vars c =
  match c with
  | None ->
      Env.dummy offsets used_closure_vars, (fun x -> x), true
  | Some (c : Flambda_static.Program_body.Computation.t) ->
      (* The env for the computation is given a dummy continuation,
         since the return continuation will be explicitly bound to a
         jump before translating the computation. *)
      let dummy_k = Continuation.create () in
      let k_exn = Exn_continuation.exn_handler c.exn_continuation in
      let c_env = Env.mk offsets dummy_k k_exn used_closure_vars in
      (* The environment for the static structure update must contain the
         variables produced by the computation. It is given dummy
         continuations, given that the return continuation will not
         be used. *)
      let s_env = Env.mk offsets dummy_k dummy_k used_closure_vars in
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
      s_env, wrap, false

let definition offsets ~used_closure_vars
      (d : Flambda_static.Program_body.Definition.t) =
  let env, wrapper, is_fully_static =
    computation_wrapper offsets used_closure_vars d.computation
  in
  let r = static_structure env is_fully_static d.static_structure in
  R.wrap_init wrapper r


(* Translate a function declaration. *)

let is_var_used v e =
  let free_names = Expr.free_names e in
  let occurrence = Name_occurrences.greatest_name_mode_var free_names v in
  match (occurrence : Name_mode.Or_absent.t) with
  | Absent -> false
  | Present _k ->
    (* CR mshinwell: I think this should always be [true].  Even if the
       variable is only used by phantom bindings, it still needs to be
       there.  This may only arise in unusual cases (e.g. [my_closure]
       that is used only by phantom bindings). *)
    true
    (* Name_mode.is_normal k *)

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

let function_decl offsets used_closure_vars fun_name _ d =
  Profile.record_call ~accumulate:true fun_name (fun () ->
    let fun_dbg = Function_declaration.dbg d in
    let result_arity = Function_declaration.result_arity d in
    let ret_machtype = machtype_of_return_arity result_arity in
    let p = Function_declaration.params_and_body d in
    Function_params_and_body.pattern_match p
      ~f:(fun ~return_continuation:k k_exn vars ~body ~my_closure ->
          try
            let args = function_args vars my_closure body in
            let k_exn = Exn_continuation.exn_handler k_exn in
            (* Init the env and create a jump id for the ret closure
               in case a trap action is attached to one of tis call *)
            let env = Env.mk offsets k k_exn used_closure_vars in
            let id, env = Env.add_jump_cont env [ret_machtype] k in
            let fun_handle_var = Backend_var.create_local "*fun_res*" in
            let fun_handler = C.var fun_handle_var in
            let fun_handle_vars = [
              Backend_var.With_provenance.create fun_handle_var,
              ret_machtype
            ] in
            (* translate the arg list and body, inserting a catch for the
               return continuation. *)
            let env, fun_args = var_list env args in
            let fun_body =
              C.ccatch
                ~rec_flag:false
                ~body:(expr env body)
                ~handlers:[C.handler id fun_handle_vars fun_handler]
            in
            let fun_flags = function_flags () in
            C.fundecl fun_name fun_args fun_body fun_flags fun_dbg
          with Misc.Fatal_error ->
            Format.eprintf "\n%sContext is:%s translating function %s to Cmm \
                with body@ %a\n"
              (Flambda_colours.error ())
              (Flambda_colours.normal ())
              fun_name
              Expr.print body;
            raise Misc.Fatal_error))

(* Programs *)

let rec program_body offsets ~used_closure_vars acc body =
  match Flambda_static.Program_body.descr body with
  | Flambda_static.Program_body.Root _sym ->
      (* The root symbol does not really deserve any particular treatment.
         Concerning gc_roots, it's like any other statically allocated symbol:
         if if has an associated computation, then it will already be included
         in the list of gc_roots, else it does not *have*  to be a root. *)
      List.fold_left (fun acc r -> R.combine r acc) R.empty acc
  | Flambda_static.Program_body.Define_symbol (def, rest) ->
      let r = definition offsets ~used_closure_vars def in
      program_body offsets ~used_closure_vars (r :: acc) rest

let program_functions offsets used_closure_vars p =
  let aux = function_decl offsets used_closure_vars in
  let fmap = Un_cps_closure.map_on_function_decl aux p in
  let all_functions = Closure_id.Map.fold (fun _ x acc -> x :: acc) fmap [] in
  (* This is to keep the current cmmgen behaviour which sorts functions by
     debuginfo (and thus keeps the order of declaration). *)
  let sorted = List.sort
      (fun f f' -> Debuginfo.compare f.Cmm.fun_dbg f'.Cmm.fun_dbg) all_functions
  in
  List.map (fun decl -> C.cfunction decl) sorted

let program (p : Flambda_static.Program.t) =
  Profile.record_call "flambda2_to_cmm" (fun () ->
      let offsets = Un_cps_closure.compute_offsets p in
      let used_closure_vars =
        Name_occurrences.closure_vars (Flambda_static.Program.free_names p)
      in
      let functions = program_functions offsets used_closure_vars p in
      let res = program_body ~used_closure_vars offsets [] p.body in
      let data, entry, gc_roots = R.to_cmm res in
      let cmm_data = C.flush_cmmgen_state () in
      let roots = List.map symbol gc_roots in
      (C.gc_root_table roots) :: data @ cmm_data @ functions @ [entry]
    )

