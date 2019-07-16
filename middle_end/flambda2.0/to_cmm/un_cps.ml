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

(* Notes:
   - an int64 on a 32-bit host is represented across two registers,
     hence most operations on them will actually need to call C primitive
      that can handle them.
   - int32 on 64 bits are represented as an int64 in the range of
     32-bit integers. Currently we trust flambda2 to insert
     double shifts to clear the higher order 32-bits between operations.
     Once the samll_arith PR comes, we can use dedicated 32-bits
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
  | Name.Var v -> C.var (Env.get_variable env v)
  | Name.Symbol s -> C.symbol (symbol s)

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
  | Const c -> const env c
  | Discriminant d -> discriminant env d

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
  | Get_tag _ ->
      C.get_tag arg dbg
  | Discriminant_of_int ->
      C.untag_int arg dbg
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
  | Project_closure c ->
      C.field_address arg (Env.closure_offset env c) dbg
  | Move_within_set_of_closures { move_from = c1; move_to = c2} ->
      let diff = (Env.closure_offset env c2) - (Env.closure_offset env c1) in
      C.field_address arg diff dbg
  | Project_var v ->
      C.get_field_gen Asttypes.Immutable arg (Env.env_var_offset env v) dbg

let binary_primitive env dbg f x y =
  match (f : Flambda_primitive.binary_primitive) with
  | Block_load (kind, _) ->
      C.block_load ~dbg kind x y
  | String_or_bigstring_load (kind, width) ->
      C.string_like_load ~dbg kind width x (C.untag_int y dbg)
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

let prim env dbg p =
  match (p : Flambda_primitive.t) with
  | Unary (f, x) ->
      unary_primitive env dbg f (simple env x)
  | Binary (f, x, y) ->
      binary_primitive env dbg f (simple env x) (simple env y)
  | Ternary (f, x, y, z) ->
      ternary_primitive env dbg f (simple env x) (simple env y) (simple env z)
  | Variadic (f, l) ->
      variadic_primitive env dbg f (List.map (simple env) l)

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

(* Function calls and continuations *)

let var_list env l =
  let flambda_vars = List.map Kinded_parameter.var l in
  let env, cmm_vars = Env.create_variables env flambda_vars in
  let vars = List.map2 (fun v v' ->
      (v, machtype_of_kinded_parameter v')
    ) cmm_vars l in
  env, vars


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
  Let.pattern_match t ~f:(fun ~bound_var:v ~body ->
      let e = Let.defining_expr t in
      let v = Var_in_binding_pos.var v in
      let env', v' = Env.create_variable env v in
      C.letin v' (named env' e) (expr env' body)
    )

and has_one_occurrence num =
  match (num : Name_occurrences.Num_occurrences.t) with
  | One -> true
  | More_than_one -> false
  | Zero ->
      Misc.fatal_errorf
        "Found unused let-bound continuation, this should not happen"

and decide_inline_cont h num_free_occurrences =
  Continuation_handler.stub h || has_one_occurrence num_free_occurrences

and let_cont env = function
  (* Single_use continuation, inlined at call site to avoid a needless jump *)
  | Let_cont.Non_recursive { handler; num_free_occurrences; } ->
      Non_recursive_let_cont_handler.pattern_match handler ~f:(fun k ~body ->
          let h = Non_recursive_let_cont_handler.handler handler in
          if decide_inline_cont h num_free_occurrences then begin
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
  let vars, handle = continuation_handler env h in
  let vars = List.map fst vars in
  let env = Env.add_inline_cont env k vars handle in
  expr env body

and let_cont_jump env k h body =
  let vars, handle = continuation_handler env h in
  let id, env = Env.add_jump_cont env (List.map snd vars) k in
  C.ccatch
    ~rec_flag:false
    ~body:(expr env body)
    ~handlers:[C.handler id vars handle]

and let_cont_rec env conts body =
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
  C.ccatch
    ~rec_flag:true
    ~body:(expr env body)
    ~handlers

and continuation_arg_tys h =
  let h = Continuation_handler.params_and_handler h in
  Continuation_params_and_handler.pattern_match h ~f:(fun args ~handler:_ ->
      List.map machtype_of_kinded_parameter args
    )

and continuation_handler env h =
  let h = Continuation_handler.params_and_handler h in
  Continuation_params_and_handler.pattern_match h ~f:(fun args ~handler ->
      let env, vars = var_list env args in
      vars, expr env handler
    )

and apply_expr env e =
  let res = apply_call env e in
  let continued = wrap_cont env res e in
  wrap_exn env continued e

and apply_call env e =
  let f = Apply_expr.callee e in
  let args = List.map (simple env) (Apply_expr.args e) in
  let dbg = Apply_expr.dbg e in
  match Apply_expr.call_kind e with
  | Call_kind.Function
      Call_kind.Function_call.Direct { return_arity; _ } ->
      let f = Un_cps_closure.closure_code (function_name f) in
      let ty = machtype_of_return_arity return_arity in
      C.direct_call ~dbg ty (C.symbol f) args
  | Call_kind.Function
      Call_kind.Function_call.Indirect_unknown_arity ->
      let f = simple env f in
      C.indirect_call ~dbg typ_val f args
  | Call_kind.Function
      Call_kind.Function_call.Indirect_known_arity { return_arity; _ } ->
      let f = simple env f in
      let ty = machtype_of_return_arity return_arity in
      C.indirect_call ~dbg ty f args
  | Call_kind.C_call { alloc; return_arity; _ } ->
      let f = function_name f in
      let ty = machtype_of_return_arity return_arity in
      C.extcall ~dbg ~alloc f ty args
  | Call_kind.Method _ ->
      todo()

and wrap_cont env res e =
  let k = Apply_expr.continuation e in
  if Continuation.equal (Env.return_cont env) k then
    res
  else begin
    match Env.get_k env k with
    | Jump ([_], id) -> C.cexit id [res]
    | Inline ([v], body) -> C.letin v res body
    | Jump _
    | Inline _ ->
        (* TODO: add support using unboxed tuples *)
        Misc.fatal_errorf
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
    | Inline ([exn_var], handler) ->
        C.trywith ~dbg:Debuginfo.none ~body:res ~exn_var ~handler
    | Jump _
    | Inline _ ->
        Misc.fatal_errorf
          "Exception continuations should only take one argument"
  end

and apply_cont env e =
  let k = Apply_cont_expr.continuation e in
  let args = List.map (simple env) (Apply_cont_expr.args e) in
  if Continuation.equal (Env.exn_cont env) k then begin
    match args with
    | [res] -> C.raise_regular Debuginfo.none res
    | _ ->
        Misc.fatal_errorf
          "Exception continuations should only applied to a single argument"
  end else if Continuation.equal (Env.return_cont env) k then begin
    match args with
    | [res] -> res
    | _ ->
        (* TODO: add support using unboxed tuples *)
        Misc.fatal_errorf
          "Multi-arguments continuation across function calls are not yet supported"
  end else begin
    match Env.get_k env k with
    | Jump (tys, id) ->
        (* The provided args should match the types in tys *)
        assert (List.length tys = List.length args);
        C.cexit id args
    | Inline (vars, body) ->
        List.fold_left2 (fun acc v e -> C.letin v e acc) body vars args
  end

and switch env s =
  let e = simple env (Switch.scrutinee s) in
  let ints, exprs =
    Discriminant.Map.fold (fun d k (ints, exprs) ->
      let i = Targetint.OCaml.to_int (Discriminant.to_int d) in
      let e = match Env.get_k env k with
        | Jump ([], id) -> C.cexit id []
        | Inline ([], body) -> body
        | Jump _
        | Inline _ ->
            Misc.fatal_errorf
              "Switch branches should be goto (zero arguments) continuations"
      in
      (i :: ints, e :: exprs)
      ) (Switch.arms s) ([], [])
  in
  let ints = Array.of_list ints in
  let exprs = Array.of_list exprs in
  (* CR gbury: try and use cmm_helper's make_switch instead
               (though make_switch uses cmmgen_state) ? *)
  C.transl_switch_clambda Location.none e ints exprs

and invalid _env _e =
  C.load Cmm.Word_int Asttypes.Mutable (C.int 0)

and set_of_closures env s =
  let fun_decls = Set_of_closures.function_decls s in
  let decls = Function_declarations.funs fun_decls in
  let elts = Set_of_closures.closure_elements s in
  let layout = Env.layout env
      (List.map fst (Closure_id.Map.bindings decls))
      (List.map fst (Var_within_closure.Map.bindings elts))
  in
  let l = fill_layout decls elts env [] 0 layout in
  C.make_closure_block l

and fill_layout decls elts env acc i = function
  | [] -> List.rev acc
  | (j, slot) :: r ->
      let acc = fill_up_to j acc i in
      let acc, offset = fill_slot decls elts env acc j slot in
      fill_layout decls elts env acc offset r

and fill_slot decls elts env acc offset slot =
  match (slot : Un_cps_closure.layout_slot) with
  | Infix_header ->
      let field = C.alloc_infix_header offset Debuginfo.none in
      field :: acc, offset + 1
  | Env_var v ->
      let field = simple env (Var_within_closure.Map.find v elts) in
      field :: acc, offset + 1
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
        acc, offset + 2
      end else begin
        let acc =
          C.symbol ~dbg name ::
          C.int_const dbg arity ::
          C.symbol ~dbg (C.curry_function_sym arity) ::
          acc
        in
        acc, offset + 3
      end

and fill_up_to j acc i =
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
  let e = C.var (Env.get_variable env var) in
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

let rec static_set_of_closures env s symbs set =
  let fun_decls = Set_of_closures.function_decls set in
  let decls = Function_declarations.funs fun_decls in
  let elts = Set_of_closures.closure_elements set in
  let layout = Env.layout env
      (List.map fst (Closure_id.Map.bindings decls))
      (List.map fst (Var_within_closure.Map.bindings elts))
  in
  let symb = C.symbol (symbol s) in
  let l, updates, length =
    fill_static_layout symb symbs decls elts env [] C.void 0 layout
  in
  C.cint (C.black_closure_header length) ::
  C.define_symbol ~global:true (symbol s) @ l, updates

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
      let field = C.cint (C.infix_header offset) in
      field :: acc, offset + 1, updates
  | Env_var v ->
      let fields, updates =
        match simple_static env (Var_within_closure.Map.find v elts) with
        | `Data fields -> fields, updates
        | `Var v ->
            let e = make_update env Cmm.Word_val s v offset in
            [C.cint 1n], C.sequence e updates
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
        static_set_of_closures env s.set_of_closures_symbol s.closure_symbols set
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
      let k = c.return_continuation in
      let k_exn = Exn_continuation.exn_handler c.exn_continuation in
      let env = Env.mk offsets k k_exn in
      let env, vars = var_list env c.computed_values in
      let free_names = Expr.free_names c.expr in
      let num_occurrences = Name_occurrences.count_continuation free_names k in
      let wrap e =
        if has_one_occurrence num_occurrences then begin
          let vars = List.map fst vars in
          let env = Env.add_inline_cont env k vars e in
          expr env c.expr
        end else begin
          let tys = List.map snd vars in
          let id, env = Env.add_jump_cont env tys k in
          let body = expr env c.expr in
          C.ccatch
            ~rec_flag:false ~body
            ~handlers:[C.handler id vars e]
        end
      in
      (* CR gbury: for the future, try and rearrange the generated cmm
         code to move assignments closer to the variable definitions *)
      env, wrap

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
        let args = function_args vars my_closure body in
        let k_exn = Exn_continuation.exn_handler k_exn in
        let env = Env.mk offsets k k_exn in
        let env, fun_args = var_list env args in
        let fun_body = expr env body in
        let fun_flags = function_flags () in
        C.fundecl fun_name fun_args fun_body fun_flags fun_dbg
      )

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
  let all_functions = Code_id.Map.fold (fun _ x acc -> x :: acc) fmap [] in
  let sorted = List.sort
      (fun f f' -> Debuginfo.compare f.Cmm.fun_dbg f'.Cmm.fun_dbg) all_functions
  in
  List.map (fun decl -> C.cfunction decl) sorted

let program (p : Flambda_static.Program.t) =
  let offsets = Un_cps_closure.compute_offsets p in
  let functions = program_functions offsets p in
  let sym, res = program_body offsets [] p.body in
  let data, entry = R.to_cmm res in
  (C.gc_root_table [symbol sym]) :: data @ functions @ [entry]

