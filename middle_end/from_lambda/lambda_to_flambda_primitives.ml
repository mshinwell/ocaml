(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module P = Flambda_primitive
module I = Flambda_kind.Standard_int
module Named = Flambda.Named
module Expr = Flambda.Expr
module K = Flambda_kind

(* CR mshinwell: Moved here from Flambda_kind


val of_block_shape : Lambda.block_shape -> num_fields:int -> t
*)

let of_block_shape (shape : Lambda.block_shape) ~num_fields =
  match shape with
  | None ->
    List.init num_fields (fun _field -> Flambda_kind.value Must_scan)
  | Some shape ->
    let shape_length = List.length shape in
    if num_fields <> shape_length then begin
      Misc.fatal_errorf "Flambda_arity.of_block_shape: num_fields is %d \
          yet the shape has %d fields"
        num_fields
        shape_length
    end;
    List.map (fun (kind : Lambda.value_kind) ->
        match kind with
        | Pgenval | Pfloatval | Pboxedintval _ -> Flambda_kind.value Must_scan
        | Pintval -> Flambda_kind.value Can_scan
        | Pnaked_intval -> Flambda_kind.naked_immediate ())
      shape

let convert_mutable_flag (flag : Asttypes.mutable_flag)
      : P.mutable_or_immutable =
  match flag with
  | Mutable -> Mutable
  | Immutable -> Immutable

let convert_comparison (comp : Lambda.comparison) : P.comparison =
  match comp with
  | Ceq -> Eq
  | Cneq -> Neq
  | Clt -> Lt
  | Cgt -> Gt
  | Cle -> Le
  | Cge -> Ge

let boxable_number_of_boxed_integer (bint : Lambda.boxed_integer)
  : Flambda_kind.Boxable_number.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

let standard_int_of_boxed_integer (bint : Lambda.boxed_integer)
  : Flambda_kind.Standard_int.t =
  match bint with
  | Pnativeint -> Naked_nativeint
  | Pint32 -> Naked_int32
  | Pint64 -> Naked_int64

let const_of_boxed_integer (i:int32) (bint : Lambda.boxed_integer)
  : Simple.Const.t =
  match bint with
  | Pnativeint -> Naked_nativeint (Targetint.of_int32 i)
  | Pint32 -> Naked_int32 i
  | Pint64 -> Naked_int64 (Int64.of_int32 i)

let convert_record_representation (repr : Types.record_representation)
   : P.record_representation =
  match repr with
  | Record_regular -> Regular
  | Record_float -> Float
  | Record_unboxed inlined -> Unboxed { inlined }
  | Record_inlined tag -> Inlined (Tag.Scannable.create_exn tag)
  | Record_extension -> Extension

let convert_immediate_or_pointer_to_scanning i_or_p =
  match i_or_p with
  | Lambda.Immediate -> K.Can_scan
  | Lambda.Pointer -> K.Must_scan

let convert_init_or_assign (i_or_a : Lambda.initialization_or_assignment)
   : P.init_or_assign =
  match i_or_a with
  | Assignment -> Assignment
  | Heap_initialization -> Initialization
  (* Root initialization cannot exist in lambda. This is
     represented by the static part of expressions in flambda. *)
  | Root_initialization -> assert false

let convert_array_kind (kind : Lambda.array_kind)
   : P.array_kind =
  match kind with
  | Pgenarray -> Dynamic_must_scan_or_naked_float
  | Paddrarray -> Must_scan
  | Pintarray -> Can_scan
  | Pfloatarray -> Naked_float

(* let convert (prim : Lambda.primitive) (args : Simple.t list) : P.t = *)
(*   match prim, args with *)
(*   | Pfield field, [arg] -> *)
(*     (\* CR pchambart: every load is annotated as mutable we must be *)
(*        careful to update that when we know it is not. This should not *)
(*        be an error. *)
(*        We need more type propagations to be precise here *\) *)
(*     Unary (Block_load (field, Not_a_float, Mutable), arg) *)
(*   | Psetfield (field, immediate_or_pointer, initialization_or_assignment), *)
(*     [block; value] -> *)
(*     let set_kind : P.block_set_kind = *)
(*       match immediate_or_pointer with *)
(*         | Immediate -> Immediate *)
(*         | Pointer -> Pointer *)
(*     in *)
(*     let init_or_assign : P.init_or_assign = *)
(*       match initialization_or_assignment with *)
(*       | Assignment -> Assignment *)
(*       | Heap_initialization -> Initialization *)
(*       (\* Root initialization cannot exist in lambda. This is *)
(*          represented by the static part of expressions in flambda. *\) *)
(*       | Root_initialization -> assert false *)
(*     in *)
(*     Binary (Block_set (field, set_kind, init_or_assign), block, value) *)
(*   | ( Pfield _ | Pnegint | Psetfield _ ), _ -> *)
(*     Misc.fatal_errorf "Closure_conversion.convert_primitive: \ *)
(*                        Wrong arrity for %a: %i" *)
(*       Printlambda.primitive prim (List.length args) *)
(*   | _ -> *)
(*     assert false *)

[@@@ocaml.warning "-37"]

type failure =
  | Division_by_zero
  | Index_out_of_bound

type expr_primitive =
  | Unary of P.unary_primitive * simple_or_prim
  | Binary of P.binary_primitive * simple_or_prim * simple_or_prim
  | Ternary of P.ternary_primitive * simple_or_prim * simple_or_prim * simple_or_prim
  | Variadic of P.variadic_primitive * (simple_or_prim list)
  | Checked of { validity_condition : expr_primitive;
                 primitive : expr_primitive;
                 failure : failure; (* Predefined exception *)
                 dbg : Debuginfo.t }

and simple_or_prim =
  | Simple of Simple.t
  | Prim of expr_primitive

let rec bind_rec
          (prim : expr_primitive)
          (dbg : Debuginfo.t)
          ~exception_continuation
          (cont : Named.t -> Expr.t)
  : Expr.t =
  match prim with
  | Unary (prim, arg) ->
    let cont (arg : Simple.t) =
      cont (Prim (Unary (prim, arg), dbg))
    in
    bind_rec_primitive arg dbg ~exception_continuation cont
  | Binary (prim, arg1, arg2) ->
    let cont (arg2 : Simple.t) =
      let cont (arg1 : Simple.t) =
        cont (Prim (Binary (prim, arg1, arg2), dbg))
      in
      bind_rec_primitive arg1 dbg ~exception_continuation cont
    in
    bind_rec_primitive arg2 dbg ~exception_continuation cont
  | Ternary (prim, arg1, arg2, arg3) ->
    let cont (arg3 : Simple.t) =
      let cont (arg2 : Simple.t) =
        let cont (arg1 : Simple.t) =
          cont (Prim (Ternary (prim, arg1, arg2, arg3), dbg))
        in
        bind_rec_primitive arg1 dbg ~exception_continuation cont
      in
      bind_rec_primitive arg2 dbg ~exception_continuation cont
    in
    bind_rec_primitive arg3 dbg ~exception_continuation cont
  | Variadic (prim, args) ->
    let cont args =
      cont (Prim (Variadic (prim, args), dbg))
    in
    let rec build_cont args_to_convert converted_args =
      match args_to_convert with
      | [] ->
        cont converted_args
      | arg :: args_to_convert ->
        let cont arg =
          build_cont args_to_convert (arg :: converted_args)
        in
        bind_rec_primitive arg dbg ~exception_continuation cont
    in
    build_cont (List.rev args) []
  | Checked _ ->
    failwith "TODO"

and bind_rec_primitive
      (prim : simple_or_prim)
      (dbg : Debuginfo.t)
      ~exception_continuation
      (cont : Simple.t -> Expr.t) : Expr.t =
  match prim with
  | Simple s ->
    cont s
  | Prim p ->
    let var = Variable.create "prim" in
    let cont named =
      Flambda.Expr.create_let var (Flambda_kind.value Must_scan) named
        (cont (Simple.var var))
    in
    bind_rec p dbg ~exception_continuation cont

let box_float (arg : expr_primitive) : expr_primitive =
  Unary (Box_number Flambda_kind.Boxable_number.Naked_float, Prim arg)
let unbox_float (arg : simple_or_prim) : simple_or_prim =
  Prim (Unary (Unbox_number Flambda_kind.Boxable_number.Naked_float, arg))
let box_bint bi (arg : expr_primitive) : expr_primitive =
  Unary (Box_number (boxable_number_of_boxed_integer bi), Prim arg)
let unbox_bint bi (arg : simple_or_prim) : simple_or_prim =
  Prim (Unary (Unbox_number (boxable_number_of_boxed_integer bi), arg))

let tagged_immediate_as_naked_nativeint (_arg : simple_or_prim) : simple_or_prim =
  failwith "TODO add a primitive for that"

let bint_binary_prim bi prim arg1 arg2 =
  box_bint bi
    (Binary (Int_arith (standard_int_of_boxed_integer bi, prim),
             unbox_bint bi arg1, unbox_bint bi arg2))
let bint_shift bi prim arg1 arg2 =
  box_bint bi
    (Binary (Int_shift (standard_int_of_boxed_integer bi, prim),
             unbox_bint bi arg1, unbox_bint bi arg2))


let convert_lprim (prim : Lambda.primitive) (args : Simple.t list)
      (dbg : Debuginfo.t) : expr_primitive =
  let args = List.map (fun arg : simple_or_prim -> Simple arg) args in
  match prim, args with
  | Pmakeblock (tag, flag, shape), _ ->
    let flag = convert_mutable_flag flag in
    let arity = of_block_shape shape ~num_fields:(List.length args) in
    Variadic (Make_block (Tag.Scannable.create_exn tag, flag, arity), args)
  | Pmakearray (kind, mutability), _ ->
    let mutability = convert_mutable_flag mutability in
    Variadic (Make_array (convert_array_kind kind, mutability), args)
  | Popaque, [arg] ->
    Unary (Opaque_identity, arg)
  | Pduprecord (repr, num_fields), [arg] ->
    Unary (Duplicate_record {
      repr = convert_record_representation repr;
      num_fields;
    }, arg)
  | Pnegint, [arg] ->
    Unary (Int_arith (I.Tagged_immediate, Neg), arg)
  | Paddint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Add), arg1, arg2)
  | Psubint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Sub), arg1, arg2)
  | Pmulint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Mul), arg1, arg2)
  | Pandint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, And), arg1, arg2)
  | Porint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Or), arg1, arg2)
  | Pxorint, [arg1; arg2] ->
    Binary (Int_arith (I.Tagged_immediate, Xor), arg1, arg2)
  | Plslint, [arg1; arg2] ->
    Binary (Int_shift (I.Tagged_immediate, Lsl), arg1, arg2)
  | Plsrint, [arg1; arg2] ->
    Binary (Int_shift (I.Tagged_immediate, Lsr), arg1, arg2)
  | Pasrint, [arg1; arg2] ->
    Binary (Int_shift (I.Tagged_immediate, Asr), arg1, arg2)
  | Pnot, [arg] ->
    Unary (Boolean_not, arg)
  | Pintcomp comp, [arg1; arg2] ->
    Binary (Int_comp (I.Tagged_immediate, convert_comparison comp), arg1, arg2)
  | Pintoffloat, [arg] ->
    Unary (Int_of_float, unbox_float arg)
  | Pfloatofint, [arg] ->
    box_float (Unary (Float_of_int, arg))
  | Pnegfloat, [arg] ->
    box_float (Unary (Float_arith Neg, unbox_float arg))
  | Pabsfloat, [arg] ->
    box_float (Unary (Float_arith Abs, unbox_float arg))
  | Paddfloat, [arg1; arg2] ->
    box_float (Binary (Float_arith Add, unbox_float arg1, unbox_float arg2))
  | Psubfloat, [arg1; arg2] ->
    box_float (Binary (Float_arith Sub, unbox_float arg1, unbox_float arg2))
  | Pmulfloat, [arg1; arg2] ->
    box_float (Binary (Float_arith Mul, unbox_float arg1, unbox_float arg2))
  | Pdivfloat, [arg1; arg2] ->
    box_float (Binary (Float_arith Div, unbox_float arg1, unbox_float arg2))
  | Pfloatcomp comp, [arg1; arg2] ->
    Binary (Float_comp (convert_comparison comp),
            unbox_float arg1, unbox_float arg2)
  | Pfield_computed, [obj; field] ->
    Binary (Block_load_computed_index, obj, field)
  | Psetfield_computed (imm_or_pointer, init_or_assign), [obj; field; value] ->
    let scanning =
      convert_immediate_or_pointer_to_scanning imm_or_pointer
    in
    Ternary
      (Block_set_computed
         (scanning, convert_init_or_assign init_or_assign),
       obj, field, value)
  | Parraylength kind, [arg] ->
    Unary (Array_length (convert_array_kind kind), arg)
  | Pduparray (kind, mutability), [arg] ->
    Unary (Duplicate_array (convert_array_kind kind,
                            convert_mutable_flag mutability), arg)
  | Pstringlength, [arg] ->
    Unary (String_length String, arg)
  | Pbyteslength, [arg] ->
    Unary (String_length Bytes, arg)
  | Pstringrefu, [arg1; arg2] ->
    Binary (String_load Eight, arg1, arg2)
  | Pbytesrefu, [arg1; arg2] ->
    Binary (String_load Eight, arg1, arg2)
  | (Pstringrefs | Pbytesrefs), [arg1; arg2] ->
    Checked {
      primitive = Binary (String_load Eight, arg1, arg2);
      validity_condition =
        Binary (Int_comp_unsigned Lt,
                (* CR pchambart:
                   Int_comp_unsigned assumes that the arguments are naked
                   integers, but it is correct for tagged integers too as
                   untagging of both arguments doesn't change the result. *)
                tagged_immediate_as_naked_nativeint arg2,
                tagged_immediate_as_naked_nativeint
                  (Prim (Unary (String_length String, arg1))));
      failure = Index_out_of_bound;
      dbg;
    }

  | Pstring_load_16 true (* unsafe *), [arg1; arg2] ->
    Binary (String_load Sixteen, arg1, arg2)
  | Pstring_load_32 true (* unsafe *), [arg1; arg2] ->
    Binary (String_load Thirty_two, arg1, arg2)
  | Pstring_load_64 true (* unsafe *), [arg1; arg2] ->
    Binary (String_load Sixty_four, arg1, arg2)

  (* TODO *)
  (* | (Pstring_load_16 false) (\* safe *\), [arg1; arg2] -> *)
  (*   Checked { *)
  (*     primitive = Binary (String_load Sixteen, arg1, arg2); *)
  (*     validity_condition = *)
  (*       Binary (Int_comp_unsigned Lt, *)
  (*               (\* CR pchambart: *)
  (*                  Int_comp_unsigned assumes that the arguments are naked *)
  (*                  integers, but it is correct for tagged integers too as *)
  (*                  untagging of both arguments doesn't change the result. *\) *)
  (*               tagged_immediate_as_naked_nativeint arg2, *)
  (*               tagged_immediate_as_naked_nativeint *)
  (*                 (Prim (Unary (String_length String, arg1)))); *)
  (*     failure = Index_out_of_bound; *)
  (*     dbg; *)
  (*   } *)
  | Pbittest, [arg1; arg2] ->
    Binary (Bit_test, arg1, arg2)

  | Pisint, [arg] ->
    Unary (Is_int, arg)
  | Pgettag, [arg] ->
    Unary (Get_tag, arg)
  | Pisout, [arg1; arg2] ->
    Binary (Int_comp_unsigned Lt,
            tagged_immediate_as_naked_nativeint arg1,
            tagged_immediate_as_naked_nativeint arg2)
  | Pbintofint bi, [arg] ->
    let dst = standard_int_of_boxed_integer bi in
    Unary (
      Box_number
        (boxable_number_of_boxed_integer bi),
      Prim (Unary (Int_conv { src = I.Tagged_immediate; dst }, arg)))
  | Pintofbint bi, [arg] ->
    let src = standard_int_of_boxed_integer bi in
    Unary (
      Int_conv { src; dst = I.Tagged_immediate },
      Prim (Unary (Unbox_number (boxable_number_of_boxed_integer bi), arg)))
  | Pcvtbint (source, destination), [arg] ->
    box_bint destination
      (Unary (Int_conv { src = standard_int_of_boxed_integer source;
                         dst = standard_int_of_boxed_integer destination },
              unbox_bint source arg))
  | Pnegbint bi, [arg] ->
    box_bint bi (Unary (Int_arith (standard_int_of_boxed_integer bi, Neg), unbox_bint bi arg))
  | Paddbint bi, [arg1; arg2] ->
    bint_binary_prim bi Add arg1 arg2
  | Psubbint bi, [arg1; arg2] ->
    bint_binary_prim bi Sub arg1 arg2
  | Pmulbint bi, [arg1; arg2] ->
    bint_binary_prim bi Mul arg1 arg2
  | Pandbint bi, [arg1; arg2] ->
    bint_binary_prim bi And arg1 arg2
  | Porbint bi, [arg1; arg2] ->
    bint_binary_prim bi Or arg1 arg2
  | Pxorbint bi, [arg1; arg2] ->
    bint_binary_prim bi Xor arg1 arg2
  | Plslbint bi, [arg1; arg2] ->
    bint_shift bi Lsl arg1 arg2
  | Plsrbint bi, [arg1; arg2] ->
    bint_shift bi Lsr arg1 arg2
  | Pasrbint bi, [arg1; arg2] ->
    bint_shift bi Asr arg1 arg2
  | Poffsetint n, [arg] ->
    let const =
      Simple.const
        (Simple.Const.Tagged_immediate
           (Immediate.int (Targetint.of_int n)))
    in
    Binary (Int_arith (I.Tagged_immediate, Add), arg, Simple const)
  | Pfield field, [arg] ->
    (* CR pchambart: every load is annotated as mutable we must be
       careful to update that when we know it is not. This should not
       be an error.
       We need more type propagations to be precise here *)
    Unary (Block_load (field, Not_a_float, Mutable), arg)
  | Pfloatfield field, [arg] ->
    Unary (Block_load (field, Float, Mutable), arg)
  | Psetfield (field, immediate_or_pointer, initialization_or_assignment),
    [block; value] ->
    let set_kind : P.block_set_kind =
      match immediate_or_pointer with
        | Immediate -> Immediate
        | Pointer -> Pointer
    in
    let init_or_assign : P.init_or_assign =
      convert_init_or_assign initialization_or_assignment
    in
    Binary (Block_set (field, set_kind, init_or_assign), block, value)

  | Psetfloatfield (field, init_or_assign), [block; value] ->
    Binary (Block_set (field, Float, convert_init_or_assign init_or_assign),
      block, value)
  | Pdivint Safe, [arg1; arg2] ->
    Checked {
      primitive =
        Binary (Int_arith (I.Tagged_immediate, Div), arg1, arg2);
      validity_condition =
        Binary (Int_comp (I.Tagged_immediate, Eq), arg2,
                Simple
                  (Simple.const
                     (Simple.Const.Tagged_immediate
                        (Immediate.int (Targetint.zero)))));
      failure = Division_by_zero;
      dbg;
    }

  | Pmodint Safe, [arg1; arg2] ->
    Checked {
      primitive =
        Binary (Int_arith (I.Tagged_immediate, Mod), arg1, arg2);
      validity_condition =
        Binary (Int_comp (I.Tagged_immediate, Eq), arg2,
                Simple
                  (Simple.const
                     (Simple.Const.Tagged_immediate
                        (Immediate.int (Targetint.zero)))));
      failure = Division_by_zero;
      dbg;
    }

  | Pdivbint { size; is_safe = Safe }, [arg1; arg2] ->
    let bi = standard_int_of_boxed_integer size in
    Checked {
      primitive =
        bint_binary_prim size Div arg1 arg2;
      validity_condition =
        Binary (Int_comp (bi, Eq), unbox_bint size arg2,
                Simple (Simple.const
                  (const_of_boxed_integer 0l size)));
      failure = Division_by_zero;
      dbg;
    }

  | Pmodbint { size; is_safe = Safe }, [arg1; arg2] ->
    let bi = standard_int_of_boxed_integer size in
    Checked {
      primitive =
        bint_binary_prim size Mod arg1 arg2;
      validity_condition =
        Binary (Int_comp (bi, Eq), unbox_bint size arg2,
                Simple (Simple.const
                  (const_of_boxed_integer 0l size)));
      failure = Division_by_zero;
      dbg;
    }

  | ( Pdivint Unsafe | Pmodint Unsafe
    | Pdivbint { is_safe = Unsafe } | Pmodbint { is_safe = Unsafe }
    | Psetglobal _ | Ploc _
    | Praise _
    | Plazyforce
    | Pccall _
    ), _ ->
    Misc.fatal_errorf "Closure_conversion.convert_primitive: \
                       Primitive %a shouldn't be here"
      Printlambda.primitive prim

  | ( Pfield _ | Pnegint | Pnot | Poffsetint _
    | Pintoffloat | Pfloatofint
    | Pnegfloat | Pabsfloat | Pstringlength
    | Pbyteslength | Pisint | Pgettag
    | Pbintofint _
    | Pintofbint _
    | Pnegbint _
    | Popaque
    | Pduprecord _
    | Parraylength _
    | Pduparray _
    | Pfloatfield _
    | Pcvtbint _
    ),
    ([] |  _ :: _ :: _) ->
    Misc.fatal_errorf "Closure_conversion.convert_primitive: \
                       Wrong arity for unary primitive %a: %i"
      Printlambda.primitive prim (List.length args)
  | ( Paddint | Psubint | Pmulint
    | Pandint | Porint | Pxorint | Plslint | Plsrint | Pasrint
    | Pdivint _ | Pmodint _ | Psetfield _ | Pintcomp _
    | Paddfloat | Psubfloat | Pmulfloat
    | Pdivfloat | Pfloatcomp _
    | Pstringrefu | Pbytesrefu
    | Pstringrefs | Pbytesrefs
    | Pisout
    | Paddbint _
    | Psubbint _
    | Pmulbint _
    | Pandbint _
    | Porbint _
    | Pxorbint _
    | Plslbint _
    | Plsrbint _
    | Pasrbint _
    | Pfield_computed
    | Pdivbint _
    | Pmodbint _
    | Pbittest
    | Psetfloatfield _
    ),
    ([] | [_] | _ :: _ :: _ :: _) ->
    Misc.fatal_errorf "Closure_conversion.convert_primitive: \
                       Wrong arity for binary primitive %a: %i"
      Printlambda.primitive prim (List.length args)
  (* | (  ), _ -> *)
  (*   Misc.fatal_errorf "Closure_conversion.convert_primitive: \ *)
  (*                      Wrong arity for %a: %i" *)
  (*     Printlambda.primitive prim (List.length args) *)

  | ( Psetfield_computed _
    ),
    ([] | [_] | [_;_] | _ :: _ :: _ :: _ :: _) ->
    Misc.fatal_errorf "Closure_conversion.convert_primitive: \
                       Wrong arity for ternary primitive %a: %i"
      Printlambda.primitive prim (List.length args)

  | ( Pidentity | Pignore | Prevapply | Pdirapply | Psequand
    | Psequor
    ), _ ->
    Misc.fatal_errorf "[%a] should have been removed by \
      [Prepare_lambda.prepare]"
      Printlambda.primitive prim

  | ( Pgetglobal _ | Pread_mutable _ ), _ ->
    Misc.fatal_errorf "[%a] should have been handled by \
      [Closure_conversion.close_named]"
      Printlambda.primitive prim


  | Pctconst _, _
    (* It's not obvious when this one should be converted. *)
    -> failwith "TODO"

  | ( Pbytes_to_string
    | Pbytes_of_string
    | Poffsetref _
    | Pbytessetu
    | Pbytessets
    | Parrayrefu _
    | Parraysetu _
    | Parrayrefs _
    | Parraysets _
    | Pbintcomp _
    | Pbigarrayref _
    | Pbigarrayset _
    | Pbigarraydim _
    | Pstring_load_16 _
    | Pstring_load_32 _
    | Pstring_load_64 _
    | Pstring_set_16 _
    | Pstring_set_32 _
    | Pstring_set_64 _
    | Pbigstring_load_16 _
    | Pbigstring_load_32 _
    | Pbigstring_load_64 _
    | Pbigstring_set_16 _
    | Pbigstring_set_32 _
    | Pbigstring_set_64 _

    | Pbswap16
    | Pbbswap _
    | Pint_as_pointer ), _
    -> failwith "TODO"

let convert_and_bind
      (prim : Lambda.primitive)
      ~(args : Simple.t list)
      ~(exception_continuation : Continuation.t)
      (dbg : Debuginfo.t)
      (cont : Named.t -> Expr.t) : Expr.t =
  let expr = convert_lprim prim args dbg in
  bind_rec ~exception_continuation expr dbg cont
