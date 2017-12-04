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

(* let convert (prim : Lambda.primitive) (args : Simple.t list) : P.t = *)
(*   match prim, args with *)
(*   | Pmakeblock (tag, flag, shape), _ -> *)
(*     let flag = convert_mutable_flag flag in *)
(*     let arity = of_block_shape shape ~num_fields:(List.length args) in *)
(*     Variadic (Make_block (Tag.Scannable.create_exn tag, flag, arity), args) *)
(*   | Pnegint, [arg] -> *)
(*     Unary (Int_arith (Flambda_kind.Standard_int.Tagged_immediate, Neg), arg) *)
(*   | Paddint, [arg1; arg2] -> *)
(*     Binary (Int_arith (Flambda_kind.Standard_int.Tagged_immediate, Add), arg1, arg2) *)
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

type expr_primitive =
  | Unary of P.unary_primitive * simple_or_prim
  | Binary of P.binary_primitive * simple_or_prim * simple_or_prim
  | Ternary of P.ternary_primitive * simple_or_prim * simple_or_prim * simple_or_prim
  | Variadic of P.variadic_primitive * (simple_or_prim list)
  | Checked of { validity_condition : expr_primitive;
                 primitive : expr_primitive;
                 failure : Ident.t; (* Predefined exception *)
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

let convert_lprim (prim : Lambda.primitive) (args : Simple.t list)
      (dbg : Debuginfo.t) : expr_primitive =
  let args = List.map (fun arg : simple_or_prim -> Simple arg) args in
  match prim, args with
  | Pmakeblock (tag, flag, shape), _ ->
    let flag = convert_mutable_flag flag in
    let arity = of_block_shape shape ~num_fields:(List.length args) in
    Variadic (Make_block (Tag.Scannable.create_exn tag, flag, arity), args)
  | Pnegint, [arg] ->
    Unary (Int_arith (Flambda_kind.Standard_int.Tagged_immediate, Neg), arg)
  | Pfield field, [arg] ->
    (* CR pchambart: every load is annotated as mutable we must be
       careful to update that when we know it is not. This should not
       be an error.
       We need more type propagations to be precise here *)
    Unary (Block_load (field, Not_a_float, Mutable), arg)
  | Psetfield (field, immediate_or_pointer, initialization_or_assignment),
    [block; value] ->
    let set_kind : P.block_set_kind =
      match immediate_or_pointer with
        | Immediate -> Immediate
        | Pointer -> Pointer
    in
    let init_or_assign : P.init_or_assign =
      match initialization_or_assignment with
      | Assignment -> Assignment
      | Heap_initialization -> Initialization
      (* Root initialization cannot exist in lambda. This is
         represented by the static part of expressions in flambda. *)
      | Root_initialization -> assert false
    in
    Binary (Block_set (field, set_kind, init_or_assign), block, value)

  (* Test checked *)

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
      failure = Predef.ident_division_by_zero;
      dbg;
    }


  | ( Pdivint Unsafe | Psetglobal _ | Ploc _ ), _ ->
    Misc.fatal_errorf "Closure_conversion.convert_primitive: \
                       Primitive %a shouldn't be here"
      Printlambda.primitive prim
  | ( Pfield _ | Pnegint | Pdivint _ | Psetfield _ ), _ ->
    Misc.fatal_errorf "Closure_conversion.convert_primitive: \
                       Wrong arrity for %a: %i"
      Printlambda.primitive prim (List.length args)

  | ( Pidentity | Pignore | Prevapply | Pdirapply ), _ ->
    Misc.fatal_errorf "[%a] should have been removed by \
      [Prepare_lambda.prepare]"
      Printlambda.primitive prim

  | ( Pgetglobal _ | Pread_mutable _ ), _ ->
    Misc.fatal_errorf "[%a] should have been handled by \
      [Closure_conversion.close_named]"
      Printlambda.primitive prim


  | ( Pbytes_to_string
    | Pbytes_of_string
    | Pfield_computed
    | Psetfield_computed _
    | Pfloatfield _
    | Psetfloatfield _
    | Pduprecord _
    | Plazyforce
    | Pccall _
    | Pccall_unboxed _
    | Praise _
    | Psequand
    | Psequor
    | Pnot
    | Paddint
    | Psubint
    | Pmulint
    | Pmodint _
    | Pandint
    | Porint
    | Pxorint
    | Plslint
    | Plsrint
    | Pasrint
    | Pintcomp _
    | Poffsetint _
    | Poffsetref _
    | Pintoffloat _
    | Pfloatofint _
    | Pnegfloat _
    | Pabsfloat _
    | Paddfloat _
    | Psubfloat _
    | Pmulfloat _
    | Pdivfloat _
    | Pfloatcomp _
    | Pstringlength
    | Pstringrefu
    | Pstringrefs
    | Pbyteslength
    | Pbytesrefu
    | Pbytessetu
    | Pbytesrefs
    | Pbytessets
    | Pmakearray _
    | Pduparray _
    | Parraylength _
    | Parrayrefu _
    | Parraysetu _
    | Parrayrefs _
    | Parraysets _
    | Pisint
    | Pgettag
    | Pisout
    | Pbittest
    | Pbintofint _
    | Pintofbint _
    | Pcvtbint _
    | Pnegbint _
    | Paddbint _
    | Psubbint _
    | Pmulbint _
    | Pdivbint _
    | Pmodbint _
    | Pandbint _
    | Porbint _
    | Pxorbint _
    | Plslbint _
    | Plsrbint _
    | Pasrbint _
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
    | Pctconst _
    | Pbswap16
    | Pbbswap _
    | Pint_as_pointer
    | Popaque
    | Preturn
    | Pmake_unboxed_tuple
    | Punboxed_tuple_field _
    | Punbox_float
    | Pbox_float
    | Punbox_int32
    | Pbox_int32
    | Punbox_int64
    | Pbox_int64
    | Punbox_nativeint
    | Pbox_nativeint
    | Puntag_immediate
    | Ptag_immediate ), _
    -> failwith "TODO"

let convert_and_bind
      (prim : Lambda.primitive)
      ~(args : Simple.t list)
      ~(exception_continuation : Continuation.t)
      (dbg : Debuginfo.t)
      (cont : Named.t -> Expr.t) : Expr.t =
  let expr = convert_lprim prim args dbg in
  bind_rec ~exception_continuation expr dbg cont
