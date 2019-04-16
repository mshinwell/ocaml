(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import

(* Simple approximation of the space cost of a primitive. *)
let prim_size (_prim : Flambda_primitive.t) = 1
(* CR mshinwell: Implement this function
  match prim with
  | Pidentity -> 0
  | Pgetglobal _ -> 1
  | Psetglobal _ -> 1
  | Pmakeblock _ -> 5 + List.length args
  | Pfield _ -> 1
  | Psetfield (_, isptr, init) ->
    begin match init with
    | Root_initialization -> 1  (* never causes a write barrier hit *)
    | Assignment | Heap_initialization ->
      match isptr with
      | Pointer -> 4
      | Immediate -> 1
    end
  | Pfloatfield _ -> 1
  | Psetfloatfield _ -> 1
  | Pduprecord _ -> 10 + List.length args
  | Pccall p -> (if p.Primitive.prim_alloc then 10 else 4) + List.length args
  | Praise _ -> 4
  | Pstringlength -> 5
  | Pbyteslength -> 5
  | Pstringrefs -> 6
  | Pbytesrefs | Pbytessets -> 6
  | Pmakearray _ -> 5 + List.length args
  | Parraylength Pgenarray -> 6
  | Parraylength _ -> 2
  | Parrayrefu Pgenarray -> 12
  | Parrayrefu _ -> 2
  | Parraysetu Pgenarray -> 16
  | Parraysetu _ -> 4
  | Parrayrefs Pgenarray -> 18
  | Parrayrefs _ -> 8
  | Parraysets Pgenarray -> 22
  | Parraysets _ -> 10
  | Pbittest -> 3
  | Pbigarrayref (_, ndims, _, _, _) -> 4 + ndims * 6
  | Pbigarrayset (_, ndims, _, _, _) -> 4 + ndims * 6
  | Psequand | Psequor ->
    Misc.fatal_error "Psequand and Psequor are not allowed in Prim \
        expressions; translate out instead (cf. closure_conversion.ml)"
  (* CR-soon mshinwell: This match must be made exhaustive.
     mshinwell: Let's do this when we have the new size computation. *)
  | _ -> 2 (* arithmetic and comparisons *)
*)

(* Simple approximation of the space cost of an Flambda expression. *)

(* CR-soon mshinwell: Investigate revised size numbers. *)

let direct_call_size = 4
let _project_size = 1

let smaller' expr ~than:threshold =
  let size = ref 0 in
  let rec expr_size expr =
    if !size > threshold then raise Exit;
    match Expr.descr expr with
    | Let let_expr ->
      named_size (Let.defining_expr let_expr);
      Let.pattern_match let_expr
        ~f:(fun ~bound_vars:_ ~body -> expr_size body)
    | Let_cont (Non_recursive { handler; _ }) ->
      Non_recursive_let_cont_handler.pattern_match handler
        ~f:(fun _cont ~body -> expr_size body);
      continuation_handler_size
        (Non_recursive_let_cont_handler.handler handler)
    | Let_cont (Recursive handlers) ->
      Recursive_let_cont_handlers.pattern_match handlers
        ~f:(fun ~body handlers ->
          expr_size body;
          let handlers = Continuation_handlers.to_map handlers in
          Continuation.Map.iter (fun _cont handler ->
              continuation_handler_size handler)
            handlers)
    | Apply apply ->
      let call_cost =
        match Apply.call_kind apply with
        | Function Direct _ -> direct_call_size
        (* CR mshinwell: Check / fix these numbers *)
        | Function Indirect_unknown_arity -> 6
        | Function Indirect_known_arity _ -> 6
        | Method _ -> 1
        | C_call _ -> 1
      in
      size := !size + call_cost
    | Apply_cont _ -> incr size
    | Switch switch -> size := !size + (5 * Switch.num_arms switch)
    | Invalid _ -> ()
  and named_size (named : Named.t) =
    if !size > threshold then raise Exit;
    match named with
    | Simple simple ->
      begin match Simple.descr simple with
      | Name _ -> ()
      | Const _  | Discriminant _ -> incr size
      end
    | Set_of_closures set_of_closures ->
      let func_decls = Set_of_closures.function_decls set_of_closures in
      let funs = Function_declarations.funs func_decls in
      Closure_id.Map.iter (fun _ func_decl ->
          Function_params_and_body.pattern_match
            (Function_declaration.params_and_body func_decl)
            ~f:(fun ~return_continuation:_ _exn_continuation _params
                    ~body ~my_closure:_ ->
              expr_size body))
        funs
    | Prim (prim, _dbg) ->
      size := !size + prim_size prim
  and continuation_handler_size handler =
    let params_and_handler = Continuation_handler.params_and_handler handler in
    Continuation_params_and_handler.pattern_match params_and_handler
      ~f:(fun _params ~handler -> expr_size handler)
  in
  try
    expr_size expr;
    if !size <= threshold then Some !size
    else None
  with Exit ->
    None

let size expr =
  match smaller' expr ~than:max_int with
  | Some size -> size
  | None ->
    (* There is no way that an expression of size max_int could fit in
       memory. *)
    assert false  (* CR mshinwell: this should not be an assertion *)

(*
let sizes exprs =
  List.fold_left (fun total expr -> total + size expr) 0 exprs
*)

module Threshold = struct
  type t =
    | Never_inline
    | Can_inline_if_no_larger_than of int

  let add t1 t2 =
    match t1, t2 with
    | Never_inline, t -> t
    | t, Never_inline -> t
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
        Can_inline_if_no_larger_than (i1 + i2)

  let sub t1 t2 =
    match t1, t2 with
    | Never_inline, _ -> Never_inline
    | t, Never_inline -> t
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
        if i1 > i2 then Can_inline_if_no_larger_than (i1 - i2)
        else Never_inline

  let min t1 t2 =
    match t1, t2 with
    | Never_inline, _ -> Never_inline
    | _, Never_inline -> Never_inline
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
      Can_inline_if_no_larger_than (min i1 i2)
end

let smaller lam ~than =
  smaller' lam ~than <> None

let can_inline lam inlining_threshold ~bonus =
  match inlining_threshold with
  | Threshold.Never_inline -> false
  | Threshold.Can_inline_if_no_larger_than inlining_threshold ->
     smaller
       lam
       ~than:(inlining_threshold + bonus)

let cost (flag : Clflags.Int_arg_helper.parsed) ~round =
  Clflags.Int_arg_helper.get ~key:round flag

let benefit_factor = 1

module Benefit = struct
  type t = {
    remove_call : int;
    remove_alloc : int;
    remove_prim : int;
    remove_branch : int;
    (* CR-someday pchambart: branch_benefit : t list; *)
    direct_call_of_indirect : int;
    requested_inline : int;
    (* Benefit to compensate the size of functions marked for inlining *)
  }

  let zero = {
    remove_call = 0;
    remove_alloc = 0;
    remove_prim = 0;
    remove_branch = 0;
    direct_call_of_indirect = 0;
    requested_inline = 0;
  }

  let remove_call t = { t with remove_call = t.remove_call + 1; }
  let remove_alloc t = { t with remove_alloc = t.remove_alloc + 1; }

  let add_primitive _prim t =
    { t with remove_prim = t.remove_prim - 1; }

  let remove_primitive _prim t =
    { t with remove_prim = t.remove_prim + 1; }

  let remove_primitive_application _prim t =
    { t with remove_prim = t.remove_prim + 1; }

  let remove_branch t = { t with remove_branch = t.remove_branch + 1; }

  let direct_call_of_indirect_known_arity t =
    { t with direct_call_of_indirect = t.direct_call_of_indirect + 1; }

  let direct_call_of_indirect_unknown_arity t =
    { t with direct_call_of_indirect = t.direct_call_of_indirect + 1; }

  let requested_inline t ~size_of =
    let size = size size_of in
    { t with requested_inline = t.requested_inline + size; }

  let print ppf b =
    Format.fprintf ppf "@[remove_call: %i@ remove_alloc: %i@ \
                        remove_prim: %i@ remove_branch: %i@ \
                        direct: %i@ requested: %i@]"
      b.remove_call
      b.remove_alloc
      b.remove_prim
      b.remove_branch
      b.direct_call_of_indirect
      b.requested_inline

  let evaluate t ~round : int =
    benefit_factor *
      (t.remove_call * (cost !Clflags.inline_call_cost ~round)
       + t.remove_alloc * (cost !Clflags.inline_alloc_cost ~round)
       + t.remove_prim * (cost !Clflags.inline_prim_cost ~round)
       + t.remove_branch * (cost !Clflags.inline_branch_cost ~round)
       + (t.direct_call_of_indirect
         * (cost !Clflags.inline_indirect_cost ~round)))
    + t.requested_inline

  let (+) t1 t2 = {
    remove_call = t1.remove_call + t2.remove_call;
    remove_alloc = t1.remove_alloc + t2.remove_alloc;
    remove_prim = t1.remove_prim + t2.remove_prim;
    remove_branch = t1.remove_branch + t2.remove_branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect + t2.direct_call_of_indirect;
    requested_inline = t1.requested_inline + t2.requested_inline;
  }

(*
  let (-) t1 t2 = {
    remove_call = t1.remove_call - t2.remove_call;
    remove_alloc = t1.remove_alloc - t2.remove_alloc;
    remove_prim = t1.remove_prim - t2.remove_prim;
    remove_branch = t1.remove_branch - t2.remove_branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect - t2.direct_call_of_indirect;
    requested_inline = t1.requested_inline - t2.requested_inline;
  }
*)

  let max ~round t1 t2 =
    let c1 = evaluate ~round t1 in
    let c2 = evaluate ~round t2 in
    if c1 > c2 then t1 else t2

(*
  let add_code lam b =
    b - (remove_code lam zero)

  let add_code_named lam b =
    b - (remove_code_named lam zero)
*)
end

let scale_inline_threshold_by = 8
