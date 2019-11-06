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

module Threshold = struct
  type t =
    | Never_inline
    | Can_inline_if_no_larger_than of Inlining_size.t

  let add t1 t2 =
    match t1, t2 with
    | Never_inline, t -> t
    | t, Never_inline -> t
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
        Can_inline_if_no_larger_than (Inlining_size.(+) i1 i2)

  let sub t1 t2 =
    match t1, t2 with
    | Never_inline, _ -> Never_inline
    | t, Never_inline -> t
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
        if Inlining_size.(>) i1 i2 then
          Can_inline_if_no_larger_than (Inlining_size.(-) i1 i2)
        else
          Never_inline

  let min t1 t2 =
    match t1, t2 with
    | Never_inline, _ -> Never_inline
    | _, Never_inline -> Never_inline
    | Can_inline_if_no_larger_than i1, Can_inline_if_no_larger_than i2 ->
      Can_inline_if_no_larger_than (Inlining_size.min i1 i2)
end

let smaller expr ~than =
  Inlining_size.(<) (Expr.size expr) than

let can_inline lam inlining_threshold ~bonus =
  Profile.record_call ~accumulate:true "inlining size calc" (fun () ->
    match inlining_threshold with
    | Threshold.Never_inline -> false
    | Threshold.Can_inline_if_no_larger_than inlining_threshold ->
       smaller
         lam
         ~than:(Inlining_size.(+) inlining_threshold bonus))

let _cost (flag : Clflags.Int_arg_helper.parsed) ~round =
  Clflags.Int_arg_helper.get ~key:round flag

let _benefit_factor = 1

module Benefit = struct
  type t = {
    remove_call : int;
    remove_alloc : int;
    remove_prim : int;
    remove_branch : int;
    (* CR-someday pchambart: branch_benefit : t list; *)
    direct_call_of_indirect : int;
    requested_inline : Inlining_size.t;
    (* Benefit to compensate the size of functions marked for inlining *)
  }

  let zero = {
    remove_call = 0;
    remove_alloc = 0;
    remove_prim = 0;
    remove_branch = 0;
    direct_call_of_indirect = 0;
    requested_inline = Inlining_size.zero;
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
    let size = Expr.size size_of in
    { t with requested_inline = Inlining_size.(+) t.requested_inline size; }

(*
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

  let (-) t1 t2 = {
    remove_call = t1.remove_call - t2.remove_call;
    remove_alloc = t1.remove_alloc - t2.remove_alloc;
    remove_prim = t1.remove_prim - t2.remove_prim;
    remove_branch = t1.remove_branch - t2.remove_branch;
    direct_call_of_indirect =
      t1.direct_call_of_indirect - t2.direct_call_of_indirect;
    requested_inline = t1.requested_inline - t2.requested_inline;
  }

  let max ~round t1 t2 =
    let c1 = evaluate ~round t1 in
    let c2 = evaluate ~round t2 in
    if c1 > c2 then t1 else t2
*)

(*
  let add_code lam b =
    b - (remove_code lam zero)

  let add_code_named lam b =
    b - (remove_code_named lam zero)
*)
end

let scale_inline_threshold_by = 8
