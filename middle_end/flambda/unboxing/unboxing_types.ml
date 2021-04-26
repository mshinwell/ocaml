(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import
module Const = Reg_width_things.Const (* CR: add this in Simplify_import ? *)

(* Typedefs for unboxing decisions *)
(* ******************************* *)

type do_not_unbox_reason =
  | Not_beneficial
  | Max_depth_exceeded
  | Incomplete_parameter_type
  | Not_enough_information_at_use

(* extra_params_and_args : epa *)
type epa = {
  param : Variable.t;
  args : EPA.Extra_arg.t Apply_cont_rewrite_id.Map.t;
}

type unboxing_decision =
  | Unique_tag_and_size of {
      tag : Tag.t;
      fields : field_decision list;
    }
  | Variant of {
      tag : epa;
      constant_constructors : const_ctors;
      fields_by_tag : field_decision list Tag.Scannable.Map.t;
    }
  | Closure_single_entry of {
      closure_id : Closure_id.t;
      vars_within_closure : field_decision Var_within_closure.Map.t;
    }
  | Number of Flambda_kind.Naked_number_kind.t * epa

and field_decision = {
  epa : epa;
  decision : decision;
}

and const_ctors =
  | Zero
  | At_least_one of {
      is_int : epa;
      ctor : decision;
    }

and decision =
  | Unbox of unboxing_decision
  | Do_not_unbox of do_not_unbox_reason

type decisions = {
  decisions : (KP.t * decision) list;
  rewrite_ids_seen : Apply_cont_rewrite_id.Set.t;
}

(* Decision update pass.

   This notion of pass is used when turning a potential unboxing
   decision into a decision compatible with the given set of uses of the
   continuation. Indeed, depending on whether enough information is
   availbale at the use site, we can end up ina  few different cases:
   - enough information, and the unboxed values are directly available,
     in which case, we can use them directly
   - the unboxed values are not available directly, but can be "reasonably"
     computed by introducing a let-binding (e.g. a block field projection).
   - the unboxed values are not available, and would be too costly to
     compute (see the example about variants a few lines down).

   Thus, the first pass is used to filter out decisions which would end
   up in the third case. *)
type pass =
  | Filter of { recursive : bool; }
  (* First pass when computing unboxing decisions. This is done before
     insepcting the handler of the continuation whose parameters we are
     trying to unbox. For non-recursive continuation, that means that
     all use-sites of the continuation are known, but for recursive
     continuations, there are likely use sites that are not known at
     this point.

     For recursive continuations, we need to prevent unboxing variants
     and closures because we cannot be sure that reasonable extra_args can, be
     compute for all use-sites. For instance:

     let rec cont k x y =
       switch y with
       | 0 -> k (Some x)
       | 1 -> k (f x) (* for some function f in scope *)

     In this case, even if we know that x is an option, to unbox it, we'd
     need to introduce a switch in the `1` branch, which is
     1) not implemented (although tecnically possible)
     2) not efficient or beneficial in most cases
  *)
  | Compute_all_extra_args
  (* Last pass, after the traversla of the handler of the continuation.
     Thus, at this point, all use-sites are known, and we can compute
     the extra args that were not compute in the first pass (i.e. for
     use-sites that were not known during the first pass). *)


(* Printing *)
(* ******** *)

let print_epa fmt { param; args = _; } =
  Format.fprintf fmt "@[<hv 1>(\
    @[<hov>(param %a)@]@ \
    @[<v 2>(args@ <...>)@]\
    )"
    Variable.print param
    (* (Apply_cont_rewrite_id.Map.print EPA.Extra_arg.print) args *)

let print_do_not_unbox_reason ppf = function
  | Not_beneficial ->
    Format.fprintf ppf "not_beneficial"
  | Max_depth_exceeded ->
    Format.fprintf ppf "max_depth_exceeded"
  | Incomplete_parameter_type ->
    Format.fprintf ppf "incomplete_parameter_type"
  | Not_enough_information_at_use ->
    Format.fprintf ppf "not_enough_information_at_use"

let rec print_decision ppf = function
  | Do_not_unbox reason ->
    Format.fprintf ppf "@[<hov 1>(do_not_unbox@ %a)@]"
      print_do_not_unbox_reason reason
  | Unbox Unique_tag_and_size { tag; fields; } ->
    Format.fprintf ppf "@[<v 1>(unique_tag_and_size@ \
      @[<h>(static_tag %a)@]@ \
      @[<hv 2>(fields@ %a)@]\
      )@]"
      Tag.print tag
      print_fields_decisions fields
  | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
    Format.fprintf ppf "@[<v 2>(variant@ \
      @[<hov>(tag %a)@]@ \
      @[<hv 2>(constant_constructors@ %a)@]@ \
      @[<v 2>(fields_by_tag@ %a)@]\
      )@]"
      print_epa tag
      print_const_ctor_num constant_constructors
      (Tag.Scannable.Map.print print_fields_decisions) fields_by_tag
  | Unbox Closure_single_entry { closure_id; vars_within_closure; } ->
    Format.fprintf ppf "@[<hov 1>(closure_single_entry@ \
      @[<hov>(closure_id@ %a)@]@ \
      @[<hv 2>(var_within_closures@ %a)@]\
      )@]"
      Closure_id.print closure_id
      (Var_within_closure.Map.print print_field_decision) vars_within_closure
  | Unbox Number (kind, epa) ->
    Format.fprintf ppf "@[<hv 1>(number@ \
      @[<h>(kind %a)@]@ \
      @[<hv 1>(var %a)@]\
    )@]"
    Flambda_kind.Naked_number_kind.print kind
    print_epa epa

and print_field_decision ppf { epa; decision; } =
  Format.fprintf ppf "@[<hv 1>(@,\
    @[<hov 1>(var %a)@]@ \
    @[<hv 1>(decision@ %a)@]\
    )@]"
    print_epa epa
    print_decision decision

and print_fields_decisions ppf l =
  let pp_sep = Format.pp_print_space in
  Format.fprintf ppf "%a"
    (Format.pp_print_list ~pp_sep print_field_decision) l

and print_const_ctor_num ppf = function
  | Zero -> Format.fprintf ppf "no_constant_constructors"
  | At_least_one { is_int; ctor; } ->
    Format.fprintf ppf "@[<hov 1>(const_ctors@ \
      @[<hov 1>(is_int@ %a)@]@ \
      @[<hov 1>(ctor@ %a)@]\
      )@]"
      print_epa is_int
      print_decision ctor

let print ppf { decisions; rewrite_ids_seen; } =
  let pp_sep = Format.pp_print_space in
  let aux ppf (param, decision) =
    Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
      KP.print param print_decision decision
  in
  Format.fprintf ppf "@[<hov 1>(\
    @[<hov 1>(decisions@ %a)@]@ \
    @[<hov 1>(rewrite_ids_seen@ %a)@]\
    )@]"
    (Format.pp_print_list ~pp_sep aux) decisions
    Apply_cont_rewrite_id.Set.print rewrite_ids_seen
