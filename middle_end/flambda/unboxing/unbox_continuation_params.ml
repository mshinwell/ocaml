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


(* Invalid constants, Poison and aliases

   A poison (or invalid_constant) value is a value that will (or should) never
   actually be read during execution of the program; but has to be provided in
   the program to respect arity.

   There are 3 cases where we produce invalid constants:
   - in dead branchs of code (i.e. when the `prove_simple` function of an
     unboxer returns `Invalid`
   - we generate extra parameters for each field of each potential tag when
     unboxing a variant and thus at each call site we have to provide values
     for each field; in particular for fields of a tag that is not the one at
     a call-site, we must provide poison values.
   - when we recursively try to unbox a field of a variant, for which a poison
     value was generated at call site, the unboxer also generates an invalid
     constant.

   Example:
   ```
   if b then
      (* 1 *) apply_cont k (Block (Tag 0) (foo))
   else
      (* 2 *) apply_cont k (Block (Tag 1) (foo bar))
   where k v =
   switch (tag v) with
   | 0 -> k1 (field0 v)
   | 1 -> k2 (field0 v) (field1 v)
   ```

   when unboxing the parameter v of k, we create the following parameters:
   - is_int and constant_ctor (not used in this case)
   - tag
   - unboxedfield_tag0_0, unboxed_field_tag1_0 and unboxed_field_tag1_1
     for the three different fields that belong to the variant.

   Thus at each call site, while the value for the unboxed field of the
   corresponding tag are known and usable, there can be no correct values
   for the values of unboxed fields of other tags,
   e.g. at call site (* 1 *), the tag of `v` is 0, and the value computed
   for unboxedfield_tag0_0 is `foo`, and we know that in the handler of
   k (and later) the unboxedfield_tag1_0 and unboxed_field_tag1_1 parameters
   will never actually be used since they should onyl be used when the tag
   of `v` is 1. However we must still provide value to the apply_cont.

   After unboxing, we thus get the following:

   ```
   if b then
      let v = Block (Tag 0) (foo) in
      (* 1 *) apply_cont k v 0 foo #poison #poison
   else
      let v = Block (Tag 1) (foo bar) in
      (* 2 *) apply_cont k v 1 #poison foo bar
   where k v unboxed_tag
      unboxedfield_tag0_0 unboxedfield_tag1_0 unboxedfield_tag1_1 =
   switch tag with
   | 0 -> k1 unboxedfield_tag0_0
   | 1 -> k2 unboxedfield_tag1_0 unboxedfield_tag1_1
   ```

   where the handler of `k` has been simplified thanks to the equations added
   to the typing env by the unboxing decisions. This is done by
   `denv_of_decision`, by performing meets on the typing environment between
   the existing type for parameters, and the shape corresponding to the
   unboxing decisions (additionally, equations about get_tag and is_int are
   added to the cse environment of the denv).

   In this example, a meet will be performed between:
   - v = Variant (Tag 0 -> (Known 1) (foo)
                  Tag 1 -> (Known 2) (foo bar)
     the type computed by the join on the continuation uses (before unboxing)
   - Variant (Tag 0 -> (Known 1) (unboxedfield_tag0_0)
              Tag 1 -> (Known 2) (unboxedfield_tag1_0 unboxedfield_tag1_1)
     the shape create from the unboxing decision
   and will result an empty env_extension, and the following updated type for the
   variant:
   - v = Variant (
      Tag 0 -> (Known 1) (foo) (env_extension (foo = unboxedfield_tag0_0))
      Tag 1 -> (Known 2) (foo bar) (env_extension (foo = unboxedfield_tag1_0)
                                                  (bar = unboxedfield_tag1_1))
     )

   The handler of `k` is then simplified, and after a switch on the tag of the
   variant, the env_extension corresponding to the tag is added to each
   branch's typing environment. In each branch, the loads to access the
   variant's fields can be resolved to the unboxed parameters added,
   eliminating reads, and in this case allowing to remove the parameter `v`,
   and thus also remove its allocation before the apply_cont to k.

   Note that simply adding the three following equations
      (foo = unboxedfield_tag0_0)
      (foo = unboxedfield_tag1_0)
      (bar = unboxedfield_tag1_1)
   to the typing env in `denv_of_decision` would be incorrect. This is because
   with these three equations at the same time, unboxedfield_tag0_0 and
   unboxedfield_tag1_0 would become aliased, and thus the simplifier would be
   allowed to substitute unboxedfield_tag0_0 to unboxedfield_tag1_0 anywhere in
   the handler, which is incorrect (and will lead to segfaults !). This problem
   is avoided in the current code because when performing the meet between the
   param type and the unboxing shape, the env_extension returned by row_like is
   empty because it is the join of the env_extensions for each tag. We then
   never open together the env_extensions that are specific to each tag.

   Now consider that a second pass of unboxing is performed on `k`. Before that
   second unboxing decision, the code will look like this:

   ```
   if b then
      (* 1 *) apply_cont k 0 foo #poison #poison
   else
      (* 2 *) apply_cont k 1 #poison foo bar
   where k unboxed_tag
    unboxedfield_tag0_0
    unboxedfield_tag1_0 unboxedfield_tag1_1 =
   switch tag with
   | 0 -> k1 unboxedfield_tag0_0
   | 1 -> k2 unboxedfield_tag1_0 unboxedfield_tag1_1
   ```

   We need here be careful about the handling of the `#poison` values with
   regards to the join performed at the continuation use sites. More
   specifically, while #poison may be locally considered as bottom on values
   during the join, it cannot be considered as bottom when it comes to aliases,
   otherwise we could end up in a situation where unboxedfield_tag0_0 and
   unboxedfield_tag1_0 become aliased.

   Currently, the values generated by this module are constants of the correct
   kind, with no special behavior with regards to type. The joins will thus use
   the actual runtime value, and avoid any problem with invalid aliases.
   However, this leads to a loss of precision for the types of unboxed
   parameters, and often prevent further unboxing. A dedicated poison
   value/constant might enable the join to avoid this loss of precision, *but*
   one should be careful that such poisons does not introduce aliases as
   described above.

   In these cases, some information could be recovered by attaching
   env_extension to each branch of a switch on the tag of the variant. This is
   not implemented yet.
*)


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


(* small helper used later *)
let pp_tag print_tag ppf tag =
  if print_tag then Format.fprintf ppf "_%d" (Tag.to_int tag)


(* Unboxers *)
(* ******** *)

type number_decider = {
  param_name : string;
  kind : Flambda_kind.Naked_number_kind.t;
  prove_is_a_boxed_number : TE.t -> T.t -> unit T.proof_allowing_kind_mismatch;
}

type unboxer = {
  var_name : string;
  invalid_const : Const.t;
  unboxing_prim : Simple.t -> Flambda_primitive.t;
  prove_simple :
    TE.t -> min_name_mode:Name_mode.t -> T.t -> Simple.t T.proof;
}

module Immediate = struct

  let decider = {
    param_name = "naked_immediate";
    kind = K.Naked_number_kind.Naked_immediate;
    prove_is_a_boxed_number = T.prove_is_a_tagged_immediate;
  }

  let unboxing_prim simple =
    Flambda_primitive.(Unary (Unbox_number Untagged_immediate, simple))

  let unboxer = {
    var_name = "naked_immediate";
    invalid_const = Const.naked_immediate (Target_imm.int (Targetint.OCaml.of_int 0xabcd));
    unboxing_prim;
    prove_simple = T.prove_untagged_int_simple;
  }

end

module Float = struct

  let decider = {
    param_name = "unboxed_float";
    kind = K.Naked_number_kind.Naked_float;
    prove_is_a_boxed_number = T.prove_is_a_boxed_float;
  }

  let unboxing_prim simple =
    Flambda_primitive.(Unary (Unbox_number Naked_float, simple))

  let unboxer = {
    var_name = "unboxed_float";
    invalid_const = Const.naked_float Numbers.Float_by_bit_pattern.zero;
    unboxing_prim;
    prove_simple = T.prove_unboxed_float_simple;
  }

end

module Int32 = struct

  let decider = {
    param_name = "unboxed_int32";
    kind = K.Naked_number_kind.Naked_int32;
    prove_is_a_boxed_number = T.prove_is_a_boxed_int32;
  }

  let unboxing_prim simple =
    Flambda_primitive.(Unary (Unbox_number Naked_int32, simple))

  let unboxer = {
    var_name = "unboxed_int32";
    invalid_const = Const.naked_int32 Int32.(div 0xabcd0l 2l);
    unboxing_prim;
    prove_simple = T.prove_unboxed_int32_simple;
  }

end

module Int64 = struct

  let decider = {
    param_name = "unboxed_int64";
    kind = K.Naked_number_kind.Naked_int64;
    prove_is_a_boxed_number = T.prove_is_a_boxed_int64;
  }

  let unboxing_prim simple =
    Flambda_primitive.(Unary (Unbox_number Naked_int64, simple))

  let unboxer = {
    var_name = "unboxed_int64";
    invalid_const = Const.naked_int64 Int64.(div 0xdcba0L 2L);
    unboxing_prim;
    prove_simple = T.prove_unboxed_int64_simple;
  }

end

module Nativeint = struct

  let decider = {
    param_name = "unboxed_nativeint";
    kind = K.Naked_number_kind.Naked_nativeint;
    prove_is_a_boxed_number = T.prove_is_a_boxed_nativeint;
  }

  let unboxing_prim simple =
    Flambda_primitive.(Unary (Unbox_number Naked_nativeint, simple))

  let unboxer = {
    var_name = "unboxed_nativeint";
    invalid_const = Const.naked_nativeint Targetint.zero;
    unboxing_prim;
    prove_simple = T.prove_unboxed_nativeint_simple;
  }

end

module Field = struct

  let unboxing_prim bak field_nth block_simple =
    let field_const = Simple.const (Const.tagged_immediate field_nth) in
    Flambda_primitive.(
      Binary (Block_load (bak, Immutable), block_simple, field_const)
    )

  let unboxer cst bak field_nth = {
    var_name = "field_at_use";
    invalid_const = cst;
    unboxing_prim = unboxing_prim bak field_nth;
    prove_simple = (fun tenv ~min_name_mode t ->
      T.prove_block_field_simple tenv ~min_name_mode t field_nth);
  }

end

module Closure_field = struct

  let unboxing_prim closure_id var closure_simple =
    Flambda_primitive.(Unary (
      Project_var { project_from = closure_id; var }, closure_simple))

  let unboxer closure_id var = {
    var_name = "closure_field_at_use";
    invalid_const = Const.const_zero;
    unboxing_prim = unboxing_prim closure_id var;
    prove_simple = (fun tenv ~min_name_mode t ->
      T.prove_project_var_simple tenv ~min_name_mode t var);
  }

end


(* Some helpers *)
(* ************ *)

let new_param name = {
  param = Variable.create name;
  args = Apply_cont_rewrite_id.Map.empty;
}

let update_param_args epa rewrite_id extra_arg =
  assert (not (Apply_cont_rewrite_id.Map.mem rewrite_id epa.args));
  let args = Apply_cont_rewrite_id.Map.add rewrite_id extra_arg epa.args in
  { epa with args; }


(* Unfold a type into a decision tree *)
(* ********************************** *)

(* CR: make this a Clflag *)
let max_unboxing_depth = 1
let unbox_numbers = true
let unbox_blocks = true
let unbox_variants = true
let unbox_closures = true

let make_optimistic_const_ctor () =
  let is_int = new_param "is_int" in
  let unboxed_const_ctor = new_param "unboxed_const_ctor" in
  let ctor = Unbox (Number (Naked_immediate, unboxed_const_ctor)) in
  At_least_one { is_int; ctor; }

let make_optimistic_number_decision tenv param_type decider : decision option =
  match decider.prove_is_a_boxed_number tenv param_type with
  | Proved () ->
    let naked_number = new_param decider.param_name in
    Some (Unbox (Number (decider.kind, naked_number)))
  | Wrong_kind | Invalid | Unknown ->
    None

let decide tenv param_type deciders : decision option =
  List.find_map (make_optimistic_number_decision tenv param_type) deciders

let deciders = [
  Immediate.decider;
  Float.decider;
  Int32.decider;
  Int64.decider;
  Nativeint.decider;
]

let rec make_optimist_decision ~depth tenv param_type : decision =
  match decide tenv param_type deciders with
  | Some decision ->
    if unbox_numbers then decision else Do_not_unbox Incomplete_parameter_type
  | None ->
    if depth >= max_unboxing_depth then Do_not_unbox Max_depth_exceeded
    else match T.prove_unique_tag_and_size tenv param_type with
      | Proved (tag, size) when unbox_blocks ->
        let fields =
          make_optimistic_fields
            ~add_tag_to_name:false ~depth
            tenv param_type tag size
        in
        Unbox (Unique_tag_and_size { tag; fields; })
      | Proved _ | Wrong_kind | Invalid | Unknown ->
        match T.prove_variant_like tenv param_type with
        | Proved { const_ctors; non_const_ctors_with_sizes; } when unbox_variants->
          let tag = new_param "tag" in
          let constant_constructors =
            match const_ctors with
            | Known set when Target_imm.Set.is_empty set -> Zero
            | Unknown | Known _ -> make_optimistic_const_ctor ()
          in
          let fields_by_tag =
            Tag.Scannable.Map.mapi (fun scannable_tag size ->
              let tag = Tag.Scannable.to_tag scannable_tag in
              make_optimistic_fields
                ~add_tag_to_name:true ~depth
                tenv param_type tag size
            ) non_const_ctors_with_sizes
          in
          Unbox (Variant { tag; constant_constructors; fields_by_tag; })
        | Proved _ | Wrong_kind | Invalid | Unknown ->
          begin match T.prove_single_closures_entry' tenv param_type with
          | Proved (closure_id, closures_entry, _fun_decl) when unbox_closures ->
            let vars_within_closure =
              make_optimistic_vars_within_closure ~depth tenv closures_entry
            in
            Unbox (Closure_single_entry { closure_id; vars_within_closure })
          | Proved _ | Wrong_kind | Invalid | Unknown -> Do_not_unbox Incomplete_parameter_type
          end

and make_optimistic_fields
      ~add_tag_to_name ~depth
      tenv param_type (tag : Tag.t) size =
  let field_kind, field_base_name =
    if Tag.equal tag Tag.double_array_tag
    then K.naked_float, "unboxed_float_field"
    else K.value, "unboxed_field"
  in
  let field_name n =
    Format.asprintf "%s%a_%d" field_base_name (pp_tag add_tag_to_name) tag n
  in
  let field_vars =
    List.init (Targetint.OCaml.to_int size)
      (fun i -> new_param (field_name i))
  in
  let type_of_var epa =
    Flambda_type.alias_type_of field_kind (Simple.var epa.param)
  in
  let field_types = List.map type_of_var field_vars in
  let tenv =
    List.fold_left (fun acc { param = var; args = _; } ->
      let name = Name_in_binding_pos.create (Name.var var) Name_mode.normal in
      TE.add_definition acc name field_kind
    ) tenv field_vars
  in
  let shape =
    Flambda_type.immutable_block ~is_unique:false tag
      ~field_kind ~fields:field_types
  in
  let env_extension =
    match T.meet tenv param_type shape with
    | Ok (_, env_extension) -> env_extension
    | Bottom ->
      Misc.fatal_errorf "Meet failed whereas prove previously succeeded"
  in
  let tenv = TE.add_env_extension tenv env_extension in
  let fields =
    List.map2 (fun epa var_type ->
      let decision = make_optimist_decision ~depth:(depth + 1) tenv var_type in
      { epa; decision; }
    ) field_vars field_types
  in
  fields

and make_optimistic_vars_within_closure
      ~depth tenv closures_entry =
  let map = T.Closures_entry.closure_var_types closures_entry in
  Var_within_closure.Map.mapi (fun var_within_closure var_type ->
    let epa = new_param (Var_within_closure.to_string var_within_closure) in
    let decision = make_optimist_decision ~depth:(depth + 1) tenv var_type in
    { epa; decision; }
  ) map


(* Decision tree -> actual typing env *)
(* ********************************** *)

let add_equation_on_var denv var shape =
  let kind = T.kind shape in
  let var_type = T.alias_type_of kind (Simple.var var) in
  match T.meet (DE.typing_env denv) var_type shape with
  | Ok (_ty, env_extension) ->
    DE.map_typing_env denv ~f:(fun tenv ->
      TE.add_env_extension tenv env_extension)
  | Bottom ->
    Misc.fatal_errorf "Meet failed whereas prove previously succeeded"

let denv_of_number_decision naked_kind shape param_var naked_var denv : DE.t =
  let naked_name =
    Var_in_binding_pos.create naked_var Name_mode.normal
  in
  let denv = DE.define_variable denv naked_name naked_kind in
  add_equation_on_var denv param_var shape

let rec denv_of_decision denv param_var decision : DE.t =
  match decision with
  | Do_not_unbox _ -> denv
  | Unbox Unique_tag_and_size { tag; fields; } ->
    let field_kind =
      if Tag.equal tag Tag.double_array_tag then K.naked_float else K.value
    in
    let denv =
      List.fold_left (fun denv { epa = { param = var; _ }; _ } ->
        let v = Var_in_binding_pos.create var Name_mode.normal in
        DE.define_variable denv v field_kind
      ) denv fields
    in
    let type_of_var field =
      Flambda_type.alias_type_of field_kind (Simple.var field.epa.param)
    in
    let field_types = List.map type_of_var fields in
    let shape =
      Flambda_type.immutable_block ~is_unique:false tag
        ~field_kind ~fields:field_types
    in
    let denv = add_equation_on_var denv param_var shape in
    List.fold_left (fun denv field ->
      denv_of_decision denv field.epa.param field.decision
    ) denv fields

  (* Closure case *)
  | Unbox Closure_single_entry { closure_id; vars_within_closure; } ->
    let denv =
      Var_within_closure.Map.fold (fun _ { epa = { param = var; _ }; _ } denv ->
        let v = Var_in_binding_pos.create var Name_mode.normal in
        DE.define_variable denv v K.value
      ) vars_within_closure denv
    in
    let map =
      Var_within_closure.Map.map
        (fun { epa = { param = var; _ }; _ } -> var) vars_within_closure
    in
    let shape =
      Flambda_type.closure_with_at_least_these_closure_vars
        ~this_closure:closure_id map
    in
    let denv = add_equation_on_var denv param_var shape in
    Var_within_closure.Map.fold (fun _ field denv ->
      denv_of_decision denv field.epa.param field.decision
    ) vars_within_closure denv

  (* Variant case*)
  | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
    (* Adapt the denv for the tag *)
    let tag_v = Var_in_binding_pos.create tag.param Name_mode.normal in
    let denv = DE.define_variable denv tag_v K.naked_immediate in
    let denv =
      DE.add_equation_on_variable denv tag.param
        (T.get_tag_for_block ~block:(Simple.var param_var))
    in
    let get_tag_prim =
      P.Eligible_for_cse.create_exn (Unary (Get_tag, Simple.var param_var))
    in
    let denv = DE.add_cse denv get_tag_prim ~bound_to:(Simple.var tag.param) in
    (* Same thing for is_int *)
    let denv =
      match constant_constructors with
      | Zero -> denv
      | At_least_one { is_int; _ } ->
        let is_int_v = Var_in_binding_pos.create is_int.param Name_mode.normal in
        let denv = DE.define_variable denv is_int_v K.naked_immediate in
        let denv =
          DE.add_equation_on_variable denv is_int.param
            (T.is_int_for_scrutinee ~scrutinee:(Simple.var param_var))
        in
        let is_int_prim =
          P.Eligible_for_cse.create_exn (Unary (Is_int, Simple.var param_var))
        in
        let denv = DE.add_cse denv is_int_prim ~bound_to:(Simple.var is_int.param) in
        denv
    in
    let denv, const_ctors =
      match constant_constructors with
      | Zero ->
        denv, T.bottom K.naked_immediate
      | At_least_one { ctor = Do_not_unbox _; _ } ->
        denv, T.unknown K.naked_immediate
      | At_least_one { ctor = Unbox Number (Naked_immediate, ctor_epa); _ } ->
        let v = Var_in_binding_pos.create ctor_epa.param Name_mode.normal in
        let denv = DE.define_variable denv v K.naked_immediate in
        let ty = Flambda_type.alias_type_of K.naked_immediate (Simple.var ctor_epa.param) in
        denv, ty
      | At_least_one { ctor = Unbox _; _ } ->
        Misc.fatal_errorf "Variant constant constructor unboxed with a kind other \
                           than naked_immediate."
    in
    let denv =
      Tag.Scannable.Map.fold (fun _ block_fields denv ->
        List.fold_left (fun denv { epa = { param = var; _ }; _ } ->
          let v = Var_in_binding_pos.create var Name_mode.normal in
          DE.define_variable denv v K.value
        ) denv block_fields)
        fields_by_tag denv
    in
    let non_const_ctors =
      Tag.Scannable.Map.map (fun block_fields ->
        List.map (fun field ->
          Flambda_type.alias_type_of K.value (Simple.var field.epa.param)
        ) block_fields
      ) fields_by_tag
    in
    let shape = T.variant ~const_ctors ~non_const_ctors in
    let denv = add_equation_on_var denv param_var shape in
    (* recursion *)
    Tag.Scannable.Map.fold (fun _ block_fields denv ->
      List.fold_left (fun denv field ->
        denv_of_decision denv field.epa.param field.decision
      ) denv block_fields
    ) fields_by_tag denv


  (* Number cases *)
  | Unbox Number (Naked_immediate, { param = naked_immediate; args = _; }) ->
    let shape = T.tagged_immediate_alias_to ~naked_immediate in
    denv_of_number_decision K.naked_immediate shape
      param_var naked_immediate denv
  | Unbox Number (Naked_float, { param = naked_float; args = _; }) ->
    let shape = T.boxed_float_alias_to ~naked_float in
    denv_of_number_decision K.naked_float shape
      param_var naked_float denv
  | Unbox Number (Naked_int32, { param = naked_int32; args = _; }) ->
    let shape = T.boxed_int32_alias_to ~naked_int32 in
    denv_of_number_decision K.naked_int32 shape
      param_var naked_int32 denv
  | Unbox Number (Naked_int64, { param = naked_int64; args = _; }) ->
    let shape = T.boxed_int64_alias_to ~naked_int64 in
    denv_of_number_decision K.naked_int64 shape
      param_var naked_int64 denv
  | Unbox Number (Naked_nativeint, { param = naked_nativeint; args = _; }) ->
    let shape = T.boxed_nativeint_alias_to ~naked_nativeint in
    denv_of_number_decision K.naked_nativeint shape
      param_var naked_nativeint denv



(* Compute extra args and filter decisions *)
(* *************************************** *)

exception Prevent_this_unboxing

let prevent_current_unboxing () = raise Prevent_this_unboxing

type unboxed_arg =
  | Poison (* used for recursive calls *)
  | Available of Simple.t
  | Generated of Variable.t
  | Added_by_wrapper_at_rewrite_use of { nth_arg : int; }

(*
let print_unboxed_arg ppf = function
  | Poison -> Format.fprintf ppf "poison"
  | Available simple -> Format.fprintf ppf "simple: %a" Simple.print simple
  | Generated v -> Format.fprintf ppf "generated: %a" Variable.print v
  | Added_by_wrapper_at_rewrite_use {nth_arg } ->
    Format.fprintf ppf "added_by_wrapper(%d)" nth_arg
*)

let type_of_arg_being_unboxed unboxed_arg =
  let aux simple = T.alias_type_of K.value simple in
  match unboxed_arg with
  | Poison -> None
  | Available simple -> Some (aux simple)
  | Generated var -> Some (aux (Simple.var var))
  | Added_by_wrapper_at_rewrite_use _ -> prevent_current_unboxing ()

let arg_being_unboxed_of_extra_arg extra_arg =
  match (extra_arg : EPA.Extra_arg.t) with
  | Already_in_scope simple -> Available simple
  | New_let_binding (var, _)
  | New_let_binding_with_named_args (var, _) -> Generated var

let extra_arg_of_arg_being_unboxed unboxer typing_env_at_use arg_being_unboxed =
  match arg_being_unboxed with
  | Poison ->
    EPA.Extra_arg.Already_in_scope (Simple.const unboxer.invalid_const)
  | Available arg_at_use ->
    let arg_type = T.alias_type_of K.value arg_at_use in
    begin match unboxer.prove_simple typing_env_at_use arg_type
                  ~min_name_mode:Name_mode.normal with
    | Proved simple ->
      EPA.Extra_arg.Already_in_scope simple
    | Invalid ->
      EPA.Extra_arg.Already_in_scope (Simple.const unboxer.invalid_const)
    | Unknown ->
      let var = Variable.create unboxer.var_name in
      let prim = unboxer.unboxing_prim arg_at_use in
      EPA.Extra_arg.New_let_binding (var, prim)
    end
  | Generated var ->
    let arg_at_use = Simple.var var in
    let var = Variable.create unboxer.var_name in
    let prim = unboxer.unboxing_prim arg_at_use in
    EPA.Extra_arg.New_let_binding (var, prim)
  | Added_by_wrapper_at_rewrite_use { nth_arg; } ->
    let var = Variable.create "unboxed_field" in
    EPA.Extra_arg.New_let_binding_with_named_args (var, (fun args ->
      let arg_simple = List.nth args nth_arg in
      unboxer.unboxing_prim arg_simple
    ))


(* Helpers for the variant case *)
(* **************************** *)

type variant_argument =
  | Not_a_constant_constructor
  | Maybe_constant_constructor of {
      is_int : Simple.t;
      arg_being_unboxed : unboxed_arg;
    }

let extra_arg_for_is_int = function
  | Maybe_constant_constructor { is_int; _ } ->
    EPA.Extra_arg.Already_in_scope is_int
  | Not_a_constant_constructor ->
    EPA.Extra_arg.Already_in_scope Simple.untagged_const_false

let extra_arg_for_ctor typing_env_at_use = function
  | Not_a_constant_constructor ->
    EPA.Extra_arg.Already_in_scope (
      Simple.untagged_const_int (Targetint.OCaml.of_int 0))
  | Maybe_constant_constructor { arg_being_unboxed; _ } ->
    match type_of_arg_being_unboxed arg_being_unboxed with
    | None ->
      EPA.Extra_arg.Already_in_scope (
        Simple.untagged_const_int (Targetint.OCaml.of_int 0))
    | Some arg_type ->
      match T.prove_untagged_int_simple_maybe typing_env_at_use
              ~min_name_mode:Name_mode.normal arg_type with
      | Proved simple ->
        EPA.Extra_arg.Already_in_scope simple
      | Unknown -> prevent_current_unboxing ()
      | Invalid ->
        (* Presumably, this means that we are in an impossible-to-reach
           case, and thus as in other cases, we only need to provide
           well-kinded values. *)
        EPA.Extra_arg.Already_in_scope (
          Simple.untagged_const_int (Targetint.OCaml.of_int 0))

let extra_args_for_const_ctor_of_variant
      constant_constructors_decision typing_env_at_use
      rewrite_id variant_arg =
  match constant_constructors_decision with
  | Zero ->
    begin match variant_arg with
    | Not_a_constant_constructor -> constant_constructors_decision
    | Maybe_constant_constructor _ ->
      Misc.fatal_errorf "The unboxed variant parameter was determined to have \
                         no constant cases when deciding to unbox it (using the \
                         parameter type), but at the use site, it is a constant \
                         constructor."
    end
  | At_least_one { ctor = Do_not_unbox reason; is_int; } ->
    let is_int =
      update_param_args is_int rewrite_id (extra_arg_for_is_int variant_arg)
    in
    At_least_one { ctor = Do_not_unbox reason; is_int; }

  | At_least_one { ctor = Unbox Number (Naked_immediate, ctor); is_int; } ->
    let is_int =
      update_param_args is_int rewrite_id (extra_arg_for_is_int variant_arg)
    in
    begin try
      let ctor =
        update_param_args ctor rewrite_id
          (extra_arg_for_ctor typing_env_at_use variant_arg)
      in
      At_least_one { ctor = Unbox (Number (Naked_immediate, ctor)); is_int; }
    with Prevent_this_unboxing ->
      At_least_one { ctor = Do_not_unbox Not_enough_information_at_use; is_int; }
    end
  | At_least_one { ctor = Unbox _ ; _ } ->
    Misc.fatal_errorf "Bad kind for unboxing the constant constructor \
                       of a variant"


(* Filter out non beneficial decisions *)
(* *********************************** *)

let is_unboxing_beneficial_for_epa epa =
  Apply_cont_rewrite_id.Map.exists (fun _ extra_arg ->
    match (extra_arg : EPA.Extra_arg.t) with
    | Already_in_scope _ -> true
    | New_let_binding _ | New_let_binding_with_named_args _ -> false
  ) epa.args

let rec filter_non_beneficial_decisions decision : decision =
  match (decision : decision) with
  | Do_not_unbox _ -> decision

  | Unbox Unique_tag_and_size { tag; fields; } ->
    let is_unboxing_beneficial, fields =
      List.fold_left_map (fun is_unboxing_beneficial { epa; decision; } ->
        let is_unboxing_beneficial =
          is_unboxing_beneficial || is_unboxing_beneficial_for_epa epa
        in
        let decision = filter_non_beneficial_decisions decision in
        is_unboxing_beneficial, { epa; decision; }
      ) false fields
    in
    if is_unboxing_beneficial then
      Unbox (Unique_tag_and_size { tag; fields; })
    else
      Do_not_unbox Not_beneficial

  | Unbox Closure_single_entry { closure_id; vars_within_closure; } ->
    let is_unboxing_beneficial = ref false in
    let vars_within_closure =
      Var_within_closure.Map.map (fun { epa; decision; } ->
        is_unboxing_beneficial :=
          !is_unboxing_beneficial || is_unboxing_beneficial_for_epa epa;
        let decision = filter_non_beneficial_decisions decision in
        { epa; decision; }
      ) vars_within_closure
    in
    if !is_unboxing_beneficial then
      Unbox (Closure_single_entry { closure_id; vars_within_closure; })
    else
      Do_not_unbox Not_beneficial

  | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
    let is_unboxing_beneficial = ref false in
    let fields_by_tag =
      Tag.Scannable.Map.map (List.map (fun { epa; decision; } ->
          is_unboxing_beneficial :=
            !is_unboxing_beneficial || is_unboxing_beneficial_for_epa epa;
          let decision = filter_non_beneficial_decisions decision in
          { epa; decision; }
      )) fields_by_tag
    in
    if !is_unboxing_beneficial then
      Unbox (Variant { tag; constant_constructors; fields_by_tag; })
    else
      Do_not_unbox Not_beneficial

  | (Unbox Number (Naked_immediate, _)) as decision ->
    (* At worse, this unboxing untags an integer *)
    decision

  | (Unbox Number (_, epa)) as decision ->
    if is_unboxing_beneficial_for_epa epa then decision else Do_not_unbox Not_beneficial



(* Helpers for the number case *)
(* *************************** *)

let compute_extra_arg_for_number kind unboxer epa
      rewrite_id typing_env_at_use arg_being_unboxed =
  let extra_arg =
    extra_arg_of_arg_being_unboxed unboxer
      typing_env_at_use arg_being_unboxed
  in
  let epa = update_param_args epa rewrite_id extra_arg in
  Unbox (Number (kind, epa))


(* Recursive descent on decisions *)
(* ****************************** *)

let are_there_unknown_use_sites = function
  | Filter { recursive; } -> recursive
  | Compute_all_extra_args -> false

let rec compute_extra_args_for_one_decision_and_use ~pass
          rewrite_id typing_env_at_use cse_at_use arg_being_unboxed decision =
  try
    compute_extra_args_for_one_decision_and_use_aux ~pass
      rewrite_id typing_env_at_use cse_at_use arg_being_unboxed decision
  with Prevent_this_unboxing ->
    begin match pass with
    | Filter _ -> Do_not_unbox Not_enough_information_at_use
    | Compute_all_extra_args ->
      Misc.fatal_errorf "This case should have been filtered out before."
    end

and compute_extra_args_for_one_decision_and_use_aux ~pass
          rewrite_id typing_env_at_use cse_at_use arg_being_unboxed decision =
  match decision with
  | Do_not_unbox _ -> decision
  | Unbox Unique_tag_and_size { tag; fields; } ->
    compute_extra_args_for_block ~pass
      rewrite_id typing_env_at_use cse_at_use arg_being_unboxed tag fields
  | Unbox Closure_single_entry { closure_id; vars_within_closure; } ->
    if are_there_unknown_use_sites pass then
      prevent_current_unboxing ()
    else
      compute_extra_args_for_closure ~pass
        rewrite_id typing_env_at_use cse_at_use arg_being_unboxed
        closure_id vars_within_closure
  | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
    if are_there_unknown_use_sites pass then
      prevent_current_unboxing ()
    else begin
      let invalid () =
        (* Invalid here means that the apply_cont is unreachable, i.e. the args
           we generated will never be actually used at runtime, so the values of
           the args do not matter, they are here to make the kind checker happy. *)
        compute_extra_args_for_variant ~pass
          rewrite_id typing_env_at_use cse_at_use arg_being_unboxed
          tag constant_constructors fields_by_tag
          (Or_unknown.Known Target_imm.Set.empty) Tag.Scannable.Map.empty
      in
      begin match type_of_arg_being_unboxed arg_being_unboxed with
      | None -> invalid ()
      | Some arg_type ->
        begin match T.prove_variant_like typing_env_at_use arg_type with
        | Wrong_kind -> Misc.fatal_errorf "Kind error while unboxing a variant"
        | Unknown -> prevent_current_unboxing ()
        | Invalid -> invalid ()
        | Proved { const_ctors; non_const_ctors_with_sizes; } ->
          compute_extra_args_for_variant ~pass
            rewrite_id typing_env_at_use cse_at_use arg_being_unboxed
            tag constant_constructors fields_by_tag
            const_ctors non_const_ctors_with_sizes
        end
      end
    end
  | Unbox Number (Naked_float, epa) ->
    compute_extra_arg_for_number Naked_float Float.unboxer epa
      rewrite_id typing_env_at_use arg_being_unboxed
  | Unbox Number (Naked_int32, epa) ->
    compute_extra_arg_for_number Naked_int32 Int32.unboxer epa
      rewrite_id typing_env_at_use arg_being_unboxed
  | Unbox Number (Naked_int64, epa) ->
    compute_extra_arg_for_number Naked_int64 Int64.unboxer epa
      rewrite_id typing_env_at_use arg_being_unboxed
  | Unbox Number (Naked_nativeint, epa) ->
    compute_extra_arg_for_number Naked_nativeint Nativeint.unboxer epa
      rewrite_id typing_env_at_use arg_being_unboxed
  | Unbox Number (Naked_immediate, epa) ->
    compute_extra_arg_for_number Naked_immediate Immediate.unboxer epa
      rewrite_id typing_env_at_use arg_being_unboxed

and compute_extra_args_for_block ~pass
      rewrite_id typing_env_at_use cse_at_use arg_being_unboxed
      tag fields =
  let size = Or_unknown.Known (Targetint.OCaml.of_int (List.length fields)) in
  let bak, invalid_const =
    if Tag.equal tag Tag.double_array_tag then
      P.Block_access_kind.Naked_floats { size; },
      Const.naked_float Numbers.Float_by_bit_pattern.zero
    else
      P.Block_access_kind.Values {
        size;
        tag = Option.get (Tag.Scannable.of_tag tag);
        field_kind = Any_value;
      }, Const.const_zero
  in
  let _, fields =
    List.fold_left_map
      (fun field_nth { epa; decision; } ->
         let unboxer = Field.unboxer invalid_const bak field_nth in
         let new_extra_arg =
           extra_arg_of_arg_being_unboxed unboxer
             typing_env_at_use arg_being_unboxed
         in
         let epa = update_param_args epa rewrite_id new_extra_arg in
         let decision =
           compute_extra_args_for_one_decision_and_use ~pass
             rewrite_id typing_env_at_use cse_at_use
             (arg_being_unboxed_of_extra_arg new_extra_arg) decision
         in
         Target_imm.(add one field_nth), { epa; decision; }
      ) Target_imm.zero fields
  in
  Unbox (Unique_tag_and_size { tag; fields; })

and compute_extra_args_for_closure ~pass
      rewrite_id typing_env_at_use cse_at_use arg_being_unboxed
      closure_id vars_within_closure =
  let vars_within_closure =
    Var_within_closure.Map.mapi (fun var { epa; decision; } ->
      let unboxer = Closure_field.unboxer closure_id var in
      let new_extra_arg =
        extra_arg_of_arg_being_unboxed unboxer
          typing_env_at_use arg_being_unboxed
      in
      let epa = update_param_args epa rewrite_id new_extra_arg in
      let decision =
        compute_extra_args_for_one_decision_and_use ~pass
          rewrite_id typing_env_at_use cse_at_use
          (arg_being_unboxed_of_extra_arg new_extra_arg) decision
      in
      { epa; decision; }
    ) vars_within_closure
  in
  Unbox (Closure_single_entry { closure_id; vars_within_closure; })

and compute_extra_args_for_variant ~pass
      rewrite_id typing_env_at_use cse_at_use arg_being_unboxed
      tag constant_constructors fields_by_tag (* unboxing decision *)
      const_ctors non_const_ctors_with_sizes (* type info at the use site *)
  =
  let are_there_constant_constructors =
    match (const_ctors : _ Or_unknown.t) with
    | Unknown -> true
    | Known set -> not (Target_imm.Set.is_empty set)
  in
  let are_there_non_constant_constructors =
    not (Tag.Scannable.Map.is_empty non_const_ctors_with_sizes)
  in
  let constant_constructors =
    if not are_there_constant_constructors then
      extra_args_for_const_ctor_of_variant
        constant_constructors typing_env_at_use
        rewrite_id Not_a_constant_constructor
    else if not are_there_non_constant_constructors then
      extra_args_for_const_ctor_of_variant
        constant_constructors typing_env_at_use rewrite_id
        (Maybe_constant_constructor
           { arg_being_unboxed; is_int = Simple.untagged_const_true;})
    else begin
      (* CR: one might want to try and use the cse at use to allow
             unboxing when the tag is not know statically but can be
             recovered through the cse. *)
      prevent_current_unboxing ()
    end
  in
  let tag_at_use_site =
    if not are_there_non_constant_constructors then
      Tag.Scannable.zero
    else
      match Tag.Scannable.Map.get_singleton non_const_ctors_with_sizes with
      | None -> prevent_current_unboxing ()
      | Some (tag, _) -> tag
  in
  let tag_extra_arg =
    tag_at_use_site
    |> Tag.Scannable.to_targetint
    |> Targetint.OCaml.of_targetint
    |> Const.untagged_const_int
    |> Simple.const
    |> (fun x -> EPA.Extra_arg.Already_in_scope x)
  in
  let tag = update_param_args tag rewrite_id tag_extra_arg in
  let fields_by_tag =
    Tag.Scannable.Map.mapi (fun tag_decision block_fields ->
      let size = List.length block_fields in
      (* see comment way above about invalid constants, poison and aliases *)
      let invalid_const = Const.const_int (Targetint.OCaml.of_int 0xbaba) in
      let bak : Flambda_primitive.Block_access_kind.t =
        Values {
          size = Known (Targetint.OCaml.of_int size);
          tag = tag_decision;
          field_kind = Any_value;
        }
      in
      let new_fields_decisions, _ =
        List.fold_left (fun (new_decisions, field_nth) { epa; decision; } ->
          let new_extra_arg, new_arg_being_unboxed =
            if are_there_non_constant_constructors
            && Tag.Scannable.equal tag_at_use_site tag_decision then begin
              let unboxer = Field.unboxer invalid_const bak field_nth in
              let new_extra_arg =
                extra_arg_of_arg_being_unboxed unboxer
                  typing_env_at_use arg_being_unboxed
              in
              let new_arg_being_unboxed =
                arg_being_unboxed_of_extra_arg new_extra_arg
              in
              new_extra_arg, new_arg_being_unboxed
            end else begin
              EPA.Extra_arg.Already_in_scope (Simple.const invalid_const), Poison
            end
          in
          let epa = update_param_args epa rewrite_id new_extra_arg in
          let decision =
            compute_extra_args_for_one_decision_and_use ~pass
              rewrite_id typing_env_at_use cse_at_use
              new_arg_being_unboxed decision
          in
          let new_decisions = { epa; decision; } :: new_decisions in
          (new_decisions, Target_imm.(add one field_nth))
        ) ([], Target_imm.zero) block_fields
      in
      List.rev new_fields_decisions
    ) fields_by_tag
  in
  Unbox (Variant { tag; constant_constructors; fields_by_tag; })



(* Add extra params and args *)
(* ************************* *)

let add_extra_params_and_args extra_params_and_args decision =
  let rec aux extra_params_and_args = function
    | Do_not_unbox _ -> extra_params_and_args
    | Unbox Unique_tag_and_size { tag; fields; } ->
      List.fold_left (fun extra_params_and_args { epa; decision; } ->
        let kind =
          if Tag.equal Tag.double_array_tag tag then
            K.With_subkind.naked_float
          else
            K.With_subkind.any_value
        in
        let extra_param = KP.create epa.param kind in
        let extra_params_and_args =
          EPA.add extra_params_and_args ~extra_param ~extra_args:epa.args
        in
        aux extra_params_and_args decision
      ) extra_params_and_args fields
    | Unbox Closure_single_entry { closure_id = _; vars_within_closure; } ->
      Var_within_closure.Map.fold (fun _ { epa; decision; } extra_params_and_args ->
        let extra_param = KP.create epa.param K.With_subkind.any_value in
        let extra_params_and_args =
          EPA.add extra_params_and_args ~extra_param ~extra_args:epa.args
        in
        aux extra_params_and_args decision
      ) vars_within_closure extra_params_and_args
    | Unbox Variant { tag; constant_constructors; fields_by_tag; } ->
      let extra_params_and_args =
        Tag.Scannable.Map.fold (fun _ block_fields extra_params_and_args ->
          List.fold_left (fun extra_params_and_args { epa; decision; } ->
            let extra_param = KP.create epa.param K.With_subkind.any_value in
            let extra_params_and_args =
              EPA.add extra_params_and_args ~extra_param ~extra_args:epa.args
            in
            aux extra_params_and_args decision
          ) extra_params_and_args block_fields
        ) fields_by_tag extra_params_and_args
      in
      let extra_params_and_args =
        match constant_constructors with
        | Zero -> extra_params_and_args
        | At_least_one { is_int; ctor = Do_not_unbox _; _ } ->
          let extra_param = KP.create is_int.param K.With_subkind.naked_immediate in
          EPA.add extra_params_and_args ~extra_param ~extra_args:is_int.args
        | At_least_one { is_int; ctor = Unbox Number (Naked_immediate, ctor); } ->
          let extra_param = KP.create is_int.param K.With_subkind.naked_immediate in
          let extra_params_and_args =
            EPA.add extra_params_and_args ~extra_param ~extra_args:is_int.args
          in
          let extra_param = KP.create ctor.param K.With_subkind.naked_immediate in
          EPA.add extra_params_and_args ~extra_param ~extra_args:ctor.args
        | At_least_one { is_int = _; ctor = Unbox _ } ->
          Misc.fatal_errorf "Trying to unbox the constant constructor of a variant \
                             with a kind other than naked_immediate."
      in
      let extra_param = KP.create tag.param K.With_subkind.naked_immediate in
      EPA.add extra_params_and_args ~extra_param ~extra_args:tag.args
    | Unbox Number (naked_number_kind, epa) ->
      (* CR pchambart: This is seriously too contrived for what it's doing.
         Flambda_kind.With_subkind needs a function for doing that *)
      let boxable_number_kind =
        K.Boxable_number.of_naked_number_kind naked_number_kind
      in
      let kind = K.Boxable_number.to_kind boxable_number_kind in
      let kind_with_subkind =
        K.With_subkind.create kind K.With_subkind.Subkind.Anything
      in
      let extra_param = KP.create epa.param kind_with_subkind in
      EPA.add extra_params_and_args ~extra_param ~extra_args:epa.args
  in
  aux extra_params_and_args decision


(* Filter decisions *)
(* **************** *)

let update_decision ~pass
      rewrite_ids_already_seen nth_arg arg_type_by_use_id = function
  | Do_not_unbox _ as decision -> decision
  | decision ->
    Apply_cont_rewrite_id.Map.fold
      (fun rewrite_id (arg_at_use : Continuation_env_and_param_types.arg_at_use) decision ->
         if Apply_cont_rewrite_id.Set.mem rewrite_id rewrite_ids_already_seen then
           decision
         else begin
           let typing_env_at_use = arg_at_use.typing_env in
           let arg_type_at_use = arg_at_use.arg_type in
           let cse_at_use = arg_at_use.cse in
           match TE.get_alias_then_canonical_simple_exn typing_env_at_use
                   ~min_name_mode:Name_mode.normal arg_type_at_use with
           | simple ->
             compute_extra_args_for_one_decision_and_use
               ~pass rewrite_id
               typing_env_at_use cse_at_use
               (Available simple) decision
           | exception Not_found ->
             compute_extra_args_for_one_decision_and_use
               ~pass rewrite_id
               typing_env_at_use cse_at_use
               (Added_by_wrapper_at_rewrite_use { nth_arg; }) decision
         end
      ) arg_type_by_use_id decision


(* Exported functions *)
(* ****************** *)

let make_decisions
      ~continuation_is_recursive
      ~arg_types_by_use_id
      denv params params_types : DE.t * decisions =
  let empty = Apply_cont_rewrite_id.Set.empty in
  let _, denv, rev_decisions, seen =
    Misc.Stdlib.List.fold_left3
      (fun (nth, denv, rev_decisions, seen) param param_type arg_type_by_use_id ->
         (* Make an optimist decision, filter it based on the arg types at the
            use sites (to prevent decisions that would be detrimental),
            and compute the necessary denv. *)
         let decision =
           make_optimist_decision ~depth:0 (DE.typing_env denv) param_type
           |> update_decision empty nth arg_type_by_use_id
                ~pass:(Filter { recursive = continuation_is_recursive; })
           |> filter_non_beneficial_decisions
         in
         let denv = denv_of_decision denv (KP.var param) decision in
         (* Compute the set of rewrite ids that have been considered when
            updating decisions, and check that all arg_type_by_use_id cover
            the same set of rewrite ids. *)
         let seen =
           match seen with
           | Some s ->
             assert (Apply_cont_rewrite_id.Map.for_all (fun id _ ->
               Apply_cont_rewrite_id.Set.mem id s) arg_type_by_use_id);
             s
           | None ->
             Apply_cont_rewrite_id.Map.fold (fun id _ acc ->
               Apply_cont_rewrite_id.Set.add id acc
             ) arg_type_by_use_id empty
         in
         (nth + 1, denv, decision :: rev_decisions, Some seen)
      ) (0, denv, [], None) params params_types arg_types_by_use_id
  in
  let rewrite_ids_seen =
    match seen with
    | None -> empty
    | Some s -> s
  in
  let decisions = List.combine params (List.rev rev_decisions) in
  denv, { decisions; rewrite_ids_seen; }


let compute_extra_params_and_args { decisions; rewrite_ids_seen; }
      ~arg_types_by_use_id existing_extra_params_and_args =
  let _, extra_params_and_args =
    List.fold_left2
      (fun (nth, extra_params_and_args) arg_type_by_use_id (_, decision) ->
         let decision =
           update_decision ~pass:Compute_all_extra_args
             rewrite_ids_seen nth arg_type_by_use_id decision
         in
         let extra_params_and_args =
           add_extra_params_and_args extra_params_and_args decision
         in
         (nth + 1, extra_params_and_args)
      ) (0, existing_extra_params_and_args) arg_types_by_use_id decisions
  in
  extra_params_and_args



