(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2019 OCamlPro SAS                                    *)
(*   Copyright 2016--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module L = Lambda

let stub_hack_prim_name = "*stub*"

let add_default_argument_wrappers lam =
  (* CR-someday mshinwell: Temporary hack to mark default argument wrappers
     as stubs.  Other possibilities:
     1. Change L.inline_attribute to add another ("stub") case;
     2. Add a "stub" field to the Lfunction record. *)
  let defs_are_all_functions (defs : (_ * L.lambda) list) =
    List.for_all (function (_, L.Lfunction _) -> true | _ -> false) defs
  in
  let f (lam : L.lambda) : L.lambda =
    match lam with
    | Llet (( Strict | Alias | StrictOpt), _k, id,
        Lfunction { kind; params; return; body = fbody; attr; loc; }, body) ->
      begin match
        Simplif.split_default_wrapper ~id ~kind ~params ~return ~body:fbody
          ~attr ~loc
      with
      | [fun_id, def] -> Llet (Alias, Pgenval, fun_id, def, body)
      | [fun_id, def; inner_fun_id, def_inner] ->
        Llet (Alias, Pgenval, inner_fun_id, def_inner,
              Llet (Alias, Pgenval, fun_id, def, body))
      | _ -> assert false
      end
    | Lletrec (defs, body) as lam ->
      if defs_are_all_functions defs then
        let defs =
          List.flatten
            (List.map
               (function
                 | (id, L.Lfunction
                     { kind; params; return; body; attr; loc; }) ->
                   Simplif.split_default_wrapper ~id ~kind ~params ~return
                     ~body ~attr ~loc
                 | _ -> assert false)
               defs)
        in
        Lletrec (defs, body)
      else lam
    | lam -> lam
  in
  L.map f lam

type block_type = Normal | Boxed_float

type letrec = {
  blocks : (Ident.t * block_type * int) list;
  (* CR pchambart: Should we preallocate with the tag ?
     How to get the tag for cases involving duprecord ?
     mshinwell: I think the tag is always zero for duprecord, but need to
     double-check.
     sdolan observes that the tag might not be zero if duprecord is ever
     generated for an inline record constructor *)
  consts : (Ident.t * L.structured_constant) list;
  pre : Lambda.lambda -> Lambda.lambda;
  effects : Lambda.lambda;
  functions : (Ident.t * Lambda.lfunction) list;
  substitution : Lambda.lambda Ident.Map.t;
  (* Alias to recursive variables should be forbidden, but they are
     not really. To prevent any problem, we apply a substitution
     to every expression *)
}

let lsequence (lam1, lam2) : L.lambda =
  let seq = Ident.create_local "sequence" in
  Llet (Strict, Pgenval, seq, lam1, lam2)

let update_dummy var expr =
  let loc = Location.none in
  let desc =
    Primitive.simple ~name:"caml_update_dummy" ~arity:2 ~alloc:true
  in
  L.Lprim (Pccall desc, [L.Lvar var; expr], loc)

let build_block var size block_type expr letrec =
  let blocks =
    (var, block_type, size) :: letrec.blocks
  in
  let effects : Lambda.lambda =
    lsequence (update_dummy var expr, letrec.effects)
  in
  { letrec with blocks; effects }

let is_a_variable (lam:Lambda.lambda) =
  match lam with
  | Lvar _ -> true
  | _ -> false

let rec prepare_letrec recursive_set current_var (lam:Lambda.lambda) letrec =
  let module T = Types in
  (* if !Clflags.dump_rawflambda *)
  (* then Format.printf "prepare_letrec:@ %a@." Printlambda.lambda lam; *)
  match lam with
  | Lfunction funct ->
    { letrec with functions = (current_var, funct) :: letrec.functions }
  | Lprim (Pduprecord (kind, size), _, _) -> begin
    match kind with
    | T.Record_regular | T.Record_inlined _ ->
      build_block current_var size Normal lam letrec
    | T.Record_extension _ ->
      build_block current_var (size + 1) Normal lam letrec
    | T.Record_unboxed _ ->
      assert false  (* CR mshinwell: this should have a proper error *)
    | T.Record_float ->
      build_block current_var size Boxed_float lam letrec
    end
  | Lprim ((Pmakeblock _ | Pmakearray (_, _)) as prim, args, dbg)
    when not (List.for_all is_a_variable args) ->
    (* Primitive arguments are not dissected. When it could matter, (i.e.
       if it contain an effect, or a let binding a variable that will end
       in the recursive set), it must be dissected before the primitive, to
       avoid messing with the evaluation order or ensure that blocks are
       preallocated then patched.

       This is done by lifting every expression (except variables) to
       a let binding before. *)
    let defs, args =
      List.fold_right (fun (def:Lambda.lambda) (defs, args) ->
        match def with
        | Lvar _ -> defs, def :: args
        | _ ->
          let id = Ident.create_local "lift_in_letrec" in
          (id, def) :: defs, (Lambda.Lvar id) :: args)
        args ([], [])
    in
    (* Bytecode evaluates effects in blocks from right to left,
       so reverse defs to preserve evaluation order.
       Relevant test: letrec/evaluation_order_3 *)
    let lam =
      List.fold_right (fun (id, def) body : Lambda.lambda ->
        Llet (Strict, Pgenval, id, def, body))
        (List.rev defs) (Lambda.Lprim (prim, args, dbg))
    in
    prepare_letrec recursive_set current_var lam letrec
  | Lprim(Pmakeblock _, args, _)
  | Lprim (Pmakearray ((Paddrarray|Pintarray), _), args, _) ->
    build_block current_var (List.length args) Normal lam letrec
  | Lprim (Pmakearray (Pfloatarray, _), args, _) ->
    build_block current_var (List.length args) Boxed_float lam letrec
  | Lconst const ->
    { letrec with consts = (current_var, const) :: letrec.consts }
  | Llet (Variable, k, id, def, body) ->
    let letrec = prepare_letrec recursive_set current_var body letrec in
    let pre tail : Lambda.lambda =
      Llet (Variable, k, id, def, letrec.pre tail)
    in
    { letrec with pre }

  | Llet ((Strict | Alias | StrictOpt) as let_kind, k, id, def, body) ->
    let free_vars = Lambda.free_variables def in
    if Ident.Set.is_empty (Ident.Set.inter free_vars recursive_set) then
      (* Non recursive let *)
      let letrec = prepare_letrec recursive_set current_var body letrec in
      let pre tail : Lambda.lambda =
        Llet (let_kind, k, id, def, letrec.pre tail)
      in
      { letrec with pre }
    else
      let letrec =
        prepare_letrec (Ident.Set.add id recursive_set)
          current_var body letrec
      in
      prepare_letrec recursive_set id def letrec
  | Lsequence (_lam1, _lam2) ->
    (* Eliminated by prepare *)
    assert false
  | Levent (body, event) ->
    let letrec = prepare_letrec recursive_set current_var body letrec in
    { letrec with effects = Levent (letrec.effects, event) }
  | Lletrec (bindings, body) ->
    let free_vars =
      List.fold_left (fun set (_, def) -> Ident.Set.union (Lambda.free_variables def) set)
        Ident.Set.empty
        bindings
    in
    if Ident.Set.is_empty (Ident.Set.inter free_vars recursive_set) then
      (* Non recursive relative to top-level letrec *)
      let letrec = prepare_letrec recursive_set current_var body letrec in
      let pre tail : Lambda.lambda =
        Lletrec (bindings, letrec.pre tail)
      in
      { letrec with pre }
    else
      let recursive_set =
        Ident.Set.union recursive_set
          (Ident.Set.of_list (List.map fst bindings))
      in
      let letrec =
        List.fold_right (fun (id, def) letrec -> prepare_letrec recursive_set id def letrec)
          bindings
          letrec
      in
      prepare_letrec recursive_set current_var body letrec
  | Lvar _ ->
    (* This cannot be a mutable variable: it is ok to copy it *)
    { letrec with
      substitution = Ident.Map.add current_var lam letrec.substitution }
  | _ ->
    (* This cannot be recursive, otherwise it should have been caught
       by the well formedness check. Hence it is ok to evaluate it
       before anything else. *)
    (* CR vlaviron: This invariant is false when this expression is the result
       of a previously dissected term, which can contain calls to
       caml_update_dummy *)
    (* let free_vars = Lambda.free_variables lam in *)
    (* assert(Ident.Set.is_empty (Ident.Set.inter free_vars recursive_set)); *)
    let pre tail : Lambda.lambda =
      Llet (Strict, Pgenval, current_var, lam, letrec.pre tail)
    in
    { letrec with pre }

let dissect_letrec ~bindings ~body =
(*
  Format.printf "dissect@ %a@.@."
    Printlambda.lambda (L.Lletrec (bindings, Lconst (Const_pointer 0)));
*)
  let recursive_set =
    Ident.Set.of_list (List.map fst bindings)
  in

  let letrec =
    List.fold_right (fun (id, def) letrec -> prepare_letrec recursive_set id def letrec)
      bindings
      { blocks = [];
        consts = [];
        pre = (fun x -> x);
        effects = body;
        functions = [];
        substitution = Ident.Map.empty;
      }
  in
  let preallocations =
    let loc = Location.none in
    List.map (fun (id, block_type, size) ->
      let fn =
        match block_type with
        | Normal -> "caml_alloc_dummy"
        | Boxed_float -> "caml_alloc_dummy_float"
      in
      let desc = Primitive.simple ~name:fn ~arity:1 ~alloc:true in
      let size : L.lambda = Lconst (Const_base (Const_int size)) in
      id, L.Lprim (Pccall desc, [size], loc))
      letrec.blocks
  in
  let functions =
    match letrec.functions with
    | [] ->
      letrec.effects
    | _ :: _ ->
      let functions =
        List.map (fun (id, lfun) -> id, L.Lfunction lfun) letrec.functions
      in
      L.Lletrec (functions, letrec.effects)
  in
  let with_preallocations =
    List.fold_left
      (fun body (id, binding) ->
         L.Llet (Strict, Pgenval, id, binding, body))
      functions
      preallocations
  in
  let with_non_rec =
    letrec.pre with_preallocations
  in
  let with_constants =
    List.fold_left
      (fun body (id, const) ->
         L.Llet (Strict, Pgenval, id, Lconst const, body))
      with_non_rec
      letrec.consts
  in
  let substituted =
    Lambda.subst (fun _id _val_desc env -> env)
      letrec.substitution with_constants
  in
(*
  Format.printf "dissected@ %a@.@."
    Printlambda.lambda substituted;
*)
  substituted

module Env : sig
  type t

  val create : current_unit_id:Ident.t -> t

  val current_unit_id : t -> Ident.t

  val add_mutable : t -> Ident.t -> t
  val is_mutable : t -> Ident.t -> bool

  val add_continuation : t -> int -> t
end = struct
  type t = {
    current_unit_id : Ident.t;
    mutables : Ident.Set.t;
    current_exception_depth : int;
    handler_exception_continuation : int Numbers.Int.Map.t; (* exception depth *)
  }

  let create ~current_unit_id =
    { current_unit_id;
      mutables = Ident.Set.empty;
      current_exception_depth = 0;
      handler_exception_continuation = Numbers.Int.Map.empty;
    }

  let current_unit_id t = t.current_unit_id

  let add_mutable t id =
    assert (not (Ident.Set.mem id t.mutables));
    { t with mutables = Ident.Set.add id t.mutables; }

  let is_mutable t id =
    Ident.Set.mem id t.mutables


  let add_continuation t cont =
    { t with
      handler_exception_continuation =
        Numbers.Int.Map.add cont t.current_exception_depth
          t.handler_exception_continuation;
    }
end

(* CR-soon mshinwell: Remove mutable state *)
let recursive_static_catches = ref Numbers.Int.Set.empty

let mark_as_recursive_static_catch cont =
  if Numbers.Int.Set.mem cont !recursive_static_catches then begin
    Misc.fatal_errorf "Static catch with continuation %d already marked as \
        recursive -- is it being redefined?"
      cont
  end;
  recursive_static_catches := Numbers.Int.Set.add cont !recursive_static_catches

let switch_for_if_then_else ~cond ~ifso ~ifnot k =
  (* CR mshinwell: We need to make sure that [cond] is {0, 1}-valued.
     The frontend should have been fixed on this branch for this. *)
  let switch : Lambda.lambda_switch =
    { sw_numconsts = 2;
      sw_consts = [0, ifnot; 1, ifso];
      sw_numblocks = 0;
      sw_blocks = [];
      sw_failaction = None;
      sw_tags_to_sizes = Tag.Scannable.Map.empty;
    }
  in
  k (L.Lswitch (cond, switch, Location.none))

let simplify_primitive (prim : L.primitive) args loc =
  match prim, args with
  (* CR mshinwell: What is happening to this?
  | (Pdivint Safe | Pmodint Safe
      | Pdivbint { is_safe = Safe; size = _; }
      | Pmodbint { is_safe = Safe; size = _; }),
    [arg1; arg2]
      when not !Clflags.unsafe ->
    let numerator = Ident.create_local "numerator" in
    let denominator = Ident.create_local "denominator" in
    let zero = Ident.create_local "zero" in
    let is_zero = Ident.create_local "is_zero" in
    let exn = Ident.create_local "division_by_zero" in
    let zero_const : Lambda.structured_constant =
      match prim with
      | Pdivint _ | Pmodint _ ->
        Const_base (Const_int 0)
      | Pdivbint { size = Pint32; is_safe = _; }
      | Pmodbint { size = Pint32; is_safe = _; } ->
        Const_base (Const_int32 0l)
      | Pdivbint { size = Pint64; is_safe = _; }
      | Pmodbint { size = Pint64; is_safe = _; } ->
        Const_base (Const_int64 0L)
      | Pdivbint { size = Pnativeint; is_safe = _; }
      | Pmodbint { size = Pnativeint; is_safe = _; } ->
        Const_base (Const_nativeint 0n)
      | _ -> assert false
    in
    let prim : Lambda.primitive =
      match prim with
      | Pdivint _ -> Pdivint Unsafe
      | Pmodint _ -> Pmodint Unsafe
      | Pdivbint { size; is_safe = _; } -> Pdivbint { size; is_safe = Unsafe; }
      | Pmodbint { size; is_safe = _; } -> Pmodbint { size; is_safe = Unsafe; }
      | _ -> assert false
    in
    let comparison : Lambda.primitive =
      match prim with
      | Pdivint _ | Pmodint _ -> Pintcomp Ceq
      | Pdivbint { size; is_safe = _; }
      | Pmodbint { size; is_safe = _; } -> Pbintcomp (size, Ceq)
      | _ -> assert false
    in
    let expr =
      L.Llet (Strict, Pgenval, zero, Lconst zero_const,
        (Llet (Strict, Pgenval, exn, Lvar Predef.ident_division_by_zero,
          (Llet (Strict, Pgenval, denominator, arg2,
            (Llet (Strict, Pgenval, numerator, arg1,
              (Llet (Strict, Pgenval, is_zero,
                (Lprim (comparison, [L.Lvar zero; L.Lvar denominator], loc)),
                (Lifthenelse (Lvar is_zero,
                  Lprim (Praise Raise_regular, [L.Lvar exn], loc),
                  (* CR-someday pchambart: find the right event.
                     mshinwell: I briefly looked at this, and couldn't
                     figure it out.
                     lwhite: I don't think any of the existing events
                     are suitable. I had to add a new one for a similar
                     case in the array data types work.
                     mshinwell: deferred CR *)
                  Lprim (prim, [L.Lvar numerator; L.Lvar denominator],
                    loc))))))))))))
    in
    prepare env expr (fun lam -> lam)
  | (Pdivint Safe | Pmodint Safe
      | Pdivbint { is_safe = Safe; size = _; }
      | Pmodbint { is_safe = Safe; size = _; }), _
      when not !Clflags.unsafe ->
    Misc.fatal_error "Pdivint / Pmodint must have exactly two arguments"
    *)
  | Pfloatcomp CFnlt, args ->
    L.Lprim (Pnot, [L.Lprim (Pfloatcomp CFlt, args, loc)], loc)
  | Pfloatcomp CFngt, args ->
    L.Lprim (Pnot, [L.Lprim (Pfloatcomp CFgt, args, loc)], loc)
  | Pfloatcomp CFnle, args ->
    L.Lprim (Pnot, [L.Lprim (Pfloatcomp CFle, args, loc)], loc)
  | Pfloatcomp CFnge, args ->
    L.Lprim (Pnot, [L.Lprim (Pfloatcomp CFge, args, loc)], loc)
  | Psequor, [arg1; arg2] ->
    let const_true = Ident.create_local "const_true" in
    let cond = Ident.create_local "cond_sequor" in
    L.Llet (Strict, Pgenval, const_true, Lconst (Const_base (Const_int 1)),
      (L.Llet (Strict, Pgenval, cond, arg1,
        switch_for_if_then_else
          ~cond:(L.Lvar cond)
          ~ifso:(L.Lvar const_true)
          ~ifnot:arg2
          (fun lam -> lam))))
  | Psequand, [arg1; arg2] ->
    let const_false = Ident.create_local "const_false" in
    let cond = Ident.create_local "cond_sequand" in
    L.Llet (Strict, Pgenval, const_false, Lconst (Const_base (Const_int 0)),
      (L.Llet (Strict, Pgenval, cond, arg1,
        switch_for_if_then_else
          ~cond:(L.Lvar cond)
          ~ifso:arg2
          ~ifnot:(L.Lvar const_false)
          (fun lam -> lam))))
  | (Psequand | Psequor), _ ->
    Misc.fatal_error "Psequand / Psequor must have exactly two arguments"
  | Pflambda_isint, _ ->
    Misc.fatal_error "[Pflambda_isint] should not exist at this stage"
  | Pisint, [arg] ->
    switch_for_if_then_else
      ~cond:(Lprim (Pflambda_isint, [arg], loc))
      ~ifso:(L.Lconst (Const_base (Const_int 1)))
      ~ifnot:(L.Lconst (Const_base (Const_int 0)))
      (fun lam -> lam)
  | (Pidentity | Pbytes_to_string | Pbytes_of_string), [arg] -> arg
  | Pignore, [arg] ->
    let ident = Ident.create_local "ignore" in
    let result = L.Lconst (Const_base (Const_int 0)) in
    L.Llet (Strict, Pgenval, ident, arg, result)
  | Pdirapply, [funct; arg]
  | Prevapply, [arg; funct] ->
    let apply : L.lambda_apply =
      { ap_func = funct;
        ap_args = [arg];
        ap_loc = loc;
        ap_should_be_tailcall = false;
        (* CR-someday lwhite: it would be nice to be able to give
           inlined attributes to functions applied with the application
           operators. *)
        ap_inlined = Default_inline;
        ap_specialised = Default_specialise;
      }
    in
    L.Lapply apply
  | _, _ -> L.Lprim (prim, args, loc)

let rec prepare env (lam : L.lambda) (k : L.lambda -> L.lambda) =
  match lam with
  | Lvar _ | Lconst _ -> k lam
  | Lapply { ap_func; ap_args; ap_loc; ap_should_be_tailcall; ap_inlined;
      ap_specialised; } ->
    prepare env ap_func (fun ap_func ->
      prepare_list env ap_args (fun ap_args ->
        k (L.Lapply {
          ap_func;
          ap_args;
          ap_loc = ap_loc;
          ap_should_be_tailcall = ap_should_be_tailcall;
          ap_inlined = ap_inlined;
          ap_specialised = ap_specialised;
        })))
  | Lfunction { kind; params; return; body; attr; loc; } ->
    prepare env body (fun body ->
      k (L.Lfunction {
        kind = kind;
        params = params;
        return = return;
        body;
        attr = attr;
        loc = loc;
      }))
  | Llet (let_kind, value_kind, id, defining_expr, body) ->
    prepare env defining_expr (fun defining_expr ->
      let env =
        match let_kind with
        | Strict | StrictOpt | Alias -> env
        | Variable -> Env.add_mutable env id
      in
      prepare env body (fun body ->
        k (L.Llet (let_kind, value_kind, id, defining_expr, body))))
  | Lletrec (bindings, body) ->
    let idents, bindings = List.split bindings in
    prepare_list env bindings (fun bindings ->
      let bindings = List.combine idents bindings in
      prepare env body (fun body ->
        k (dissect_letrec ~bindings ~body)))
  | Lprim (Pfield _, [Lprim (Pgetglobal id, [],_)], _)
      when Ident.same id (Env.current_unit_id env) ->
    Misc.fatal_error "[Pfield (Pgetglobal ...)] for the current compilation \
      unit is forbidden upon entry to the middle end"
  | Lprim (Psetfield (_, _, _), [Lprim (Pgetglobal _, [], _); _], _) ->
    Misc.fatal_error "[Psetfield (Pgetglobal ...)] is \
      forbidden upon entry to the middle end"
  | Lprim (Pfield (i, _), _, _) when i < 0 ->
    Misc.fatal_error "Pfield with negative field index"
  | Lprim (Pfloatfield (i, _), _, _) when i < 0 ->
    Misc.fatal_error "Pfloatfield with negative field index"
  | Lprim (Psetfield (i, _, _), _, _) when i < 0 ->
    Misc.fatal_error "Psetfield with negative field index"
  | Lprim (Pmakeblock (tag, _, _), _, _)
      when tag < 0 || tag >= Obj.no_scan_tag ->
    Misc.fatal_errorf "Pmakeblock with wrong or non-scannable block tag %d" tag
  | Lprim (prim, args, loc) ->
    prepare_list env args (fun args ->
      k (simplify_primitive prim args loc))
  | Lswitch (scrutinee, switch, loc) ->
    prepare env scrutinee (fun scrutinee ->
      let const_nums, sw_consts = List.split switch.sw_consts in
      let block_nums, sw_blocks = List.split switch.sw_blocks in
      prepare_option env switch.sw_failaction (fun sw_failaction ->
        prepare_list env sw_consts (fun sw_consts ->
          prepare_list env sw_blocks (fun sw_blocks ->
            let sw_failaction, wrap_switch =
              match sw_failaction with
              | None -> None, (fun lam -> lam)
              | Some failaction ->
                let failaction_cont = L.next_raise_count () in
                let wrap_switch lam : L.lambda =
                  Lstaticcatch (lam, (failaction_cont, []), failaction)
                in
                Some (L.Lstaticraise (failaction_cont, [])), wrap_switch
            in
            let consts_switch : L.lambda_switch =
              { sw_numconsts = switch.sw_numconsts;
                sw_consts = List.combine const_nums sw_consts;
                sw_numblocks = 0;
                sw_blocks = [];
                sw_failaction;
                sw_tags_to_sizes = Tag.Scannable.Map.empty;
              }
            in
            (* CR mshinwell: Merge this file into Cps_conversion then delete
               [sw_tags_to_sizes]. *)
            let tags_to_sizes =
              List.fold_left (fun tags_to_sizes
                        ({ sw_tag; sw_size; } : L.lambda_switch_block_key) ->
                  match Tag.Scannable.create sw_tag with
                  | Some tag ->
                    let size = Targetint.OCaml.of_int sw_size in
                    Tag.Scannable.Map.add tag size tags_to_sizes
                  | None ->
                    Misc.fatal_errorf "Bad tag %d in [Lswitch]" sw_tag)
                Tag.Scannable.Map.empty
                block_nums
            in
            let block_nums =
              List.map (fun ({ sw_tag; _} : L.lambda_switch_block_key) ->
                  sw_tag)
                block_nums
            in
            let blocks_switch : L.lambda_switch =
              { sw_numconsts = switch.sw_numblocks;
                sw_consts = List.combine block_nums sw_blocks;
                sw_numblocks = 0;
                sw_blocks = [];
                sw_failaction;
                (* XXX What about the size for the failaction? ... *)
                sw_tags_to_sizes = tags_to_sizes;
              }
            in
            let consts_switch : L.lambda =
              L.Lswitch (scrutinee, consts_switch, loc)
            in
            let blocks_switch : L.lambda =
              L.Lswitch (
               Lprim (Pgettag, [scrutinee], Location.none),
               blocks_switch, loc)
            in
            let isint_switch : L.lambda_switch =
              { sw_numconsts = 2;
                sw_consts = [0, blocks_switch; 1, consts_switch];
                sw_numblocks = 0;
                sw_blocks = [];
                sw_failaction = None;
                sw_tags_to_sizes = Tag.Scannable.Map.empty;
              }
            in
            let switch =
              if switch.sw_numconsts = 0 then blocks_switch
              else if switch.sw_numblocks = 0 then consts_switch
              else
                L.Lswitch (L.Lprim (Pflambda_isint, [scrutinee], Location.none),
                  isint_switch, loc)
            in
            k (wrap_switch switch)))))
  | Lstringswitch (scrutinee, cases, default, loc) ->
    prepare env (Matching.expand_stringswitch loc scrutinee cases default) k
  | Lstaticraise (cont, args) ->
    prepare_list env args (fun args ->
      k (L.Lstaticraise (cont, args)))
  | Lstaticcatch (body, (cont, args), handler) ->
    let env_body = Env.add_continuation env cont in
    prepare env_body body (fun body ->
      prepare env handler (fun handler ->
        k (L.Lstaticcatch (body, (cont, args), handler))))
  | Ltrywith (body, id, handler) ->
    prepare env body (fun body ->
      prepare env handler (fun handler ->
        k (L.Ltrywith (body, id, handler))))
  | Lifthenelse (cond, ifso, ifnot) ->
    prepare env cond (fun cond ->
      prepare env ifso (fun ifso ->
        prepare env ifnot (fun ifnot ->
          switch_for_if_then_else ~cond ~ifso ~ifnot k)))
  | Lsequence (lam1, lam2) ->
    let ident = Ident.create_local "sequence" in
    prepare env (L.Llet (Strict, Pgenval, ident, lam1, lam2)) k
  | Lwhile (cond, body) ->
    let cont = L.next_raise_count () in
    mark_as_recursive_static_catch cont;
    let cond_result = Ident.create_local "cond_result" in
    let lam : L.lambda =
      Lstaticcatch (
        Lstaticraise (cont, []),
        (cont, []),
        Llet (Strict, Pgenval, cond_result, cond,
          Lifthenelse (Lvar cond_result,
            Lsequence (
              body,
              Lstaticraise (cont, [])),
            Lconst (Const_base (Const_int 0)))))
    in
    prepare env lam k
  | Lfor (ident, start, stop, dir, body) ->
    let loc = Location.none in
    let cont = L.next_raise_count () in
    mark_as_recursive_static_catch cont;
    let stop_ident = Ident.create_local "stop" in
    let test =
      match dir with
      | Upto -> L.Lprim (Pintcomp Cle, [L.Lvar ident; L.Lvar stop_ident], loc)
      | Downto -> L.Lprim (Pintcomp Cge, [L.Lvar ident; L.Lvar stop_ident], loc)
    in
    let one : L.lambda = Lconst (Const_base (Const_int 1)) in
    let next_value_of_counter =
      match dir with
      | Upto -> L.Lprim (Paddint, [L.Lvar ident; one], loc)
      | Downto -> L.Lprim (Psubint, [L.Lvar ident; one], loc)
    in
    let lam : L.lambda =
      (* CR mshinwell: check evaluation order of start vs. end *)
      Llet (Strict, Pgenval, stop_ident, stop,
        Lstaticcatch (
          Lstaticraise (cont, [start]),
          (cont, [ident, Pgenval]),
          Lifthenelse (test,
            Lsequence (
              body,
              Lstaticraise (cont, [next_value_of_counter])),
            Lconst (Const_base (Const_int 0)))))
    in
    prepare env lam k
  | Lassign (ident, lam) ->
    if not (Env.is_mutable env ident) then begin
      Misc.fatal_errorf "Lassign on non-mutable variable %a"
        Ident.print ident
    end;
    prepare env lam (fun lam -> k (L.Lassign (ident, lam)))
  | Lsend (meth_kind, meth, obj, args, loc) ->
    prepare env meth (fun meth ->
      prepare env obj (fun obj ->
        prepare_list env args (fun args ->
          k (L.Lsend (meth_kind, meth, obj, args, loc)))))
  | Levent (body, _event) -> prepare env body k
  | Lifused _ ->
    (* [Lifused] is used to mark that this expression should be alive only if
       an identifier is.  Every use should have been removed by
       [Simplif.simplify_lets], either by replacing by the inner expression,
       or by completely removing it (replacing by unit). *)
    Misc.fatal_error "[Lifused] should have been removed by \
        [Simplif.simplify_lets]"

and prepare_list env lams k =
  match lams with
  | [] -> k []
  | lam::lams ->
    prepare env lam (fun lam ->
      prepare_list env lams (fun lams -> k (lam::lams)))

and prepare_option env lam_opt k =
  match lam_opt with
  | None -> k None
  | Some lam -> prepare env lam (fun lam -> k (Some lam))

let run lam =
  recursive_static_catches := Numbers.Int.Set.empty;
  let current_unit_id =
    Compilation_unit.get_persistent_ident
      (Compilation_unit.get_current_exn ())
  in
  let env = Env.create ~current_unit_id in
  let lam = add_default_argument_wrappers lam in
  let lam = prepare env lam (fun lam -> lam) in
  lam, !recursive_static_catches
