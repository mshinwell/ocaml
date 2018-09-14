(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

[@@@ocaml.warning "-60"]

module K = Flambda_kind

let fprintf = Format.fprintf

module Call_kind = struct
  (* CR-someday xclerc: we could add annotations to external declarations
     (akin to [@@noalloc]) in order to be able to refine the computation of
     effects/coeffects for such functions. *)

  let check_arity arity =
    match arity with
    | [] -> Misc.fatal_error "Invalid empty arity"
    | _::_ -> ()

  module Function_call = struct
    type t =
      | Direct of {
          closure_id : Closure_id.t;
          return_arity : Flambda_arity.t;
        }
      | Indirect_unknown_arity
      | Indirect_known_arity of {
          param_arity : Flambda_arity.t;
          return_arity : Flambda_arity.t;
        }

    let print ppf call =
      match call with
      | Direct { closure_id; return_arity; } ->
        fprintf ppf "@[(Direct %a %a)@]"
          Closure_id.print closure_id
          Flambda_arity.print return_arity
      | Indirect_unknown_arity ->
        fprintf ppf "Indirect_unknown_arity"
      | Indirect_known_arity { param_arity; return_arity; } ->
        fprintf ppf "@[(Indirect_known_arity %a -> %a)@]"
          Flambda_arity.print param_arity
          Flambda_arity.print return_arity

    let invariant t =
      match t with
      | Direct { closure_id = _; return_arity; } -> check_arity return_arity
      | Indirect_unknown_arity -> ()
      | Indirect_known_arity { param_arity; return_arity; } ->
        check_arity param_arity;
        check_arity return_arity

    let return_arity call : Flambda_arity.t =
      match call with
      | Direct { return_arity; _ }
      | Indirect_known_arity { return_arity; _ } -> return_arity
      | Indirect_unknown_arity -> [Flambda_kind.value ()]
  end

  type method_kind = Self | Public | Cached

  let print_method_kind ppf kind =
    match kind with
    | Self -> fprintf ppf "Self"
    | Public -> fprintf ppf "Public"
    | Cached -> fprintf ppf "Cached"

  type t =
    | Function of Function_call.t
    | Method of { kind : method_kind; obj : Name.t; }
    | C_call of {
        alloc : bool;
        param_arity : Flambda_arity.t;
        return_arity : Flambda_arity.t;
      }

  let print ppf t =
    match t with
    | Function call -> Function_call.print ppf call
    | Method { kind; obj; } ->
      fprintf ppf "@[(Method %a : %a)@]"
        Name.print obj
        print_method_kind kind
    | C_call { alloc; param_arity; return_arity; } ->
      fprintf ppf "@[(C (alloc %b) : %a -> %a)@]"
        alloc
        Flambda_arity.print param_arity
        Flambda_arity.print return_arity

  let invariant t =
    match t with
    | Function call -> Function_call.invariant call
    | Method { kind = _; obj = _; } -> ()
    | C_call { alloc = _; param_arity; return_arity; } ->
      check_arity param_arity;
      check_arity return_arity

  let direct_function_call closure_id ~return_arity =
    let t = Function (Direct { closure_id; return_arity; }) in
    invariant t;
    t

  let indirect_function_call_unknown_arity () = Function Indirect_unknown_arity

  let indirect_function_call_known_arity ~param_arity ~return_arity =
    let t = Function (Indirect_known_arity { param_arity; return_arity; }) in
    invariant t;
    t

  let method_call kind ~obj = Method { kind; obj; }

  let c_call ~alloc ~param_arity ~return_arity =
    let t = C_call { alloc; param_arity; return_arity; } in
    invariant t;
    t

  let return_arity t : Flambda_arity.t =
    match t with
    | Function call -> Function_call.return_arity call
    | Method _ -> [Flambda_kind.value ()]
    | C_call { return_arity; _ } -> return_arity

  let free_names t =
    match t with
    | Function _ | C_call _ -> Name_occurrences.create ()
    | Method { kind = _; obj; } ->
      Name_occurrences.singleton_in_terms (Name obj)

  let apply_name_permutation t perm =
    match t with
    | Function _ | C_call _ -> t
    | Method { kind; obj; } ->
      let obj' = Name_permutation.apply_name perm obj in
      if obj == obj' then t
      else
        Method {
          kind;
          obj = obj';
        }
end

module Apply = struct
  type t = {
    callee : Name.t;
    continuation : Continuation.t;
    exn_continuation : Continuation.t;
    args : Simple.t list;
    call_kind : Call_kind.t;
    dbg : Debuginfo.t;
    inline : Inline_attribute.t;
    specialise : Specialise_attribute.t;
  }

  let print ppf { callee; continuation; exn_continuation; args; call_kind;
        dbg; inline; specialise; } =
    fprintf ppf "@[<hov 1>(\
        @[<hov 1>(callee %a)@]@ \
        @[<hov 1>(continuation %a)@]@ \
        @[<hov 1>(exn_continuation %a)@]@ \
        @[<hov 1>(args %a)@]@ \
        @[<hov 1>(call_kind %a)@]@ \
        @[<hov 1>(dbg %a)@]@ \
        @[<hov 1>(inline %a)@]@ \
        @[<hov 1>(specialise %a)@])@]"
      Name.print callee
      Continuation.print continuation
      Continuation.print exn_continuation
      Simple.List.print args
      Call_kind.print call_kind
      Debuginfo.print_compact dbg
      Inline_attribute.print inline
      Specialise_attribute.print specialise

  let invariant env
        ({ callee;
          continuation;
          exn_continuation;
          args;
          call_kind;
          dbg;
          inline;
          specialise;
        } as t) =
      let unbound_continuation cont reason =
        Misc.fatal_errorf "Unbound continuation %a in %s: %a"
          Continuation.print cont
          reason
          print t
      in
      let module E = Invariant_env in
      Call_kind.invariant call_kind;
(*
      let stack = E.current_continuation_stack env in
*)
      E.check_name_is_bound_and_of_kind env callee (K.value ());
      begin match call_kind with
      | Function (Direct { closure_id = _; return_arity = _; }) ->
        (* Note that [return_arity] is checked for all the cases below. *)
        E.check_simples_are_bound env args
      | Function Indirect_unknown_arity ->
        E.check_simples_are_bound_and_of_kind env args (K.value ())
      | Function (Indirect_known_arity { param_arity; return_arity = _; }) ->
        ignore (param_arity : Flambda_arity.t);
        E.check_simples_are_bound env args
      | Method { kind; obj; } ->
        ignore (kind : Call_kind.method_kind);
        E.check_name_is_bound_and_of_kind env obj (K.value ());
        E.check_simples_are_bound_and_of_kind env args (K.value ())
      | C_call { alloc = _; param_arity = _; return_arity = _; } ->
        (* CR mshinwell: Check exactly what Cmmgen can compile and then
           add further checks on [param_arity] and [return_arity] *)
        begin match callee with
        | Symbol _ -> ()
        | Var _ | Logical_var _ ->
          (* CR-someday mshinwell: We could expose indirect C calls at the
             source language level. *)
          Misc.fatal_errorf "For [C_call] applications the callee must be \
              directly specified as a [Symbol]:@ %a"
            print t
        end
      end;
      begin match E.find_continuation_opt env continuation with
      | None ->
        unbound_continuation continuation "[Apply] term"
      | Some (arity, kind (*, cont_stack *)) ->
        begin match kind with
        | Normal -> ()
        | Exn_handler ->
          Misc.fatal_errorf "Continuation %a is an exception handler \
              but is used in this [Apply] term as a return continuation:@ %a"
            Continuation.print continuation
            print t
        end;
        let expected_arity = Call_kind.return_arity call_kind in
        if not (Flambda_arity.compatible arity ~if_used_at:expected_arity)
        then begin
          Misc.fatal_errorf "Continuation %a called with wrong arity in \
              this [Apply] term: expected %a but used at %a:@ %a"
            Continuation.print continuation
            Flambda_arity.print expected_arity
            Flambda_arity.print arity
            print t
        end (*;
        E.Continuation_stack.unify continuation stack cont_stack *)
      end;
      begin match E.find_continuation_opt env exn_continuation with
      | None ->
        unbound_continuation continuation
          "[Apply] term exception continuation"
      | Some (arity, kind (*, cont_stack *)) ->
        begin match kind with
        | Normal ->
          Misc.fatal_errorf "Continuation %a is a normal continuation \
              but is used in this [Apply] term as an exception handler:@ %a"
            Continuation.print continuation
            print t
        | Exn_handler -> ()
        end;
        let expected_arity = [Flambda_kind.value ()] in
        if not (Flambda_arity.equal arity expected_arity) then begin
          Misc.fatal_errorf "Exception continuation %a named in this \
              [Apply] term has the wrong arity: expected %a but have %a:@ %a"
            Continuation.print continuation
            Flambda_arity.print expected_arity
            Flambda_arity.print arity
            print t
        end (*;
        E.Continuation_stack.unify exn_continuation stack cont_stack *)
      end;
      ignore (dbg : Debuginfo.t);
      ignore (inline : Inline_attribute.t);
      ignore (specialise : Specialise_attribute.t)

  let create ~callee ~continuation ~exn_continuation ~args ~call_kind ~dbg
        ~inline ~specialise =
    (* CR mshinwell: We should still be able to check some of the invariant
       properties now.  (We can't do them all as we don't have the
       environment.) *)
    { callee;
      continuation;
      exn_continuation;
      args;
      call_kind;
      dbg;
      inline;
      specialise;
    }

  let callee t = t.callee
  let continuation t = t.continuation
  let exn_continuation t = t.exn_continuation
  let args t = t.args
  let call_kind t = t.call_kind
  let dbg t = t.dbg
  let inline t = t.inline
  let specialise t = t.specialise

  let free_names
        { callee;
          continuation;
          exn_continuation;
          args;
          call_kind;
          dbg = _;
          inline = _;
          specialise = _;
        } =
    let module BN = Bindable_name in
    Name_occurrences.union_list [
      Name_occurrences.of_list_in_terms [
        BN.Name callee;
        BN.Continuation continuation;
        BN.Continuation exn_continuation;
      ];
      Simple.List.free_names args;
      Call_kind.free_names call_kind;
    ]

  let continuation_counts
        { callee = _;
          continuation;
          exn_continuation;
          args = _;
          call_kind = _;
          dbg = _;
          inline = _;
          specialise = _;
        } =
    Continuation_counts.create_list [
      continuation;
      exn_continuation;
    ]

  let apply_name_permutation
        ({ callee;
           continuation;
           exn_continuation;
           args;
           call_kind;
           dbg;
           inline;
           specialise;
        } as t)
        perm =
    let continuation' =
      Name_permutation.apply_continuation perm continuation
    in
    let exn_continuation' =
      Name_permutation.apply_continuation perm exn_continuation
    in
    let callee' = Name_permutation.apply_name perm callee in
    let args' = Simple.List.apply_name_permutation args perm in
    let call_kind' = Call_kind.apply_name_permutation call_kind perm in
    if continuation == continuation'
      && exn_continuation == exn_continuation'
      && callee == callee'
      && args == args'
      && call_kind == call_kind'
    then
      t
    else
      { callee = callee';
        continuation = continuation';
        exn_continuation = exn_continuation';
        args = args';
        call_kind = call_kind';
        dbg;
        inline;
        specialise;
      }
end

module Trap_action = struct
  type t =
    | Push of { exn_handler : Continuation.t; }
    | Pop of {
        exn_handler : Continuation.t;
        take_backtrace : bool;
      }

  let print ppf t =
    match t with
    | Push { exn_handler; } ->
      fprintf ppf "%spush%s %a %sthen%s "
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        Continuation.print exn_handler
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
    | Pop { exn_handler; take_backtrace; } ->
      fprintf ppf "%spop%s%s %a %sthen%s "
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        (if take_backtrace then " with backtrace" else "")
        Continuation.print exn_handler
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())

  let free_names = function
    | Push { exn_handler; }
    | Pop { exn_handler; take_backtrace = _; } ->
      Name_occurrences.singleton_in_terms (Continuation exn_handler)

  let apply_name_permutation t perm =
    match t with
    | Push { exn_handler; } ->
      let exn_handler' = Name_permutation.apply_continuation perm exn_handler in
      if exn_handler == exn_handler' then t
      else Push { exn_handler = exn_handler'; }
    | Pop { exn_handler; take_backtrace; } ->
      let exn_handler' = Name_permutation.apply_continuation perm exn_handler in
      if exn_handler == exn_handler' then t
      else Pop { exn_handler = exn_handler'; take_backtrace; }

  let continuation_counts = function
    | Push { exn_handler; }
    | Pop { exn_handler; take_backtrace = _; } ->
      Continuation_counts.create_singleton exn_handler

  module Option = struct
    type nonrec t = t option

    let print ppf = function
      | None -> ()
      | Some t -> print ppf t

(*
    let free_names = function
      | None -> Name_occurrences.create ()
      | Some trap_action -> free_names trap_action

    let apply_name_permutation t perm =
      match t with
      | None -> None
      | Some trap_action ->
        let trap_action' = apply_name_permutation trap_action perm in
        if trap_action == trap_action' then t
        else Some trap_action'
*)
  end
end

module Apply_cont = struct
  type t = {
    k : Continuation.t;
    args : Simple.t list;
    trap_action : Trap_action.Option.t;
  }

  let print ppf { k; args; trap_action; } =
    match args with
    | [] ->
      fprintf ppf "@[<2>(%a%sgoto%s@ %a)@]"
        Trap_action.Option.print trap_action
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        Continuation.print k
    | _ ->
      fprintf ppf "@[<2>(%a%sapply_cont%s@ %a@ %a)@]"
        Trap_action.Option.print trap_action
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        Continuation.print k
        Simple.List.print args

  let invariant env ({ k; args; trap_action; } as t) =
    let module E = Invariant_env in
    let unbound_continuation cont reason =
      Misc.fatal_errorf "Unbound continuation %a in %s: %a"
        Continuation.print cont
        reason
        print t
    in
    let args_arity = List.map (fun arg -> E.kind_of_simple env arg) args in
    let arity, kind (*, cont_stack *) =
      match E.find_continuation_opt env k with
      | Some result -> result
      | None -> unbound_continuation k "[Apply_cont] term"
    in
(*
    let stack = E.current_continuation_stack env in
    E.Continuation_stack.unify cont stack cont_stack;
*)
    if not (Flambda_arity.equal args_arity arity) then begin
      Misc.fatal_errorf "Continuation %a called with wrong arity in \
          this [Apply_cont] term: expected %a but found %a:@ %a"
        Continuation.print k
        Flambda_arity.print arity
        Flambda_arity.print args_arity
        print t
    end;
    begin match kind with
    | Normal -> ()
    | Exn_handler ->
      Misc.fatal_errorf "Continuation %a is an exception handler \
          but is used in this [Apply_cont] term as a normal continuation:@ \
          %a"
        Continuation.print k
        print t
    end;
    let check_exn_handler exn_handler =
      match E.find_continuation_opt env exn_handler with
      | None ->
        unbound_continuation exn_handler "[Apply] trap handler"
      | Some (arity, kind (*, cont_stack *)) ->
        begin match kind with
        | Exn_handler -> ()
        | Normal ->
          Misc.fatal_errorf "Continuation %a is a normal continuation  \
              but is used in the trap action of this [Apply] term as an \
              exception handler:@ %a"
            Continuation.print exn_handler
            print t
        end;
        assert (not (Continuation.equal k exn_handler));
        let expected_arity = [K.value ()] in
        if not (Flambda_arity.equal arity expected_arity) then begin
          Misc.fatal_errorf "Exception handler continuation %a has \
              the wrong arity for the trap handler action of this \
              [Apply] term: expected %a but found %a:@ %a"
            Continuation.print k
            Flambda_arity.print expected_arity
            Flambda_arity.print arity
            print t
        end;
        ()
(*
        cont_stack
*)
    in
(*
    let current_stack = E.current_continuation_stack env in
*)
    (* CR mshinwell for pchambart: We need to fix this.  I've removed the
       trap IDs since we don't need them for compilation, and they would be
       another kind of name that needs freshening (which is weird since they
       don't have any binding site). *)
(*
    let stack, cont_stack =
*)
      match trap_action with
      | None -> () (*current_stack, cont_stack *)
      | Some (Push { exn_handler }) ->
        check_exn_handler exn_handler
(*
        let cont_stack = check_exn_handler exn_handler in
        E.Continuation_stack.push id exn_handler current_stack, cont_stack
*)
      | Some (Pop { exn_handler; take_backtrace = _; }) ->
        check_exn_handler exn_handler

(*
        let cont_stack = check_exn_handler exn_handler in
        current_stack, E.Continuation_stack.push id exn_handler cont_stack
*)
(*
    in
    E.Continuation_stack.unify cont stack cont_stack
    current_stack
    *)

  let create ?trap_action k ~args = { k; args; trap_action; }

  let goto k =
    { k;
      args = [];
      trap_action = None;
    }

  let continuation t = t.k
  let args t = t.args
  let trap_action t = t.trap_action

  let free_names { k; args; trap_action; } =
    let trap_action_free_names =
      match trap_action with
      | None -> Name_occurrences.create ()
      | Some trap_action -> Trap_action.free_names trap_action
    in
    Name_occurrences.union_list [
      Name_occurrences.singleton_in_terms (Continuation k);
      Simple.List.free_names args;
      trap_action_free_names;
    ]

  let continuation_counts { k; args = _; trap_action; } =
    let trap_action_continuation_counts =
      match trap_action with
      | None -> Continuation_counts.create ()
      | Some trap_action -> Trap_action.continuation_counts trap_action
    in
    Continuation_counts.union_list [
      Continuation_counts.create_singleton k;
      trap_action_continuation_counts;
    ]

  let apply_name_permutation ({ k; args; trap_action; } as t) perm =
    let k' = Name_permutation.apply_continuation perm k in
    let args' = Simple.List.apply_name_permutation args perm in
    let trap_action' =
      match trap_action with
      | None -> None
      | Some trap_action' ->
        let new_trap_action' =
          Trap_action.apply_name_permutation trap_action' perm
        in
        if new_trap_action' == trap_action' then trap_action
        else Some new_trap_action'
    in
    if k == k' && args == args' && trap_action == trap_action' then t
    else { k = k'; args = args'; trap_action = trap_action'; }
end

module Switch = struct
  type t = {
    scrutinee : Name.t;
    arms : Continuation.t Discriminant.Map.t;
  }

  let print_arms ppf arms =
    let spc = ref false in
    Discriminant.Map.iter (fun discriminant l ->
        if !spc then fprintf ppf "@ " else spc := true;
        fprintf ppf "@[<hv 1>| %a ->@ %sgoto%s %a@]"
          Discriminant.print discriminant
          (Misc_color.bold_cyan ())
          (Misc_color.reset ())
          Continuation.print l)
      arms

  let print ppf { scrutinee; arms; } =
    fprintf ppf
      "@[<v 1>(%sswitch%s %a@ @[<v 0>%a@])@]"
      (Misc_color.bold_cyan ())
      (Misc_color.reset ())
      Name.print scrutinee
      print_arms arms

  let invariant env ({ scrutinee; arms; } as t) =
    let module E = Invariant_env in
    let unbound_continuation cont reason =
      Misc.fatal_errorf "Unbound continuation %a in %s: %a"
        Continuation.print cont
        reason
        print t
    in
    E.check_name_is_bound_and_of_kind env scrutinee (K.fabricated ());
    assert (Discriminant.Map.cardinal arms >= 2);
    let check discr k =
      ignore (discr : Discriminant.t);
      match E.find_continuation_opt env k with
      | None ->
        unbound_continuation k "[Switch] term"
      | Some (arity, kind (*, cont_stack *)) ->
(*
        let current_stack = E.current_continuation_stack env in
        E.Continuation_stack.unify k cont_stack current_stack;
*)
        begin match kind with
        | Normal -> ()
        | Exn_handler ->
          Misc.fatal_errorf "Continuation %a is an exception handler \
              but is used in this [Switch] as a normal continuation:@ %a"
            Continuation.print k
            print t
        end;
        if List.length arity <> 0 then begin
          Misc.fatal_errorf "Continuation %a is used in this [Switch] \
              and thus must have arity [], but has arity %a"
            Continuation.print k
            Flambda_arity.print arity
        end
    in
    Discriminant.Map.iter check arms

  let create ~scrutinee ~arms = { scrutinee; arms; }

  let iter t ~f = Discriminant.Map.iter f t.arms

  let num_arms t = Discriminant.Map.cardinal t.arms

  let scrutinee t = t.scrutinee
  let arms t = t.arms

  let free_names { scrutinee; arms; } =
    let free_names_in_arms =
      Discriminant.Map.fold (fun _discr k free_names ->
          Name_occurrences.add free_names (Continuation k) In_terms)
        arms
        (Name_occurrences.create ())
    in
    Name_occurrences.union_list [
      Name_occurrences.singleton_in_terms (Name scrutinee);
      free_names_in_arms;
    ]

  let apply_name_permutation ({ scrutinee; arms; } as t) perm =
    let scrutinee' = Name_permutation.apply_name perm scrutinee in
    let arms' =
      Discriminant.Map.map_sharing (fun k ->
          Name_permutation.apply_continuation perm k)
        arms
    in
    if scrutinee == scrutinee' && arms == arms' then t
    else { scrutinee = scrutinee'; arms = arms'; }

  let continuation_counts { scrutinee = _; arms; } =
    Continuation_counts.create_list (Discriminant.Map.data arms)
end

type switch_creation_result =
  | Have_deleted_comparison_but_not_branch
  | Have_deleted_comparison_and_branch
  | Nothing_deleted

module rec Expr : sig
  type t =
    | Let of Let.t
    | Let_cont of Let_cont.t
    | Apply of Apply.t
    | Apply_cont of Apply_cont.t
    | Switch of Switch.t
    | Invalid of Invalid_term_semantics.t

  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val invariant : Invariant_env.t -> t -> unit

  include Contains_names.S with type t := t

  val create_let : Variable.t -> Flambda_kind.t -> Named.t -> t -> t

  val create_switch
     : scrutinee:Name.t
    -> arms:Continuation.t Discriminant.Map.t
    -> Expr.t * switch_creation_result

  val if_then_else
     : scrutinee:Name.t
    -> if_true:Continuation.t
    -> if_false:Continuation.t
    -> t

  val bind
     : bindings:(Variable.t * Flambda_kind.t * Named.t) list
    -> body:t
    -> t

  val invalid : unit -> t
  type maybe_named =
    | Is_expr of t
    | Is_named of Named.t
  val iter_general
     : toplevel:bool
    -> (Expr.t -> unit)
    -> (Named.t -> unit)
    -> maybe_named
    -> unit
  val no_effects_or_coeffects : t -> bool
  val continuation_counts_toplevel : t -> Continuation_counts.t

  type with_wrapper =
    | Unchanged of { handler : Continuation_handler.t; }
    | With_wrapper of {
        new_k : Continuation.t;
        new_handler : Continuation_handler.t;
        wrapper_handler : Continuation_handler.t;
      }

  val build_let_cont_with_wrappers
     : body:t
    -> Recursive.t
    -> with_wrappers:with_wrapper Continuation.Map.t
    -> t
end = struct
  include Expr

  let invariant env t =
    match t with
    | Let let_expr -> Let.invariant env let_expr
    | Let_cont let_cont -> Let_cont.invariant env let_cont
    | Apply_cont apply_cont -> Apply_cont.invariant env apply_cont
    | Apply apply -> Apply.invariant env apply
    | Switch switch -> Switch.invariant env switch
    | Invalid _ -> ()

  let print_with_cache ~cache ppf (t : t) =
    match t with
    | Let let_expr -> Let.print_with_cache ~cache ppf let_expr
    | Let_cont let_cont -> Let_cont.print_with_cache ~cache ppf let_cont
    | Apply apply -> Apply.print ppf apply
    | Apply_cont apply_cont -> Apply_cont.print ppf apply_cont
    | Switch switch -> Switch.print ppf switch
    | Invalid semantics ->
      fprintf ppf "@[%sInvalid %a%s@]"
        (Misc_color.bold_cyan ())
        Invalid_term_semantics.print semantics
        (Misc_color.reset ())

  let print ppf (t : t) =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names t =
    match t with
    | Let let_expr -> Let.free_names let_expr
    | Let_cont let_cont -> Let_cont.free_names let_cont
    | Apply apply -> Apply.free_names apply
    | Apply_cont apply_cont -> Apply_cont.free_names apply_cont
    | Switch switch -> Switch.free_names switch
    | Invalid _ -> Name_occurrences.create ()

  let apply_name_permutation t perm =
    match t with
    | Let let_expr ->
      let let_expr' = Let.apply_name_permutation let_expr perm in
      if let_expr == let_expr' then t
      else Let let_expr'
    | Let_cont let_cont ->
      let let_cont' = Let_cont.apply_name_permutation let_cont perm in
      if let_cont == let_cont' then t
      else Let_cont let_cont'
    | Apply apply ->
      let apply' = Apply.apply_name_permutation apply perm in
      if apply == apply' then t
      else Apply apply'
    | Apply_cont apply_cont ->
      let apply_cont' = Apply_cont.apply_name_permutation apply_cont perm in
      if apply_cont == apply_cont' then t
      else Apply_cont apply_cont'
    | Switch switch ->
      let switch' = Switch.apply_name_permutation switch perm in
      if switch == switch' then t
      else Switch switch'
    | Invalid _ -> t

  let continuation_counts_toplevel t =
    match t with
    | Let let_expr -> Let.continuation_counts_toplevel let_expr
    | Let_cont let_cont -> Let_cont.continuation_counts_toplevel let_cont
    | Apply apply -> Apply.continuation_counts apply
    | Apply_cont apply_cont -> Apply_cont.continuation_counts apply_cont
    | Switch switch -> Switch.continuation_counts switch
    | Invalid _ -> Continuation_counts.create ()

  let create_let bound_var kind defining_expr body : t =
    begin match !Clflags.dump_flambda_let with
    | None -> ()
    | Some stamp ->
      Variable.debug_when_stamp_matches bound_var ~stamp ~f:(fun () ->
        Printf.eprintf "Creation of [Let] with stamp %d:\n%s\n%!"
          stamp
          (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int)))
    end;
    Let (Let.create ~bound_var ~kind ~defining_expr ~body)

  let invalid () =
    if !Clflags.treat_invalid_code_as_unreachable then
      Invalid Treat_as_unreachable
    else
      Invalid Halt_and_catch_fire

  let create_switch ~scrutinee ~arms =
    if Discriminant.Map.cardinal arms < 1 then
      invalid (), Have_deleted_comparison_and_branch
    else
      let change_to_goto k =
        Apply_cont (Apply_cont.goto k), Have_deleted_comparison_but_not_branch
      in
      match Discriminant.Map.get_singleton arms with
      | Some (_discriminant, k) -> change_to_goto k
      | None ->
        (* CR mshinwell: We should do a partial invariant check here (one
           which doesn't require [Invariant_env.t]. *)
        let destinations =
          Continuation.Set.of_list (Discriminant.Map.data arms)
        in
        match Continuation.Set.get_singleton destinations with
        | Some k -> change_to_goto k
        | None ->
          let switch = Switch.create ~scrutinee ~arms in
          Switch switch, Nothing_deleted

  let if_then_else ~scrutinee ~if_true ~if_false =
    let arms =
      Discriminant.Map.of_list [
        Discriminant.bool_true, if_true;
        Discriminant.bool_false, if_false;
      ]
    in
    let expr, _ = create_switch ~scrutinee ~arms in
    expr

  let no_effects_or_coeffects (t : t) =
    match t with
    | Let let_expr -> Let.no_effects_or_coeffects let_expr
    | Let_cont let_cont -> Let_cont.no_effects_or_coeffects let_cont
    | Apply_cont _ | Switch _ -> true
    | Apply _ | Invalid _ -> false

  (* CR mshinwell: Maybe this should assign the fresh names? *)
  let bind ~bindings ~body =
    List.fold_left (fun expr (bound_var, kind, defining_expr) ->
        Expr.create_let bound_var kind defining_expr expr)
      body bindings

  let build_let_cont_with_wrappers ~body (recursive : Recursive.t)
        ~with_wrappers =
    match recursive with
    | Non_recursive ->
      begin match Continuation.Map.bindings with_wrappers with
      | [k, Unchanged { handler; }] ->
        Let_cont (Let_cont.create_non_recursive k ~body handler)
      | [k, With_wrapper { new_k; new_handler; wrapper_handler; }] ->
        let for_k =
          Let_cont.create_non_recursive k
            ~body
            wrapper_handler
        in
        let for_new_k =
          Let_cont.create_non_recursive new_k
            ~body:(Let_cont for_k)
            new_handler
        in
        Let_cont for_new_k
      | _ -> Misc.fatal_error "build_let_cont_with_wrappers"
      end
    | Recursive ->
      let handlers =
        Continuation.Map.fold
          (fun k (with_wrapper : with_wrapper) handlers ->
            match with_wrapper with
            | Unchanged { handler; } ->
              Continuation.Map.add k handler handlers
            | With_wrapper { new_k; new_handler; wrapper_handler; } ->
              Continuation.Map.add new_k new_handler
                (Continuation.Map.add k wrapper_handler handlers))
          with_wrappers
          Continuation.Map.empty
      in
      Let_cont (Let_cont.create_recursive ~body handlers)

(* To be re-enabled as we find we need them.
   Iteration is simple enough not to probably not require these.

  module Iterators = struct
    let iter_lets t ~for_defining_expr ~for_last_body ~for_each_let =
      let rec loop (t : t) =
        match t with
        | Let let_expr ->
          for_each_let let_expr;
          for_defining_expr (Let.kind let_expr) (Let.defining_expr let_expr);
          loop (Let.pattern_match let_expr ~f:(fun _bound_var body -> body))
        | t ->
          for_last_body t
      in
      loop t

    let iter_general ~toplevel f f_named maybe_named =
      let rec aux (t : t) =
        match t with
        | Let _ ->
          iter_lets t
            ~for_defining_expr:(fun _kind named -> aux_named named)
            ~for_last_body:aux
            ~for_each_let:f
        (* CR mshinwell: add tail recursive case for Let_cont? *)
        | _ ->
          f t;
          match t with
          | Apply _ | Apply_cont _ | Switch _ -> ()
          | Let _ -> assert false
          | Let_cont { body; handlers; _ } ->
            aux body;
            begin match handlers with
            | Non_recursive { name = _; handler = { handler; _ }; } ->
              aux handler
            | Recursive handlers ->
              Continuation.Map.iter (fun _cont
                    { Continuation_handler.
                      params = _;
                      stub = _;
                      handler;
                      is_exn_handler = _;
                    } ->
                  aux handler)
                handlers
            end
          | Invalid _ -> ()
      and aux_named (named : Named.t) =
        f_named named;
        match named with
        | Simple _ | Prim _ -> ()
        | Set_of_closures set ->
          if not toplevel then begin
            Set_of_closures.iter_function_bodies set 

            Closure_id.Map.iter (fun _ (decl : Function_declaration.t) ->
                aux decl.body)
              funcs.funs
          end
      in
      match maybe_named with
      | Is_expr expr -> aux expr
      | Is_named named -> aux_named named

    let iter f f_named t = iter_general ~toplevel:false f f_named (Is_expr t)

    let iter_expr t ~f = iter f (fun _ -> ()) t

    let iter_named t ~f = iter (fun (_ : t) -> ()) f t

    (* CR-soon mshinwell: Remove "let_rec" from this name (ditto for the
       toplevel-only variant) *)
    let iter_all_immutable_let_and_let_rec_bindings t ~f =
      iter_expr (function
          | Let { var; defining_expr; _ } -> f var defining_expr
          | _ -> ())
        t

    let iter_sets_of_closures t ~f =
      iter_named t ~f:(function
        | Set_of_closures set -> f set
        | Simple _ | Prim _ -> ())

    let iter_function_bodies t ~f =
      iter_sets_of_closures t ~f:(fun (set : Set_of_closures.t) ->
        Set_of_closures.Iterators.iter_function_bodies set ~f)

    module Toplevel_only = struct
      (* CR mshinwell: "toplevel" again -- confusing.  We need two separate
         words:
         1. Not under a lambda
         2. Directly bound in the static part (cf. Flambda_static). *)
      let iter f f_named t =
        iter_general ~toplevel:true f f_named (Is_expr t)

      let iter_all_immutable_let_and_let_rec_bindings t ~f =
        iter_general ~toplevel:true
          (function
            | Let { var; defining_expr; _ } -> f var defining_expr
            | _ -> ())
          (fun _ -> ())
          (Is_expr t)
    end
  end
*)

(* To be re-enabled as we need it
  module Mappers = struct
    let map_lets t ~for_defining_expr ~for_last_body ~after_rebuild =
      let rec loop (t : t) ~rev_lets =
        match t with
        | Let { var; kind; defining_expr; body; _ } ->
          let new_defining_expr = for_defining_expr var kind defining_expr in
          let original =
            if new_defining_expr == defining_expr then
              Some t
            else
              None
          in
          let rev_lets = (var, kind, new_defining_expr, original) :: rev_lets in
          loop body ~rev_lets
        | t ->
          let last_body = for_last_body t in
          (* As soon as we see a change, we have to rebuild that [Let] and every
             outer one. *)
          let seen_change = ref (not (last_body == t)) in
          List.fold_left
            (fun (t : t) (var, kind, defining_expr, original) : t ->
              let let_expr =
                match original with
                | Some original when not !seen_change -> original
                | Some _ | None ->
                  seen_change := true;
                  create_let var kind defining_expr t
              in
              let new_let = after_rebuild let_expr in
              if not (new_let == let_expr) then begin
                seen_change := true
              end;
              new_let)
            last_body
            rev_lets
      in
      loop t ~rev_lets:[]

    let map_general ~toplevel f f_named tree =
      let rec aux (tree : t) =
        match tree with
        | Let _ ->
          map_lets tree ~for_defining_expr:aux_named ~for_last_body:aux
            ~after_rebuild:f
        | _ ->
          let exp : t =
            match tree with
            | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> tree
            | Let _ -> assert false
            | Let_cont { body; handlers; } ->
              let new_body = aux body in
              match handlers with
              | Non_recursive { name; handler =
                  ({ handler = handler_expr; _ } as handler); } ->
                let new_handler_expr = aux handler_expr in
                if new_body == body && new_handler_expr == handler_expr then
                  tree
                else
                  Let_cont {
                    body = new_body;
                    handlers = Non_recursive {
                      name;
                      handler = { handler with handler = new_handler_expr; }
                    };
                  }
              | Recursive handlers ->
                let something_changed = ref false in
                let candidate_handlers =
                  Continuation.Map.map
                    (fun (handler : Continuation_handler.t) ->
                      let new_handler = aux handler.handler in
                      if not (new_handler == handler.handler) then begin
                        something_changed := true
                      end;
                      { handler with handler = new_handler; })
                    handlers
                in
                if !something_changed || not (new_body == body) then
                  Let_cont {
                    body = new_body;
                    handlers = Recursive candidate_handlers;
                  }
                else
                  tree
          in
          f exp
      and aux_named (id : Variable.t) _kind (named : Named.t) =
        let named : Named.t =
          match named with
          | Simple _ | Prim _ -> named
          | Set_of_closures ({ function_decls; closure_elements;
              direct_call_surrogates }) ->
            if toplevel then named
            else begin
              let done_something = ref false in
              let funs =
                Closure_id.Map.map (fun (func_decl : Function_declaration.t) ->
                    let new_body = aux func_decl.body in
                    if new_body == func_decl.body then begin
                      func_decl
                    end else begin
                      done_something := true;
                      Function_declaration.update_body func_decl
                        ~body:new_body
                    end)
                  function_decls.funs
              in
              if not !done_something then
                named
              else
                let function_decls =
                  Function_declarations.update function_decls ~funs
                in
                let set_of_closures =
                  Set_of_closures.create ~function_decls ~closure_elements
                    ~direct_call_surrogates
                in
                Set_of_closures set_of_closures
            end
        in
        f_named id named
      in
      aux tree

    let map f f_named t =
      map_general ~toplevel:false f (fun _ n -> f_named n) t

    let map_expr f t = map f (fun named -> named) t
    let map_named f_named t = map (fun t -> t) f_named t

    let map_named_with_id f_named t =
      map_general ~toplevel:false (fun t -> t) f_named t

    let map_symbols t ~f =
      map_named (function
          | (Simple simple) as named ->
            let new_simple = Simple.map_symbol simple ~f in
            if new_simple == simple then
              named
            else
              Simple new_simple
          | (Set_of_closures _ | Prim _) as named -> named)
        t

    let map_apply t ~f =
      map (function
          | (Apply apply) as expr ->
            let new_apply = f apply in
            if new_apply == apply then
              expr
            else
              Apply new_apply
          | expr -> expr)
        (fun named -> named)
        t

    let map_sets_of_closures t ~f =
      map_named (function
          | (Set_of_closures set_of_closures) as named ->
            let new_set_of_closures = f set_of_closures in
            if new_set_of_closures == set_of_closures then named
            else Set_of_closures new_set_of_closures
          | (Simple _ | Prim _) as named -> named)
        t

    let map_function_bodies ?ignore_stubs t ~f =
      map_sets_of_closures t ~f:(fun (set : Set_of_closures.t) ->
        Set_of_closures.Mappers.map_function_bodies ?ignore_stubs set ~f)

    (* CR mshinwell: duplicate function *)
    let map_all_immutable_let_and_let_rec_bindings (expr : t)
          ~(f : Variable.t -> Named.t -> Named.t) : t =
      map_named_with_id f expr

    module Toplevel_only = struct
      let map f f_named t =
        map_general ~toplevel:true f (fun _ n -> f_named n) t

      let map_expr f_expr t = map f_expr (fun named -> named) t
      let map_named f_named t = map (fun t -> t) f_named t

      let map_sets_of_closures tree ~f =
        map_named (function
            | (Set_of_closures set_of_closures) as named ->
              let new_set_of_closures = f set_of_closures in
              if new_set_of_closures == set_of_closures then named
              else Set_of_closures new_set_of_closures
            | (Simple _ | Prim _) as named -> named)
          tree
      end
  end

  module Folders = struct
    let fold_lets_option (t : t) ~init ~for_defining_expr
          ~for_last_body ~filter_defining_expr =
      let finish ~last_body ~acc ~rev_lets =
        let module W = With_free_names in
        let acc, t =
          List.fold_left (fun (acc, t) (var, kind, defining_expr) ->
              let free_names_of_body = W.free_names t in
              let acc, var, kind, defining_expr =
                filter_defining_expr acc var kind defining_expr
                  free_names_of_body
              in
              match defining_expr with
              | None ->
                acc, t
              | Some defining_expr ->
                let let_expr =
                  W.create_let_reusing_body var kind defining_expr t
                in
                acc, W.of_expr let_expr)
            (acc, W.of_expr last_body)
            rev_lets
        in
        W.contents t, acc
      in
      let rec loop (t : t) ~acc ~rev_lets =
        match t with
        | Let let_binding ->
          Let.pattern_match let_binding
            ~f:(fun var { kind; defining_expr; body; } ->
              let acc, bindings, var, kind, (defining_expr : Reachable.t) =
                for_defining_expr acc var kind defining_expr
              in
              begin match defining_expr with
              | Reachable defining_expr ->
                let rev_lets =
                  (var, kind, defining_expr) :: (List.rev bindings) @ rev_lets
                in
                loop body ~acc ~rev_lets
              | Invalid semantics ->
                let rev_lets = (List.rev bindings) @ rev_lets in
                let body : Expr.t = Invalid semantics in
                let last_body, acc = for_last_body acc body in
                finish ~last_body ~acc ~rev_lets
              end)
        | t ->
          let last_body, acc = for_last_body acc t in
          finish ~last_body ~acc ~rev_lets
      in
      loop t ~acc:init ~rev_lets:[]
  end
*)
end and Expr_with_permutation : sig
  include Contains_names.S
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val create : ?perm:Name_permutation.t -> Expr.t -> t
  val expr : t -> Expr.t
end = struct
  type t = {
    expr_without_perm : Expr.t;
    perm : Name_permutation.t;
    expr_with_perm_and_free_names : (Expr.t * Name_occurrences.t) Lazy.t;
  }

  let create ?perm expr =
    let perm =
      match perm with
      | None -> Name_permutation.create ()
      | Some perm -> perm
    in
    let expr_with_perm_and_free_names =
      lazy (
        let expr = Expr.apply_name_permutation expr perm in
        let free_names = Expr.free_names expr in
        expr, free_names)
    in
    { expr_without_perm = expr;
      perm;
      expr_with_perm_and_free_names;
    }

  let expr t = fst (Lazy.force t.expr_with_perm_and_free_names)

  let free_names t = snd (Lazy.force t.expr_with_perm_and_free_names)

  let apply_name_permutation
        { expr_without_perm;
          perm;
          expr_with_perm_and_free_names = _;
        } perm' =
    let perm = Name_permutation.compose ~first:perm ~second:perm' in
    { expr_without_perm;
      perm;
      expr_with_perm_and_free_names =
        lazy (
          let expr = Expr.apply_name_permutation expr_without_perm perm in
          let free_names = Expr.free_names expr in
          expr, free_names);
    }

  let print ppf t = Expr.print ppf (expr t)

  let print_with_cache ~cache ppf t = Expr.print_with_cache ~cache ppf (expr t)
end and Named : sig
  type t =
    | Simple of Simple.t
    | Prim of Flambda_primitive.t * Debuginfo.t
    | Set_of_closures of Set_of_closures.t

  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val invariant : Invariant_env.t -> t -> Flambda_primitive.result_kind

  val no_effects_or_coeffects : t -> bool

  val box_value
      : Name.t
     -> Flambda_kind.t
     -> Debuginfo.t
     -> Named.t * Flambda_kind.t

  val unbox_value
      : Name.t
     -> Flambda_kind.t
     -> Debuginfo.t
     -> Named.t * Flambda_kind.t

  val at_most_generative_effects : t -> bool

  val dummy_value : Flambda_kind.t -> t
end = struct
  include Named

  let print_with_cache ~cache ppf (t : t) =
    match t with
    | Simple simple ->
      fprintf ppf "%s%a%s"
        (Misc_color.bold_green ())
        Simple.print simple
        (Misc_color.reset ())
    | Prim (prim, dbg) ->
      fprintf ppf "@[<2>(%a%a)@]"
        Flambda_primitive.print prim
        Debuginfo.print_or_elide dbg
    | Set_of_closures set_of_closures ->
      Set_of_closures.print_with_cache ~cache ppf set_of_closures

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  (* CR mshinwell: It seems that the type [Flambda_primitive.result_kind]
     should move into [K], now it's used here. *)
  let invariant env t : Flambda_primitive.result_kind =
    try
      let module E = Invariant_env in
      match t with
      | Simple simple ->
        Singleton (E.kind_of_simple env simple)
      | Set_of_closures set_of_closures ->
        Set_of_closures.invariant env set_of_closures;
        Singleton (K.fabricated ())
      | Prim (prim, dbg) ->
        Flambda_primitive.invariant env prim;
        ignore (dbg : Debuginfo.t);
        Flambda_primitive.result_kind prim
    with Misc.Fatal_error ->
      Misc.fatal_errorf "(during invariant checks) Context is:@ %a" print t

  let free_names t =
    match t with
    | Simple simple -> Simple.free_names simple
    | Prim (prim, _dbg) -> Flambda_primitive.free_names prim
    | Set_of_closures set -> Set_of_closures.free_names set

  let apply_name_permutation t perm =
    match t with
    | Simple simple ->
      let simple' = Simple.apply_name_permutation simple perm in
      if simple == simple' then t
      else Simple simple'
    | Prim (prim, dbg) ->
      let prim' = Flambda_primitive.apply_name_permutation prim perm in
      if prim == prim' then t
      else Prim (prim', dbg)
    | Set_of_closures set ->
      let set' = Set_of_closures.apply_name_permutation set perm in
      if set == set' then t
      else Set_of_closures set

  let box_value name (kind : Flambda_kind.t) dbg : Named.t * Flambda_kind.t =
    let simple = Simple.name name in
    match kind with
    | Value -> Simple simple, kind
    | Naked_number Naked_immediate ->
      Misc.fatal_error "Not yet supported"
    | Naked_number Naked_float ->
      Prim (Unary (Box_number Naked_float, simple), dbg), K.value ()
    | Naked_number Naked_int32 ->
      Prim (Unary (Box_number Naked_int32, simple), dbg), K.value ()
    | Naked_number Naked_int64 ->
      Prim (Unary (Box_number Naked_int64, simple), dbg), K.value ()
    | Naked_number Naked_nativeint ->
      Prim (Unary (Box_number Naked_nativeint, simple), dbg), K.value ()
    | Fabricated ->
      Misc.fatal_error "Cannot box values of [Fabricated] kind"

  let unbox_value name (kind : Flambda_kind.t) dbg : Named.t * Flambda_kind.t =
    let simple = Simple.name name in
    match kind with
    | Value -> Simple simple, kind
    | Naked_number Naked_immediate ->
      Misc.fatal_error "Not yet supported"
    | Naked_number Naked_float ->
      Prim (Unary (Unbox_number Naked_float, simple), dbg), K.naked_float ()
    | Naked_number Naked_int32 ->
      Prim (Unary (Unbox_number Naked_int32, simple), dbg), K.naked_int32 ()
    | Naked_number Naked_int64 ->
      Prim (Unary (Unbox_number Naked_int64, simple), dbg), K.naked_int64 ()
    | Naked_number Naked_nativeint ->
      Prim (Unary (Unbox_number Naked_nativeint, simple), dbg),
        K.naked_nativeint ()
    | Fabricated ->
      Misc.fatal_error "Cannot box values of [Fabricated] kind"

  let no_effects_or_coeffects (t : t) =
    match t with
    | Simple _ -> true
    | Prim (prim, _) -> Flambda_primitive.no_effects_or_coeffects prim
    | Set_of_closures _ -> true

  let at_most_generative_effects (t : t) =
    match t with
    | Simple _ -> true
    | Prim (prim, _) -> Flambda_primitive.at_most_generative_effects prim
    | Set_of_closures _ -> true

  let dummy_value (kind : K.t) : t =
    let simple = 
      match kind with
      | Value -> Simple.const_zero
      | Naked_number Naked_immediate ->
        Simple.const (Untagged_immediate Immediate.zero)
      | Naked_number Naked_float ->
        Simple.const (Naked_float Numbers.Float_by_bit_pattern.zero)
      | Naked_number Naked_int32 ->
        Simple.const (Naked_int32 Int32.zero)
      | Naked_number Naked_int64 ->
        Simple.const (Naked_int64 Int64.zero)
      | Naked_number Naked_nativeint ->
        Simple.const (Naked_nativeint Targetint.zero)
      | Fabricated ->
        Simple.discriminant Discriminant.zero
    in
    Simple simple

(* To be re-enabled
  module Iterators = struct
    let iter f f_named t =
      Expr.iter_general ~toplevel:false f f_named (Is_named t)

    let iter_named f_named t =
      Expr.iter_general ~toplevel:false (fun (_ : Expr.t) -> ()) f_named
        (Is_named t)

    module Toplevel_only = struct
      let iter f f_named t =
        Expr.iter_general ~toplevel:true f f_named (Is_named t)
    end
  end
*)
end and Reachable : sig
  type t =
    | Reachable of Named.t
    | Invalid of Invalid_term_semantics.t

  val reachable : Named.t -> t
  val invalid : unit -> t

  val print : Format.formatter -> t -> unit
end = struct
  type t =
    | Reachable of Named.t
    | Invalid of Invalid_term_semantics.t

  let reachable named = Reachable named

  let invalid () =
    if !Clflags.treat_invalid_code_as_unreachable then
      Invalid Treat_as_unreachable
    else
      Invalid Halt_and_catch_fire

  let print ppf t =
    match t with
    | Reachable named -> Named.print ppf named
    | Invalid sem -> Invalid_term_semantics.print ppf sem
end and Let : sig
  include Contains_names.S

  val create
     : bound_var:Variable.t
    -> kind:Flambda_kind.t
    -> defining_expr:Named.t
    -> body:Expr.t
    -> t

  val invariant : Invariant_env.t -> t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val kind : t -> Flambda_kind.t
  val defining_expr : t -> Named.t

  val pattern_match
     : t
    -> f:(bound_var:Variable.t -> body:Expr.t -> 'a)
    -> 'a

  val continuation_counts_toplevel : t -> Continuation_counts.t

  val no_effects_or_coeffects : t -> bool
end = struct
  module Bound_var_and_body =
    Name_abstraction.Make (Bound_variable) (Expr_with_permutation)

  type t = {
    bound_var_and_body : Bound_var_and_body.t;
    kind : Flambda_kind.t;
    defining_expr : Named.t;
  }

  let pattern_match t ~f =
    Bound_var_and_body.pattern_match t.bound_var_and_body
      ~f:(fun bound_var body ->
        f ~bound_var ~body:(Expr_with_permutation.expr body))

  let print_with_cache ~cache ppf
        ({ bound_var_and_body = _; kind; defining_expr; } as t) =
    let rec let_body (expr : Expr.t) =
      match expr with
      | Let ({ bound_var_and_body = _; kind; defining_expr; } as t) ->
        pattern_match t ~f:(fun ~bound_var ~body ->
          fprintf ppf "@ @[<2>%a@[@ %s:: %a%s@]@ %a@]"
            Variable.print bound_var
            (Misc_color.bold_white ())
            Flambda_kind.print kind
            (Misc_color.reset ())
            (Named.print_with_cache ~cache) defining_expr;
          let_body body)
      | _ -> expr
    in
    pattern_match t ~f:(fun ~bound_var ~body ->
      fprintf ppf "@[<2>(%slet%s@ @[<hv 1>(@[<2>%a@[@ %s:: %a%s@]@ %a@]"
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        Variable.print bound_var
        (Misc_color.bold_white ())
        Flambda_kind.print kind
        (Misc_color.reset ())
        (Named.print_with_cache ~cache) defining_expr;
      let expr = let_body body in
      fprintf ppf ")@]@ %a)@]"
        (Expr.print_with_cache ~cache) expr)

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let create ~bound_var ~kind ~defining_expr ~body =
    let bound_var_and_body =
      Bound_var_and_body.create bound_var (Expr_with_permutation.create body)
    in
    { bound_var_and_body;
      kind;
      defining_expr;
    }

  let invariant env t =
    let module E = Invariant_env in
    pattern_match t ~f:(fun ~bound_var ~body ->
      let named_kind =
        match Named.invariant env t.defining_expr with
        | Singleton kind -> Some kind
        | Unit -> Some (K.value ())
        | Never_returns -> None
      in
      begin match named_kind with
      | None -> ()
      | Some named_kind ->
        if not (K.equal named_kind t.kind) then begin
          Misc.fatal_errorf "[Let] expression inferred kind (%a)@ is not \
              equal to the annotated kind (%a);@ [Let] expression is:@ %a"
            K.print named_kind
            K.print t.kind
            print t
        end
      end;
      let env = E.add_variable env bound_var t.kind in
      Expr.invariant env body)

  let kind t = t.kind
  let defining_expr t = t.defining_expr

  let free_names ({ bound_var_and_body = _; kind = _; defining_expr; } as t) =
    pattern_match t ~f:(fun ~bound_var ~body ->
      let from_defining_expr = Named.free_names defining_expr in
      let from_body = Expr.free_names body in
      Name_occurrences.union from_defining_expr
        (Name_occurrences.remove from_body (Name (Name.var bound_var))))

  let apply_name_permutation ({ bound_var_and_body; kind; defining_expr; } as t)
        perm =
    let bound_var_and_body' =
      Bound_var_and_body.apply_name_permutation bound_var_and_body perm
    in
    let defining_expr' =
      Named.apply_name_permutation defining_expr perm
    in
    if bound_var_and_body == bound_var_and_body'
      && defining_expr == defining_expr'
    then t
    else
      { bound_var_and_body = bound_var_and_body';
        kind;
        defining_expr = defining_expr';
      }

  let continuation_counts_toplevel
        ({ bound_var_and_body = _; kind = _; defining_expr = _; } as t) =
    pattern_match t ~f:(fun ~bound_var:_ ~body ->
      Expr.continuation_counts_toplevel body)

  let no_effects_or_coeffects
        ({ bound_var_and_body = _; kind = _; defining_expr; } as t) =
    Named.no_effects_or_coeffects defining_expr
      && pattern_match t ~f:(fun ~bound_var:_ ~body ->
           Expr.no_effects_or_coeffects body)
end and Let_cont : sig
  type t = private
    | Non_recursive of Non_recursive_let_cont_handler.t
    | Recursive of Recursive_let_cont_handlers.t
  include Contains_names.S with type t := t
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val invariant : Invariant_env.t -> t -> unit
  val create_non_recursive
     : Continuation.t
    -> Continuation_handler.t
    -> body:Expr.t
    -> t
  val create_recursive
     : Continuation_handlers.t
    -> body:Expr.t
    -> t
  val continuation_counts_toplevel : t -> Continuation_counts.t
(*
  val to_continuation_map : t -> Continuation_handlers.t
  val map : t -> f:(Continuation_handlers.t -> Continuation_handlers.t) -> t
*)
  val no_effects_or_coeffects : t -> bool
end = struct
  type t =
    | Non_recursive of Non_recursive_let_cont_handler.t
    | Recursive of Recursive_let_cont_handlers.t

  let invariant _env _t = assert false
(* To be re-enabled
    let module E = Invariant_env in
    let add_parameters env params =
      E.add_kinded_parameters env (T.Parameters.kinded_params params)
    in
(*
    let handler_stack = E.Continuation_stack.var () in
*)
    let env =
      match handlers with
      | Non_recursive { name; handler; } ->
        let kind : E.continuation_kind =
          if handler.is_exn_handler then Exn_handler else Normal
        in
        let params = handler.params in
        let arity = Flambda_type.Parameters.arity params in
        let env = add_parameters env params in
(*
        let env = E.set_current_continuation_stack env handler_stack in
*)
        loop env handler.handler;
        E.add_continuation env name arity kind (* handler_stack *)
      | Recursive handlers ->
        let recursive_env =
          Continuation.Map.fold
            (fun cont (handler : Continuation_handler.t) env ->
              let arity = Flambda_type.Parameters.arity handler.params in
              let kind : Invariant_env.continuation_kind =
                if handler.is_exn_handler then Exn_handler else Normal
              in
              E.add_continuation env cont arity kind handler_stack)
            handlers
            env
        in
        Continuation.Map.iter
          (fun name ({ params; stub; is_exn_handler; handler; }
                : Continuation_handler.t) ->
            if is_exn_handler then begin
              Misc.fatal_errorf "Continuation %a is declared [Recursive] \
                  but is an exception handler"
                Continuation.print name
            end;
            let env = add_parameters recursive_env params in
(*
            let env = E.set_current_continuation_stack env handler_stack in
*)
            loop env handler;
            ignore (stub : bool))
          handlers;
        recursive_env
    in
    loop env body
*)

  let print_with_cache ~cache ppf t =
    if !Clflags.dump_let_cont then begin
      (* Printing the same way as for [Let] is easier when debugging lifting
         passes. *)
      Misc.fatal_error "Needs re-enabling"
(*
      let rec let_cont_body (expr : Expr.t) =
        match expr with
        | Let_cont { body; handlers; } ->
          fprintf ppf "@ @[<2>%a@]"
            (Let_cont_handlers.print_with_cache ~cache) handlers;
          let_cont_body body
        | _ -> ul
      in
      fprintf ppf "@[<2>(%slet_cont%s@ @[<hv 1>(@[<2>%a@]"
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        (Let_cont_handlers.print_with_cache ~cache) handlers;
      let expr = let_cont_body body in
      fprintf ppf ")@]@ %a)@]" (print_with_cache0 ~cache) expr
*)
    end else begin
      let rec gather_let_conts let_conts let_cont =
        match let_cont with
        | Non_recursive handler ->
          Non_recursive_let_cont_handler.pattern_match handler
            ~f:(fun k ~(body : Expr.t) ->
              let let_conts, body =
                match body with
                | Let_cont let_cont -> gather_let_conts let_conts let_cont
                | _ -> let_conts, body
              in
              let handler = Non_recursive_let_cont_handler.handler handler in
              (k, handler) :: let_conts, body)
        | Recursive handlers ->
          Recursive_let_cont_handlers.pattern_match handlers
            ~f:(fun ~(body : Expr.t) handlers ->
              let let_conts, body =
                match body with
                | Let_cont let_cont -> gather_let_conts let_conts let_cont
                | _ -> let_conts, body
              in
              (Continuation.Map.bindings handlers) @ let_conts, body)
      in
      let let_conts, body = gather_let_conts [] t in
      fprintf ppf "@[<2>(@[<v 0>%a@;@[<v 0>"
        (Expr.print_with_cache ~cache) body;
      let first = ref true in
      List.iter (fun (k, handler) ->
          Continuation_handler.print_using_where_with_cache ~cache
            ppf k handler ~first:!first;
          first := false)
        let_conts;
      fprintf ppf "@]@])@]"
    end

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let create_non_recursive k handler ~body =
    Non_recursive (Non_recursive_let_cont_handler.create k handler ~body)

  let create_recursive handlers ~body =
    if Continuation_handlers.contains_exn_handler handlers then begin
      Misc.fatal_error "Exception-handling continuations cannot be recursive"
    end;
    Recursive (Recursive_let_cont_handlers.create handlers ~body)

(* To be re-enabled

  let to_continuation_map t =
    match t with
    | Non_recursive { name; handler } -> Continuation.Map.singleton name handler
    | Recursive handlers -> handlers

  let map (t : t) ~f =
    match t with
    | Non_recursive { name; handler } ->
      let handlers = f (Continuation.Map.singleton name handler) in
      begin match Continuation.Map.bindings handlers with
      | [ name, handler ] -> Non_recursive { name; handler; }
      | _ ->
        Misc.fatal_errorf "Flambda.map: the provided mapping function \
          returned more than one handler for a [Non_recursive] binding"
      end
    | Recursive handlers -> Recursive (f handlers)
*)

  let no_effects_or_coeffects t =
    match t with
    | Non_recursive handler ->
      Non_recursive_let_cont_handler.no_effects_or_coeffects handler
    | Recursive handlers ->
      Recursive_let_cont_handlers.no_effects_or_coeffects handlers

  let free_names t =
    match t with
    | Non_recursive handler ->
      Non_recursive_let_cont_handler.free_names handler
    | Recursive handlers ->
      Recursive_let_cont_handlers.free_names handlers

  let apply_name_permutation t perm =
    match t with
    | Non_recursive handler ->
      let handler' =
        Non_recursive_let_cont_handler.apply_name_permutation handler perm
      in
      if handler == handler' then t
      else Non_recursive handler'
    | Recursive handlers ->
      let handlers' =
        Recursive_let_cont_handlers.apply_name_permutation handlers perm
      in
      if handlers == handlers' then t
      else Recursive handlers'

  let continuation_counts_toplevel t =
    match t with
    | Non_recursive handler ->
      Non_recursive_let_cont_handler.continuation_counts_toplevel handler
    | Recursive handlers ->
      Recursive_let_cont_handlers.continuation_counts_toplevel handlers
end and Non_recursive_let_cont_handler : sig
  include Contains_names.S

  val create
     : Continuation.t
    -> body:Expr.t
    -> Continuation_handler.t
    -> t

  val pattern_match
     : t
    -> f:(Continuation.t -> body:Expr.t -> 'a)
    -> 'a

  val handler : t -> Continuation_handler.t

  val no_effects_or_coeffects : t -> bool

  val continuation_counts_toplevel : t -> Continuation_counts.t
end = struct
  module Continuation_and_body =
    Name_abstraction.Make (Bound_continuation) (Expr)

  type t = {
    continuation_and_body : Continuation_and_body.t;
    handler : Continuation_handler.t;
  }

  let create continuation ~body handler =
    let continuation_and_body =
      Continuation_and_body.create continuation body
    in
    { continuation_and_body;
      handler;
    }

  let pattern_match t ~f =
    Continuation_and_body.pattern_match t.continuation_and_body
      ~f:(fun continuation body -> f continuation ~body)

  let handler t = t.handler

  let free_names { continuation_and_body; handler; } =
    Name_occurrences.union
      (Continuation_and_body.free_names continuation_and_body)
      (Continuation_handler.free_names handler)

  let apply_name_permutation { continuation_and_body; handler; } perm =
    let continuation_and_body' =
      Continuation_and_body.apply_name_permutation continuation_and_body perm
    in
    let handler' =
      Continuation_handler.apply_name_permutation handler perm
    in
    { handler = handler';
      continuation_and_body = continuation_and_body';
    }

  let no_effects_or_coeffects ({ continuation_and_body = _; handler; } as t) =
    Continuation_handler.no_effects_or_coeffects handler
      && pattern_match t ~f:(fun _k ~body -> Expr.no_effects_or_coeffects body)

  let continuation_counts_toplevel _t =
    Misc.fatal_error "Not yet implemented"
end and Recursive_let_cont_handlers0 : sig
  include Contains_names.S

  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val create
     : body:Expr.t
    -> Continuation_handlers.t
    -> t

  val body : t -> Expr.t
  val handlers : t -> Continuation_handlers.t
end = struct
  type t = {
    handlers : Continuation_handlers.t;
    body : Expr_with_permutation.t;
  }

  (* CR mshinwell: Do something about these.  They are needed for the
     name abstraction building functor below. *)
  let print _ppf _t = Misc.fatal_error "Not used"
  let print_with_cache ~cache:_ _ppf _t = Misc.fatal_error "Not used"

  let create ~body handlers =
    { handlers;
      body = Expr_with_permutation.create body;
    }

  let handlers t = t.handlers
  let body t = Expr_with_permutation.expr t.body

  let free_names { handlers; body; } =
    Name_occurrences.union (Continuation_handlers.free_names handlers)
      (Expr_with_permutation.free_names body)

  let apply_name_permutation { handlers; body; } perm =
    let handlers' =
      Continuation_handlers.apply_name_permutation handlers perm
    in
    let body' =
      Expr_with_permutation.apply_name_permutation body perm
    in
    { handlers = handlers';
      body = body';
    }
end and Recursive_let_cont_handlers : sig
  include Contains_names.S

  val create
     : body:Expr.t
    -> Continuation_handlers.t
    -> t

  val pattern_match
     : t
    -> f:(body:Expr.t -> Continuation_handlers.t -> 'a)
    -> 'a

  val no_effects_or_coeffects : t -> bool

  val continuation_counts_toplevel : t -> Continuation_counts.t
end = struct
  include Name_abstraction.Make (Bound_continuations)
    (Recursive_let_cont_handlers0)

  let create ~body handlers =
    let bound = Continuation_handlers.domain handlers in
    let handlers0 =
      Recursive_let_cont_handlers0.create ~body handlers
    in
    create bound handlers0

  let pattern_match t ~f =
    pattern_match t ~f:(fun _bound handlers0 ->
      let body = Recursive_let_cont_handlers0.body handlers0 in
      let handlers = Recursive_let_cont_handlers0.handlers handlers0 in
      f ~body handlers)

  let no_effects_or_coeffects t =
    pattern_match t ~f:(fun ~body handlers ->
      Expr.no_effects_or_coeffects body
        && Continuation_handlers.no_effects_or_coeffects handlers)

  let continuation_counts_toplevel _t =
    Misc.fatal_error "Not yet implemented"
end and Params_and_handler : sig
  type t

  include Contains_names.S with type t := t

  val create
     : Kinded_parameter.t list
    -> param_relations:Flambda_type.Typing_env_extension.t
    -> handler:Expr.t
    -> t

  val pattern_match
     : t
    -> f:(Kinded_parameter.t list
      -> param_relations:Flambda_type.Typing_env_extension.t
      -> handler:Expr.t
      -> 'a)
    -> 'a
end = struct
  module T0 = struct
    type t = {
      param_relations : Flambda_type.Typing_env_extension.t;
      handler : Expr_with_permutation.t;
    }

    let print_with_cache ~cache ppf { param_relations; handler; } =
      fprintf ppf "@[<hov 1>(\
          @[<hov 1>(param_relations %a)@]@ \
          @[<hov 1>(handler %a)@]\
          )@]"
        (Flambda_type.Typing_env_extension.print_with_cache ~cache)
        param_relations
        (Expr_with_permutation.print_with_cache ~cache) handler

    let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let free_names { param_relations; handler; } =
      Name_occurrences.union
        (Flambda_type.Typing_env_extension.free_names param_relations)
        (Expr_with_permutation.free_names handler)

    let apply_name_permutation ({ param_relations; handler; } as t) perm =
      let param_relations' =
        Flambda_type.Typing_env_extension.apply_name_permutation
          param_relations perm
      in
      let handler' =
        Expr_with_permutation.apply_name_permutation handler perm
      in
      if param_relations == param_relations' && handler == handler' then t
      else { param_relations = param_relations'; handler = handler'; }
  end

  include Name_abstraction.Make (Bound_kinded_parameter_list) (T0)

  let create params ~param_relations ~handler =
    let t0 : T0.t =
      { param_relations;
        handler = Expr_with_permutation.create handler;
      }
    in
    create params t0

  let pattern_match t ~f =
    pattern_match t ~f:(fun params { param_relations; handler; } ->
      f params ~param_relations ~handler:(Expr_with_permutation.expr handler))
end and Continuation_handlers : sig
  type t = Continuation_handler.t Continuation.Map.t

  include Contains_names.S with type t := t

  val domain : t -> Continuation.Set.t
  val contains_exn_handler : t -> bool
  val no_effects_or_coeffects : t -> bool
end = struct
  include Continuation_handlers

  let free_names t =
    Continuation.Map.fold (fun _k handler free_names ->
        Name_occurrences.union free_names
          (Continuation_handler.free_names handler))
      t
      (Name_occurrences.create ())

  let apply_name_permutation t perm =
    Continuation.Map.fold (fun k handler result ->
        let k = Name_permutation.apply_continuation perm k in
        let handler =
          Continuation_handler.apply_name_permutation handler perm
        in
        Continuation.Map.add k handler result)
      t
      Continuation.Map.empty

  let domain t = Continuation.Map.keys t

  let contains_exn_handler t =
    Continuation.Map.exists (fun _cont handler ->
        Continuation_handler.is_exn_handler handler)
      t

  let no_effects_or_coeffects t =
    Continuation.Map.for_all (fun _cont handler ->
        Continuation_handler.no_effects_or_coeffects handler)
      t
end and Continuation_handler : sig
  include Contains_names.S

  val print_using_where_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Continuation.t
    -> t
    -> first:bool
    -> unit
(*
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val print : Format.formatter -> t -> unit
*)
  val create
     : Kinded_parameter.t list
    -> param_relations:Flambda_type.Typing_env_extension.t
    -> handler:Expr.t
    -> Flambda_type.Parameters.t
    -> stub:bool
    -> is_exn_handler:bool
    -> t
  val pattern_match
     : t
    -> f:(Kinded_parameter.t list
      -> param_relations:Flambda_type.Typing_env_extension.t
      -> handler:Expr.t
      -> 'a)
    -> 'a
  val stub : t -> bool
  val is_exn_handler : t -> bool

  val no_effects_or_coeffects : t -> bool
end = struct
  type t = {
    params_and_handler : Params_and_handler.t;
    param_relations_lvs : Flambda_type.Parameters.t;
    stub : bool;
    is_exn_handler : bool;
  }

  let pattern_match t ~f =
    Params_and_handler.pattern_match t.params_and_handler ~f

  let print_using_where_with_cache ~cache ppf k
        ({ params_and_handler = _; stub; is_exn_handler; } as t) ~first =
    if not first then begin
      fprintf ppf "@ "
    end;
    pattern_match t ~f:(fun params ~param_relations ~handler ->
      fprintf ppf "@[<v 2>%swhere%s @[%a%s%s@[%a"
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
(*
        (if first_and_non_recursive then "" else "and ")
*)
        Continuation.print k
        (if stub then " *stub*" else "")
        (if is_exn_handler then "*exn* " else "")
        Kinded_parameter.List.print params;
      if not (Flambda_type.Typing_env_extension.is_empty param_relations)
      then begin
        fprintf ppf " [%a]"
          Flambda_type.Typing_env_extension.print param_relations
      end;
      fprintf ppf "@]@] =@ %a"
        (Expr.print_with_cache ~cache) handler)

(*
  let print_with_cache ~cache ppf (t : t) =
    match t with
    | Non_recursive { name; handler = {
        params; stub; handler; is_exn_handler; }; } ->
      fprintf ppf "%a@ %s%s%a=@ %a"
        Continuation.print name
        (if stub then "*stub* " else "")
        (if is_exn_handler then "*exn* " else "")
        (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
        (Expr.print_with_cache ~cache) handler
    | Recursive handlers ->
      let first = ref true in
      Continuation.Map.iter (fun name
              { Continuation_handler.params; stub; is_exn_handler; handler; } ->
          if !first then begin
            fprintf ppf "@;rec "
          end else begin
            fprintf ppf "@;and "
          end;
          fprintf ppf "%a@ %s%s%a=@ %a"
            Continuation.print name
            (if stub then "*stub* " else "")
            (if is_exn_handler then "*exn* " else "")
            (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
            (Expr.print_with_cache ~cache) handler;
          first := false)
        handlers

  let print_with_cache ~cache ppf { params_and_handler; stub; handler; } =
    Params_and_handler.pattern_match params_and_handler
      ~f:(fun params ~handler ->
        fprintf ppf "%s%s%a@ =@ %a"
          (if stub then "*stub* " else "")
          (if is_exn_handler then "*exn* " else "")
          (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
          Expr.print handler)

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t
*)

  let create params ~param_relations ~handler ~param_relations_lvs ~stub
        ~is_exn_handler =
    let params_and_handler =
      Params_and_handler.create params ~param_relations ~handler
    in
    { params_and_handler;
      param_relations_lvs;
      stub;
      is_exn_handler;
    }

  let stub t = t.stub
  let is_exn_handler t = t.is_exn_handler

  let free_names
        { params_and_handler; param_relations_lvs; stub = _;
          is_exn_handler = _;
        } =
    Name_occurrences.union (Params_and_handler.free_names params_and_handler)
      (Flambda_type.Parameters.free_names param_relations_lvs)

  let apply_name_permutation
        ({ params_and_handler; stub; is_exn_handler; } as t) perm =
    let params_and_handler' =
      Params_and_handler.apply_name_permutation params_and_handler perm
    in
    if params_and_handler == params_and_handler' then t
    else
      { params_and_handler = params_and_handler';
        stub;
        is_exn_handler;
      }

  let no_effects_or_coeffects t =
    pattern_match t ~f:(fun _params ~param_relations:_ ~handler ->
      Expr.no_effects_or_coeffects handler)
end and Set_of_closures : sig
  type t = {
    function_decls : Function_declarations.t;
    set_of_closures_ty : Flambda_type.t;
    closure_elements : Simple.t Var_within_closure.Map.t;
    direct_call_surrogates : Closure_id.t Closure_id.Map.t;
  }
  include Contains_names.S with type t := t
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val invariant : Invariant_env.t -> t -> unit
  val create
     : function_decls:Function_declarations.t
    -> closure_elements:Simple.t Var_within_closure.Map.t
    -> direct_call_surrogates:Closure_id.t Closure_id.Map.t
    -> t
  val function_decls : t -> Function_declarations.t
  val closure_elements : t -> Simple.t Var_within_closure.Map.t
  val direct_call_surrogates : t -> Closure_id.t Closure_id.Map.t
  val has_empty_environment : t -> bool
end = struct
  include Set_of_closures

  let invariant _env _t = assert false
(* To be re-enabled 
        { function_decls; free_vars; direct_call_surrogates = _; } =
    (* CR mshinwell: Some of this should move into
       [Function_declarations.invariant] *)
    let module E = Invariant_env in
    (* CR-soon mshinwell: check [direct_call_surrogates] *)
    let { Function_declarations. set_of_closures_origin; funs; } =
      function_decls
    in
    ignore (set_of_closures_origin : Set_of_closures_origin.t);
    let functions_in_closure = Closure_id.Map.keys funs in
    Var_within_closure.Map.iter
      (fun var (var_in_closure : Free_var.t) ->
        E.add_var_within_closure env var;
        E.check_variable_is_bound env var_in_closure.var)
      free_vars;
    let _all_params, _all_free_vars =
      (* CR mshinwell: change to [iter] *)
      Closure_id.Map.fold (fun fun_var function_decl acc ->
          let all_params, all_free_vars = acc in
          (* CR-soon mshinwell: check function_decl.all_symbols *)
          let { Function_declaration. params; body; stub; dbg; my_closure;
                continuation_param = return_cont;
                exn_continuation_param; result_arity; _ } =
            function_decl
          in
          let return_arity = T.Parameters.arity results in
          (* CR mshinwell: Check arity of [exn_continuation_param] *)
          if Continuation.equal return_cont exn_continuation_param
          then begin
            Misc.fatal_errorf "Function declaration's return and exception \
                continuations must be distinct: %a"
              (Function_declaration.print fun_var) function_decl
          end;
          assert (Closure_id.Set.mem fun_var functions_in_closure);
          E.add_closure_id env fun_var;
          ignore (stub : bool);
          ignore (dbg : Debuginfo.t);
          let free_variables =
            Name.set_to_var_set
              (Name_occurrences.everything (Expr.free_names body))
          in
          let kinded_params = T.Parameters.kinded_params params in
          (* Check that every variable free in the body of the function is
             either the distinguished "own closure" variable or one of the
             function's parameters. *)
          let allowed_free_variables =
            Variable.Set.add my_closure (
              Kinded_parameter.List.var_set kinded_params)
          in
          let parameters_with_kinds =
            List.map (fun param ->
                let var = Kinded_parameter.var param in
                let kind = Kinded_parameter.kind param in
                var, kind)
              kinded_params
          in
          let bad =
            Variable.Set.diff free_variables allowed_free_variables
          in
          if not (Variable.Set.is_empty bad) then begin
            Misc.fatal_errorf "The function bound to closure ID %a contains \
                illegal free variables.  The only free variables allowed in \
                the body of a function are the distinguished [my_closure] \
                variable and the function's parameters: %a"
              Closure_id.print fun_var
              (Function_declaration.print fun_var) function_decl
          end;
          (* CR mshinwell: We should allow ordered dependencies left-to-right
             in the parameter list.  Parameters' types maybe can also depend
             on [my_closure]? *)
          (* Check that free names in parameters' types are bound. *)
(* XXX temporarily disabled
          List.iter (fun param ->
              let ty = Typed_parameter.ty param in
              let fns = Flambda_type.free_names ty in
              (* CR mshinwell: This should make sure that the (set of) kinds
                 implied by the modal type in [Name_occurrences] matches up
                 with the kind in the environment.
                 We should also check this when we see a use of a name.
                 For example, a [Debug_only] name may not occur inside the
                 defining expression of a [Let]-binding of kind [Value]. *)
              Name.Set.iter (fun fn -> E.check_name_is_bound env fn)
                (Name_occurrences.everything fns))
            params;
          (* Check that projections on parameters only describe projections
             from other parameters of the same function. *)
          let params' = Typed_parameter.List.var_set params in
*)
(*
          List.iter (fun param ->
              match Typed_parameter.equalities param with
              | [] -> ()
              | _ ->
                (* XXX this needs finishing -- in fact probably not
                   needed now *)
                ()
                (* Old code:
                let projecting_from = Projection.projecting_from projection in
                if not (Variable.Set.mem projecting_from params') then begin
                  Misc.fatal_errorf "Projection %a does not describe a \
                      projection from a parameter of the function %a"
                    Projection.print projection
                    print t
                end *)  )
            params;
          (* Check that parameters are unique across all functions in the
             declaration. *)
          let old_all_params_size = Variable.Set.cardinal all_params in
          let params = params' in
          let params_size = Variable.Set.cardinal params in
          let all_params = Variable.Set.union all_params params in
          let all_params_size = Variable.Set.cardinal all_params in
          if all_params_size <> old_all_params_size + params_size then begin
            Misc.fatal_errorf "Function declarations have overlapping \
                parameters: %a"
              print t
          end;
*)
          (* Check the body of the function. *)
          let body_env =
            E.prepare_for_function_body env
              ~parameters_with_kinds
              ~my_closure
              ~return_cont
              ~return_cont_arity:return_arity
              ~exception_cont:exn_continuation_param
          in
          Expr.invariant body_env body;
          all_params, Variable.Set.union free_variables all_free_vars)
        funs (Variable.Set.empty, Variable.Set.empty)
    in
    Var_within_closure.Map.iter
      (fun _in_closure0 (outer_var : Free_var.t) ->
        E.check_variable_is_bound env outer_var.var;
        ()
        (* XXX also needs finishing -- same as above
        match outer_var.projection with
        | None -> ()
        | Some projection ->
          let projecting_from = Projection.projecting_from projection in
          let in_closure =
            Free_vars.find_by_variable free_vars projecting_from
          in
          match in_closure with
          | None ->
            Misc.fatal_errorf "Closure variable %a equal to outer variable %a \
                is deemed equal to a projection from %a; but %a does not \
                correspond to any closure variable"
              Var_within_closure.print in_closure0
              Free_var.print outer_var
              Variable.print projecting_from
              Variable.print projecting_from
          | Some _in_closure -> () *) )
      free_vars
*)

  let create ~function_decls ~set_of_closures_ty ~closure_elements
        ~direct_call_surrogates =
    { function_decls;
      set_of_closures_ty;
      closure_elements;
      direct_call_surrogates;
    }

  let function_decls t = t.function_decls
  let set_of_closures_ty t = t.set_of_closures_ty
  let closure_elements t = t.closure_elements
  let direct_call_surrogates t = t.direct_call_surrogates

  let has_empty_environment t =
    Var_within_closure.Map.is_empty t.closure_elements

  let print_with_cache ~cache ppf
        { function_decls; 
          set_of_closures_ty;
          closure_elements;
          direct_call_surrogates;
        } =
    fprintf ppf "@[<hov 1>(%sset_of_closures%s@ \
        @[<hov 1>(function_decls %a)@]@ \
        @[<hov 1>(set_of_closures_ty %a)@]@ \
        @[<hov 1>(closure_elements %a)@]\
        @[<hov 1>(direct_call_surrogates %a)@]@ \
        )@]"
      (Misc_color.bold_green ())
      (Misc_color.reset ())
      (Function_declarations.print_with_cache ~cache) function_decls
      (Flambda_type.print_with_cache ~cache) set_of_closures_ty
      (Var_within_closure.Map.print Simple.print) closure_elements
      (Closure_id.Map.print Closure_id.print) direct_call_surrogates

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names
        { function_decls;
          set_of_closures_ty;
          closure_elements;
          direct_call_surrogates = _;
        } =
    Name_occurrences.union_list [
      Function_declarations.free_names function_decls;
      Flambda_type.free_names set_of_closures_ty;
      Simple.List.free_names (Var_within_closure.Map.data closure_elements);
    ]

  let apply_name_permutation
        ({ function_decls; 
           set_of_closures_ty;
           closure_elements;
           direct_call_surrogates;
         } as t) perm =
    let function_decls' =
      Function_declarations.apply_name_permutation function_decls perm
    in
    let set_of_closures_ty' =
      Flambda_type.apply_name_permutation set_of_closures_ty perm
    in
    let closure_elements' =
      Var_within_closure.Map.map_sharing (fun simple ->
          Simple.apply_name_permutation simple perm)
        closure_elements
    in
    if function_decls == function_decls'
      && set_of_closures_ty == set_of_closures_ty'
      && closure_elements == closure_elements'
    then t
    else
      { function_decls = function_decls';
        set_of_closures_ty = set_of_closures_ty';
        closure_elements = closure_elements';
        direct_call_surrogates;
      }

(*
  let variables_bound_by_the_closure t =
    Var_within_closure.Map.keys t.closure_elements

  let find_free_variable cv ({ free_vars; _ } : t) =
    let free_var : Free_var.t =
      Var_within_closure.Map.find cv free_vars
    in
    free_var.var
*)

(* To be re-enabled as we need it
  module Iterators = struct
    let iter_function_bodies t ~f =
      Closure_id.Map.iter (fun _ (function_decl : Function_declaration.t) ->
          let continuation_arity = T.Parameters.arity function_decl.results in
          f ~continuation_arity function_decl.continuation_param
            function_decl.body)
        t.function_decls.funs
  end

  module Mappers = struct
    let map_symbols ({ function_decls; free_vars; direct_call_surrogates; }
          as set_of_closures) ~f =
      let done_something = ref false in
      let funs =
        Closure_id.Map.map (fun (func_decl : Function_declaration.t) ->
            let body = Expr.Mappers.map_symbols func_decl.body ~f in
            if not (body == func_decl.body) then begin
              done_something := true;
            end;
            Function_declaration.update_body func_decl ~body)
          function_decls.funs
      in
      if not !done_something then
        set_of_closures
      else
        let function_decls =
          Function_declarations.update function_decls ~funs
        in
        create ~function_decls ~in_closure:free_vars ~direct_call_surrogates

    let map_function_bodies ?ignore_stubs (set_of_closures : t) ~f =
      let done_something = ref false in
      let funs =
        Closure_id.Map.map (fun (function_decl : Function_declaration.t) ->
            let new_body =
              match ignore_stubs, function_decl.stub with
              | Some (), true -> function_decl.body
              | _, _ ->
                let body =
                  Expr.Mappers.map_function_bodies ?ignore_stubs
                    function_decl.body ~f
                in
                let continuation_arity =
                  T.Parameters.arity function_decl.results
                in
                f ~continuation_arity function_decl.continuation_param body
            in
            if new_body == function_decl.body then
              function_decl
            else begin
              done_something := true;
              Function_declaration.update_body function_decl
                ~body:new_body
            end)
          set_of_closures.function_decls.funs
      in
      if not !done_something then
        set_of_closures
      else
        let function_decls =
          Function_declarations.update set_of_closures.function_decls ~funs
        in
        create ~function_decls ~in_closure:set_of_closures.free_vars
          ~direct_call_surrogates:set_of_closures.direct_call_surrogates
  end

  module Folders = struct
    let fold_function_decls_ignoring_stubs (t : t) ~init ~f =
      Closure_id.Map.fold (fun closure_id function_decl acc ->
          f ~closure_id ~function_decl acc)
        t.function_decls.funs
        init
  end
*)
end and Function_declarations : sig
  include Contains_names.S
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val create : Function_declaration.t Closure_id.Map.t -> t
  val set_of_closures_origin : t -> Set_of_closures_origin.t
  val funs : t -> Function_declaration.t Closure_id.Map.t
  val find : t -> Closure_id.t -> Function_declaration.t
  val update : t -> funs:Function_declaration.t Closure_id.Map.t -> t
  val import_for_pack
     : t
    -> (Set_of_closures_origin.t -> Set_of_closures_origin.t)
    -> t
end = struct
  type t = {
    set_of_closures_origin : Set_of_closures_origin.t;
    funs : Function_declaration.t Closure_id.Map.t;
  }

  let create funs =
    let compilation_unit = Compilation_unit.get_current_exn () in
    let set_of_closures_origin =
      Set_of_closures_origin.create compilation_unit
    in
    { set_of_closures_origin;
      funs;
    }

  let set_of_closures_origin t = t.set_of_closures_origin
  let funs t = t.funs

  let find ({ funs; set_of_closures_origin = _ } : t) closure_id =
    Closure_id.Map.find closure_id funs

  let update function_decls ~funs =
    let set_of_closures_origin = function_decls.set_of_closures_origin in
    { set_of_closures_origin;
      funs;
    }

  let import_for_pack function_decls import_set_of_closures_origin =
    { set_of_closures_origin =
        import_set_of_closures_origin function_decls.set_of_closures_origin;
      funs = function_decls.funs;
    }

  let print_with_cache ~cache ppf (t : t) =
    let funs ppf t =
      Closure_id.Map.iter (fun var decl ->
          (Function_declaration.print_with_cache ~cache) var ppf decl)
        t
    in
    fprintf ppf "@[<2>(%a)(origin = %a)@]" funs t.funs
      Set_of_closures_origin.print t.set_of_closures_origin

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names { set_of_closures_origin = _; funs; } =
    Closure_id.Map.fold
      (fun _closure_id (func_decl : Function_declaration.t) syms ->
        Name_occurrences.union syms (Function_declaration.free_names func_decl))
      funs
      (Name_occurrences.create ())

  let apply_name_permutation ({ set_of_closures_origin; funs; } as t) perm =
    let funs' =
      Closure_id.Map.map_sharing (fun func_decl ->
          Function_declaration.apply_name_permutation func_decl perm)
        funs
    in
    if funs == funs' then t
    else { set_of_closures_origin; funs = funs'; }
end and Params_and_body : sig
  type t

  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit

  val create
     : Kinded_parameter.t list
    -> param_relations:Flambda_type.Typing_env_extension.t
    -> body:Expr.t
    -> my_closure:Variable.t
    -> t

  val pattern_match
     : t
    -> f:(Kinded_parameter.t list
      -> param_relations:Flambda_type.Typing_env_extension.t
      -> body:Expr.t
      -> my_closure:Variable.t
      -> 'a)
    -> 'a
end = struct
  module T0 = struct
    type t = {
      param_relations : Flambda_type.Typing_env_extension.t;
      body : Expr_with_permutation.t;
    }

    let print_with_cache ~cache:_ ppf { param_relations; body; } =
      fprintf ppf "@[<hov 1>(\
          @[<hov 1>(param_relations %a)@]@ \
          @[<hov 1>(body %a)@]\
          )@]"
        Flambda_type.Typing_env_extension.print param_relations
        Expr_with_permutation.print body

    let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let free_names { param_relations; body; } =
      Name_occurrences.union
        (Flambda_type.Typing_env_extension.free_names param_relations)
        (Expr_with_permutation.free_names body)

    let apply_name_permutation ({ param_relations; body;} as t) perm =
      let param_relations' =
        Flambda_type.Typing_env_extension.apply_name_permutation
          param_relations perm
      in
      let body' =
        Expr_with_permutation.apply_name_permutation body perm
      in
      if param_relations == param_relations' && body == body' then t
      else { param_relations = param_relations'; body = body'; }
  end

  include
    Name_abstraction.Make2 (Bound_kinded_parameter_list) (Bound_variable) (T0)

  let create params ~param_relations ~body ~my_closure =
    let t0 : T0.t =
      { param_relations;
        body = Expr_with_permutation.create body;
      }
    in
    create params my_closure t0

  let pattern_match t ~f =
    pattern_match t ~f:(fun params my_closure t0 ->
      f params ~param_relations:t0.param_relations
        ~body:(Expr_with_permutation.expr t0.body) ~my_closure)
end and Function_declaration : sig
  include Contains_names.S
  val create
     : closure_origin:Closure_origin.t
    -> continuation_param:Continuation.t
    -> exn_continuation_param:Continuation.t
    -> params_and_body:Params_and_body.t
    -> result_arity:Flambda_arity.t
    -> stub:bool
    -> dbg:Debuginfo.t
    -> inline:Inline_attribute.t
    -> specialise:Specialise_attribute.t
    -> is_a_functor:bool
    -> t
  val print : Closure_id.t -> Format.formatter -> t -> unit
  val print_with_cache
     : cache:Printing_cache.t
    -> Closure_id.t
    -> Format.formatter
    -> t
    -> unit
  val closure_origin : t -> Closure_origin.t
  val continuation_param : t -> Continuation.t
  val exn_continuation_param : t -> Continuation.t
  val params_and_body : t -> Params_and_body.t
  val code_id : t -> Code_id.t
  val result_arity : t -> Flambda_arity.t
  val stub : t -> bool
  val dbg : t -> Debuginfo.t
  val inline : t -> Inline_attribute.t
  val specialise : t -> Specialise_attribute.t
  val is_a_functor : t -> bool
  val update_params_and_body : t -> Params_and_body.t -> t
end = struct
  type t = {
    closure_origin : Closure_origin.t;
    continuation_param : Continuation.t;
    exn_continuation_param : Continuation.t;
    params_and_body : Params_and_body.t;
    code_id : Code_id.t;
    result_arity : Flambda_arity.t;
    stub : bool;
    dbg : Debuginfo.t;
    inline : Inline_attribute.t;
    specialise : Specialise_attribute.t;
    is_a_functor : bool;
  }

  let create ~closure_origin ~continuation_param ~exn_continuation_param
        ~params_and_body ~result_arity ~stub ~dbg
        ~(inline : Inline_attribute.t)
        ~(specialise : Specialise_attribute.t)
        ~is_a_functor : t =
    begin match stub, inline with
    | true, (Never_inline | Default_inline)
    | false, (Never_inline | Default_inline | Always_inline | Unroll _) -> ()
    | true, (Always_inline | Unroll _) ->
      Misc.fatal_errorf
        "Stubs may not be annotated as [Always_inline] or [Unroll]: %a"
        Params_and_body.print params_and_body
    end;
    begin match stub, specialise with
    | true, (Never_specialise | Default_specialise)
    | false, (Never_specialise | Default_specialise | Always_specialise) -> ()
    | true, Always_specialise ->
      Misc.fatal_errorf
        "Stubs may not be annotated as [Always_specialise]: %a"
        Params_and_body.print params_and_body
    end;
    { closure_origin;
      continuation_param;
      exn_continuation_param;
      params_and_body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
      result_arity;
      stub;
      dbg;
      inline;
      specialise;
      is_a_functor;
    }

  let print_with_cache ~cache closure_id ppf
        { closure_origin;
          continuation_param;
          exn_continuation_param;
          params_and_body;
          code_id = _;
          result_arity;
          stub;
          dbg = _;
          inline;
          specialise;
          is_a_functor;
        } =
    let stub = if stub then " *stub*" else "" in
    let is_a_functor = if is_a_functor then " *functor*" else "" in
    Params_and_body.pattern_match params_and_body
      ~f:(fun params ~param_relations ~body ~my_closure ->
        fprintf ppf
          "@[<2>(%a%s%s%a%a@ (my_closure %a)@ (origin %a)@ =@ \
            %sfun%s@[<2> <%a> <exn %a>@] %a@ @[<2>@ :: %s%a%s"
          Closure_id.print closure_id
          stub
          is_a_functor
          Inline_attribute.print inline
          Specialise_attribute.print specialise
          Variable.print my_closure
          Closure_origin.print closure_origin
          (Misc_color.bold_cyan ())
          (Misc_color.reset ())
          Continuation.print continuation_param
          Continuation.print exn_continuation_param
          Kinded_parameter.List.print params
          (Misc_color.bold_white ())
          Flambda_arity.print result_arity
          (Misc_color.reset ());
        if not (Flambda_type.Typing_env_extension.is_empty param_relations)
        then begin
          fprintf ppf " [%a]"
            (Flambda_type.Typing_env_extension.print_with_cache ~cache)
            param_relations
        end;
        fprintf ppf "@]@ ->\ @ @[<2>%a@])@]@ "
          (Expr.print_with_cache ~cache) body)

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let closure_origin t = t.closure_origin
  let continuation_param t = t.continuation_param
  let exn_continuation_param t = t.exn_continuation_param
  let params_and_body t = t.params_and_body
  let code_id t = t.code_id
  let result_arity t = t.result_arity
  let stub t = t.stub
  let dbg t = t.dbg
  let inline t = t.inline
  let specialise t = t.specialise
  let is_a_functor t = t.is_a_functor

  let update_params_and_body t params_and_body =
    { t with
      params_and_body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
    }

  let free_names
        { closure_origin = _;
          continuation_param = _;
          exn_continuation_param = _;
          params_and_body;
          code_id = _;
          result_arity = _;
          stub = _;
          dbg = _;
          inline = _;
          specialise = _;
          is_a_functor = _;
        } =
    Params_and_body.free_names params_and_body

  let apply_name_permutation
        ({ closure_origin;
           continuation_param;
           exn_continuation_param;
           params_and_body;
           code_id;
           result_arity;
           stub;
           dbg;
           inline;
           specialise;
           is_a_functor;
         } as t) perm =
    let params_and_body' =
      Params_and_body.apply_name_permutation params_and_body perm
    in
    if params_and_body == params_and_body' then t
    else
      { closure_origin;
        continuation_param;
        exn_continuation_param;
        params_and_body = params_and_body';
        code_id;
        result_arity;
        stub;
        dbg;
        inline;
        specialise;
        is_a_functor;
      }

(*
  let find_declaration_variable _closure_id _t =
    (* CR mshinwell for pchambart: What should this do? *)
    assert false  (* XXX *)
*)

  let _fun_vars_referenced_in_decls (_function_decls : t) ~backend:_ =
(*
    let fun_vars = Variable.Map.keys function_decls.funs in
    let symbols_to_fun_vars =
      let module Backend = (val backend : Backend_intf.S) in
      Variable.Set.fold (fun fun_var symbols_to_fun_vars ->
          let closure_id = Closure_id.wrap fun_var in
          let symbol = Backend.closure_symbol closure_id in
          Symbol.Map.add symbol fun_var symbols_to_fun_vars)
        fun_vars
        Symbol.Map.empty
    in
    Variable.Map.map (fun (func_decl : Function_declaration.t) ->
        let from_symbols =
          Symbol.Set.fold (fun symbol fun_vars' ->
              match Symbol.Map.find symbol symbols_to_fun_vars with
              | exception Not_found -> fun_vars'
              | fun_var ->
                assert (Variable.Set.mem fun_var fun_vars);
                Variable.Set.add fun_var fun_vars')
            func_decl.free_symbols
            Variable.Set.empty
        in
        let from_variables =
          Variable.Set.inter func_decl.free_variables fun_vars
        in
        Variable.Set.union from_symbols from_variables)
      function_decls.funs
*)
    (* CR pchambart: this needs another way to do it *)
    assert false

(* To be re-enabled
  let closures_required_by_entry_point ~(entry_point : Closure_id.t) ~backend
      (function_decls : t) =
    let dependencies =
      fun_vars_referenced_in_decls function_decls ~backend
    in
    let set = ref Closure_id.Set.empty in
    let queue = Queue.create () in
    let add v =
      if not (Closure_id.Set.mem v !set) then begin
        set := Closure_id.Set.add v !set;
        Queue.push v queue
      end
    in
    add entry_point;
    while not (Queue.is_empty queue) do
      let closure_id = Queue.pop queue in
      match Closure_id.Map.find closure_id dependencies with
      | exception Not_found -> ()
      | fun_dependencies ->
        Closure_id.Set.iter (fun dep ->
            if Closure_id.Map.mem dep function_decls.funs then
              add dep)
          fun_dependencies
    done;
    !set

  let contains_stub (fun_decls : t) =
    let number_of_stub_functions =
      Closure_id.Map.cardinal
        (Closure_id.Map.filter
          (fun _ ({ stub; _ } : Function_declaration.t) -> stub)
          fun_decls.funs)
    in
    number_of_stub_functions > 0
*)

(*
  let map_parameter_types t ~f =
    let funs =
      Closure_id.Map.map (fun (decl : Function_declaration.t) ->
          Function_declaration.map_parameter_types decl ~f)
        t.funs
    in
    update t ~funs
*)
end and Flambda_type : Flambda_type0_intf.S with module Expr := Expr
  = Flambda_type0.Make (Expr)

(* To be re-enabled (and thought about some to determine if we still need
   this
module With_free_names = struct
  type 'a t =
    | Expr : Expr.t * Name_occurrences.t -> Expr.t t
    | Named : Flambda_kind.t * Named.t -> Named.t t

  let print (type a) ppf (t : a t) =
    match t with
    | Expr (expr, _) -> Expr.print ppf expr
    | Named (_, named, _) -> Named.print ppf named

  let of_defining_expr_of_let (let_expr : Let.t) =
    Named (let_expr.kind, let_expr.defining_expr)

  let of_body_of_let (let_expr : Let.t) =
    Expr (let_expr.body, let_expr.free_names_of_body)

  let of_expr expr =
    Expr (expr, Expr.free_names expr)

  let of_named kind named =
    Named (kind, named, Named.free_names named)

  let to_named (t : Named.t t) =
    match t with
    | Named (_, named, _) -> named

  let create_let_reusing_defining_expr var (t : Named.t t) body : Expr.t =
    match t with
    | Named (kind, defining_expr) ->
      Let {
        var;
        kind;
        defining_expr;
        body;
        free_names_of_defining_expr;
      }

  let create_let_reusing_body var kind defining_expr (t : Expr.t t) : Expr.t =
    match t with
    | Expr (body, free_names_of_body) ->
      Let {
        var;
        kind;
        defining_expr;
        body;
        free_names_of_defining_expr = Named.free_names defining_expr;
        free_names_of_body;
      }

  let create_let_reusing_both var (t1 : Named.t t) (t2 : Expr.t t) : Expr.t =
    match t1, t2 with
    | Named (kind, defining_expr, free_names_of_defining_expr),
        Expr (body, free_names_of_body) ->
      Let {
        var;
        kind;
        defining_expr;
        body;
        free_names_of_defining_expr;
        free_names_of_body;
      }

  let contents (type a) (t : a t) : a =
    match t with
    | Expr (expr, _) -> expr
    | Named (_, named, _) -> named

  let free_names (type a) (t : a t) =
    match t with
    | Expr (_, free_names) -> free_names
    | Named (_, _, free_names) -> free_names
end
*)
