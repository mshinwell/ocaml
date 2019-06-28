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

module K = Flambda_kind

type t = {
  callee : Simple.t;
  continuation : Continuation.t;
  exn_continuation : Exn_continuation.t;
  args : Simple.t list;
  call_kind : Call_kind.t;
  dbg : Debuginfo.t;
  inline : Inline_attribute.t;
}

let print ppf { callee; continuation; exn_continuation; args; call_kind;
      dbg; inline; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(callee %a)@]@ \
      @[<hov 1>(args (%a))@]@ \
      @[<hov 1>(continuation %a)@]@ \
      @[<hov 1>(exn_continuation %a)@]@ \
      @[<hov 1>(call_kind %a)@]@ \
      @[<hov 1>(dbg %a)@]@ \
      @[<hov 1>(inline %a)@])@]"
    Simple.print callee
    Simple.List.print args
    Continuation.print continuation
    Exn_continuation.print exn_continuation
    Call_kind.print call_kind
    Debuginfo.print_compact dbg
    Inline_attribute.print inline

let print_with_cache ~cache:_ ppf t = print ppf t

let invariant env
      ({ callee;
        continuation;
        exn_continuation;
        args;
        call_kind;
        dbg;
        inline;
      } as t) =
    let unbound_continuation cont reason =
      Misc.fatal_errorf "Unbound continuation %a in %s: %a"
        Continuation.print cont
        reason
        print t
    in
    let module E = Invariant_env in
    Call_kind.invariant env call_kind;
(*
    let stack = E.current_continuation_stack env in
*)
    E.check_simple_is_bound_and_of_kind env callee K.value;
    begin match call_kind with
    | Function (Direct { closure_id = _; return_arity = _; }) ->
      (* Note that [return_arity] is checked for all the cases below. *)
      E.check_simples_are_bound env args
    | Function Indirect_unknown_arity ->
      E.check_simples_are_bound_and_of_kind env args K.value
    | Function (Indirect_known_arity { param_arity; return_arity = _; }) ->
      ignore (param_arity : Flambda_arity.t);
      E.check_simples_are_bound env args
    | Method { kind; obj; } ->
      ignore (kind : Call_kind.method_kind);
      E.check_simple_is_bound_and_of_kind env obj K.value;
      E.check_simples_are_bound_and_of_kind env args K.value
    | C_call { alloc = _; param_arity = _; return_arity = _; } ->
      (* CR mshinwell: Check exactly what Cmmgen can compile and then
         add further checks on [param_arity] and [return_arity] *)
      begin match callee with
      | Name (Symbol _) -> ()
      | _ ->
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
      if not (Flambda_arity.equal arity expected_arity)
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
    begin match
      E.find_continuation_opt env
        (Exn_continuation.exn_handler exn_continuation)
    with
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
      let expected_arity = [Flambda_kind.value] in
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
    ignore (inline : Inline_attribute.t)

let create ~callee ~continuation exn_continuation ~args ~call_kind dbg ~inline =
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
  }

let callee t = t.callee
let continuation t = t.continuation
let exn_continuation t = t.exn_continuation
let args t = t.args
let call_kind t = t.call_kind
let dbg t = t.dbg
let inline t = t.inline

let free_names
      { callee;
        continuation;
        exn_continuation;
        args;
        call_kind;
        dbg = _;
        inline = _;
      } =
  Name_occurrences.union_list [
    Simple.free_names callee;
    Name_occurrences.singleton_continuation continuation;
    Exn_continuation.free_names exn_continuation;
    Simple.List.free_names args;
    Call_kind.free_names call_kind;
  ]

let apply_name_permutation
      ({ callee;
         continuation;
         exn_continuation;
         args;
         call_kind;
         dbg;
         inline;
      } as t)
      perm =
  let continuation' =
    Name_permutation.apply_continuation perm continuation
  in
  let exn_continuation' =
    Exn_continuation.apply_name_permutation exn_continuation perm
  in
  let callee' = Simple.apply_name_permutation callee perm in
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
    }

let with_continuation t continuation =
  { t with continuation; }

let with_call_kind t call_kind =
  { t with call_kind; }

let with_continuation_callee_and_args t continuation ~callee ~args =
  { t with
    continuation;
    callee;
    args;
  }
