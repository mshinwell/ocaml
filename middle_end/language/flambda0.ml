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

module K = Flambda_kind

let fprintf = Format.fprintf

module Call_kind = struct
  (* CR-someday xclerc: we could add annotations to external declarations
     (akin to [@@noalloc]) in order to be able to refine the computation of
     effects/coeffects for such functions. *)

  type function_call =
    | Direct of {
        closure_id : Closure_id.t;
        (* CR mshinwell: Should this arity really permit "bottom"? *)
        return_arity : Flambda_arity.t;
        (** [return_arity] describes what the callee returns.  It matches up
            with the arity of [continuation] in the enclosing [apply]
            record. *)
      }
    | Indirect_unknown_arity
    | Indirect_known_arity of {
        param_arity : Flambda_arity.t;
        return_arity : Flambda_arity.t;
      }

  let print_function_call ppf call =
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

  let return_arity_function_call call : Flambda_arity.t =
    match call with
    | Direct { return_arity; _ }
    | Indirect_known_arity { return_arity; _ } -> return_arity
    | Indirect_unknown_arity -> [Flambda_kind.value ()]

  type method_kind = Self | Public | Cached

  let print_method_kind ppf kind =
    let fprintf = Format.fprintf in
    match kind with
    | Self -> fprintf ppf "Self"
    | Public -> fprintf ppf "Public"
    | Cached -> fprintf ppf "Cached"

  type t =
    | Function of function_call
    | Method of { kind : method_kind; obj : Name.t; }
    | C_call of {
        alloc : bool;
        param_arity : Flambda_arity.t;
        return_arity : Flambda_arity.t;
      }

  let print ppf t =
    let fprintf = Format.fprintf in
    match t with
    | Function call -> print_function_call ppf call
    | Method { kind; obj; } ->
      fprintf ppf "@[(Method %a : %a)@]"
        Name.print obj
        print_method_kind kind
    | C_call { alloc; param_arity; return_arity; } ->
      fprintf ppf "@[(C (alloc %b) : %a -> %a)@]"
        alloc
        Flambda_arity.print param_arity
        Flambda_arity.print return_arity

  let return_arity t : Flambda_arity.t =
    match t with
    | Function call -> return_arity_function_call call
    | Method _ -> [Flambda_kind.value ()]
    | C_call { return_arity; _ } -> return_arity

  let free_names t =
    match t with
    | Function (Direct { closure_id = _; return_arity = _; })
    | Function Indirect_unknown_arity
    | Function (Indirect_known_arity { param_arity = _; return_arity = _; })
    | C_call { alloc = _; param_arity = _; return_arity = _; } ->
      Name_occurrences.create ()
    | Method { kind = _; obj; } ->
      Name_occurrences.create_from_set_in_terms (Name.Set.singleton obj)

  let apply_name_permutation t perm =
    match t with
    | Function (Direct { closure_id = _; return_arity = _; })
    | Function Indirect_unknown_arity
    | Function (Indirect_known_arity { param_arity = _; return_arity = _; })
    | C_call { alloc = _; param_arity = _; return_arity = _; } -> t
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
    Format.fprintf ppf "@[<hov 1>(\
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

  let free_names
        { callee;
          continuation = _;
          exn_continuation = _;
          args;
          call_kind;
          dbg = _;
          inline = _;
          specialise = _;
        } =
    let from_callee =
      Name_occurrences.create_from_set_in_terms (Name.Set.singleton callee)
    in
    let from_args = Simple.List.free_names args in
    let from_call_kind = Call_kind.free_names call_kind in
    Name_occurrences.union from_callee
      (Name_occurrences.union from_args from_call_kind)

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
    let callee' = Name_permutation.apply_name perm callee in
    let args' = Name_permutation.apply_simples perm args in
    let call_kind' = Call_kind.apply_name_permutation call_kind perm in
    if callee == callee' && args == args' && call_kind == call_kind' then
      t
    else
      { callee = callee';
        continuation;
        exn_continuation;
        args = args';
        call_kind = call_kind';
        dbg;
        inline;
        specialise;
      }
end

module Trap_action = struct
  type t =
    | Push of { id : Trap_id.t; exn_handler : Continuation.t; }
    | Pop of {
        id : Trap_id.t;
        exn_handler : Continuation.t;
        take_backtrace : bool;
      }

  include Hashtbl.Make_with_map (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Push { id = id1; exn_handler = exn_handler1; },
          Push { id = id2; exn_handler = exn_handler2; } ->
        let c = Trap_id.compare id1 id2 in
        if c <> 0 then c
        else Continuation.compare exn_handler1 exn_handler2
      | Pop { id = id1; exn_handler = exn_handler1;
              take_backtrace = take_backtrace1; },
          Pop { id = id2; exn_handler = exn_handler2;
                take_backtrace = take_backtrace2; } ->
        let c = Trap_id.compare id1 id2 in
        if c <> 0 then c
        else
          let c = Continuation.compare exn_handler1 exn_handler2 in
          if c <> 0 then c
          else
            Pervasives.compare take_backtrace1 take_backtrace2
      | Push _, Pop _ -> -1
      | Pop _, Push _ -> 1

    let hash t =
      match t with
      | Push { id; exn_handler; } ->
        Hashtbl.hash (Trap_id.hash id, Continuation.hash exn_handler)
      | Pop { id; exn_handler; take_backtrace; } ->
        Hashtbl.hash (Trap_id.hash id, Continuation.hash exn_handler,
          take_backtrace)

    let print ppf t =
      match t with
      | Push { id; exn_handler; } ->
        fprintf ppf "%spush%s %a %a %sthen%s "
          (Misc_color.bold_cyan ())
          (Misc_color.reset ())
          Trap_id.print id
          Continuation.print exn_handler
          (Misc_color.bold_cyan ())
          (Misc_color.reset ())
      | Pop { id; exn_handler; take_backtrace; } ->
        fprintf ppf "%spop%s%s %a %a %sthen%s "
          (Misc_color.bold_cyan ())
          (Misc_color.reset ())
          (if take_backtrace then " with backtrace" else "")
          Trap_id.print id
          Continuation.print exn_handler
          (Misc_color.bold_cyan ())
          (Misc_color.reset ())
  end)

  module Option = struct
    let print ppf = function
      | None -> ()
      | Some t -> print ppf t
  end
end

module Switch = struct
  type t = Continuation.t Discriminant.Map.t

  let invariant t =
    let module E = Invariant_env in
    assert (Discriminant.Map.cardinal t >= 2);
    let check discr cont =
      ignore (discr : Discriminant.t);
      match E.find_continuation_opt env cont with
      | None ->
        unbound_continuation cont "[Switch] term"
      | Some (arity, kind, cont_stack) ->
        let current_stack = E.current_continuation_stack env in
        E.Continuation_stack.unify cont cont_stack current_stack;
        begin match kind with
        | Normal -> ()
        | Exn_handler ->
          Misc.fatal_errorf "Continuation %a is an exception handler \
              but is used in this [Switch] as a normal continuation:@ %a"
            Continuation.print cont
            print expr
        end;
        if List.length arity <> 0 then begin
          Misc.fatal_errorf "Continuation %a is used in this [Switch] \
              and thus must have arity [], but has arity %a"
            Continuation.print cont
            Flambda_arity.print arity
        end
    in
    Discriminant.Map.iter check t

  let iter t ~f = Discriminant.Map.iter f t

  let num_arms t = Discriminant.Map.cardinal t

  let arms t = t

  include Map.Make_with_set (struct
    type nonrec t = t

    let compare t1 t2 =
      if t1 == t2 then 0
      else Discriminant.Map.compare Continuation.compare t1 t2

    let print ppf (t : t) =
      let spc = ref false in
      Discriminant.Map.iter (fun discriminant l ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<hv 1>| %a ->@ %sgoto%s %a@]"
            Discriminant.print discriminant
            (Misc_color.bold_cyan ())
            (Misc_color.reset ())
            Continuation.print l)
        t
  end)
end

type recursive =
  | Non_recursive
  | Recursive

type mutable_or_immutable =
  | Mutable
  | Immutable

module rec Expr : sig
  type t =
    | Let of Let.t
    | Let_cont of Let_cont.t
    | Apply of Apply.t
    | Apply_cont of Continuation.t * Trap_action.t option * Simple.t list
    | Switch of Name.t * Switch.t
    | Invalid of Invalid_term_semantics.t

  val create_let : Variable.t -> Flambda_kind.t -> Named.t -> t -> t
  val create_switch
     : scrutinee:Name.t
    -> arms:Continuation.t Discriminant.Map.t
    -> Expr.t
  val free_names_advanced
     : ?ignore_uses_as_callee:unit
    -> ?ignore_uses_as_argument:unit
    -> ?ignore_uses_as_continuation_argument:unit
    -> ?ignore_uses_in_project_var:unit
    -> ?ignore_uses_in_apply_cont:unit
    -> t
    -> Name_occurrences.t
  val free_names : t -> Name_occurrences.t
  val free_variables : t -> Variable.Set.t
  val free_symbols : t -> Symbol.Set.t
  val used_names
     : ?ignore_uses_as_callee:unit
    -> ?ignore_uses_as_argument:unit
    -> ?ignore_uses_as_continuation_argument:unit
    -> ?ignore_uses_in_project_var:unit
    -> t
    -> Name_occurrences.t
  val free_continuations : t -> Continuation.Set.t
  val invalid : unit -> t
  val iter_lets
     : t
    -> for_defining_expr:(Variable.t -> Flambda_kind.t -> Named.t -> unit)
    -> for_last_body:(t -> unit)
    -> for_each_let:(t -> unit)
    -> unit
  val map_lets
     : t
    -> for_defining_expr:(Variable.t -> Flambda_kind.t -> Named.t -> Named.t)
    -> for_last_body:(t -> t)
    -> after_rebuild:(t -> t)
    -> t
  type maybe_named =
    | Is_expr of t
    | Is_named of Named.t
  val iter_general
     : toplevel:bool
    -> (Expr.t -> unit)
    -> (Named.t -> unit)
    -> maybe_named
    -> unit
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
end = struct
  include Expr

  let invariant env expr =
    let module E = Invariant_env in
    let unbound_continuation cont reason =
      Misc.fatal_errorf "Unbound continuation %a in %s: %a"
        Continuation.print cont
        reason
        print expr
    in
    let add_parameters env params =
      (* CR mshinwell: Need to think carefully about exactly what the
         freshness criterion is for [Parameters] *)
      E.add_kinded_parameters env (T.Parameters.kinded_params params)
    in
    let rec loop env (t : t) : unit =
      match t with
      | Let let_expr -> Let.invariant env let_expr
      | Let_cont let_cont -> Let_cont.invariant env let_cont
      | Apply_cont (cont, trap_action, args) ->
        let args_arity = List.map (fun arg -> E.kind_of_simple env arg) args in
        let arity, kind, cont_stack =
          match E.find_continuation_opt env cont with
          | Some result -> result
          | None -> unbound_continuation cont "[Apply_cont] term"
        in
        let stack = E.current_continuation_stack env in
        E.Continuation_stack.unify cont stack cont_stack;
        if not (Flambda_arity.equal args_arity arity) then begin
          Misc.fatal_errorf "Continuation %a called with wrong arity in \
              this [Apply_cont] term: expected %a but found %a:@ %a"
            Continuation.print cont
            Flambda_arity.print arity
            Flambda_arity.print args_arity
            print expr
        end;
        begin match kind with
        | Normal -> ()
        | Exn_handler ->
          Misc.fatal_errorf "Continuation %a is an exception handler \
              but is used in this [Apply_cont] term as a normal continuation:@ \
              %a"
            Continuation.print cont
            print expr
        end;
        let check_trap_action exn_handler =
          match E.find_continuation_opt env exn_handler with
          | None ->
            unbound_continuation exn_handler "[Apply] trap handler"
          | Some (arity, kind, cont_stack) ->
            begin match kind with
            | Exn_handler -> ()
            | Normal ->
              Misc.fatal_errorf "Continuation %a is a normal continuation  \
                  but is used in the trap action of this [Apply] term as an \
                  exception handler:@ %a"
                Continuation.print exn_handler
                print expr
            end;
            assert (not (Continuation.equal cont exn_handler));
            let expected_arity = [K.value ()] in
            if not (Flambda_arity.equal arity expected_arity) then begin
              Misc.fatal_errorf "Exception handler continuation %a has \
                  the wrong arity for the trap handler action of this \
                  [Apply] term: expected %a but found %a:@ %a"
                Continuation.print cont
                Flambda_arity.print expected_arity
                Flambda_arity.print arity
                print expr
            end;
            cont_stack
        in
        let current_stack = E.current_continuation_stack env in
        let stack, cont_stack =
          match trap_action with
          | None -> current_stack, cont_stack
          | Some (Push { id; exn_handler }) ->
            let cont_stack = check_trap_action exn_handler in
            E.Continuation_stack.push id exn_handler current_stack, cont_stack
          | Some (Pop { id; exn_handler; take_backtrace = _; }) ->
            let cont_stack = check_trap_action exn_handler in
            current_stack, E.Continuation_stack.push id exn_handler cont_stack
        in
        E.Continuation_stack.unify cont stack cont_stack
      | Apply ({ func; continuation; exn_continuation; args; call_kind; dbg;
                 inline; specialise; } as apply) ->
        let stack = E.current_continuation_stack env in
        E.check_name_is_bound_and_of_kind env func (K.value ());
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
          begin match func with
          | Symbol _ -> ()
          | Var _ ->
            (* CR-someday mshinwell: We could expose indirect C calls at the
               source language level. *)
            Misc.fatal_errorf "For [C_call] applications the callee must be \
                directly specified as a [Symbol], not via a [Var]:@ %a"
              Apply.print apply
          end
        end;
        begin match E.find_continuation_opt env continuation with
        | None ->
          unbound_continuation continuation "[Apply] term"
        | Some (arity, kind, cont_stack) ->
          begin match kind with
          | Normal -> ()
          | Exn_handler ->
            Misc.fatal_errorf "Continuation %a is an exception handler \
                but is used in this [Apply] term as a return continuation:@ %a"
              Continuation.print continuation
              print expr
          end;
          let expected_arity = Call_kind.return_arity call_kind in
          if not (Flambda_arity.compatible arity ~if_used_at:expected_arity)
          then begin
            Misc.fatal_errorf "Continuation %a called with wrong arity in \
                this [Apply] term: expected %a but used at %a:@ %a"
              Continuation.print continuation
              Flambda_arity.print expected_arity
              Flambda_arity.print arity
              print expr
          end;
          E.Continuation_stack.unify continuation stack cont_stack
        end;
        begin match E.find_continuation_opt env exn_continuation with
        | None ->
          unbound_continuation continuation
            "[Apply] term exception continuation"
        | Some (arity, kind, cont_stack) ->
          begin match kind with
          | Normal ->
            Misc.fatal_errorf "Continuation %a is a normal continuation \
                but is used in this [Apply] term as an exception handler:@ %a"
              Continuation.print continuation
              print expr
          | Exn_handler -> ()
          end;
          let expected_arity = [Flambda_kind.value ()] in
          if not (Flambda_arity.equal arity expected_arity) then begin
            Misc.fatal_errorf "Exception continuation %a named in this \
                [Apply] term has the wrong arity: expected %a but have %a:@ %a"
              Continuation.print continuation
              Flambda_arity.print expected_arity
              Flambda_arity.print arity
              print expr
          end;
          E.Continuation_stack.unify exn_continuation stack cont_stack
        end;
        ignore (dbg : Debuginfo.t);
        ignore (inline : inline_attribute);
        ignore (specialise : specialise_attribute)
      | Switch (arg, switch) ->
        E.check_name_is_bound_and_of_kind env arg (K.fabricated ());
        Switch.invariant env switch
      | Invalid _ -> ()
    in
    loop env expr

  let rec print_with_cache0 ~cache ppf (t : t) =
    match t with
    | Apply ({ func; continuation; exn_continuation; args; call_kind; inline;
               specialise; dbg; }) ->
      Format.fprintf ppf "@[<2>(apply@ \
          (func %a)@ \
          (args %a)@ \
          (call_kind %a)@ \
          (inline %a)@ \
          (specialise %a)@ \
          (dbg %a)@ \
          (continuation %a)@ \
          (exn_continuation %a))@]"
        Name.print func
        Simple.List.print args
        Call_kind.print call_kind
        Inline_attribute.print inline
        Specialise_attribute.print specialise
        Debuginfo.print_or_elide dbg
        Continuation.print continuation
        Continuation.print exn_continuation
    | Let { var = id; kind; defining_expr = arg; body; _ } ->
      let rec letbody (ul : t) =
        match ul with
        | Let { var = id; kind; defining_expr = arg; body; _ } ->
          fprintf ppf "@ @[<2>%a@[@ %s:: %a%s@]@ %a@]"
            Variable.print id
            (Misc_color.bold_white ())
            Flambda_kind.print kind
            (Misc_color.reset ())
            (Named.print_with_cache ~cache) arg;
          letbody body
        | _ -> ul
      in
      fprintf ppf "@[<2>(%slet%s@ @[<hv 1>(@[<2>%a@[@ %s:: %a%s@]@ %a@]"
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        Variable.print id
        (Misc_color.bold_white ())
        Flambda_kind.print kind
        (Misc_color.reset ())
        (Named.print_with_cache ~cache) arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" print expr
    | Switch (scrutinee, sw) ->
      fprintf ppf
        "@[<v 1>(%sswitch%s %a@ @[<v 0>%a@])@]"
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        Name.print scrutinee Switch.print sw
    | Apply_cont (i, trap_action, []) ->
      fprintf ppf "@[<2>(%a%sgoto%s@ %a)@]"
        Trap_action.Option.print trap_action
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        Continuation.print i
    | Apply_cont (i, trap_action, ls) ->
      fprintf ppf "@[<2>(%a%sapply_cont%s@ %a@ %a)@]"
        Trap_action.Option.print trap_action
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        Continuation.print i
        Simple.List.print ls
    | Let_cont { body; handlers; } ->
      (* Printing the same way as for [Let] is easier when debugging lifting
         passes. *)
      if !Clflags.dump_let_cont then begin
        let rec let_cont_body (ul : t) =
          match ul with
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
      end else begin
        (* CR mshinwell: Share code with ilambda.ml *)
        let rec gather_let_conts let_conts (t : t) =
          match t with
          | Let_cont let_cont ->
            gather_let_conts (let_cont.handlers :: let_conts) let_cont.body
          | body -> let_conts, body
        in
        let let_conts, body = gather_let_conts [] t in
        let pp_sep ppf () = fprintf ppf "@ " in
        fprintf ppf "@[<2>(@[<v 0>%a@;@[<v 0>%a@]@])@]"
          (print_with_cache0 ~cache) body
          (Format.pp_print_list ~pp_sep
            (Let_cont_handlers.print_using_where_with_cache ~cache)) let_conts
      end
    | Invalid _ ->
      fprintf ppf "%sunreachable%s"
          (Misc_color.bold_cyan ())
          (Misc_color.reset ())

  let print_with_cache ~cache ppf (t : t) =
    print_with_cache0 ~cache ppf t
(*
    Printing_cache.with_cache cache ppf "expr" t
      (fun ppf () -> print_with_cache0 ~cache ppf t)
*)

  let print ppf (t : t) =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let name_usage ?ignore_uses_as_callee
      ?ignore_uses_as_argument ?ignore_uses_as_continuation_argument
      ?ignore_uses_in_project_var ?ignore_uses_in_apply_cont
      ~all_used_names tree =
    let free = ref (Name_occurrences.create ()) in
    let bound = ref (Name_occurrences.create ()) in
    let free_names names =
      free := Name_occurrences.union !free names
    in
    let free_name_in_term name =
      let new_free = Name_occurrences.add !free name In_terms in
      free := new_free
    in
    let free_names_in_term names =
      let new_free = Name_occurrences.add_set !free names In_terms in
      free := new_free
    in
    (* CR mshinwell: this can be renamed now *)
    let free_names_promoted_to_kind names (kind : K.t) =
      match kind with
      | Value | Naked_number _ | Fabricated -> free_names names
    in
    let bound_name_in_term name =
      let new_bound = Name_occurrences.add !bound name In_terms in
      bound := new_bound
    in
    (* CR mshinwell: likewise *)
    let bound_name_of_kind name (kind : K.t) =
      match kind with
      | Value | Naked_number _ | Fabricated -> bound_name_in_term name
    in
    (* N.B. This function assumes that all bound identifiers are distinct. *)
    let rec aux (flam : t) : unit =
      match flam with
      | Apply { func; args; call_kind; _ } ->
        begin match ignore_uses_as_callee with
        | None -> free_name_in_term func
        | Some () -> ()
        end;
        begin match call_kind with
        | Function (Direct { closure_id = _; return_arity = _; })
        | Function Indirect_unknown_arity
        | Function (Indirect_known_arity {
            param_arity = _; return_arity = _; }) -> ()
        | Method { kind = _; obj; } -> free_name_in_term obj
        | C_call { alloc = _; param_arity = _; return_arity = _; } -> ()
        end;
        begin match ignore_uses_as_argument with
        | None -> free_names_in_term (Simple.List.free_names args)
        | Some () -> ()
        end
      | Let { var; kind; free_names_of_defining_expr; free_names_of_body;
              defining_expr; body; _ } ->
        bound_name_of_kind (Name.var var) kind;
        if all_used_names
            || ignore_uses_as_callee <> None
            || ignore_uses_as_argument <> None
            || ignore_uses_as_continuation_argument <> None
            || ignore_uses_in_project_var <> None
            || ignore_uses_in_apply_cont <> None
        then begin
          (* In these cases we can't benefit from the pre-computed free
             name sets. *)
          free_names_promoted_to_kind
            (Named.name_usage ?ignore_uses_in_project_var defining_expr)
            kind;
          aux body
        end else begin
          free_names_promoted_to_kind free_names_of_defining_expr kind;
          free_names free_names_of_body
        end
      | Apply_cont (_, _, args) ->
        (* CR mshinwell: why two names? *)
        begin match ignore_uses_in_apply_cont with
        | Some () -> ()
        | None ->
          match ignore_uses_as_continuation_argument with
          | None -> free_names_in_term (Simple.List.free_names args)
          | Some () -> ()
        end
      | Let_cont { handlers; body; } ->
        aux body;
        begin match handlers with
        | Non_recursive { name = _; handler = { Continuation_handler.
            params; handler; _ }; } ->
          free_names (Flambda_type.Parameters.free_names params);
          aux handler
        | Recursive handlers ->
          Continuation.Map.iter (fun _name { Continuation_handler.
            params; handler; _ } ->
              free_names (Flambda_type.Parameters.free_names params);
              aux handler)
            handlers
        end
      | Switch (var, _) -> free_name_in_term var
      | Invalid _ -> ()
    in
    aux tree;
    if all_used_names then !free
    else Name_occurrences.diff !free !bound

  let free_names ?ignore_uses_as_callee ?ignore_uses_as_argument
      ?ignore_uses_as_continuation_argument ?ignore_uses_in_project_var
      ?ignore_uses_in_apply_cont t =
    name_usage ?ignore_uses_as_callee ?ignore_uses_as_argument
      ?ignore_uses_as_continuation_argument ?ignore_uses_in_project_var
      ?ignore_uses_in_apply_cont ~all_used_names:false t

  let free_names t : Name_occurrences.t = free_names t

  let used_names ?ignore_uses_as_callee ?ignore_uses_as_argument
      ?ignore_uses_as_continuation_argument ?ignore_uses_in_project_var t =
    name_usage ?ignore_uses_as_callee ?ignore_uses_as_argument
      ?ignore_uses_as_continuation_argument ?ignore_uses_in_project_var
      ~all_used_names:true t

  let invalid () =
    if !Clflags.treat_invalid_code_as_unreachable then
      Invalid Treat_as_unreachable
    else
      Invalid Halt_and_catch_fire

  type switch_creation_result =
    | Have_not_deleted_branch
    | Have_deleted_branch

  let create_switch ~scrutinee ~arms =
    if Discriminant.Map.cardinal arms < 1 then
      invalid (), Have_deleted_branch
    else
      match Discriminant.Map.get_singleton arms with
      | None ->
        Switch.invariant arms;
        Switch arms, Have_not_deleted_branch
      | Some (_discriminant, k) ->
        Apply_cont (k, None, []), Have_deleted_branch

  let rec free_continuations (t : t) =
    match t with
    | Let { body; _ } ->
      (* No continuations occur in a [Named.t] except inside closures---and
         closures do not have free continuations.  As such we don't need
         to traverse the defining expression of the let. *)
      free_continuations body
    | Let_cont { body; handlers; } ->
      let free_and_bound =
        Let_cont_handlers.free_and_bound_continuations handlers
      in
      Continuation.Set.union free_and_bound.free
        (Continuation.Set.diff (free_continuations body)
          free_and_bound.bound)
    | Apply_cont (cont, trap_action, _args) ->
      let trap_action =
        match trap_action with
        | Some (Push { exn_handler; _ })
        | Some (Pop { exn_handler; _ }) ->
          Continuation.Set.singleton exn_handler
        | None -> Continuation.Set.empty
      in
      Continuation.Set.add cont trap_action
    | Apply {
        func = _;
        continuation;
        exn_continuation;
        args = _;
        call_kind = _;
        dbg = _;
        inline = _;
        specialise = _;
      } ->
      Continuation.Set.of_list [continuation; exn_continuation]
    | Switch (_scrutinee, switch) ->
      Continuation.Set.of_list (Discriminant.Map.data switch)
    | Invalid _ -> Continuation.Set.empty

  let create_let var kind defining_expr body : t =
    begin match !Clflags.dump_flambda_let with
    | None -> ()
    | Some stamp ->
      Variable.debug_when_stamp_matches var ~stamp ~f:(fun () ->
        Printf.eprintf "Creation of [Let] with stamp %d:\n%s\n%!"
          stamp
          (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int)))
    end;
    let free_names_of_defining_expr = Named.free_names defining_expr in
    Let {
      var;
      kind;
      defining_expr;
      body;
      free_names_of_defining_expr;
      free_names_of_body = free_names body;
    }

  let iter_lets t ~for_defining_expr ~for_last_body ~for_each_let =
    let rec loop (t : t) =
      match t with
      | Let { var; kind; defining_expr; body; _ } ->
        for_each_let t;
        for_defining_expr var kind defining_expr;
        loop body
      | t ->
        for_last_body t
    in
    loop t

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
        List.fold_left (fun (t : t) (var, kind, defining_expr, original) : t ->
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

  let iter_general ~toplevel f f_named maybe_named =
    let rec aux (t : t) =
      match t with
      | Let _ ->
        iter_lets t
          ~for_defining_expr:(fun _var _kind named -> aux_named named)
          ~for_last_body:aux
          ~for_each_let:f
      (* CR mshinwell: add tail recursive case for Let_cont *)
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
      | Set_of_closures { function_decls = funcs; _; } ->
        if not toplevel then begin
          Closure_id.Map.iter (fun _ (decl : Function_declaration.t) ->
              aux decl.body)
            funcs.funs
        end
    in
    match maybe_named with
    | Is_expr expr -> aux expr
    | Is_named named -> aux_named named

  let apply_name_permutation t perm =
    match t with
    | Let let_expr ->
      let let_expr' = Let.apply_name_permutation let_expr perm in
      if let_expr == let_expr' then t
      else Let let_expr'
    | Let_cont let_cont ->
      let let_cont = Let_cont.apply_name_permutation let_cont perm in
       if let_cont == let_cont' then t
      else Let_cont let_cont'
    | Apply apply ->
      let apply' = Apply.apply_name_permutation apply perm in
      if apply == apply' then t
      else Apply apply'
    | Apply_cont (k, trap_action_opt, args) ->
      let args' = Name_permutation.apply_simples perm args in
      if args == args' then t
      else Apply_cont (k, trap_action_opt, args')
    | Switch (scrutinee, switch) ->
      let scrutinee' = Name_permutation.apply_name perm scrutinee in
      if scrutinee == scrutinee' then t
      else Switch (scrutinee', switch)
    | Invalid _ -> t

  let if_then_else ~scrutinee ~if_true ~if_false =
    let arms =
      Discriminant.Map.of_list [
        Discriminant.bool_true, if_true;
        Discriminant.bool_false, if_false;
      ]
    in
    create_switch ~scrutinee ~arms

  let rec no_effects_or_coeffects (t : t) =
    match t with
    | Let { defining_expr; body; _ } ->
      Named.no_effects_or_coeffects defining_expr
        && no_effects_or_coeffects body
    | Let_cont { body; handlers; } ->
      no_effects_or_coeffects body
        && Let_cont_handlers.no_effects_or_coeffects handlers
    | Apply_cont _
    | Switch _ -> true
    | Apply _
    | Invalid _ -> false

  let description_of_toplevel_node (expr : Expr.t) =
    match expr with
    | Let { var; _ } -> Format.asprintf "let %a" Variable.print var
    | Let_cont  _ -> "let_cont"
    | Apply _ -> "apply"
    | Apply_cont  _ -> "apply_cont"
    | Switch _ -> "switch"
    | Invalid _ -> "invalid"

  let bind ~bindings ~body =
    List.fold_left (fun expr (var, kind, var_def) ->
        Expr.create_let var kind var_def expr)
      body bindings

  type with_wrapper =
    | Unchanged of { handler : Continuation_handler.t; }
    | With_wrapper of {
        new_cont : Continuation.t;
        new_handler : Continuation_handler.t;
        wrapper_handler : Continuation_handler.t;
      }

  let build_let_cont_with_wrappers ~body ~(recursive : F0.recursive)
        ~with_wrappers : Expr.t =
    match recursive with
    | Non_recursive ->
      begin match Continuation.Map.bindings with_wrappers with
      | [cont, Unchanged { handler; }] ->
        Let_cont {
          body;
          handlers = Non_recursive { name = cont; handler; };
        }
      | [cont, With_wrapper { new_cont; new_handler; wrapper_handler; }] ->
        Let_cont {
          body = Let_cont {
            body;
            handlers = Non_recursive {
              name = cont;
              handler = wrapper_handler;
            };
          };
          handlers = Non_recursive {
            name = new_cont;
            handler = new_handler;
          };
        }
      | _ -> assert false
      end
    | Recursive ->
      let handlers =
        Continuation.Map.fold
          (fun cont (with_wrapper : with_wrapper) handlers ->
            match with_wrapper with
            | Unchanged { handler; } ->
              Continuation.Map.add cont handler handlers
            | With_wrapper { new_cont; new_handler; wrapper_handler; } ->
              Continuation.Map.add new_cont new_handler
                (Continuation.Map.add cont wrapper_handler handlers))
          with_wrappers
          Continuation.Map.empty
      in
      Let_cont {
        body;
        handlers = Recursive handlers;
      }

  module Iterators = struct
    let iter_lets = F0.Expr.iter_lets

    let iter f f_named t = iter_general ~toplevel:false f f_named (Is_expr t)

    let iter_expr f t = iter f (fun _ -> ()) t

    let iter_named f_named t = iter (fun (_ : t) -> ()) f_named t

    let iter_subexpressions f f_named (t : t) =
      match t with
      | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> ()
      | Let { defining_expr; body; _ } ->
        f_named defining_expr;
        f body
      | Let_cont { body; handlers =
          Non_recursive { handler = { handler; _ }; _ } } ->
        f body;
        f handler
      | Let_cont { body; handlers = Recursive handlers; } ->
        f body;
        Continuation.Map.iter
          (fun _cont ({ handler; _ } : Continuation_handler.t) -> f handler)
          handlers

    (* CR-soon mshinwell: Remove "let_rec" from this name (ditto for the
       toplevel-only variant) *)
    let iter_all_immutable_let_and_let_rec_bindings t ~f =
      iter_expr (function
          | Let { var; defining_expr; _ } -> f var defining_expr
          | _ -> ())
        t

    let iter_sets_of_closures f t =
      iter_named (function
          | Set_of_closures clos -> f clos
          | Simple _ | Prim _ -> ())
        t

    let iter_function_bodies t ~f =
      iter_sets_of_closures (fun (set : Set_of_closures.t) ->
          Set_of_closures.Iterators.iter_function_bodies set ~f)
        t

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

  module Mappers = struct
    let map_lets = F0.Expr.map_lets

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
            (* CR-soon mshinwell: There's too much code duplication here with
               [map_subexpressions]. *)
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

    let map_subexpressions f f_named (tree : t) : t =
      match tree with
      | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> tree
      | Let { var; kind; defining_expr; body; _ } ->
        let new_named = f_named var defining_expr in
        let new_body = f body in
        if new_named == defining_expr && new_body == body then
          tree
        else
          create_let var kind new_named new_body
      | Let_cont { body; handlers; } ->
        let new_body = f body in
        match handlers with
        | Non_recursive { name; handler =
            ({ handler = handler_expr; _ } as handler); } ->
          let new_handler_expr = f handler_expr in
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
                let new_handler = f handler.handler in
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

    let map_symbols tree ~f =
      map_named (function
          | (Simple simple) as named ->
            let new_simple = Simple.map_symbol simple ~f in
            if new_simple == simple then
              named
            else
              Simple new_simple
          | (Set_of_closures _ | Prim _) as named -> named)
        tree

    let map_apply tree ~f =
      map (function
          | (Apply apply) as expr ->
            let new_apply = f apply in
            if new_apply == apply then
              expr
            else
              Apply new_apply
          | expr -> expr)
        (fun named -> named)
        tree

    let map_sets_of_closures tree ~f =
      map_named (function
          | (Set_of_closures set_of_closures) as named ->
            let new_set_of_closures = f set_of_closures in
            if new_set_of_closures == set_of_closures then named
            else Set_of_closures new_set_of_closures
          | (Simple _ | Prim _) as named -> named)
        tree

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
              | Invalid invalid_term_semantics ->
                let rev_lets = (List.rev bindings) @ rev_lets in
                let body : Expr.t = Invalid invalid_term_semantics in
                let last_body, acc = for_last_body acc body in
                finish ~last_body ~acc ~rev_lets
              end)
        | t ->
          let last_body, acc = for_last_body acc t in
          finish ~last_body ~acc ~rev_lets
      in
      loop t ~acc:init ~rev_lets:[]
  end

  let all_defined_continuations_toplevel expr =
    let defined_continuations = ref Continuation.Set.empty in
    Iterators.Toplevel_only.iter (fun (expr : t) ->
        match expr with
        | Let_cont { handlers; _ } ->
          let conts = Let_cont_handlers.bound_continuations handlers in
          defined_continuations :=
            Continuation.Set.union conts
              !defined_continuations
        | _ -> ())
      (fun _named -> ())
      expr;
    !defined_continuations

  let count_continuation_uses_toplevel (expr : t) =
    let counts = Continuation.Tbl.create 42 in
    let use cont =
      match Continuation.Tbl.find counts cont with
      | exception Not_found -> Continuation.Tbl.add counts cont 1
      | count -> Continuation.Tbl.replace counts cont (count + 1)
    in
    Iterators.Toplevel_only.iter (fun (expr : t) ->
        match expr with
        | Apply { continuation; _ } -> use continuation
        | Apply_cont (cont, None, _) -> use cont
        | Apply_cont (cont, Some (Push { exn_handler; _ }), _)
        | Apply_cont (cont, Some (Pop { exn_handler; _ }), _) ->
          use cont;
          use exn_handler
        | Switch (_, switch) ->
          Switch.iter switch ~f:(fun _value cont -> use cont)
        | Let _ | Let_cont _ | Invalid _ -> ())
      (fun _named -> ())
      expr;
    Continuation.Tbl.to_map counts
end Expr_with_permutation : sig
  include Contains_names.S
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
          expr_with_perm_and_free_names;
        } perm' =
    let perm = Name_permutation.compose perm perm' in
    { expr_without_perm;
      perm;
      expr_with_perm_and_free_names =
        lazy (
          let expr = Expr.apply_name_permutation expr perm in
          let free_names = Expr.free_names expr in
          expr, free_names);
    }
end and Named : sig
  type t =
    | Simple of Simple.t
    | Prim of Flambda_primitive.t * Debuginfo.t
    | Set_of_closures of Set_of_closures.t

  val free_names
     : ?ignore_uses_in_project_var:unit
    -> t
    -> Name_occurrences.t
  val free_symbols : t -> Symbol.Set.t
  val free_symbols_helper : Symbol.Set.t ref -> t -> unit
  val used_names
     : ?ignore_uses_in_project_var:unit
    -> t
    -> Name_occurrences.t
  val name_usage
     : ?ignore_uses_in_project_var:unit
    -> t
    -> Name_occurrences.t
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
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
end = struct
  include Named

  let print_with_cache ~cache ppf (t : t) =
    match t with
    | Simple simple ->
      Format.fprintf ppf "%s%a%s"
        (Misc_color.bold_green ())
        Simple.print simple
        (Misc_color.reset ())
    | Set_of_closures set_of_closures ->
      Set_of_closures.print_with_cache ~cache ppf set_of_closures
    | Prim (prim, dbg) ->
      fprintf ppf "@[<2>(%a%a)@]"
        Flambda_primitive.print prim
        Debuginfo.print_or_elide dbg

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names t =
    match t with
    | Simple simple ->
      Name_occurrences.create_from_set_in_terms (Simple.free_names simple)
    | _ ->
      (* CR mshinwell: remove mutable state *)
      let free = ref (Name_occurrences.create ()) in
      let free_names names =
        free := Name_occurrences.union !free names
      in
      let free_names_in_term names =
        let new_free = Name_occurrences.add_set !free names In_terms in
        free := new_free
      in
      begin match t with
      | Simple simple ->
        free_names_in_term (Simple.free_names simple)
      | Set_of_closures set ->
        free_names (Set_of_closures.free_names set)
      (* CR mshinwell: Move some of this to flambda_primitive.ml *)
      | Prim (Unary (_prim, x0), _dbg) ->
        free_names_in_term (Simple.free_names x0)
      | Prim (Binary (_prim, x0, x1), _dbg) ->
        free_names_in_term (Simple.free_names x0);
        free_names_in_term (Simple.free_names x1)
      | Prim (Ternary (_prim, x0, x1, x2), _dbg) ->
        free_names_in_term (Simple.free_names x0);
        free_names_in_term (Simple.free_names x1);
        free_names_in_term (Simple.free_names x2)
      | Prim (Variadic (_prim, xs), _dbg) ->
        List.iter (fun x -> free_names_in_term (Simple.free_names x)) xs
      end;
      !free

  let apply_name_permutation t perm =
    match t with
    | Simple simple ->
      let simple' = Name_permutation.apply_simple perm simple in
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

  let primitive_invariant env (t : Flambda_primitive.t) =
    (* CR mshinwell: This cannot go in [Flambda_primitive] due to a
       circularity.  However a refactored version with some callbacks
       probably could, and that's probably a good change. *)
    let module E = Invariant_env in
    let module P = Flambda_primitive in
    match t with
    | Unary (prim, x0) ->
      let kind0 = P.arg_kind_of_unary_primitive prim in
      E.check_simple_is_bound_and_of_kind env x0 kind0;
      begin match prim, x0 with
      | Project_closure closure_id, set_of_closures ->
        E.check_simple_is_bound_and_of_kind env set_of_closures
          (K.fabricated ());
        E.add_use_of_closure_id env closure_id
      | Move_within_set_of_closures { move_from; move_to; }, closure ->
        E.check_simple_is_bound_and_of_kind env closure
          (K.value ());
        E.add_use_of_closure_id env move_from;
        E.add_use_of_closure_id env move_to
      | Project_var (closure_id, var), closure ->
        E.add_use_of_closure_id env closure_id;
        E.add_use_of_var_within_closure env var;
        E.check_simple_is_bound_and_of_kind env closure
          (K.value ())
      | Duplicate_block _, _
      | Is_int, _
      | Get_tag _, _
      | Discriminant_of_int, _
      | Array_length _, _
      | Bigarray_length _, _
      | String_length _, _
      | Int_as_pointer, _
      | Opaque_identity, _
      | Int_arith _, _
      | Float_arith _, _
      | Num_conv _, _
      | Boolean_not, _
      | Unbox_number _, _
      | Box_number _, _ -> ()  (* None of these contain names. *)
      end
    | Binary (prim, x0, x1) ->
      let kind0, kind1 = P.args_kind_of_binary_primitive prim in
      E.check_simple_is_bound_and_of_kind env x0 kind0;
      E.check_simple_is_bound_and_of_kind env x1 kind1;
      begin match prim with
      (* None of these currently contain names: this is here so that we
         are reminded to check upon adding a new primitive. *)
      | Block_load _
      | String_or_bigstring_load _
      | Phys_equal _
      | Int_arith _
      | Int_shift _
      | Int_comp _
      | Float_arith _
      | Float_comp _ -> ()
      end
    | Ternary (prim, x0, x1, x2) ->
      let kind0, kind1, kind2 = P.args_kind_of_ternary_primitive prim in
      E.check_simple_is_bound_and_of_kind env x0 kind0;
      E.check_simple_is_bound_and_of_kind env x1 kind1;
      E.check_simple_is_bound_and_of_kind env x2 kind2;
      begin match prim with
      | Block_set _
      | Bytes_or_bigstring_set _ -> ()
      end
    | Variadic (prim, xs) ->
      let kinds =
        match P.args_kind_of_variadic_primitive prim with
        | Variadic kinds -> kinds
        | Variadic_all_of_kind kind ->
          List.init (List.length xs) (fun _index -> kind)
      in
      List.iter2 (fun var kind ->
          E.check_simple_is_bound_and_of_kind env var kind)
        xs kinds;
      begin match prim with
      | Make_block _
      | Bigarray_set _
      | Bigarray_load _ -> ()
      end

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
        primitive_invariant env prim;
        ignore (dbg : Debuginfo.t);
        Flambda_primitive.result_kind prim
    with Misc.Fatal_error ->
      Misc.fatal_errorf "(during invariant checks) Context is:@ %a" print t
end and Let0 : sig
  include Contains_names.S
  val create : Flambda_kind.t -> Named.t -> Expr.t -> t
  val kind : t -> Flambda_kind.t
  val defining_expr : t -> Named.t
  val body : t -> Expr.t
  val free_names_of_body : t -> Name_occurrences.t
(*
  val map_defining_expr : Let.t -> f:(Named.t -> Named.t) -> Expr.t
*)
end = struct
  type t = {
    kind : Flambda_kind.t;
    defining_expr : Named.t;
    body : Expr_with_permutation.t;
  }

  let create kind defining_expr body =
    { kind;
      defining_expr;
      body = Expr_with_permutation.create body;
    }

  let kind t = t.kind
  let defining_expr t = t.defining_expr

  let body t = Expr_with_permutation.expr t.body
  let free_names_of_body t = Expr_with_permutation.free_names t.body

  let free_names { kind = _; defining_expr; body = _; } =
    let from_defining_expr = Named.free_names defining_expr in
    let from_body = free_names_of_body t in
    Name_occurrences.union from_defining_expr from_body

  let apply_name_permutation { kind; defining_expr; body; } perm =
    { kind;
      defining_expr = Named.apply_name_permutation defining_expr perm;
      body = Expr_with_permutation.apply_name_permutation body perm;
    }

(*
  let map_defining_expr { kind; defining_expr; body_with_free_names; } ~f =
    let defining_expr' = f defining_expr in
    if defining_expr == defining_expr' then
      t
    else
      Let {
        kind;
        defining_expr = defining_expr';
        body_with_free_names;
      }
*)
end and Let : sig
  include module type of struct
    include Name_abstraction.Make (Bound_variable) (Let0).t
  end

  val create
     : bound_var:Variable.t
    -> kind:Flambda_kind.t
    -> defining_expr:Named.t
    -> body:Expr.t
    -> t

  val invariant : Invariant_env.t -> t -> unit

  val map_defining_expr : t -> f:(Named.t -> Named.t) -> Expr.t
end = struct
  include Name_abstraction.Make (Bound_variable) (Let0)

  let create ~bound_var ~kind ~defining_expr ~body =
    let let0 : Let0.t =
      { kind;
        defining_expr;
        body;
        free_names_of_body = Expr.free_names body;
      }
    in
    create bound_var let0

  let invariant env t =
    let module E = Invariant_env in
    Name_abstraction.pattern_match t ~f:(fun bound_var let0 ->
      let kind = Let0.kind let0 in
      let defining_expr = Let0.defining_expr let0 in
      let named_kind =
        match Named.invariant env defining_expr with
        | Singleton kind -> Some kind
        | Unit -> Some (K.value ())
        | Never_returns -> None
      in
      begin match named_kind with
      | None -> ()
      | Some named_kind ->
        if not (K.equal named_kind kind) then begin
          Misc.fatal_errorf "[Let] expression inferred kind (%a)@ is not \
              equal to the annotated kind (%a);@ [Let] expression is:@ %a"
            K.print named_kind
            K.print kind
            print t
        end
      end;
      let env = E.add_variable env bound_var kind in
      loop env (Let0.body let0))
end and Let_cont : sig
  type t = private
    | Non_recursive of Non_recursive_let_cont_handler.t
    | Recursive of Recursive_let_cont_handlers.t
  include Contains_names.S with type t := t
  val create_non_recursive
     : Continuation.t
    -> handler:Continuation_handler.t
    -> body:Expr.t
    -> t
  val create_exception_handler
     : Continuation.t
    -> handler:Continuation_handler.t
    -> body:Expr.t
    -> t
  val create_recursive
     : handlers:Continuation_handlers.t
    -> body:Expr.t
    -> t
  val bound_continuations : t -> Continuation.Set.t
  val free_continuations : t -> Continuation.Set.t
  type free_and_bound = {
    free : Continuation.Set.t;
    bound : Continuation.Set.t;
  }
  val free_and_bound_continuations : t -> free_and_bound
  val to_continuation_map : t -> Continuation_handlers.t
  val map : t -> f:(Continuation_handlers.t -> Continuation_handlers.t) -> t
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val print_using_where_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit
end = struct
  type t =
    | Non_recursive of Non_recursive_let_cont_handler.t
    | Recursive of Recursive_let_cont_handlers.t

  let invariant env t =
    let module E = Invariant_env in
    let handler_stack = E.Continuation_stack.var () in
    let env =
      match handlers with
      | Non_recursive { name; handler; } ->
        let kind : E.continuation_kind =
          if handler.is_exn_handler then Exn_handler else Normal
        in
        let params = handler.params in
        let arity = Flambda_type.Parameters.arity params in
        let env = add_parameters env params in
        let env = E.set_current_continuation_stack env handler_stack in
        loop env handler.handler;
        E.add_continuation env name arity kind handler_stack
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
            let env = E.set_current_continuation_stack env handler_stack in
            loop env handler;
            ignore (stub : bool))
          handlers;
        recursive_env
    in
    loop env body

  let to_continuation_map t =
    match t with
    | Non_recursive { name; handler } -> Continuation.Map.singleton name handler
    | Recursive handlers -> handlers

  let free_and_bound_continuations (t : t) : free_and_bound =
    match t with
    | Non_recursive { name; handler = { handler; _ }; } ->
      let fcs = Expr.free_continuations handler in
      if Continuation.Set.mem name fcs then begin
        Misc.fatal_errorf "Non_recursive [Let_cont] handler appears to be \
            recursive:@ \n%a"
          print t
      end;
      { free = fcs;
        bound = Continuation.Set.singleton name;
      }
    | Recursive handlers ->
      let bound_conts = Continuation.Map.keys handlers in
      let fcs =
        Continuation.Map.fold (fun _name
              { Continuation_handler. handler; _ } fcs ->
            Continuation.Set.union fcs
              (Continuation.Set.diff (Expr.free_continuations handler)
                bound_conts))
          handlers
          Continuation.Set.empty
      in
      { free = fcs;
        bound = bound_conts;
      }

  let free_continuations t = (free_and_bound_continuations t).free
  let bound_continuations t = (free_and_bound_continuations t).bound

  (* CR mshinwell: This code should be shared with the calculation of
     free names in [Expr], above.  For the moment there is a hack here. *)
  let free_names (t : t) =
    Expr.free_names (Let_cont {
      body = Invalid Halt_and_catch_fire;
      handlers = t;
    })

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

  let print_using_where_with_cache ~cache ppf (t : t) =
    match t with
    | Non_recursive {
        name;
        handler = { params; stub; handler; is_exn_handler; };
      } ->
      fprintf ppf "@[<v 2>%swhere%s %a%s%s@ @[%a@] =@ %a@]"
        (Misc_color.bold_cyan ())
        (Misc_color.reset ())
        Continuation.print name
        (if stub then " *stub*" else "")
        (if is_exn_handler then "*exn* " else "")
        (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
        (Expr.print_with_cache ~cache) handler
    | Recursive handlers ->
      let first = ref true in
      fprintf ppf "@[<v 2>%swhere%s rec "
        (Misc_color.bold_cyan ())
        (Misc_color.reset ());
      Continuation.Map.iter (fun name
              { Continuation_handler. params; stub; is_exn_handler;
                handler; } ->
          if not !first then fprintf ppf "@ ";
          fprintf ppf "@[%s%a%s%s@[%a@]@] =@ %a"
            (if !first then "" else "and ")
            Continuation.print name
            (if stub then " *stub*" else "")
            (if is_exn_handler then "*exn* " else "")
            (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
            (Expr.print_with_cache ~cache) handler;
          first := false)
        handlers;
      fprintf ppf "@]"

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

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t
end and Non_recursive_let_cont_handler0 : sig
  include Contains_names.S
  val create
     : handler:Continuation_handler.t
    -> body:Expr.t
    -> t
  val handler : t -> Continuation_handler.t
  val body : t -> Expr.t
end = struct
  type t = {
    handler : Continuation_handler.t;
    body : Expr_with_permutation.t;
  }

  let free_names t =
    Name_occurrences.union (Continuation_handler.free_names handler)
      (Expr_with_permutation.free_names body)

  let apply_name_permutation { handler; body; } perm =
    let handler' =
      Continuation_handlers.apply_name_permutation handler perm
    in
    let body' =
      Expr_with_permutation.apply_name_permutation body perm
    in
    { handler = handler';
      body = body';
    }
end and Non_recursive_let_cont_handler : sig
  include module type of struct
    include Name_abstraction.Make (Bound_continuation)
      (Non_recursive_let_cont_handler0)
  end
end = struct
  include Name_abstraction.Make (Bound_continuation)
    (Non_recursive_let_cont_handler0)
end and Recursive_let_cont_handlers0 : sig
  include Contains_names.S
  val create
     : handlers:Continuation_handlers.t
    -> body:Expr.t
    -> t
  val handlers : t -> Continuation_handler.t Continuation.Map.t
  val body : t -> Expr.t
end = struct
  type t = {
    handlers : Continuation_handlers.t;
    body : Expr_with_permutation.t;
  }

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
  include module type of struct
    include Name_abstraction.Make (Bound_continuations)
      (Recursive_let_cont_handlers0)
  end
end = struct
  include Name_abstraction.Make (Bound_continuations)
    (Recursive_let_cont_handlers0)
end and Continuation_handlers : sig
  type t = Continuation_handler.t Continuation.Map.t

  val no_effects_or_coeffects : t -> bool
end = struct
  include Continuation_handlers

  let no_effects_or_coeffects t =
    Continuation.Map.for_all (fun _cont handler ->
        Continuation_handler.no_effects_or_coeffects handler)
      t
end and Continuation_handler0 : sig
  include Contains_names.S
  val print : Format.formatter -> t -> unit
  val params : t -> Flambda_type.Parameters.t
  val stub : t -> bool
  val is_exn_handler : t -> bool
  val handler : t -> Expr.t
end = struct
  type t = {
    params : Flambda_type.Parameters.t;
    stub : bool;
    is_exn_handler : bool;
    handler : Expr_with_permutation.t;
  }

  let print_with_cache ~cache ppf { params; stub; handler; is_exn_handler; } =
    fprintf ppf "%s%s%a@ =@ %a"
      (if stub then "*stub* " else "")
      (if is_exn_handler then "*exn* " else "")
      (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
      Expr.print handler

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let params t = t.params
  let stub t = t.stub
  let is_exn_handler t = t.is_exn_handler
  let handler t = Expr_with_permutation.expr t.handler

  let free_names { params; stub = _; is_exn_handler = _; handler; } =
    Name_occurrences.union (Flambda_type.Parameters.free_names params)
      (Expr_with_permutation.free_names handler)

  let apply_name_permutation
        ({ params; stub = _; is_exn_handler = _; handler; } as t)
        perm =
    let params' =
      Flambda_type.Parameters.apply_name_permutation params perm
    in
    let handler' = Expr_with_permutation.apply_name_permutation handler perm in
    if params == params' && handler == handler' then t
    else
      { params = params';
        stub;
        is_exn_handler;
        handler = handler';
      }
end and Continuation_handler :
  module type of struct
    include Name_abstraction.Make (Bound_continuations)
      (Continuation_handler0)
  end
end = struct
  include Name_abstraction.Make (Bound_continuations) (Continuation_handler0)
end and Set_of_closures : sig
  type t = {
    function_decls : Function_declarations.t;
    closure_elements : Simple.t Var_within_closure.Map.t;
    direct_call_surrogates : Closure_id.t Closure_id.Map.t;
  }
  include Contains_names.S with type t := t
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val create
     : function_decls:Function_declarations.t
    -> closure_elements:Simple.t Var_within_closure.Map.t
    -> direct_call_surrogates:Closure_id.t Closure_id.Map.t
    -> t
  val function_decls : t -> Function_declarations.t
  val closure_elements : t -> Simple.t Var_within_closure.Map.t
  val direct_call_surrogates : Closure_id.t Closure_id.Map.t
  val has_empty_environment : t -> bool
end = struct
  include Set_of_closures

  let create ~function_decls ~closure_elements ~direct_call_surrogates =
    { function_decls;
      closure_elements;
      direct_call_surrogates;
    }

  let function_decls t = t.function_decls
  let closure_elements t = t.closure_elements
  let direct_call_surrogates t = t.direct_call_surrogates

  let has_empty_environment t =
    Var_within_closure.Map.is_empty t.closure_elements

  let print_with_cache ~cache ppf
        { function_decls; 
          closure_elements;
          direct_call_surrogates;
        } =
      fprintf ppf "@[<hov 1>(%sset_of_closures%s@ \
          %a@ \
          @[<hov 1>(closure_elements (%a))@]@ \
          @[<hov 1>(direct_call_surrogates %a)@]@ \
          @[<hov 1>(set_of_closures_origin %a)@]\
          )@]"
        (Misc_color.bold_green ())
        (Misc_color.reset ())
        (Function_declarations.print_with_cache ~cache) function_decls
        (Var_within_closure.Map.print Simple.print) closure_elements
        (Closure_id.Map.print Closure_id.print) direct_call_surrogates
        Set_of_closures_origin.print function_decls.set_of_closures_origin

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names
        { function_decls; 
          closure_elements;
          direct_call_surrogates = _;
        } =
    let in_decls = Function_declarations.free_names function_decls in
    let in_closure_elements =
      Name_occurrences.create_from_set_in_terms
        (Simple.List.free_names
          (Var_within_closure.Map.data closure_elements))
    in
    Name_occurrences.union in_decls in_closure_elements

  let apply_name_permutation
        ({ function_decls; 
           closure_elements;
           direct_call_surrogates;
         } as t) perm =
    let function_decls' =
      Function_declarations.apply_name_permutation function_decls perm
    in
    let closure_elements' =
      Var_within_closure.Map.map_sharing (fun simple ->
          Name_permutation.apply_simple perm simple)
        closure_elements
    in
    if function_decls == function_decls'
      && closure_elements == closure_elements'
    then t
    else
      { function_decls = function_decls';
        closure_elements = closure_elements';
        direct_call_surrogates;
      }

  let variables_bound_by_the_closure t =
    Var_within_closure.Map.keys t.free_vars

  let find_free_variable cv ({ free_vars; _ } : t) =
    let free_var : Free_var.t =
      Var_within_closure.Map.find cv free_vars
    in
    free_var.var

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

  let invariant env
        { function_decls; free_vars; direct_call_surrogates = _; } =
    (* CR mshinwell: Some of this should move into
       [Function_declarations.invariant] *)
    let module E = Invariant_env in
    (* CR-soon mshinwell: check [direct_call_surrogates] *)
    let { Function_declarations. set_of_closures_id;
          set_of_closures_origin; funs; } =
      function_decls
    in
    E.add_set_of_closures_id env set_of_closures_id;
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
end and Function_declarations : sig
  include Contains_names.S
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val create : funs:Function_declaration.t Closure_id.Map.t -> t
  val set_of_closures_origin : Set_of_closures_origin.t
  val funs : t -> Function_declaration.t Closure_id.Map.t
  val find : Closure_id.t -> t -> Function_declaration.t
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

  let create ~funs =
    let compilation_unit = Compilation_unit.get_current_exn () in
    let set_of_closures_origin =
      Set_of_closures_origin.create compilation_unit
    in
    { set_of_closures_origin;
      funs;
    }

  let set_of_closures_origin t = t.set_of_closures_origin
  let funs t = t.funs

  let find cf ({ funs; set_of_closures_origin = _ } : t) =
    Closure_id.Map.find cf funs

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
      t.funs
      (Name_occurrences.create ())

  let apply_name_permutation ({ set_of_closures_origin; funs; } as t) perm =
    let funs' =
      Closure_id.Map.map_sharing (fun func_decl ->
          Function_declaration.apply_name_permutation func_decl perm)
        funs
    in
    if funs == funs' then t
    else { set_of_closures_origin; funs = funs'; }
end and Function_declaration : sig
  include Contains_names.S
  val create
     : closure_origin:Closure_origin.t
    -> continuation_param:Continuation.t
    -> exn_continuation_param:Continuation.t
    -> params:Flambda_type.Parameters.t
    -> body:Expr.t
    -> result_arity:Flambda_arity.t
    -> stub:bool
    -> dbg:Debuginfo.t
    -> inline:Inline_attribute.t
    -> specialise:Specialise_attribute.t
    -> is_a_functor:bool
    -> my_closure:Variable.t
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
  val params : t -> Flambda_type.Parameters.t
  val body : t -> Expr.t
  val code_id : t -> Code_id.t
  val free_names_in_body : t -> Name_occurrences.t
  val result_arity : t -> Flambda_arity.t
  val stub : t -> bool
  val dbg : t -> Debuginfo.t
  val inline : t -> Inline_attribute.t
  val specialise : t -> Specialise_attribute.t
  val is_a_functor : t -> bool
  val my_closure : t -> Variable.t
  val update_body : t -> body:Expr.t -> t
  val update_params : t -> params:Flambda_type.Parameters.t -> t
  val update_params_and_body
     : t
    -> params:Flambda_type.Parameters.t
    -> body:Expr.t
    -> t
end = struct
  type t = {
    closure_origin : Closure_origin.t;
    continuation_param : Continuation.t;
    exn_continuation_param : Continuation.t;
    params : Flambda_type.Parameters.t;
    body : Expr_with_permutation.t;
    (* Function declarations' bodies are completely closed with respect to
       names that might be permuted: as such, they have empty support.
       However we can still use [Expr_with_permutation] to manage the delayed
       calculation of free names in such bodies. *)
    code_id : Code_id.t;
    free_names_in_body : Name_occurrences.t;
    result_arity : Flambda_arity.t;
    stub : bool;
    dbg : Debuginfo.t;
    inline : Inline_attribute.t;
    specialise : Specialise_attribute.t;
    is_a_functor : bool;
    my_closure : Variable.t;
  }

  let create ~closure_origin ~continuation_param ~exn_continuation_param
        ~params ~body ~result_arity ~stub ~dbg
        ~(inline : Inline_attribute.t)
        ~(specialise : Specialise_attribute.t)
        ~is_a_functor ~my_closure : t =
    begin match stub, inline with
    | true, (Never_inline | Default_inline)
    | false, (Never_inline | Default_inline | Always_inline | Unroll _) -> ()
    | true, (Always_inline | Unroll _) ->
      Misc.fatal_errorf
        "Stubs may not be annotated as [Always_inline] or [Unroll]: %a"
        Expr.print body
    end;
    begin match stub, specialise with
    | true, (Never_specialise | Default_specialise)
    | false, (Never_specialise | Default_specialise | Always_specialise) -> ()
    | true, Always_specialise ->
      Misc.fatal_errorf
        "Stubs may not be annotated as [Always_specialise]: %a"
        Expr.print body
    end;
    { closure_origin;
      continuation_param;
      exn_continuation_param;
      params;
      body = Expr_with_permutation.create body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
      result_arity;
      stub;
      dbg;
      inline;
      specialise;
      is_a_functor;
      my_closure;
    }

  (* CR mshinwell: use record pattern match *)
  let print_with_cache ~cache closure_id ppf (f : t) =
    let stub =
      if f.stub then
        " *stub*"
      else
        ""
    in
    let is_a_functor =
      if f.is_a_functor then
        " *functor*"
      else
        ""
    in
    (* CR mshinwell: Use [Inline_attribute.print_...] etc. *)
    let inline =
      match f.inline with
      | Always_inline -> " *inline*"
      | Never_inline -> " *never_inline*"
      | Unroll _ -> " *unroll*"
      | Default_inline -> ""
    in
    let specialise =
      match f.specialise with
      | Always_specialise -> " *specialise*"
      | Never_specialise -> " *never_specialise*"
      | Default_specialise -> ""
    in
    fprintf ppf
      "@[<2>(%a%s%s%s%s@ (my_closure %a)@ (origin %a)@ =@ \
        %sfun%s@[<2> <%a> <exn %a>@] %a@ @[<2>@ :: %s%a%s@]@ ->@ @[<2>%a@])@]@ "
      Closure_id.print closure_id
      stub
      is_a_functor inline specialise
      Variable.print f.my_closure
      Closure_origin.print f.closure_origin
      (Misc_color.bold_cyan ())
      (Misc_color.reset ())
      Continuation.print f.continuation_param
      Continuation.print f.exn_continuation_param
      (Flambda_type.Parameters.print_or_omit_with_cache ~cache) f.params
      (Misc_color.bold_white ())
      Flambda_arity.print f.result_arity
      (Misc_color.reset ())
      (Expr.print_with_cache ~cache) f.body

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  (* CR mshinwell: use record pattern match *)
  let update_body t ~body : t =
    { closure_origin = t.closure_origin;
      params = t.params;
      continuation_param = t.continuation_param;
      exn_continuation_param = t.exn_continuation_param;
      body = Expr_with_permutation.create body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
      result_arity = t.result_arity;
      stub = t.stub;
      dbg = t.dbg;
      inline = t.inline;
      specialise = t.specialise;
      is_a_functor = t.is_a_functor;
      my_closure = t.my_closure
    }

  (* CR mshinwell: use record pattern match *)
  let update_params_and_body t ~params ~body : t =
    { closure_origin = t.closure_origin;
      params;
      continuation_param = t.continuation_param;
      exn_continuation_param = t.exn_continuation_param;
      body = Expr_with_permutation.create body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
      result_arity = t.result_arity;
      stub = t.stub;
      dbg = t.dbg;
      inline = t.inline;
      specialise = t.specialise;
      is_a_functor = t.is_a_functor;
      my_closure = t.my_closure;
      (* CR pchambart: Updating that field ([my_closure]) is probably needed
         also *)
    }

  let update_params t ~params =
    update_params_and_body t ~params ~body:t.body

  let free_names
        { closure_origin = _;
          continuation_param = _;
          exn_continuation_param = _;
          params;
          body = _;
          code_id = _;
          result_arity = _;
          stub = _;
          dbg = _;
          inline = _;
          specialise = _;
          is_a_functor = _;
          my_closure = _;
        } =
    Flambda_type.Parameters.free_names params

  let apply_name_permutation
        ({ closure_origin;
           continuation_param;
           exn_continuation_param;
           params;
           body;
           code_id;
           result_arity;
           stub;
           dbg;
           inline;
           specialise;
           is_a_functor;
           my_closure;
         } as t) perm =
    let params' = Flambda_type.Parameters.apply_name_permutation params perm in
    if params == params' then t
    else
      { closure_origin;
        continuation_param;
        exn_continuation_param;
        params = params';
        body;
        code_id;
        result_arity;
        stub;
        dbg;
        inline;
        specialise;
        is_a_functor;
        my_closure;
      }

  let find_declaration_variable _closure_id _t =
    (* CR mshinwell for pchambart: What should this do?  Return the
       [my_closure]? *)
    assert false  (* XXX *)

  let fun_vars_referenced_in_decls (_function_decls : t) ~backend:_ =
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

  let all_functions_parameters (function_decls : t) =
    Closure_id.Map.fold
      (fun _ ({ params; _ } : Function_declaration.t) set ->
        let params = T.Parameters.kinded_params params in
        Variable.Set.union set (Kinded_parameter.List.var_set params))
      function_decls.funs Variable.Set.empty

  let contains_stub (fun_decls : t) =
    let number_of_stub_functions =
      Closure_id.Map.cardinal
        (Closure_id.Map.filter
          (fun _ ({ stub; _ } : Function_declaration.t) -> stub)
          fun_decls.funs)
    in
    number_of_stub_functions > 0

(*
  let map_parameter_types t ~f =
    let funs =
      Closure_id.Map.map (fun (decl : Function_declaration.t) ->
          Function_declaration.map_parameter_types decl ~f)
        t.funs
    in
    update t ~funs
*)

  (* CR mshinwell: Tidy up, stop computing the [subst] that used to be
     returned *)
  let rename_names (func_decls : t) subst =
    let freshen_func_decl (func_decl : Function_declaration.t) subst =
(*
      let params_rev, subst =
        List.fold_left (fun (params_rev, subst) param ->
            let var = Typed_parameter.var param in
            let fresh_var, subst =
              Freshening.add_variable subst var
            in
            let param =
              Typed_parameter.map_var ~f:(fun _var -> fresh_var) param
            in
            param :: params_rev, subst)
          ([], subst)
          func_decl.params
      in
*)
      let params = T.Parameters.rename_variables func_decl.params subst in
      let results = T.Parameters.rename_variables func_decl.results subst in
      (* Since all parameters are distinct, even between functions, we can
         just use a single substitution. *)
      (* CR mshinwell: Why does this [toplevel_substitution] need to happen?
         Can't this subst be put into the environment and then applied
         as needed? *)
      let body =
        Expr.toplevel_substitution subst func_decl.body
      in
      let function_decl =
        Function_declaration.update_params_results_and_body func_decl
          ~params ~body ~results
      in
      function_decl, subst
    in
    let funs, _subst =
      Closure_id.Map.fold (fun closure_id func_decl (funs, subst) ->
          let func_decl, subst =
            freshen_func_decl func_decl subst
          in
          Closure_id.Map.add closure_id func_decl funs, subst)
        func_decls.funs
        (Closure_id.Map.empty, subst)
    in
    update func_decls ~funs

  let freshen t freshening =
    let subst = Freshening.name_substitution freshening in
    rename_names t subst
end and Flambda_type : Flambda_type0_intf.S with module Expr := Expr
  = Flambda_type0.Make (Expr)

module With_free_names = struct
  type 'a t =
    | Expr : Expr.t * Name_occurrences.t -> Expr.t t
    | Named : Flambda_kind.t * Named.t * Name_occurrences.t -> Named.t t

  let print (type a) ppf (t : a t) =
    match t with
    | Expr (expr, _) -> Expr.print ppf expr
    | Named (_, named, _) -> Named.print ppf named

  let of_defining_expr_of_let (let_expr : Let.t) =
    Named (let_expr.kind, let_expr.defining_expr,
      let_expr.free_names_of_defining_expr)

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
    | Named (kind, defining_expr, free_names_of_defining_expr) ->
      Let {
        var;
        kind;
        defining_expr;
        body;
        free_names_of_defining_expr;
        free_names_of_body = Expr.free_names body;
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
