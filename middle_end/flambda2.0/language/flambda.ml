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
(* CR mshinwell: Fix warning 60! *)
[@@@ocaml.warning "-60"]
[@@@ocaml.warning "-32"]

module K = Flambda_kind
module KP = Kinded_parameter

module Apply = Apply_expr
module Apply_cont = Apply_cont_expr
module Switch = Switch_expr

let fprintf = Format.fprintf

module rec Expr : sig
  type t
  type descr =
    | Let of Let.t
    | Let_cont of Let_cont.t
    | Apply of Apply.t
    | Apply_cont of Apply_cont.t
    | Switch of Switch.t
    | Invalid of Invalid_term_semantics.t
  include Expr_std.S with type t := t
  val descr : t -> descr
  type let_creation_result =
    | Have_deleted of Named.t
    | Nothing_deleted
  val create_let0
     : Variable.t
    -> Flambda_kind.t
    -> Named.t
    -> t
    -> t * let_creation_result
  val create_let : Variable.t -> Flambda_kind.t -> Named.t -> t -> t
  val create_let_cont : Let_cont.t -> t
  val create_apply : Apply.t -> t
  val create_apply_cont : Apply_cont.t -> t
  type switch_creation_result =
    | Have_deleted_comparison_but_not_branch
    | Have_deleted_comparison_and_branch
    | Nothing_deleted
  val create_switch0
     : scrutinee:Name.t
    -> arms:Continuation.t Discriminant.Map.t
    -> Expr.t * switch_creation_result
  val create_switch
     : scrutinee:Name.t
    -> arms:Continuation.t Discriminant.Map.t
    -> Expr.t
  val create_if_then_else
     : scrutinee:Name.t
    -> if_true:Continuation.t
    -> if_false:Continuation.t
    -> t
  val create_invalid : unit -> t
  val bind
     : bindings:(Variable.t * Flambda_kind.t * Named.t) list
    -> body:t
    -> t
  val bind_parameters_to_simples
     : bind:Kinded_parameter.t list
    -> target:Simple.t list
    -> t
    -> t
  val link_continuations
     : bind:Continuation.t
    -> target:Continuation.t
    -> arity:Flambda_arity.t
    -> t
    -> t
end = struct
  type descr =
    | Let of Let.t
    | Let_cont of Let_cont.t
    | Apply of Apply.t
    | Apply_cont of Apply_cont.t
    | Switch of Switch.t
    | Invalid of Invalid_term_semantics.t

  type free_names =
    | Ok of Name_occurrences.t
    | Not_computed

  type t = {
    descr : descr;
    free_names : free_names;
    delayed_permutation : Name_permutation.t;
    (* [delayed_permutation] must be applied to both [descr] and [free_names]
       before they are used. *)
    (* CR mshinwell: we should maybe try to statically enforce this *)
  }

  let apply_name_permutation t perm =
    let delayed_permutation =
      Name_permutation.compose ~second:perm ~first:t.delayed_permutation
    in
    { t with
      delayed_permutation;
    }

  let descr t =
    let perm = t.delayed_permutation in
    if Name_permutation.is_empty perm then
      t.descr
    else
      match t.descr with
      | Let let_expr ->
        let let_expr' = Let.apply_name_permutation let_expr perm in
        if let_expr == let_expr' then t.descr
        else Let let_expr'
      | Let_cont let_cont ->
        let let_cont' = Let_cont.apply_name_permutation let_cont perm in
        if let_cont == let_cont' then t.descr
        else Let_cont let_cont'
      | Apply apply ->
        let apply' = Apply.apply_name_permutation apply perm in
        if apply == apply' then t.descr
        else Apply apply'
      | Apply_cont apply_cont ->
        let apply_cont' = Apply_cont.apply_name_permutation apply_cont perm in
        if apply_cont == apply_cont' then t.descr
        else Apply_cont apply_cont'
      | Switch switch ->
        let switch' = Switch.apply_name_permutation switch perm in
        if switch == switch' then t.descr
        else Switch switch'
      | Invalid _ -> t.descr

  let invariant env t =
    match descr t with
    | Let let_expr -> Let.invariant env let_expr
    | Let_cont let_cont -> Let_cont.invariant env let_cont
    | Apply_cont apply_cont -> Apply_cont.invariant env apply_cont
    | Apply apply -> Apply.invariant env apply
    | Switch switch -> Switch.invariant env switch
    | Invalid _ -> ()

  (* CR mshinwell: We might want printing functions that show the delayed
     permutation, etc. *)

  let print_with_cache ~cache ppf (t : t) =
    match descr t with
    | Let let_expr -> Let.print_with_cache ~cache ppf let_expr
    | Let_cont let_cont -> Let_cont.print_with_cache ~cache ppf let_cont
    | Apply apply ->
      Format.fprintf ppf "@[<hov 1>(%sapply%s@ %a)@]"
        (Misc.Color.bold_cyan ())
        (Misc.Color.reset ())
        Apply.print apply
    | Apply_cont apply_cont -> Apply_cont.print ppf apply_cont
    | Switch switch -> Switch.print ppf switch
    | Invalid semantics ->
      fprintf ppf "@[%sInvalid %a%s@]"
        (Misc.Color.bold_cyan ())
        Invalid_term_semantics.print semantics
        (Misc.Color.reset ())

  let print ppf (t : t) =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names t =
    match t.free_names with
    | Ok free_names ->
      Name_occurrences.apply_name_permutation free_names t.delayed_permutation
    | Not_computed ->
      match descr t with
      | Let let_expr -> Let.free_names let_expr
      | Let_cont let_cont -> Let_cont.free_names let_cont
      | Apply apply -> Apply.free_names apply
      | Apply_cont apply_cont -> Apply_cont.free_names apply_cont
      | Switch switch -> Switch.free_names switch
      | Invalid _ -> Name_occurrences.empty

  let create descr =
    { descr;
      delayed_permutation = Name_permutation.empty;
      free_names = Not_computed;
    }

  type let_creation_result =
    | Have_deleted of Named.t
    | Nothing_deleted

  let create_let0 bound_var kind defining_expr body : t * let_creation_result =
    begin match !Clflags.dump_flambda_let with
    | None -> ()
    | Some stamp ->
      Variable.debug_when_stamp_matches bound_var ~stamp ~f:(fun () ->
        Printf.eprintf "Creation of [Let] with stamp %d:\n%s\n%!"
          stamp
          (Printexc.raw_backtrace_to_string (Printexc.get_callstack max_int)))
    end;
    let free_names_of_body = free_names body in
    (* If the [Let]-binding is redundant, don't even create it. *)
    if (not (Name_occurrences.mem_var free_names_of_body bound_var))
      && Named.at_most_generative_effects defining_expr
    then
      body, Have_deleted defining_expr
    else
      (* To save space, only keep free names on the outer term. *)
      let body =
        { body with
          free_names = Not_computed;
        }
      in
      let let_expr = Let.create ~bound_var ~kind ~defining_expr ~body in
      let free_names = Let.free_names let_expr in
      let t =
        { descr = Let (Let.create ~bound_var ~kind ~defining_expr ~body);
          delayed_permutation = Name_permutation.empty;
          free_names = Ok free_names;
        }
      in
      t, Nothing_deleted

  let create_let bound_var kind defining_expr body : t =
    let expr, _ = create_let0 bound_var kind defining_expr body in
    expr

  let create_let_cont let_cont = create (Let_cont let_cont)
  let create_apply apply = create (Apply apply)
  let create_apply_cont apply_cont = create (Apply_cont apply_cont)

  let create_invalid () =
    if !Clflags.treat_invalid_code_as_unreachable then
      create (Invalid Treat_as_unreachable)
    else
      create (Invalid Halt_and_catch_fire)

  type switch_creation_result =
    | Have_deleted_comparison_but_not_branch
    | Have_deleted_comparison_and_branch
    | Nothing_deleted

  let create_switch0 ~scrutinee ~arms : t * switch_creation_result =
    if Discriminant.Map.cardinal arms < 1 then
      create_invalid (), Have_deleted_comparison_and_branch
    else
      let change_to_goto k =
        create_apply_cont (Apply_cont.goto k),
          Have_deleted_comparison_but_not_branch
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
          create (Switch switch), Nothing_deleted

  let create_switch ~scrutinee ~arms =
    let expr, _ = create_switch0 ~scrutinee ~arms in
    expr

  let create_if_then_else ~scrutinee ~if_true ~if_false =
    let arms =
      Discriminant.Map.of_list [
        Discriminant.bool_true, if_true;
        Discriminant.bool_false, if_false;
      ]
    in
    create_switch ~scrutinee ~arms

  let bind ~bindings ~body =
    List.fold_left (fun expr (bound_var, kind, defining_expr) ->
        create_let bound_var kind defining_expr expr)
      body bindings

  let bind_parameters_to_simples ~bind ~target t =
    if List.compare_lengths bind target <> 0 then begin
      Misc.fatal_errorf "Lists of differing lengths: %a and %a"
        KP.List.print bind
        Simple.List.print target
    end;
    List.fold_left2 (fun expr bind target ->
        let var = KP.var bind in
        let kind = KP.kind bind in
        create_let var kind (Named.create_simple target) expr)
      t
      (List.rev bind) (List.rev target)

  let link_continuations ~bind ~target ~arity t =
    let params =
      List.map (fun kind ->
          let param = Parameter.wrap (Variable.create "param") in
          KP.create param kind)
        arity
    in
    let params_and_handler =
      let apply_cont_target =
        let args = List.map (fun param -> KP.simple param) params in
        Apply_cont.create target ~args
      in
      Continuation_params_and_handler.create params
        ~handler:(create_apply_cont apply_cont_target)
    in
    let handler =
      Continuation_handler.create ~params_and_handler
        ~stub:true
        ~is_exn_handler:false
    in
    Let_cont.create_non_recursive bind handler ~body:t
end and Named : sig
  type t =
    | Simple of Simple.t
    | Prim of Flambda_primitive.t * Debuginfo.t
    | Set_of_closures of Set_of_closures.t
  include Expr_std.S with type t := t
  val create_simple : Simple.t -> t
  val create_prim : Flambda_primitive.t -> Debuginfo.t -> t
  val create_set_of_closures : Set_of_closures.t -> t
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

  val invariant_returning_kind
     : Invariant_env.t
    -> t
    -> Flambda_primitive.result_kind
end = struct
  type t =
    | Simple of Simple.t
    | Prim of Flambda_primitive.t * Debuginfo.t
    | Set_of_closures of Set_of_closures.t

  let create_simple simple = Simple simple
  let create_prim prim dbg = Prim (prim, dbg)
  let create_set_of_closures set_of_closures = Set_of_closures set_of_closures

  let print_with_cache ~cache ppf (t : t) =
    match t with
    | Simple simple ->
      fprintf ppf "%s%a%s"
        (Misc.Color.bold_green ())
        Simple.print simple
        (Misc.Color.reset ())
    | Prim (prim, dbg) ->
      fprintf ppf "@[<hov 1>(%a%a)@]"
        Flambda_primitive.print prim
        Debuginfo.print_or_elide dbg
    | Set_of_closures set_of_closures ->
      Set_of_closures.print_with_cache ~cache ppf set_of_closures

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  (* CR mshinwell: It seems that the type [Flambda_primitive.result_kind]
     should move into [K], now it's used here. *)
  let invariant_returning_kind env t : Flambda_primitive.result_kind =
    try
      let module E = Invariant_env in
      match t with
      | Simple simple ->
        Singleton (E.kind_of_simple env simple)
      | Set_of_closures set_of_closures ->
        Set_of_closures.invariant env set_of_closures;
        Singleton K.fabricated
      | Prim (prim, dbg) ->
        Flambda_primitive.invariant env prim;
        ignore (dbg : Debuginfo.t);
        Flambda_primitive.result_kind prim
    with Misc.Fatal_error ->
      Misc.fatal_errorf "(during invariant checks) Context is:@ %a" print t

  let invariant env t =
    ignore ((invariant_returning_kind env t) : Flambda_primitive.result_kind)

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
      Prim (Unary (Box_number Naked_float, simple), dbg), K.value
    | Naked_number Naked_int32 ->
      Prim (Unary (Box_number Naked_int32, simple), dbg), K.value
    | Naked_number Naked_int64 ->
      Prim (Unary (Box_number Naked_int64, simple), dbg), K.value
    | Naked_number Naked_nativeint ->
      Prim (Unary (Box_number Naked_nativeint, simple), dbg), K.value
    | Fabricated ->
      Misc.fatal_error "Cannot box values of [Fabricated] kind"

  let unbox_value name (kind : Flambda_kind.t) dbg : Named.t * Flambda_kind.t =
    let simple = Simple.name name in
    match kind with
    | Value -> Simple simple, kind
    | Naked_number Naked_immediate ->
      Misc.fatal_error "Not yet supported"
    | Naked_number Naked_float ->
      Prim (Unary (Unbox_number Naked_float, simple), dbg), K.naked_float
    | Naked_number Naked_int32 ->
      Prim (Unary (Unbox_number Naked_int32, simple), dbg), K.naked_int32
    | Naked_number Naked_int64 ->
      Prim (Unary (Unbox_number Naked_int64, simple), dbg), K.naked_int64
    | Naked_number Naked_nativeint ->
      Prim (Unary (Unbox_number Naked_nativeint, simple), dbg),
        K.naked_nativeint
    | Fabricated ->
      Misc.fatal_error "Cannot box values of [Fabricated] kind"

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
        Simple.const (Naked_immediate Immediate.zero)
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
end and Let : sig
  type t

  include Expr_std.S with type t := t

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
end = struct
  module Bound_var_and_body =
    Name_abstraction.Make (Bindable_variable_in_terms) (Expr)

  type t = {
    bound_var_and_body : Bound_var_and_body.t;
    kind : Flambda_kind.t;
    defining_expr : Named.t;
  }

  let pattern_match t ~f =
    Bound_var_and_body.pattern_match t.bound_var_and_body
      ~f:(fun bound_var body -> f ~bound_var ~body)

  let print_with_cache ~cache ppf
        ({ bound_var_and_body = _; kind; defining_expr; } as t) =
    let rec let_body (expr : Expr.t) =
      match Expr.descr expr with
      | Let ({ bound_var_and_body = _; kind; defining_expr; } as t) ->
        pattern_match t ~f:(fun ~bound_var ~body ->
          fprintf ppf "@ @[<hov 1>%a@[@ %s:: %a%s@]@ %a@]"
            Variable.print bound_var
            (Misc.Color.bold_white ())
            Flambda_kind.print kind
            (Misc.Color.reset ())
            (Named.print_with_cache ~cache) defining_expr;
          let_body body)
      | _ -> expr
    in
    pattern_match t ~f:(fun ~bound_var ~body ->
      fprintf ppf "@[<2>(%slet%s@ @[<hov 1>(@[<hov 1>%a@[@ %s:: %a%s@]@ %a@]"
        (Misc.Color.bold_cyan ())
        (Misc.Color.reset ())
        Variable.print bound_var
        (Misc.Color.bold_white ())
        Flambda_kind.print kind
        (Misc.Color.reset ())
        (Named.print_with_cache ~cache) defining_expr;
      let expr = let_body body in
      fprintf ppf ")@]@ %a)@]"
        (Expr.print_with_cache ~cache) expr)

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let create ~bound_var ~kind ~defining_expr ~body =
    let bound_var_and_body = Bound_var_and_body.create bound_var body in
    { bound_var_and_body;
      kind;
      defining_expr;
    }

  let invariant env t =
    let module E = Invariant_env in
    pattern_match t ~f:(fun ~bound_var ~body ->
      let named_kind =
        match Named.invariant_returning_kind env t.defining_expr with
        | Singleton kind -> Some kind
        | Unit -> Some K.value
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
        (Name_occurrences.remove_var from_body bound_var))

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
end and Let_cont : sig
  type t = private
    | Non_recursive of {
        handler : Non_recursive_let_cont_handler.t;
        num_free_occurrences : int;
      }
    | Recursive of Recursive_let_cont_handlers.t
  include Expr_std.S with type t := t
  val create_non_recursive
     : Continuation.t
    -> Continuation_handler.t
    -> body:Expr.t
    -> Expr.t
  val create_recursive
     : Continuation_handlers.t
    -> body:Expr.t
    -> Expr.t
  val should_inline_out : t -> Non_recursive_let_cont_handler.t option
end = struct
  type t =
    | Non_recursive of {
        handler : Non_recursive_let_cont_handler.t;
        num_free_occurrences : int;
      }
    | Recursive of Recursive_let_cont_handlers.t

  (* CR mshinwell: A sketch of code for the invariant check is on cps_types. *)
  let invariant _env _t = ()

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
        (Misc.Color.bold_cyan ())
        (Misc.Color.reset ())
        (Let_cont_handlers.print_with_cache ~cache) handlers;
      let expr = let_cont_body body in
      fprintf ppf ")@]@ %a)@]" (print_with_cache0 ~cache) expr
*)
    end else begin
      let rec gather_let_conts let_conts let_cont =
        match let_cont with
        | Non_recursive { handler; num_free_occurrences = _; } ->
          Non_recursive_let_cont_handler.pattern_match handler
            ~f:(fun k ~(body : Expr.t) ->
              let let_conts, body =
                match Expr.descr body with
                | Let_cont let_cont -> gather_let_conts let_conts let_cont
                | _ -> let_conts, body
              in
              let handler = Non_recursive_let_cont_handler.handler handler in
              (k, handler) :: let_conts, body)
        | Recursive handlers ->
          Recursive_let_cont_handlers.pattern_match handlers
            ~f:(fun ~(body : Expr.t) handlers ->
              let let_conts, body =
                match Expr.descr body with
                | Let_cont let_cont -> gather_let_conts let_conts let_cont
                | _ -> let_conts, body
              in
              (Continuation.Map.bindings handlers) @ let_conts, body)
      in
      let let_conts, body = gather_let_conts [] t in
      fprintf ppf "@[<2>(@[<v 0>%a@;@[<v 0>"
        (Expr.print_with_cache ~cache) body;
      let first = ref true in
      List.iter (fun (cont, handler) ->
          Continuation_handler.print_using_where_with_cache ~cache
            ppf cont handler ~first:!first;
          first := false)
        let_conts;
      fprintf ppf "@]@])@]"
    end

  let print ppf t =
    print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let create_non_recursive k handler ~body =
    let free_names = Expr.free_names body in
    let num_free_occurrences =
      Name_occurrences.count_continuation free_names k
    in
    (* We don't inline out linear uses of continuations here, as it could
       result in quadratic behaviour.  However we can safely avoid creating
       a completely unused continuation binding. *)
    if num_free_occurrences < 1 then
      body
    else
      match Expr.descr body with
      | Apply_cont apply_cont when Apply_cont.is_goto apply_cont k ->
        (* CR mshinwell: This could work for the >0 arity-case too, to handle
           continuation aliases. *)
        Continuation_params_and_handler.pattern_match
          (Continuation_handler.params_and_handler handler)
          ~f:(fun params ~handler:handler_expr ->
            match params with
            | [] -> handler_expr
            | _ ->
              Misc.fatal_errorf
                "Continuation handler expected to have zero arity: %a"
                Continuation_handler.print handler)
      | _ ->
        let handler = Non_recursive_let_cont_handler.create k handler ~body in
        Expr.create_let_cont (Non_recursive { handler; num_free_occurrences; })

  let create_recursive handlers ~body =
    if Continuation_handlers.contains_exn_handler handlers then begin
      Misc.fatal_error "Exception-handling continuations cannot be recursive"
    end;
    Expr.create_let_cont
      (Recursive (Recursive_let_cont_handlers.create handlers ~body))

  let free_names t =
    match t with
    | Non_recursive { handler; num_free_occurrences = _; } ->
      Non_recursive_let_cont_handler.free_names handler
    | Recursive handlers ->
      Recursive_let_cont_handlers.free_names handlers

  let apply_name_permutation t perm =
    match t with
    | Non_recursive { handler; num_free_occurrences; } ->
      let handler' =
        Non_recursive_let_cont_handler.apply_name_permutation handler perm
      in
      if handler == handler' then t
      else Non_recursive { handler = handler'; num_free_occurrences; }
    | Recursive handlers ->
      let handlers' =
        Recursive_let_cont_handlers.apply_name_permutation handlers perm
      in
      if handlers == handlers' then t
      else Recursive handlers'

  let should_inline_out t =
    match t with
    | Non_recursive { handler = non_rec_handler; num_free_occurrences; } ->
      let handler = Non_recursive_let_cont_handler.handler non_rec_handler in
      let stub = Continuation_handler.stub handler in
      let is_exn_handler = Continuation_handler.is_exn_handler handler in
      if (stub || num_free_occurrences <= 1) && not is_exn_handler then
        Some non_rec_handler
      else
        None
    | Recursive _ -> None
end and Non_recursive_let_cont_handler : sig
  type t

  include Expr_std.S with type t := t

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
end = struct
  module Continuation_and_body =
    Name_abstraction.Make (Bindable_continuation) (Expr)

  type t = {
    continuation_and_body : Continuation_and_body.t;
    handler : Continuation_handler.t;
  }

  let invariant _env _t = ()

  let print _ppf _t = Misc.fatal_error "Not yet implemented"

  let print_with_cache ~cache:_ _ppf _t = Misc.fatal_error "Not yet implemented"

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
end and Recursive_let_cont_handlers0 : sig
  type t

  include Expr_std.S with type t := t

  val create
     : body:Expr.t
    -> Continuation_handlers.t
    -> t

  val body : t -> Expr.t
  val handlers : t -> Continuation_handlers.t
end = struct
  type t = {
    handlers : Continuation_handlers.t;
    body : Expr.t;
  }

  let invariant _env _t = ()

  let print _ppf _t = Misc.fatal_error "Not yet implemented"
  let print_with_cache ~cache:_ _ppf _t = Misc.fatal_error "Not yet implemented"

  let create ~body handlers =
    { handlers;
      body;
    }

  let handlers t = t.handlers
  let body t = t.body

  let free_names { handlers; body; } =
    Name_occurrences.union (Continuation_handlers.free_names handlers)
      (Expr.free_names body)

  let apply_name_permutation { handlers; body; } perm =
    let handlers' =
      Continuation_handlers.apply_name_permutation handlers perm
    in
    let body' =
      Expr.apply_name_permutation body perm
    in
    { handlers = handlers';
      body = body';
    }
end and Recursive_let_cont_handlers : sig
  type t

  include Expr_std.S with type t := t
  val create
     : body:Expr.t
    -> Continuation_handlers.t
    -> t
  val pattern_match
     : t
    -> f:(body:Expr.t -> Continuation_handlers.t -> 'a)
    -> 'a
end = struct
  include Name_abstraction.Make_list (Bindable_continuation)
    (Recursive_let_cont_handlers0)

  let invariant _env _t = ()

  let create ~body handlers =
    let bound = Continuation_handlers.domain handlers in
    let handlers0 = Recursive_let_cont_handlers0.create ~body handlers in
    create (Continuation.Set.elements bound) handlers0

  let pattern_match t ~f =
    pattern_match t ~f:(fun _bound handlers0 ->
      let body = Recursive_let_cont_handlers0.body handlers0 in
      let handlers = Recursive_let_cont_handlers0.handlers handlers0 in
      f ~body handlers)
end and Continuation_params_and_handler : sig
  type t

  include Expr_std.S with type t := t

  val create
     : Kinded_parameter.t list
    -> handler:Expr.t
    -> t

  val pattern_match
     : t
    -> f:(Kinded_parameter.t list
      -> handler:Expr.t
      -> 'a)
    -> 'a
end = struct
  module T0 = struct
    type t = {
      handler : Expr.t;
    }

    let print_with_cache ~cache ppf { handler; } =
      fprintf ppf "@[<hov 1>(\
          @[<hov 1>(handler %a)@]\
          )@]"
        (Expr.print_with_cache ~cache) handler

    let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let free_names { handler; } =
      Expr.free_names handler

    let apply_name_permutation ({ handler; } as t) perm =
      let handler' =
        Expr.apply_name_permutation handler perm
      in
      if handler == handler' then t
      else { handler = handler'; }
  end

  include Name_abstraction.Make_list (Kinded_parameter) (T0)

  let invariant _env _t = ()

  let print ppf t : unit = print ppf t

  let print_with_cache ~cache ppf t : unit = print_with_cache ~cache ppf t

  let create params ~handler =
    let t0 : T0.t =
      { handler;
      }
    in
    create params t0

  let pattern_match t ~f =
    pattern_match t ~f:(fun params { handler; } ->
      f params ~handler)
end and Continuation_handlers : sig
  type t = Continuation_handler.t Continuation.Map.t

  include Expr_std.S with type t := t

  val to_map : t -> Continuation_handler.t Continuation.Map.t
  val domain : t -> Continuation.Set.t
  val contains_exn_handler : t -> bool
end = struct
  type t = Continuation_handler.t Continuation.Map.t

  let invariant _env _t = ()

  let print_with_cache ~cache:_ _ppf _t = Misc.fatal_error "Not yet implemented"

  let print _ppf _t = Misc.fatal_error "Not yet implemented"

  let to_map t = t

  let free_names t =
    Continuation.Map.fold (fun _k handler free_names ->
        Name_occurrences.union free_names
          (Continuation_handler.free_names handler))
      t
      (Name_occurrences.empty)

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
end and Continuation_handler : sig
  type t
  include Expr_std.S with type t := t

  val print_using_where_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> Continuation.t
    -> t
    -> first:bool
    -> unit

  val create
     : params_and_handler:Continuation_params_and_handler.t
    -> stub:bool
    -> is_exn_handler:bool
    -> t
  val params_and_handler : t -> Continuation_params_and_handler.t
  val stub : t -> bool
  val is_exn_handler : t -> bool
  type behaviour = private
    | Unreachable of { arity : Flambda_arity.t; }
    | Alias_for of { arity : Flambda_arity.t; alias_for : Continuation.t; }
    | Unknown of { arity : Flambda_arity.t; }
  val behaviour : t -> behaviour
  val arity : t -> Flambda_arity.t
  val with_params_and_handler : t -> Continuation_params_and_handler.t -> t
end = struct
  type t = {
    params_and_handler : Continuation_params_and_handler.t;
    stub : bool;
    is_exn_handler : bool;
  }

  let invariant _env _t = ()

  let print_using_where_with_cache ~cache ppf k
        ({ params_and_handler = _; stub; is_exn_handler; } as t) ~first =
    if not first then begin
      fprintf ppf "@ "
    end;
    Continuation_params_and_handler.pattern_match t.params_and_handler
      ~f:(fun params ~handler ->
        fprintf ppf "@[<hov 1>%swhere%s @[<hov 1>%a%s%s@[<hov 1>%a"
          (Misc.Color.bold_cyan ())
          (Misc.Color.reset ())
  (*
          (if first_and_non_recursive then "" else "and ")
  *)
          Continuation.print k
          (if stub then " *stub* " else "")
          (if is_exn_handler then "*exn* " else "")
          Kinded_parameter.List.print params;
        fprintf ppf "@]@] =@ %a"
          (Expr.print_with_cache ~cache) handler)

  let print _ppf _t =
    (* XXX Where do we get [k] from? *)
    Misc.fatal_error "Not yet implemented"

  let print_with_cache ~cache:_ _ppf _t =
    Misc.fatal_error "Not yet implemented"

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
    Continuation_params_and_handler.pattern_match params_and_handler
      ~f:(fun params ~handler ->
        fprintf ppf "%s%s%a@ =@ %a"
          (if stub then "*stub* " else "")
          (if is_exn_handler then "*exn* " else "")
          (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
          Expr.print handler)

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t
*)

  let create ~params_and_handler ~stub ~is_exn_handler =
    { params_and_handler;
      stub;
      is_exn_handler;
    }

  let params_and_handler t = t.params_and_handler
  let stub t = t.stub
  let is_exn_handler t = t.is_exn_handler

  let free_names
        { params_and_handler; stub = _; is_exn_handler = _; } =
    Continuation_params_and_handler.free_names params_and_handler

  let apply_name_permutation
        ({ params_and_handler; stub;
           is_exn_handler; } as t) perm =
    let params_and_handler' =
      Continuation_params_and_handler.apply_name_permutation
        params_and_handler perm
    in
    if params_and_handler == params_and_handler' then t
    else
      { params_and_handler = params_and_handler';
        stub;
        is_exn_handler;
      }

  type behaviour =
    | Unreachable of { arity : Flambda_arity.t; }
    | Alias_for of { arity : Flambda_arity.t; alias_for : Continuation.t; }
    | Unknown of { arity : Flambda_arity.t; }

  let behaviour t : behaviour =
    (* This could be replaced by a more sophisticated analysis, but for the
       moment we just use a simple syntactic check. *)
    Continuation_params_and_handler.pattern_match t.params_and_handler
      ~f:(fun params ~handler ->
        let arity = Kinded_parameter.List.arity params in
        if t.is_exn_handler then
          Unknown { arity; }
        else
          match Expr.descr handler with
          | Apply_cont apply_cont ->
            begin match Apply_cont.trap_action apply_cont with
            | Some _ -> Unknown { arity; }
            | None ->
              let args = Apply_cont.args apply_cont in
              let params = List.map KP.simple params in
              if Misc.Stdlib.List.compare Simple.compare args params = 0 then
                Alias_for {
                  arity;
                  alias_for = Apply_cont.continuation apply_cont;
                }
              else
                Unknown { arity; }
            end
          | Invalid _ -> Unreachable { arity; }
          | _ -> Unknown { arity; })

  let arity t =
    Continuation_params_and_handler.pattern_match t.params_and_handler
      ~f:(fun params ~handler:_ -> Kinded_parameter.List.arity params)

  let with_params_and_handler t params_and_handler =
    { t with params_and_handler; }
end and Set_of_closures : sig
  type t = {
    function_decls : Function_declarations.t;
    closure_elements : Simple.t Var_within_closure.Map.t;
  }
  include Expr_std.S with type t := t
  val create
     : function_decls:Function_declarations.t
    -> closure_elements:Simple.t Var_within_closure.Map.t
    -> t
  val function_decls : t -> Function_declarations.t
  val closure_elements : t -> Simple.t Var_within_closure.Map.t
  val has_empty_environment : t -> bool
end = struct
  type t = {
    function_decls : Function_declarations.t;
    closure_elements : Simple.t Var_within_closure.Map.t;
  }

  (* CR mshinwell: A sketch of code for the invariant check is on cps_types. *)
  let invariant _env _t = ()

  let create ~function_decls ~closure_elements =
    { function_decls;
      closure_elements;
    }

  let function_decls t = t.function_decls
  let closure_elements t = t.closure_elements

  let has_empty_environment t =
    Var_within_closure.Map.is_empty t.closure_elements

  let print_with_cache ~cache ppf
        { function_decls; 
          closure_elements;
        } =
    fprintf ppf "@[<hov 1>(%sset_of_closures%s@ \
        @[<hov 1>(function_decls@ %a)@]@ \
        @[<hov 1>(closure_elements@ %a)@]@ \
        )@]"
      (Misc.Color.bold_green ())
      (Misc.Color.reset ())
      (Function_declarations.print_with_cache ~cache) function_decls
      (Var_within_closure.Map.print Simple.print) closure_elements

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names
        { function_decls;
          closure_elements;
        } =
    Name_occurrences.union_list [
      Function_declarations.free_names function_decls;
      Simple.List.free_names (Var_within_closure.Map.data closure_elements);
    ]

  let apply_name_permutation
        ({ function_decls; 
           closure_elements;
         } as t) perm =
    let function_decls' =
      Function_declarations.apply_name_permutation function_decls perm
    in
    let closure_elements' =
      Var_within_closure.Map.map_sharing (fun simple ->
          Simple.apply_name_permutation simple perm)
        closure_elements
    in
    if function_decls == function_decls'
      && closure_elements == closure_elements'
    then t
    else
      { function_decls = function_decls';
        closure_elements = closure_elements';
      }
end and Function_declarations : sig
  type t
  include Expr_std.S with type t := t
  val create : Function_declaration.t Closure_id.Map.t -> t
  val set_of_closures_origin : t -> Set_of_closures_origin.t
  val funs : t -> Function_declaration.t Closure_id.Map.t
  val find : t -> Closure_id.t -> Function_declaration.t
  val update : t -> funs:Function_declaration.t Closure_id.Map.t -> t
end = struct
  type t = {
    set_of_closures_origin : Set_of_closures_origin.t;
    funs : Function_declaration.t Closure_id.Map.t;
  }

  let invariant _env _t = ()

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

  let print_with_cache ~cache ppf (t : t) =
    let funs ppf t =
      Closure_id.Map.iter (fun _ decl ->
          Function_declaration.print_with_cache ~cache ppf decl)
        t
    in
    fprintf ppf "@[<hov 1>(funs@ %a)@ (set_of_closures_origin %a)@]"
      funs t.funs
      Set_of_closures_origin.print t.set_of_closures_origin

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let free_names { set_of_closures_origin = _; funs; } =
    Closure_id.Map.fold
      (fun _closure_id (func_decl : Function_declaration.t) syms ->
        Name_occurrences.union syms (Function_declaration.free_names func_decl))
      funs
      (Name_occurrences.empty)

  let apply_name_permutation ({ set_of_closures_origin; funs; } as t) perm =
    let funs' =
      Closure_id.Map.map_sharing (fun func_decl ->
          Function_declaration.apply_name_permutation func_decl perm)
        funs
    in
    if funs == funs' then t
    else { set_of_closures_origin; funs = funs'; }
end and Function_params_and_body : sig
  type t

  include Expr_std.S with type t := t

  val create
     : return_continuation:Continuation.t
    -> Exn_continuation.t
    -> Kinded_parameter.t list
    -> body:Expr.t
    -> my_closure:Variable.t
    -> t

  val pattern_match
     : t
    -> f:(return_continuation:Continuation.t
      -> Exn_continuation.t
      -> Kinded_parameter.t list
      -> body:Expr.t
      -> my_closure:Variable.t
      -> 'a)
    -> 'a
end = struct
  module T0 = struct
    type t = {
      body : Expr.t;
    }

    let print_with_cache ~cache:_ ppf { body; } =
      fprintf ppf "@[<hov 1>(\
          @[<hov 1>(body %a)@]\
          )@]"
        Expr.print body

    let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let free_names { body; } =
      Expr.free_names body

    let apply_name_permutation ({ body;} as t) perm =
      let body' =
        Expr.apply_name_permutation body perm
      in
      if body == body' then t
      else { body = body'; }
  end
  module T1 = Name_abstraction.Make_list (Kinded_parameter) (T0)
  module T2 = Name_abstraction.Make (Bindable_exn_continuation) (T1)
  include Name_abstraction.Make (Bindable_continuation) (T2)

  let invariant _env _t = ()

  let print ppf t : unit = print ppf t

  let print_with_cache ~cache ppf t : unit = print_with_cache ~cache ppf t

  let create ~return_continuation exn_continuation params ~body ~my_closure =
    let t0 : T0.t =
      { body;
      }
    in
    let my_closure =
      Kinded_parameter.create (Parameter.wrap my_closure) K.value
    in
    let t1 = T1.create (params @ [my_closure]) t0 in
    let t2 = T2.create exn_continuation t1 in
    create return_continuation t2

  let pattern_match t ~f =
    pattern_match t ~f:(fun return_continuation t2 ->
      T2.pattern_match t2 ~f:(fun exn_continuation t1 ->
        T1.pattern_match t1 ~f:(fun params_and_my_closure t0 ->
          let params, my_closure =
            match List.rev params_and_my_closure with
            | my_closure::params_rev ->
              List.rev params_rev, Kinded_parameter.var my_closure
            | [] -> assert false  (* see [create], above. *)
          in
          f ~return_continuation exn_continuation params ~body:t0.body
            ~my_closure)))
end and Function_declaration : sig
  include Expr_std.S
  val create
     : closure_origin:Closure_origin.t
    -> params_and_body:Function_params_and_body.t
    -> result_arity:Flambda_arity.t
    -> stub:bool
    -> dbg:Debuginfo.t
    -> inline:Inline_attribute.t
    -> is_a_functor:bool
    -> t
  val print : Format.formatter -> t -> unit
  val print_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit
  val closure_origin : t -> Closure_origin.t
  val params_and_body : t -> Function_params_and_body.t
  val code_id : t -> Code_id.t
  val result_arity : t -> Flambda_arity.t
  val stub : t -> bool
  val dbg : t -> Debuginfo.t
  val inline : t -> Inline_attribute.t
  val is_a_functor : t -> bool
  val update_params_and_body : t -> Function_params_and_body.t -> t
end = struct
  type t = {
    closure_origin : Closure_origin.t;
    params_and_body : Function_params_and_body.t;
    code_id : Code_id.t;
    result_arity : Flambda_arity.t;
    stub : bool;
    dbg : Debuginfo.t;
    inline : Inline_attribute.t;
    is_a_functor : bool;
  }

  let invariant _env _t = ()

  let create ~closure_origin ~params_and_body ~result_arity ~stub ~dbg
        ~(inline : Inline_attribute.t)
        ~is_a_functor : t =
    begin match stub, inline with
    | true, (Never_inline | Default_inline)
    | false, (Never_inline | Default_inline | Always_inline | Unroll _) -> ()
    | true, (Always_inline | Unroll _) ->
      Misc.fatal_errorf
        "Stubs may not be annotated as [Always_inline] or [Unroll]: %a"
        Function_params_and_body.print params_and_body
    end;
    { closure_origin;
      params_and_body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
      result_arity;
      stub;
      dbg;
      inline;
      is_a_functor;
    }

  let print_with_cache ~cache ppf
        { closure_origin;
          params_and_body;
          code_id = _;
          result_arity;
          stub;
          dbg;
          inline;
          is_a_functor;
        } =
    (* CR mshinwell: It's a bit strange that this doesn't use
       [Function_params_and_body.print_with_cache].  However a proper
       function to print in a more human-readable form will probably look more
       like this code. *)
    Function_params_and_body.pattern_match params_and_body
      ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure ->
        fprintf ppf "@[<hov 1>(\
            @[<hov 1>(closure_origin@ %a)@]@ \
            @[<hov 1>(return_continuation@ %a)@]@ \
            @[<hov 1>(exn_continuation@ %a)@]@ \
            @[<hov 1>(stub@ %b)@]@ \
            @[<hov 1>(dbg@ %a)@]@ \
            @[<hov 1>(inline@ %a)@]@ \
            @[<hov 1>(is_a_functor@ %b)@]@ \
            @[<hov 1>(params@ %a)@]@ \
            @[<hov 1>(my_closure@ %s%a%s)@]@ \
            @[<hov 1>(result_arity@ %a)@]@ \
            @[<hov 1>(body@ %a)@]@ \
            )@]"
          Closure_origin.print closure_origin
          Continuation.print return_continuation
          Exn_continuation.print exn_continuation
          stub
          Debuginfo.print_compact dbg
          Inline_attribute.print inline
          is_a_functor
          Kinded_parameter.List.print params
          (Misc.Color.bold_magenta ())
          Variable.print my_closure
          (Misc.Color.reset ())
          Flambda_arity.print result_arity
          (Expr.print_with_cache ~cache) body)

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let closure_origin t = t.closure_origin
  let params_and_body t = t.params_and_body
  let code_id t = t.code_id
  let result_arity t = t.result_arity
  let stub t = t.stub
  let dbg t = t.dbg
  let inline t = t.inline
  let is_a_functor t = t.is_a_functor

  let update_params_and_body t params_and_body =
    { t with
      params_and_body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
    }

  let free_names
        { closure_origin = _;
          params_and_body;
          code_id = _;
          result_arity = _;
          stub = _;
          dbg = _;
          inline = _;
          is_a_functor = _;
        } =
    Function_params_and_body.free_names params_and_body

  let apply_name_permutation
        ({ closure_origin;
           params_and_body;
           code_id;
           result_arity;
           stub;
           dbg;
           inline;
           is_a_functor;
         } as t) perm =
    let params_and_body' =
      Function_params_and_body.apply_name_permutation params_and_body perm
    in
    if params_and_body == params_and_body' then t
    else
      { closure_origin;
        params_and_body = params_and_body';
        code_id;
        result_arity;
        stub;
        dbg;
        inline;
        is_a_functor;
      }
end and Flambda_type : Flambda_type0_intf.S
    with type term_language_function_declaration := Function_declaration.t
  = Flambda_type0.Make (Function_declaration)

(* CR mshinwell: Consider counting numbers of names in Name_occurrences *)
(* CR mshinwell: Check that apply_cont is well-formed when there is a
   trap installation or removal. *)
(* CR-someday pchambart: for sum types, we should probably add an exhaustive
   pattern in ignores functions to be reminded if a type change *)
(* CR-someday mshinwell: We should make "direct applications should not have
   overapplication" be an invariant throughout.  At the moment I think this is
   only true after [Simplify] has split overapplications. *)

module Import = struct
  module Apply = Apply
  module Apply_cont = Apply_cont
  module Continuation_handler = Continuation_handler
  module Continuation_handlers = Continuation_handlers
  module Continuation_params_and_handler = Continuation_params_and_handler
  module Expr = Expr
  module Function_declaration = Function_declaration
  module Function_declarations = Function_declarations
  module Function_params_and_body = Function_params_and_body
  module Let_cont = Let_cont
  module Let = Let
  module Named = Named
  module Non_recursive_let_cont_handler = Non_recursive_let_cont_handler
  module Recursive_let_cont_handlers = Recursive_let_cont_handlers
  module Set_of_closures = Set_of_closures
  module Switch = Switch
end
