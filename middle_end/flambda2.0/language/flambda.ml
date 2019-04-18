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

module Apply = Apply_expr
module Apply_cont = Apply_cont_expr
module Switch = Switch_expr

let fprintf = Format.fprintf

type switch_creation_result =
  | Have_deleted_comparison_but_not_branch
  | Have_deleted_comparison_and_branch
  | Nothing_deleted

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
    | Apply apply -> Apply.print ppf apply
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

  let continuation_counts t =
    match descr t with
    | Let let_expr -> Let.continuation_counts let_expr
    | Let_cont let_cont -> Let_cont.continuation_counts let_cont
    | Apply apply -> Apply.continuation_counts apply
    | Apply_cont apply_cont -> Apply_cont.continuation_counts apply_cont
    | Switch switch -> Switch.continuation_counts switch
    | Invalid _ -> Continuation_counts.empty

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
      let free_names : free_names =
        Ok (Name_occurrences.remove_var free_names_of_body bound_var)
      in
      let t =
        { descr = Let (Let.create ~bound_var ~kind ~defining_expr ~body);
          delayed_permutation = Name_permutation.empty;
          free_names;
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

  (* CR mshinwell: Maybe this should assign the fresh names? *)
  let bind ~bindings ~body =
    List.fold_left (fun expr (bound_var, kind, defining_expr) ->
        Expr.create_let bound_var kind defining_expr expr)
      body bindings
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
  include Named

  let print_with_cache ~cache ppf (t : t) =
    match t with
    | Simple simple ->
      fprintf ppf "%s%a%s"
        (Misc.Color.bold_green ())
        Simple.print simple
        (Misc.Color.reset ())
    | Prim (prim, dbg) ->
      fprintf ppf "@[<2>(%a%a)@]"
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
        Singleton (K.fabricated ())
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
  module Bound_var_and_body = Name_abstraction.Make (Expr)

  type t = {
    bound_var_and_body : Bound_var_and_body.t;
    kind : Flambda_kind.t;
    defining_expr : Named.t;
  }

  let pattern_match _t ~f:_ = assert false
(*
    Bound_var_and_body.pattern_match t.bound_var_and_body
      ~f:(fun bound_var body ->
        f ~bound_var ~body:(Expr.expr body))
*)

  let print_with_cache ~cache ppf
        ({ bound_var_and_body = _; kind; defining_expr; } as t) =
    let rec let_body (expr : Expr.t) =
      match Expr.descr expr with
      | Let ({ bound_var_and_body = _; kind; defining_expr; } as t) ->
        pattern_match t ~f:(fun ~bound_var ~body ->
          fprintf ppf "@ @[<2>%a@[@ %s:: %a%s@]@ %a@]"
            Bindable_name.print bound_var
            (Misc.Color.bold_white ())
            Flambda_kind.print kind
            (Misc.Color.reset ())
            (Named.print_with_cache ~cache) defining_expr;
          let_body body)
      | _ -> expr
    in
    pattern_match t ~f:(fun ~bound_var ~body ->
      fprintf ppf "@[<2>(%slet%s@ @[<hv 1>(@[<2>%a@[@ %s:: %a%s@]@ %a@]"
        (Misc.Color.bold_cyan ())
        (Misc.Color.reset ())
        Bindable_name.print bound_var
        (Misc.Color.bold_white ())
        Flambda_kind.print kind
        (Misc.Color.reset ())
        (Named.print_with_cache ~cache) defining_expr;
      let expr = let_body body in
      fprintf ppf ")@]@ %a)@]"
        (Expr.print_with_cache ~cache) expr)

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

  let create ~bound_var ~kind ~defining_expr ~body =
    let bound_var = Bindable_name.Name (Name.var bound_var) in
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
        | Unit -> Some (K.value ())
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
      (* XXX Bindable_name strikes again *)
      let bound_var =
        match (bound_var : Bindable_name.t) with
        | Name (Var var) -> var
        | _ -> assert false
      in
      let env = E.add_variable env bound_var t.kind in
      Expr.invariant env body)

  let kind t = t.kind
  let defining_expr t = t.defining_expr

  let free_names ({ bound_var_and_body = _; kind = _; defining_expr; } as t) =
    pattern_match t ~f:(fun ~bound_var ~body ->
      let from_defining_expr = Named.free_names defining_expr in
      let from_body = Expr.free_names body in
      (* XXX Bindable_name strikes again *)
      let bound_var =
        match (bound_var : Bindable_name.t) with
        | Name (Var var) -> var
        | _ -> assert false
      in
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

  let continuation_counts
        ({ bound_var_and_body = _; kind = _; defining_expr = _; } as t) =
    pattern_match t ~f:(fun ~bound_var:_ ~body ->
      Expr.continuation_counts body)
end and Let_cont : sig
  type t = private
    | Non_recursive of Non_recursive_let_cont_handler.t
    | Recursive of Recursive_let_cont_handlers.t
  include Expr_std.S with type t := t
  val create_non_recursive
     : Continuation.t
    -> Continuation_handler.t
    -> body:Expr.t
    -> t
  val create_recursive
     : Continuation_handlers.t
    -> body:Expr.t
    -> t
(*
  val to_continuation_map : t -> Continuation_handlers.t
  val map : t -> f:(Continuation_handlers.t -> Continuation_handlers.t) -> t
*)
end = struct
  type t =
    | Non_recursive of Non_recursive_let_cont_handler.t
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
        | Non_recursive handler ->
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

  let continuation_counts t =
    match t with
    | Non_recursive handler ->
      Non_recursive_let_cont_handler.continuation_counts handler
    | Recursive handlers ->
      Recursive_let_cont_handlers.continuation_counts handlers
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
  module Continuation_and_body = Name_abstraction.Make (Expr)

  type t = {
    continuation_and_body : Continuation_and_body.t;
    handler : Continuation_handler.t;
  }

  let create continuation ~body handler =
    let continuation_and_body =
      Continuation_and_body.create (Continuation continuation) body
    in
    { continuation_and_body;
      handler;
    }

  let pattern_match _t ~f:_ = assert false
(*
    Continuation_and_body.pattern_match t.continuation_and_body
      ~f:(fun continuation body -> f continuation ~body)
*)

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

  let continuation_counts _t =
    Misc.fatal_error "Not yet implemented"
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

  (* CR mshinwell: Do something about these.  They are needed for the
     name abstraction building functor below. *)
  let print _ppf _t = Misc.fatal_error "Not used"
  let print_with_cache ~cache:_ _ppf _t = Misc.fatal_error "Not used"

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
  include Contains_names.S

  val create
     : body:Expr.t
    -> Continuation_handlers.t
    -> t

  val pattern_match
     : t
    -> f:(body:Expr.t -> Continuation_handlers.t -> 'a)
    -> 'a

  val continuation_counts : t -> Continuation_counts.t
end = struct
  include Name_abstraction.Make_list (Recursive_let_cont_handlers0)

  let create ~body handlers =
    let bound = Continuation_handlers.domain handlers in
    let bound = (* XXX *)
      List.map (fun k -> Bindable_name.Continuation k)
        (Continuation.Set.elements bound)
    in
    let handlers0 =
      Recursive_let_cont_handlers0.create ~body handlers
    in
    create bound handlers0

  let pattern_match t ~f =
    pattern_match t ~f:(fun _bound handlers0 ->
      let body = Recursive_let_cont_handlers0.body handlers0 in
      let handlers = Recursive_let_cont_handlers0.handlers handlers0 in
      f ~body handlers)

  let continuation_counts _t =
    Misc.fatal_error "Not yet implemented"
end and Params_and_handler : sig
  type t

  include Expr_std.S with type t := t

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
      handler : Expr.t;
    }

    let print_with_cache ~cache ppf { param_relations; handler; } =
      fprintf ppf "@[<hov 1>(\
          @[<hov 1>(param_relations %a)@]@ \
          @[<hov 1>(handler %a)@]\
          )@]"
        (Flambda_type.Typing_env_extension.print_with_cache ~cache)
        param_relations
        (Expr.print_with_cache ~cache) handler

    let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let free_names { param_relations; handler; } =
      Name_occurrences.union
        (Flambda_type.Typing_env_extension.free_names param_relations)
        (Expr.free_names handler)

    let apply_name_permutation ({ param_relations; handler; } as t) perm =
      let param_relations' =
        Flambda_type.Typing_env_extension.apply_name_permutation
          param_relations perm
      in
      let handler' =
        Expr.apply_name_permutation handler perm
      in
      if param_relations == param_relations' && handler == handler' then t
      else { param_relations = param_relations'; handler = handler'; }
  end

  include Name_abstraction.Make_list (T0)

  let create params ~param_relations ~handler =
    let t0 : T0.t =
      { param_relations;
        handler;
      }
    in
    let params = (* XXX *)
      List.map (fun p -> Bindable_name.Name (Kinded_parameter.name p))
        params
    in
    create params t0

  let pattern_match _t ~f:_ = assert false
(*
    pattern_match t ~f:(fun params { param_relations; handler; } ->
      f params ~param_relations ~handler:(Expr.expr handler))
*)
end and Continuation_handlers : sig
  type t = Continuation_handler.t Continuation.Map.t

  include Expr_std.S with type t := t

  val domain : t -> Continuation.Set.t
  val contains_exn_handler : t -> bool
end = struct
  include Continuation_handlers

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
    (* XXX Print [param_relations_lvs] *)
        ({ params_and_handler = _; param_relations_lvs = _;
           stub; is_exn_handler; } as t) ~first =
    if not first then begin
      fprintf ppf "@ "
    end;
    pattern_match t ~f:(fun params ~param_relations ~handler ->
      fprintf ppf "@[<v 2>%swhere%s @[%a%s%s@[%a"
        (Misc.Color.bold_cyan ())
        (Misc.Color.reset ())
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

  let create params ~param_relations ~handler param_relations_lvs ~stub
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
        ({ params_and_handler; param_relations_lvs; stub;
           is_exn_handler; } as t) perm =
    let params_and_handler' =
      Params_and_handler.apply_name_permutation params_and_handler perm
    in
    let param_relations_lvs =
      Flambda_type.Parameters.apply_name_permutation param_relations_lvs perm
    in
    if params_and_handler == params_and_handler' then t
    else
      { params_and_handler = params_and_handler';
        param_relations_lvs;
        stub;
        is_exn_handler;
      }
end and Set_of_closures : sig
  type t = {
    function_decls : Function_declarations.t;
    set_of_closures_ty : Flambda_type.t;
    closure_elements : Simple.t Var_within_closure.Map.t;
    direct_call_surrogates : Closure_id.t Closure_id.Map.t;
  }
  include Expr_std.S with type t := t
  val create
     : function_decls:Function_declarations.t
    -> set_of_closures_ty:Flambda_type.t
    -> closure_elements:Simple.t Var_within_closure.Map.t
    -> direct_call_surrogates:Closure_id.t Closure_id.Map.t
    -> t
  val function_decls : t -> Function_declarations.t
  val set_of_closures_ty : t -> Flambda_type.t
  val closure_elements : t -> Simple.t Var_within_closure.Map.t
  val direct_call_surrogates : t -> Closure_id.t Closure_id.Map.t
  val has_empty_environment : t -> bool
end = struct
  include Set_of_closures

  (* CR mshinwell: A sketch of code for the invariant check is on cps_types. *)
  let invariant _env _t = ()

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
      (Misc.Color.bold_green ())
      (Misc.Color.reset ())
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
    fprintf ppf "@[<2>(%a)(origin = %a)@]" funs t.funs
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
end and Params_and_body : sig
  type t

  include Expr_std.S with type t := t

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
      body : Expr.t;
    }

    let print_with_cache ~cache:_ ppf { param_relations; body; } =
      fprintf ppf "@[<hov 1>(\
          @[<hov 1>(param_relations %a)@]@ \
          @[<hov 1>(body %a)@]\
          )@]"
        Flambda_type.Typing_env_extension.print param_relations
        Expr.print body

    let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

    let free_names { param_relations; body; } =
      Name_occurrences.union
        (Flambda_type.Typing_env_extension.free_names param_relations)
        (Expr.free_names body)

    let apply_name_permutation ({ param_relations; body;} as t) perm =
      let param_relations' =
        Flambda_type.Typing_env_extension.apply_name_permutation
          param_relations perm
      in
      let body' =
        Expr.apply_name_permutation body perm
      in
      if param_relations == param_relations' && body == body' then t
      else { param_relations = param_relations'; body = body'; }
  end

  include Name_abstraction.Make_list (T0)

  let create params ~param_relations ~body ~my_closure =
    let t0 : T0.t =
      { param_relations;
        body;
      }
    in
    let params = (* XXX *)
      List.map (fun p -> Bindable_name.Name (Kinded_parameter.name p))
        params
    in
    let bound = params @ [Bindable_name.Name (Name.var my_closure)] in
    create bound t0

  let print ppf t : unit = print ppf t

  let pattern_match _t ~f:_ = assert false
(*
    pattern_match t ~f:(fun params_and_my_closure t0 ->
      f params ~param_relations:t0.param_relations
        ~body:(Expr.expr t0.body) ~my_closure)
*)
end and Function_declaration : sig
  include Expr_std.S
  val create
     : closure_origin:Closure_origin.t
    -> continuation_param:Continuation.t
    -> exn_continuation:Continuation.t
    -> params_and_body:Params_and_body.t
    -> result_arity:Flambda_arity.t
    -> stub:bool
    -> dbg:Debuginfo.t
    -> inline:Inline_attribute.t
    -> specialise:Specialise_attribute.t
    -> is_a_functor:bool
    -> t
  val print : Format.formatter -> t -> unit
  val print_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit
  val closure_origin : t -> Closure_origin.t
  val continuation_param : t -> Continuation.t
  val exn_continuation : t -> Continuation.t
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
    exn_continuation : Exn_continuation.t;
    params_and_body : Params_and_body.t;
    code_id : Code_id.t;
    result_arity : Flambda_arity.t;
    stub : bool;
    dbg : Debuginfo.t;
    inline : Inline_attribute.t;
    specialise : Specialise_attribute.t;
    is_a_functor : bool;
  }

  let create ~closure_origin ~continuation_param ~exn_continuation
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
      exn_continuation;
      params_and_body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
      result_arity;
      stub;
      dbg;
      inline;
      specialise;
      is_a_functor;
    }

  let print_with_cache ~cache ppf
        { closure_origin;
          continuation_param;
          exn_continuation;
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
          "@[<2>(%s%s%a%a@ (my_closure %a)@ (origin %a)@ =@ \
            %sfun%s@[<2> <%a> <exn %a>@] %a@ @[<2>@ :: %s%a%s"
          stub
          is_a_functor
          Inline_attribute.print inline
          Specialise_attribute.print specialise
          Variable.print my_closure
          Closure_origin.print closure_origin
          (Misc.Color.bold_cyan ())
          (Misc.Color.reset ())
          Continuation.print continuation_param
          Exn_continuation.print exn_continuation
          Kinded_parameter.List.print params
          (Misc.Color.bold_white ())
          Flambda_arity.print result_arity
          (Misc.Color.reset ());
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
  let exn_continuation t = t.exn_continuation
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
          exn_continuation = _;
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
           exn_continuation;
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
        exn_continuation;
        params_and_body = params_and_body';
        code_id;
        result_arity;
        stub;
        dbg;
        inline;
        specialise;
        is_a_functor;
      }
end and Flambda_type : Flambda_type0_intf.S
    with type term_language_function_declaration := Function_declaration.t
  = Flambda_type0.Make (Function_declaration)

(* CR mshinwell: Check that apply_cont is well-formed when there is a
   trap installation or removal. *)
(* CR-someday pchambart: for sum types, we should probably add an exhaustive
   pattern in ignores functions to be reminded if a type change *)
(* CR-someday mshinwell: We should make "direct applications should not have
   overapplication" be an invariant throughout.  At the moment I think this is
   only true after [Simplify] has split overapplications. *)
