(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
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

  let equal_function_call call1 call2 =
    match call1, call2 with
    | Direct { closure_id = closure_id1; return_arity = return_arity1; },
        Direct { closure_id = closure_id2; return_arity = return_arity2; } ->
      Closure_id.equal closure_id1 closure_id2
        && Flambda_arity.equal return_arity1 return_arity2
    | Indirect_unknown_arity, Indirect_unknown_arity -> true
    | Indirect_known_arity {
        param_arity = param_arity1; return_arity = return_arity1;
      },
        Indirect_known_arity {
          param_arity = param_arity2; return_arity = return_arity2;
        } ->
      Flambda_arity.equal param_arity1 param_arity2
        && Flambda_arity.equal return_arity1 return_arity2
    | Direct _, _
    | Indirect_unknown_arity, _
    | Indirect_known_arity _, _ -> false

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
    | Indirect_unknown_arity -> [Flambda_kind.value Unknown]

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

  let equal t1 t2 =
    match t1, t2 with
    | Function call1, Function call2 ->
      equal_function_call call1 call2
    | Method { kind = kind1; obj = obj1; },
        Method { kind = kind2; obj = obj2; } ->
      Name.equal obj1 obj2
        && begin match kind1, kind2 with
           | Self, Self
           | Public, Public
           | Cached, Cached -> true
           | Self, _
           | Public, _
           | Cached, _ -> false
           end
    | C_call { alloc = alloc1; param_arity = param_arity1;
               return_arity = return_arity1; },
        C_call { alloc = alloc2; param_arity = param_arity2;
                 return_arity = return_arity2; } ->
      Pervasives.compare alloc1 alloc2 = 0
        && Flambda_arity.equal param_arity1 param_arity2
        && Flambda_arity.equal return_arity1 return_arity2
    | Function _, _
    | Method _, _
    | C_call _, _ -> false

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
    | Method _ -> [Flambda_kind.value Unknown]
    | C_call { return_arity; _ } -> return_arity
end

type inline_attribute =
  | Always_inline
  | Never_inline
  | Unroll of int
  | Default_inline

let print_inline_attribute ppf attr =
  let fprintf = Format.fprintf in
  match attr with
  | Always_inline -> fprintf ppf "Always_inline"
  | Never_inline -> fprintf ppf "Never_inline"
  | Unroll n -> fprintf ppf "@[(Unroll %d)@]" n
  | Default_inline -> fprintf ppf "Default_inline"

type specialise_attribute =
  | Always_specialise
  | Never_specialise
  | Default_specialise

let print_specialise_attribute ppf attr =
  let fprintf = Format.fprintf in
  match attr with
  | Always_specialise -> fprintf ppf "Always_specialise"
  | Never_specialise -> fprintf ppf "Never_specialise"
  | Default_specialise -> fprintf ppf "Default_specialise"

module Apply = struct
  type t = {
    func : Name.t;
    continuation : Continuation.t;
    exn_continuation : Continuation.t;
    args : Simple.t list;
    call_kind : Call_kind.t;
    dbg : Debuginfo.t;
    inline : inline_attribute;
    specialise : specialise_attribute;
  }

  let print ppf { func; continuation; exn_continuation; args; call_kind;
        dbg; inline; specialise; } =
    Format.fprintf ppf "@[(\
        (func %a)@ \
        (continuation %a)@ \
        (exn_continuation %a)@ \
        (args %a)@ \
        (call_kind %a)@ \
        (dbg %a)@ \
        (inline %a)@ \
        (specialise %a))@]"
      Name.print func
      Continuation.print continuation
      Continuation.print exn_continuation
      Simple.List.print args
      Call_kind.print call_kind
      Debuginfo.print_compact dbg
      print_inline_attribute inline
      print_specialise_attribute specialise

  let equal
        { func = func1;
          continuation = continuation1;
          exn_continuation = exn_continuation1;
          args = args1;
          call_kind = call_kind1;
          dbg = dbg1;
          inline = inline1;
          specialise = specialise1;
        }
        { func = func2;
          continuation = continuation2;
          exn_continuation = exn_continuation2;
          args = args2;
          call_kind = call_kind2;
          dbg = dbg2;
          inline = inline2;
          specialise = specialise2;
        } =
    Name.equal func1 func2
      && Continuation.equal continuation1 continuation2
      && Continuation.equal exn_continuation1 exn_continuation2
      && Misc.Stdlib.List.equal Simple.equal args1 args2
      && Call_kind.equal call_kind1 call_kind2
      && Debuginfo.equal dbg1 dbg2
      && inline1 = inline2
      && specialise1 = specialise2
end

type assign = {
  being_assigned : Mutable_variable.t;
  new_value : Simple.t;
}

module Free_var = struct
  type t = {
    var : Variable.t;
    equalities : Flambda_primitive.With_fixed_value.Set.t;
  }

  let create var =
    { var;
      equalities = Flambda_primitive.With_fixed_value.Set.empty;
    }

  let var t = t.var

  let print ppf (t : t) =
    if Flambda_primitive.With_fixed_value.Set.is_empty t.equalities then
      fprintf ppf "%a" Variable.print t.var
    else
      fprintf ppf "%a(={%a})"
        Variable.print t.var
        Flambda_primitive.With_fixed_value.Set.print t.equalities

  let free_names t = Name.Set.singleton (Name.var t.var)

  let equal { var = var1; equalities = equalities1; }
        { var = var2; equalities = equalities2; } =
    Variable.equal var1 var2
      && Flambda_primitive.With_fixed_value.Set.equal equalities1 equalities2

  let map_var t ~f =
    { t with var = f t.var; }
end

module Free_vars = struct
  (* CR mshinwell: We could make this abstract in the interface and maintain
     the reverse map too. *)
  type t = Free_var.t Var_within_closure.Map.t

  let find_by_variable t var =
    let exception Found of Var_within_closure.t in
    try
      Var_within_closure.Map.iter (fun in_closure (outer_var : Free_var.t) ->
          if Variable.equal var outer_var.var then raise (Found in_closure))
        t;
      None
    with Found in_closure -> Some in_closure

  let print ppf free_vars =
    Var_within_closure.Map.iter (fun inner_var outer_var ->
        fprintf ppf "@ %a -rename-> %a"
          Var_within_closure.print inner_var
          Free_var.print outer_var)
      free_vars

  let all_outer_variables t =
    let outer_vars = Var_within_closure.Map.data t in
    Variable.Set.of_list (List.map Free_var.var outer_vars)

  let free_names t =
    Var_within_closure.Map.fold (fun _ free_var free_names ->
        Name.Set.union free_names (Free_var.free_names free_var))
      t
      Name.Set.empty

  let equal ~equal_type:_ t1 t2 =
    Var_within_closure.Map.equal Free_var.equal t1 t2

  let map_vars t ~f =
    Var_within_closure.Map.map (fun free_var ->
        Free_var.map_var free_var ~f)
      t
end

module Trap_action = struct
  type t =
    | Push of { id : Trap_id.t; exn_handler : Continuation.t; }
    | Pop of {
        id : Trap_id.t;
        exn_handler : Continuation.t;
        take_backtrace : bool;
      }

  include Identifiable.Make (struct
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

    let equal t1 t2 = (compare t1 t2 = 0)

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
        fprintf ppf "push %a %a then "
          Trap_id.print id
          Continuation.print exn_handler
      | Pop { id; exn_handler; take_backtrace; } ->
        fprintf ppf "pop%s %a %a then "
          (if take_backtrace then " with backtrace" else "")
          Trap_id.print id
          Continuation.print exn_handler
  end)

  module Option = struct
    let print ppf = function
      | None -> ()
      | Some t -> print ppf t
  end
end

module Switch = struct
  type t =
    | Value of Continuation.t Targetint.OCaml.Map.t
    | Fabricated of Continuation.t Tag.Map.t

  include Identifiable.Make_no_hash (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Value _, Fabricated _ -> -1
      | Fabricated _, Value _ -> 1
      | Value arms1, Value arms2 ->
        Targetint.OCaml.Map.compare Continuation.compare arms1 arms2
      | Fabricated arms1, Fabricated arms2 ->
        Tag.Map.compare Continuation.compare arms1 arms2

    let equal t1 t2 = (compare t1 t2 = 0)

    let print ppf (t : t) =
      let spc = ref false in
      match t with
      | Value arms ->
        Targetint.OCaml.Map.iter (fun n l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>| %a ->@ goto %a@]"
              Targetint.OCaml.print n
              Continuation.print l)
          arms
      | Fabricated arms ->
        Tag.Map.iter (fun tag l ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>| tag %a ->@ goto %a@]"
              Tag.print tag
              Continuation.print l)
          arms
  end)
end

type invalid_term_semantics =
  | Treat_as_unreachable
  | Halt_and_catch_fire

type recursive =
  | Non_recursive
  | Recursive

type mutable_or_immutable =
  | Mutable
  | Immutable

module rec Expr : sig
  type t =
    | Let of Let.t
    | Let_mutable of Let_mutable.t
    | Let_cont of Let_cont.t
    | Apply of Apply.t
    | Apply_cont of Continuation.t * Trap_action.t option * Simple.t list
    | Switch of Name.t * Switch.t
    | Invalid of invalid_term_semantics

  val create_let : Variable.t -> Flambda_kind.t -> Named.t -> t -> t
  val create_int_switch
     : scrutinee:Name.t
    -> arms:Continuation.t Targetint.OCaml.Map.t
    -> Expr.t
  val create_int_switch'
     : scrutinee:Name.t
    -> arms:Continuation.t Targetint.OCaml.Map.t
    -> Expr.t * bool
  val create_tag_switch
     : scrutinee:Name.t
    -> arms:Continuation.t Tag.Map.t
    -> Expr.t
  val create_tag_switch'
     : scrutinee:Name.t
    -> arms:Continuation.t Tag.Map.t
    -> Expr.t * bool
  val free_names_advanced
     : ?ignore_uses_as_callee:unit
    -> ?ignore_uses_as_argument:unit
    -> ?ignore_uses_as_continuation_argument:unit
    -> ?ignore_uses_in_project_var:unit
    -> ?ignore_uses_in_apply_cont:unit
    -> t
    -> Name.Set.t
  val free_names : t -> Name.Set.t
  val used_names
     : ?ignore_uses_as_callee:unit
    -> ?ignore_uses_as_argument:unit
    -> ?ignore_uses_as_continuation_argument:unit
    -> ?ignore_uses_in_project_var:unit
    -> t
    -> Name.Set.t
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
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
  val print : Format.formatter -> t -> unit
end = struct
  include Expr

  let name_usage ?ignore_uses_as_callee
      ?ignore_uses_as_argument ?ignore_uses_as_continuation_argument
      ?ignore_uses_in_project_var ?ignore_uses_in_apply_cont
      ~all_used_names tree =
    let free = ref Name.Set.empty in
    let bound = ref Name.Set.empty in
    let free_names ids = free := Name.Set.union ids !free in
    let free_name fv = free := Name.Set.add fv !free in
    let bound_name var = bound := Name.Set.add (Name.var var) !bound in
    (* N.B. This function assumes that all bound identifiers are distinct. *)
    let rec aux (flam : t) : unit =
      match flam with
      | Apply { func; args; call_kind; _ } ->
        begin match ignore_uses_as_callee with
        | None -> free_name func
        | Some () -> ()
        end;
        begin match call_kind with
        | Function (Direct { closure_id = _; return_arity = _; })
        | Function Indirect_unknown_arity
        | Function (Indirect_known_arity {
            param_arity = _; return_arity = _; }) -> ()
        | Method { kind = _; obj; } -> free_name obj
        | C_call { alloc = _; param_arity = _; return_arity = _; } -> ()
        end;
        begin match ignore_uses_as_argument with
        | None -> free_names (Simple.List.free_names args)
        | Some () -> ()
        end
      | Let { var; free_names_of_defining_expr; free_names_of_body;
              defining_expr; body; _ } ->
        bound_name var;
        if all_used_names
            || ignore_uses_as_callee <> None
            || ignore_uses_as_argument <> None
            || ignore_uses_as_continuation_argument <> None
            || ignore_uses_in_project_var <> None
            || ignore_uses_in_apply_cont <> None
        then begin
          (* In these cases we can't benefit from the pre-computed free
             name sets. *)
          free_names
            (Named.name_usage ?ignore_uses_in_project_var defining_expr);
          aux body
        end else begin
          free_names free_names_of_defining_expr;
          free_names free_names_of_body
        end
      | Let_mutable { initial_value; body; _ } ->
        free_names (Simple.free_names initial_value);
        aux body
      | Apply_cont (_, _, args) ->
        (* CR mshinwell: why two names? *)
        begin match ignore_uses_in_apply_cont with
        | Some () -> ()
        | None ->
          match ignore_uses_as_continuation_argument with
          | None -> free_names (Simple.List.free_names args)
          | Some () -> ()
        end
      | Let_cont { handlers; body; } ->
        aux body;
        (* CR-soon mshinwell: Move the following into a separate function in
           the [Let_cont] module. *)
        begin match handlers with
        | Non_recursive { name = _; handler = { Continuation_handler.
            params; handler; _ }; } ->
          List.iter (fun param -> bound_name (Typed_parameter.var param))
            params;
          aux handler
        | Recursive handlers ->
          Continuation.Map.iter (fun _name { Continuation_handler.
            params; handler; _ } ->
              List.iter (fun param ->
                  bound_name (Typed_parameter.var param))
                params;
              aux handler)
            handlers
        end
      | Switch (var, _) -> free_name var
      | Invalid _ -> ()
    in
    aux tree;
    if all_used_names then
      !free
    else
      Name.Set.diff !free !bound

  let free_names ?ignore_uses_as_callee ?ignore_uses_as_argument
      ?ignore_uses_as_continuation_argument ?ignore_uses_in_project_var
      ?ignore_uses_in_apply_cont t =
    name_usage ?ignore_uses_as_callee ?ignore_uses_as_argument
      ?ignore_uses_as_continuation_argument ?ignore_uses_in_project_var
      ?ignore_uses_in_apply_cont ~all_used_names:false t

  let free_names t : Name.Set.t = free_names t

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

  module Create_switch (S : sig
    module T : Identifiable.S
    val create : Continuation.t T.Map.t -> Switch.t
  end) = struct
    let create_switch' ~scrutinee ~arms =
      let num_possible_values = S.T.Map.cardinal arms in
      if num_possible_values < 1 then
        invalid (), true
      else
        match S.T.Map.get_singleton arms with
        | Some (_arm, cont) ->
          let t : t = Apply_cont (cont, None, []) in
          t, true
        | None ->
          let t : t = Switch (scrutinee, S.create arms) in
          t, false
  end

  module Create_int_switch = Create_switch (struct
    module T = Targetint.OCaml
    let create map : Switch.t = Value map
  end)

  module Create_tag_switch = Create_switch (struct
    module T = Tag
    let create map : Switch.t = Fabricated map
  end)

  let create_int_switch' = Create_int_switch.create_switch'
  let create_tag_switch' = Create_tag_switch.create_switch'

  let create_int_switch ~scrutinee ~arms =
    let switch, _benefit = create_int_switch' ~scrutinee ~arms in
    switch

  let create_tag_switch ~scrutinee ~arms =
    let switch, _benefit = create_tag_switch' ~scrutinee ~arms in
    switch

  let rec free_continuations (t : t) =
    match t with
    | Let { body; _ }
    | Let_mutable { body; _ } ->
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
    | Switch (_scrutinee, Value int_switch) ->
      Continuation.Set.of_list (Targetint.OCaml.Map.data int_switch)
    | Switch (_scrutinee, Fabricated tag_switch) ->
      Continuation.Set.of_list (Tag.Map.data tag_switch)
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
        | Let_mutable { body; _ } -> aux body
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
      | Simple _ | Read_mutable _ | Prim _ | Assign _ -> ()
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

  let equal ~equal_type t1 t2 =
    match t1, t2 with
    | Let let1, Let let2 -> Let.equal ~equal_type let1 let2
    | Let_mutable lm1, Let_mutable lm2 -> Let_mutable.equal ~equal_type lm1 lm2
    | Let_cont lc1, Let_cont lc2 -> Let_cont.equal ~equal_type lc1 lc2
    | Apply apply1, Apply apply2 -> Apply.equal apply1 apply2
    | Apply_cont (cont1, trap1, args1), Apply_cont (cont2, trap2, args2) ->
      Continuation.equal cont1 cont2
        && Misc.Stdlib.Option.equal Trap_action.equal trap1 trap2
        && Simple.List.equal args1 args2
    | Switch (name1, switch1), Switch (name2, switch2) ->
      Name.equal name1 name2 && Switch.equal switch1 switch2
    | Invalid invalid1, Invalid invalid2 ->
      Pervasives.compare invalid1 invalid2 = 0
    | (Let _ | Let_mutable _ | Let_cont _ | Apply _ | Apply_cont _
        | Switch _ | Invalid _), _ -> false

  let rec print ppf (t : t) =
    match t with
    | Apply ({ func; continuation; exn_continuation; args; call_kind; inline;
               specialise; dbg; }) ->
      fprintf ppf "@[<2>(apply %a %a%a%a@ <Kr=%a Ke=%a> %a %a)@]"
        Call_kind.print call_kind
        print_inline_attribute inline
        print_specialise_attribute specialise
        Debuginfo.print_or_elide dbg
        Continuation.print continuation
        Continuation.print exn_continuation
        Name.print func
        Simple.List.print args
    | Let { var = id; kind; defining_expr = arg; body; _ } ->
      let rec letbody (ul : t) =
        match ul with
        | Let { var = id; defining_expr = arg; body; _ } ->
          fprintf ppf "@ @[<2>%a@[@ :: %a@]@ %a@]"
            Variable.print id
            Flambda_kind.print kind
            Named.print arg;
          letbody body
        | _ -> ul
      in
      fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a@[@ :: %a@]@ %a@]"
        Variable.print id
        Flambda_kind.print kind
        Named.print arg;
      let expr = letbody body in
      fprintf ppf ")@]@ %a)@]" print expr
    | Let_mutable { var; initial_value; body; contents_type; } ->
      fprintf ppf "@[<2>(let_mutable%a@ @[<2>%a@ %a@]@ %a)@]"
        Flambda_type.print contents_type
        Mutable_variable.print var
        Simple.print initial_value
        print body
    | Switch (scrutinee, sw) ->
      fprintf ppf
        "@[<v 1>(switch %a@ @[<v 0>%a@])@]"
        Name.print scrutinee Switch.print sw
    | Apply_cont (i, trap_action, []) ->
      fprintf ppf "@[<2>(%agoto@ %a)@]"
        Trap_action.Option.print trap_action
        Continuation.print i
    | Apply_cont (i, trap_action, ls) ->
      fprintf ppf "@[<2>(%aapply_cont@ %a@ %a)@]"
        Trap_action.Option.print trap_action
        Continuation.print i
        Simple.List.print ls
    | Let_cont { body; handlers; } ->
      (* Printing the same way as for [Let] is easier when debugging lifting
         passes. *)
      if !Clflags.dump_let_cont then begin
        let rec let_cont_body (ul : t) =
          match ul with
          | Let_cont { body; handlers; } ->
            fprintf ppf "@ @[<2>%a@]" Let_cont_handlers.print handlers;
            let_cont_body body
          | _ -> ul
        in
        fprintf ppf "@[<2>(let_cont@ @[<hv 1>(@[<2>%a@]"
          Let_cont_handlers.print handlers;
        let expr = let_cont_body body in
        fprintf ppf ")@]@ %a)@]" print expr
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
          Expr.print body
          (Format.pp_print_list ~pp_sep
            Let_cont_handlers.print_using_where) let_conts
      end
    | Invalid _ -> fprintf ppf "unreachable"

  let print ppf t =
    fprintf ppf "%a" print t
end and Named : sig
  type t =
    | Simple of Simple.t
    | Prim of Flambda_primitive.t * Debuginfo.t
    | Set_of_closures of Set_of_closures.t
    | Assign of assign
    | Read_mutable of Mutable_variable.t

  val free_names
     : ?ignore_uses_in_project_var:unit
    -> t
    -> Name.Set.t
  val free_symbols : t -> Symbol.Set.t
  val free_symbols_helper : Symbol.Set.t ref -> t -> unit
  val used_names
     : ?ignore_uses_in_project_var:unit
    -> t
    -> Name.Set.t
  val name_usage
     : ?ignore_uses_in_project_var:unit
    -> t
    -> Name.Set.t
  val print : Format.formatter -> t -> unit
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
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
end = struct
  include Named

  let name_usage ?ignore_uses_in_project_var (t : t) =
    match t with
    | Simple simple -> Simple.free_names simple
    | _ ->
      let free = ref Name.Set.empty in
      let free_names fvs = free := Name.Set.union fvs !free in
      begin match t with
      | Simple simple -> free_names (Simple.free_names simple)
      | Read_mutable _ -> ()
      | Assign { being_assigned = _; new_value; } ->
        free_names (Simple.free_names new_value)
      | Set_of_closures set ->
        free_names (Set_of_closures.free_names set)
      | Prim (Unary (Project_var _, x0), _dbg) ->
        begin match ignore_uses_in_project_var with
        | None -> free_names (Simple.free_names x0)
        | Some () -> ()
        end
      | Prim (Unary (_prim, x0), _dbg) ->
        free_names (Simple.free_names x0)
      | Prim (Binary (_prim, x0, x1), _dbg) ->
        free_names (Simple.free_names x0);
        free_names (Simple.free_names x1)
      | Prim (Ternary (_prim, x0, x1, x2), _dbg) ->
        free_names (Simple.free_names x0);
        free_names (Simple.free_names x1);
        free_names (Simple.free_names x2)
      | Prim (Variadic (_prim, xs), _dbg) ->
        List.iter (fun x -> free_names (Simple.free_names x)) xs
      end;
      !free

  let free_names ?ignore_uses_in_project_var t =
    name_usage ?ignore_uses_in_project_var t

  let used_names ?ignore_uses_in_project_var named =
    name_usage ?ignore_uses_in_project_var named

  let print ppf (t : t) =
    match t with
    | Simple simple -> Simple.print ppf simple
    | Set_of_closures set_of_closures ->
      Set_of_closures.print ppf set_of_closures
    | Prim (prim, dbg) ->
      fprintf ppf "@[<2>(%a%a)@]"
        Flambda_primitive.print prim
        Debuginfo.print_or_elide dbg
    | Read_mutable mut_var ->
      fprintf ppf "Read_mut(%a)" Mutable_variable.print mut_var
    | Assign { being_assigned; new_value; } ->
      fprintf ppf "@[<2>(assign@ %a@ %a)@]"
        Mutable_variable.print being_assigned
        Simple.print new_value

  let box_value name (kind : Flambda_kind.t) dbg : Named.t * Flambda_kind.t =
    let simple = Simple.name name in
    match kind with
    | Value _ -> Simple simple, kind
    | Naked_number Naked_immediate ->
      Misc.fatal_error "Not yet supported"
    | Naked_number Naked_float ->
      Prim (Unary (Box_number Naked_float, simple), dbg),
        K.value Definitely_pointer
    | Naked_number Naked_int32 ->
      Prim (Unary (Box_number Naked_int32, simple), dbg),
        K.value Definitely_pointer
    | Naked_number Naked_int64 ->
      Prim (Unary (Box_number Naked_int64, simple), dbg),
        K.value Definitely_pointer
    | Naked_number Naked_nativeint ->
      Prim (Unary (Box_number Naked_nativeint, simple), dbg),
        K.value Definitely_pointer
    | Fabricated _ ->
      Misc.fatal_error "Cannot box values of [Fabricated] kind"
    | Phantom _ ->
      (* CR mshinwell: this should probably be supported? *)
      Misc.fatal_error "Cannot box values of [Phantom] kind"

  let unbox_value name (kind : Flambda_kind.t) dbg : Named.t * Flambda_kind.t =
    let simple = Simple.name name in
    match kind with
    | Value _ -> Simple simple, kind
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
    | Fabricated _ ->
      Misc.fatal_error "Cannot box values of [Fabricated] kind"
    | Phantom _ ->
      (* CR mshinwell: this should probably be supported? *)
      Misc.fatal_error "Cannot box values of [Phantom] kind"

  let equal ~equal_type t1 t2 =
    match t1, t2 with
    | Simple s1, Simple s2 -> Simple.equal s1 s2
    | Prim (prim1, dbg1), Prim (prim2, dbg2) ->
      Flambda_primitive.equal prim1 prim2 && Debuginfo.equal dbg1 dbg2
    | Set_of_closures set1, Set_of_closures set2 ->
      Set_of_closures.equal ~equal_type set1 set2
    | Assign { being_assigned = being_assigned1; new_value = new_value1; },
        Assign { being_assigned = being_assigned2; new_value = new_value2; } ->
      Mutable_variable.equal being_assigned1 being_assigned2
        && Simple.equal new_value1 new_value2
    | Read_mutable mut1, Read_mutable mut2 ->
      Mutable_variable.equal mut1 mut2
    | (Simple _ | Prim _ | Set_of_closures _ | Assign _ | Read_mutable _), _ ->
      false
end and Let : sig
  type t = {
    var : Variable.t;
    kind : Flambda_kind.t;
    defining_expr : Named.t;
    body : Expr.t;
    free_names_of_defining_expr : Name.Set.t;
    free_names_of_body : Name.Set.t;
  }

  val map_defining_expr : Let.t -> f:(Named.t -> Named.t) -> Expr.t
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
end = struct
  include Let

  let map_defining_expr (let_expr : Let.t) ~f : Expr.t =
    let defining_expr = f let_expr.defining_expr in
    if defining_expr == let_expr.defining_expr then
      Let let_expr
    else
      let free_names_of_defining_expr =
        Named.free_names defining_expr
      in
      Let {
        var = let_expr.var;
        kind = let_expr.kind;
        defining_expr;
        body = let_expr.body;
        free_names_of_defining_expr;
        free_names_of_body = let_expr.free_names_of_body;
      }

  let equal ~equal_type
        { var = var1;
          kind = kind1;
          defining_expr = defining_expr1;
          body = body1;
          free_names_of_defining_expr = free_names_of_defining_expr1;
          free_names_of_body = free_names_of_body1;
        }
        { var = var2;
          kind = kind2;
          defining_expr = defining_expr2;
          body = body2;
          free_names_of_defining_expr = free_names_of_defining_expr2;
          free_names_of_body = free_names_of_body2;
        } =
    Variable.equal var1 var2
      && Flambda_kind.equal kind1 kind2
      && Named.equal ~equal_type defining_expr1 defining_expr2
      && Expr.equal ~equal_type body1 body2
      && Name.Set.equal free_names_of_defining_expr1
        free_names_of_defining_expr2
      && Name.Set.equal free_names_of_body1
        free_names_of_body2
end and Let_mutable : sig
  type t = {
    var : Mutable_variable.t;
    initial_value : Simple.t;
    contents_type : Flambda_type.t;
    body : Expr.t;
  }
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
end = struct
  include Let_mutable

  let equal ~equal_type
        { var = var1;
          initial_value = initial_value1;
          contents_type = contents_type1;
          body = body1;
        }
        { var = var2;
          initial_value = initial_value2;
          contents_type = contents_type2;
          body = body2;
        } =
    Mutable_variable.equal var1 var2
      && Simple.equal initial_value1 initial_value2
      && equal_type contents_type1 contents_type2
      && Expr.equal ~equal_type body1 body2
end and Let_cont : sig
  type t = {
    body : Expr.t;
    handlers : Let_cont_handlers.t;
  }
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
end = struct
  include Let_cont

  let equal ~equal_type
        { body = body1; handlers = handlers1; }
        { body = body2; handlers = handlers2; } =
    Expr.equal ~equal_type body1 body2
      && Let_cont_handlers.equal ~equal_type handlers1 handlers2
end and Let_cont_handlers : sig
  type t =
    | Non_recursive of {
        name : Continuation.t;
        handler : Continuation_handler.t;
      }
    | Recursive of Continuation_handlers.t

  val free_names : t -> Name.Set.t
  val bound_continuations : t -> Continuation.Set.t
  val free_continuations : t -> Continuation.Set.t
  type free_and_bound = {
    free : Continuation.Set.t;
    bound : Continuation.Set.t;
  }
  val free_and_bound_continuations : t -> free_and_bound
  val to_continuation_map : t -> Continuation_handlers.t
  val map : t -> f:(Continuation_handlers.t -> Continuation_handlers.t) -> t
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
  val print : Format.formatter -> t -> unit
  val print_using_where : Format.formatter -> t -> unit
end = struct
  include Let_cont_handlers

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

  let free_names (t : t) =
    Continuation.Map.fold (fun _name
          { Continuation_handler. params; handler; _ } fvs ->
        Name.Set.union fvs
          (Name.Set.union
            (Typed_parameter.List.free_names params)
            (Name.Set.diff (Expr.free_names handler)
              (Typed_parameter.List.name_set params))))
      (to_continuation_map t)
      Name.Set.empty

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

  let equal ~equal_type t1 t2 =
    match t1, t2 with
    | Non_recursive { name = name1; handler = handler1; },
        Non_recursive { name = name2; handler = handler2; } ->
      Continuation.equal name1 name2
        && Continuation_handler.equal ~equal_type handler1 handler2
    | Recursive handlers1, Recursive handlers2 ->
      Continuation_handlers.equal ~equal_type handlers1 handlers2
    | Non_recursive _, Recursive _
    | Recursive _, Non_recursive _ -> false

  let print_using_where ppf (t : t) =
    match t with
    | Non_recursive {
        name;
        handler = { params; stub; handler; is_exn_handler; };
      } ->
      fprintf ppf "@[<v 2>where %a%s%s%s@[%a@]%s =@ %a@]"
        Continuation.print name
        (if stub then " *stub*" else "")
            (if is_exn_handler then "*exn* " else "")
        (match params with [] -> "" | _ -> " (")
        Typed_parameter.List.print params
        (match params with [] -> "" | _ -> ")")
        Expr.print handler
    | Recursive handlers ->
      let first = ref true in
      fprintf ppf "@[<v 2>where rec ";
      Continuation.Map.iter (fun name
              { Continuation_handler. params; stub; is_exn_handler;
                handler; } ->
          if not !first then fprintf ppf "@ ";
          fprintf ppf "@[%s%a%s%s%s@[%a@]%s@] =@ %a"
            (if !first then "" else "and ")
            Continuation.print name
            (if stub then " *stub*" else "")
            (if is_exn_handler then "*exn* " else "")
            (match params with [] -> "" | _ -> " (")
            Typed_parameter.List.print params
            (match params with [] -> "" | _ -> ")")
            Expr.print handler;
          first := false)
        handlers;
      fprintf ppf "@]"

  let print ppf (t : t) =
    match t with
    | Non_recursive { name; handler = {
        params; stub; handler; is_exn_handler; }; } ->
      fprintf ppf "%a@ %s%s%s%a%s=@ %a"
        Continuation.print name
        (if stub then "*stub* " else "")
        (if is_exn_handler then "*exn* " else "")
        (match params with [] -> "" | _ -> "(")
        Typed_parameter.List.print params
        (match params with [] -> "" | _ -> ") ")
        Expr.print handler
    | Recursive handlers ->
      let first = ref true in
      Continuation.Map.iter (fun name
              { Continuation_handler.params; stub; is_exn_handler; handler; } ->
          if !first then begin
            fprintf ppf "@;rec "
          end else begin
            fprintf ppf "@;and "
          end;
          fprintf ppf "%a@ %s%s%s%a%s=@ %a"
            Continuation.print name
            (if stub then "*stub* " else "")
            (if is_exn_handler then "*exn* " else "")
            (match params with [] -> "" | _ -> "(")
            Typed_parameter.List.print params
            (match params with [] -> "" | _ -> ") ")
            Expr.print handler;
          first := false)
        handlers
end and Continuation_handlers : sig
  type t = Continuation_handler.t Continuation.Map.t
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
end = struct
  include Continuation_handlers

  let equal ~equal_type t1 t2 =
    Continuation.Map.equal (Continuation_handler.equal ~equal_type) t1 t2
end and Continuation_handler : sig
  type t = {
    params : Typed_parameter.t list;
    stub : bool;
    is_exn_handler : bool;
    handler : Expr.t;
  }
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
end = struct
  include Continuation_handler

  let equal ~equal_type
        { params = params1; stub = stub1; is_exn_handler = is_exn_handler1;
          handler = handler1; }
        { params = params2; stub = stub2; is_exn_handler = is_exn_handler2;
          handler = handler2; } =
    Typed_parameter.List.equal ~equal_type params1 params2
      && Pervasives.compare stub1 stub2 = 0
      && Pervasives.compare is_exn_handler1 is_exn_handler2 = 0
      && Expr.equal ~equal_type handler1 handler2
end and Set_of_closures : sig
  type t = {
    function_decls : Function_declarations.t;
    free_vars : Free_vars.t;  (* CR mshinwell: rename to "in_closure" *)
    direct_call_surrogates : Closure_id.t Closure_id.Map.t;
  }

  val create
     : function_decls:Function_declarations.t
    -> in_closure:Free_vars.t
    -> direct_call_surrogates:Closure_id.t Closure_id.Map.t
    -> t
  val free_names : t -> Name.Set.t
  val has_empty_environment : t -> bool
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
  val print : Format.formatter -> t -> unit
end = struct
  include Set_of_closures

  let create ~(function_decls : Function_declarations.t) ~in_closure:free_vars
        ~direct_call_surrogates =
    { function_decls;
      free_vars;
      direct_call_surrogates;
    }

  let has_empty_environment t =
    Var_within_closure.Map.is_empty t.free_vars

  let print ppf t =
    match t with
    | { function_decls; free_vars; direct_call_surrogates = _; } ->
      let funs ppf t =
        Closure_id.Map.iter (fun var decl ->
            Function_declaration.print var ppf decl)
          t
      in
      fprintf ppf "@[<2>(set_of_closures id=%a@ %a@ @[<2>free_vars={%a@ }@]@ \
          @[<2>direct_call_surrogates=%a@]@ \
          @[<2>set_of_closures_origin=%a@]@]"
        Set_of_closures_id.print function_decls.set_of_closures_id
        funs function_decls.funs
        Free_vars.print free_vars
        (Closure_id.Map.print Closure_id.print) t.direct_call_surrogates
        Set_of_closures_origin.print function_decls.set_of_closures_origin

  let free_names t =
    let in_decls = Function_declarations.free_names t.function_decls in
    let in_free_vars = Free_vars.free_names t.free_vars in
    Name.Set.union in_decls in_free_vars

  let equal ~equal_type
        { function_decls = function_decls1;
          free_vars = free_vars1;
          direct_call_surrogates = direct_call_surrogates1;
        }
        { function_decls = function_decls2;
          free_vars = free_vars2;
          direct_call_surrogates = direct_call_surrogates2;
        } =
    Function_declarations.equal ~equal_type function_decls1 function_decls2
      && Free_vars.equal ~equal_type free_vars1 free_vars2
      && Closure_id.Map.equal Closure_id.equal direct_call_surrogates1
        direct_call_surrogates2
end and Function_declarations : sig
  type t = {
    set_of_closures_id : Set_of_closures_id.t;
    set_of_closures_origin : Set_of_closures_origin.t;
    funs : Function_declaration.t Closure_id.Map.t;
  }

  val create : funs:Function_declaration.t Closure_id.Map.t -> t
  val find : Closure_id.t -> t -> Function_declaration.t
  val update : t -> funs:Function_declaration.t Closure_id.Map.t -> t
  val import_for_pack
     : t
    -> (Set_of_closures_id.t -> Set_of_closures_id.t)
    -> (Set_of_closures_origin.t -> Set_of_closures_origin.t)
    -> t
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
  val print : Format.formatter -> t -> unit
  val free_names : t -> Name.Set.t
end = struct
  include Function_declarations

  let create ~funs =
    let compilation_unit = Compilation_unit.get_current_exn () in
    let set_of_closures_id = Set_of_closures_id.create compilation_unit in
    let set_of_closures_origin =
      Set_of_closures_origin.create set_of_closures_id
    in
    { set_of_closures_id;
      set_of_closures_origin;
      funs;
    }

  let find cf ({ funs; set_of_closures_id = _;
          set_of_closures_origin = _ } : t) =
    Closure_id.Map.find cf funs

  let update function_decls ~funs =
    let compilation_unit = Compilation_unit.get_current_exn () in
    let set_of_closures_id = Set_of_closures_id.create compilation_unit in
    let set_of_closures_origin = function_decls.set_of_closures_origin in
    { set_of_closures_id;
      set_of_closures_origin;
      funs;
    }

  let import_for_pack function_decls
        import_set_of_closures_id import_set_of_closures_origin =
    { set_of_closures_id =
        import_set_of_closures_id function_decls.set_of_closures_id;
      set_of_closures_origin =
        import_set_of_closures_origin function_decls.set_of_closures_origin;
      funs = function_decls.funs;
    }

  let print ppf (t : t) =
    let funs ppf t =
      Closure_id.Map.iter (fun var decl ->
          Function_declaration.print var ppf decl)
        t
    in
    fprintf ppf "@[<2>(%a)(origin = %a)@]" funs t.funs
      Set_of_closures_origin.print t.set_of_closures_origin

  let free_names t =
    Closure_id.Map.fold
      (fun _closure_id (func_decl : Function_declaration.t) syms ->
        Name.Set.union syms (Function_declaration.free_names func_decl))
      t.funs
      Name.Set.empty

  let equal ~equal_type
        { set_of_closures_id = set_of_closures_id1;
          set_of_closures_origin = set_of_closures_origin1;
          funs = funs1;
        }
        { set_of_closures_id = set_of_closures_id2;
          set_of_closures_origin = set_of_closures_origin2;
          funs = funs2;
        } =
    Set_of_closures_id.equal set_of_closures_id1 set_of_closures_id2
      && Set_of_closures_origin.equal set_of_closures_origin1
        set_of_closures_origin2
      && Closure_id.Map.equal (Function_declaration.equal ~equal_type)
        funs1 funs2
end and Function_declaration : sig
  type t = {
    closure_origin : Closure_origin.t;
    continuation_param : Continuation.t;
    exn_continuation_param : Continuation.t;
    return_arity : Flambda_arity.t;
    params : Typed_parameter.t list;
    body : Expr.t;
    free_names_in_body : Name.Set.t;
    stub : bool;
    dbg : Debuginfo.t;
    inline : inline_attribute;
    specialise : specialise_attribute;
    is_a_functor : bool;
    my_closure : Variable.t;
  }

  val create
     : params:Typed_parameter.t list
    -> continuation_param:Continuation.t
    -> exn_continuation_param:Continuation.t
    -> return_arity:Flambda_arity.t
    -> my_closure:Variable.t
    -> body:Expr.t
    -> stub:bool
    -> dbg:Debuginfo.t
    -> inline:inline_attribute
    -> specialise:specialise_attribute
    -> is_a_functor:bool
    -> closure_origin:Closure_origin.t
    -> t
  val update_body : t -> body:Expr.t -> t
  val update_params : t -> params:Typed_parameter.t list -> t
  val update_params_and_body
    : t
    -> params:Typed_parameter.t list
    -> body:Expr.t
    -> t
  val used_params : t -> Variable.Set.t
  val free_names : t -> Name.Set.t
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
  val print : Closure_id.t -> Format.formatter -> t -> unit
end = struct
  include Function_declaration

  let create ~params ~continuation_param ~exn_continuation_param
        ~return_arity ~my_closure ~body ~stub ~dbg
        ~(inline : inline_attribute)
        ~(specialise : specialise_attribute) ~is_a_functor
        ~closure_origin : t =
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
      params;
      continuation_param;
      exn_continuation_param;
      return_arity;
      body;
      free_names_in_body = Expr.free_names body;
      stub;
      dbg;
      inline;
      specialise;
      is_a_functor;
      my_closure;
    }

  let update_body t ~body : t =
    { closure_origin = t.closure_origin;
      params = t.params;
      continuation_param = t.continuation_param;
      exn_continuation_param = t.exn_continuation_param;
      return_arity = t.return_arity;
      body;
      free_names_in_body = Expr.free_names body;
      stub = t.stub;
      dbg = t.dbg;
      inline = t.inline;
      specialise = t.specialise;
      is_a_functor = t.is_a_functor;
      my_closure = t.my_closure
    }

  let update_params_and_body t ~params ~body : t =
    { closure_origin = t.closure_origin;
      params;
      continuation_param = t.continuation_param;
      exn_continuation_param = t.exn_continuation_param;
      return_arity = t.return_arity;
      body;
      free_names_in_body = Expr.free_names body;
      stub = t.stub;
      dbg = t.dbg;
      inline = t.inline;
      specialise = t.specialise;
      is_a_functor = t.is_a_functor;
      my_closure = t.my_closure; (* XXX Updating that field is probably needed also *)
    }

  let update_params t ~params =
    update_params_and_body t ~params ~body:t.body

  let used_params t =
    Variable.Set.filter (fun param ->
        Name.Set.mem (Name.var param) (Expr.free_names t.body))
      (Typed_parameter.List.var_set t.params)

  let free_names t =
    let my_closure = Name.var t.my_closure in
    let params = Typed_parameter.List.free_names t.params in
    let bound = Name.Set.add my_closure params in
    Name.Set.diff t.free_names_in_body bound

  let equal ~equal_type
        { closure_origin = closure_origin1;
          continuation_param = continuation_param1;
          exn_continuation_param = exn_continuation_param1;
          return_arity = return_arity1;
          params = params1;
          body = body1;
          free_names_in_body = free_names_in_body1;
          stub = stub1;
          dbg = dbg1;
          inline = inline1;
          specialise = specialise1;
          is_a_functor = is_a_functor1;
          my_closure = my_closure1;
        }
        { closure_origin = closure_origin2;
          continuation_param = continuation_param2;
          exn_continuation_param = exn_continuation_param2;
          return_arity = return_arity2;
          params = params2;
          body = body2;
          free_names_in_body = free_names_in_body2;
          stub = stub2;
          dbg = dbg2;
          inline = inline2;
          specialise = specialise2;
          is_a_functor = is_a_functor2;
          my_closure = my_closure2;
        } =
    Closure_origin.equal closure_origin1 closure_origin2
      && Continuation.equal continuation_param1 continuation_param2
      && Continuation.equal exn_continuation_param1 exn_continuation_param2
      && Flambda_arity.equal return_arity1 return_arity2
      && Typed_parameter.List.equal ~equal_type params1 params2
      && Expr.equal ~equal_type body1 body2
      && Name.Set.equal free_names_in_body1 free_names_in_body2
      && Pervasives.compare stub1 stub2 = 0
      && Debuginfo.equal dbg1 dbg2
      && Pervasives.compare inline1 inline2 = 0
      && Pervasives.compare specialise1 specialise2 = 0
      && Pervasives.compare is_a_functor1 is_a_functor2 = 0
      && Variable.equal my_closure1 my_closure2

  let print closure_id ppf (f : t) =
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
      "@[<2>(%a%s%s%s%s@ (origin %a)@ =@ \
        fun@[<2> <%a> <exn %a>@] %a@[<2>@ :: %a@]@ ->@ @[<2>%a@])@]@ "
      Closure_id.print closure_id
      stub
      is_a_functor inline specialise
      Closure_origin.print f.closure_origin
      Continuation.print f.continuation_param
      Continuation.print f.exn_continuation_param
      Typed_parameter.List.print f.params
      Flambda_arity.print f.return_arity
      Expr.print f.body
end and Typed_parameter : sig
  type t
  val create : (Parameter.t -> Flambda_type.t -> t) Flambda_type.type_accessor
  val create_from_kind : Parameter.t -> Flambda_kind.t -> t
  val var : t -> Variable.t
  val simple : t -> Simple.t
  val ty : t -> Flambda_type.t
  val kind : t -> Flambda_kind.t
  val equalities : t -> Flambda_primitive.With_fixed_value.Set.t
  val with_type : t -> Flambda_type.t -> t
  val map_var : t -> f:(Variable.t -> Variable.t) -> t
  val map_type : t -> f:(Flambda_type.t -> Flambda_type.t) -> t
  val free_names : t -> Name.Set.t
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool
  val rename : t -> t
  module List : sig
    type nonrec t = t list
    val vars : t -> Variable.t list
    val simples : t -> Simple.t list
    val var_set : t -> Variable.Set.t
    val name_set : t -> Name.Set.t
    val equal_vars : t -> Variable.t list -> bool
    val rename : t -> t
    val arity : t -> Flambda_kind.t list
    val free_names : t -> Name.Set.t
    val print : Format.formatter -> t -> unit
    val equal
       : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
      -> t
      -> t
      -> bool
  end
(*  include Identifiable.S with type t := t *)
  val print : Format.formatter -> t -> unit
end = struct
  type t = {
    param : Parameter.t;
    equalities : Flambda_primitive.With_fixed_value.Set.t;
    (* CR mshinwell: Add an invariant check that [kind] matches [ty] *)
    ty : Flambda_type.t;
    (* [kind] is here so that you don't need an environment, which
       [Flambda_type.kind] needs, to determine the parameter's kind. *)
    kind : Flambda_kind.t;
  }

  let create ~type_of_name param ty =
    let kind = Flambda_type.kind ~type_of_name ty in
    { param;
      equalities = Flambda_primitive.With_fixed_value.Set.empty;
      ty;
      kind;
    }

  let create_from_kind param kind =
    { param;
      equalities = Flambda_primitive.With_fixed_value.Set.empty;
      ty = Flambda_type.unknown kind;
      kind;
    }

  let var t = Parameter.var t.param
  let simple t = Simple.var (var t)
  let equalities t = t.equalities
  let ty t = t.ty
  let kind t = t.kind

  let with_type t ty = { t with ty; }

  let rename t = { t with param = Parameter.rename t.param; }

  let map_var t ~f = { t with param = Parameter.map_var f t.param; }

  let map_type t ~f = { t with ty = f t.ty; }

  let free_names t =
    (* The variable within [t] is always presumed to be a binding
       occurrence, so the only free variables are those within the
       equality (if such exists) and the type. *)
    Flambda_primitive.With_fixed_value.Set.fold (fun prim from_equalities ->
        Name.Set.union from_equalities
          (Flambda_primitive.With_fixed_value.free_names prim))
      t.equalities
      (Flambda_type.free_names t.ty)

(*
  include Identifiable.Make (struct
    type nonrec t = t

    let compare { param = param1; projection = projection1; ty = ty1; }
          { param = param2; projection = projection2; ty = ty2; } =
      let c = Parameter.compare param1 param2 in
      if c <> 0 then c
      else
        let c = Projection.compare projection1 projection2 in
        if c <> 0 then c
        else
          Flambda_type.compare ...

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash t =

  end)
*)

  let equal ~equal_type
        { param = param1; equalities = equalities1; ty = ty1; kind = kind1; }
        { param = param2; equalities = equalities2; ty = ty2; kind = kind2; } =
    Parameter.equal param1 param2
      && Flambda_primitive.With_fixed_value.Set.equal equalities1 equalities2
      && equal_type ty1 ty2
      && Flambda_kind.equal kind1 kind2

  let print ppf { param; equalities; ty; kind = _; } =
    Format.fprintf ppf "@[(%a : %a%a)@]"
      Parameter.print param
      Flambda_type.print ty
      Flambda_primitive.With_fixed_value.Set.print equalities

  module List = struct
    type nonrec t = t list

    let free_names t =
      Name.Set.union_list (List.map free_names t)

    let vars t = List.map var t

    let simples t = List.map simple t

    let equal_vars t1 t2 =
      List.length t1 = List.length t2
        && List.for_all2 (fun param1 var2 -> Variable.equal (var param1) var2)
             t1 t2

    let var_set t = Variable.Set.of_list (vars t)

    let name_set t = Name.Set.of_list (List.map Name.var (vars t))

    let rename t = List.map (fun t -> rename t) t

    let arity t = List.map (fun t -> kind t) t

    let equal ~equal_type t1 t2 =
      List.compare_lengths t1 t2 = 0
        && List.for_all2 (equal ~equal_type) t1 t2

    let print ppf t =
      Format.pp_print_list ~pp_sep:Format.pp_print_space print ppf t
  end
end and Flambda_type : sig
  include Flambda_type0_intf.S with type expr := Expr.t
end = Flambda_type0.Make (Expr)

module With_free_names = struct
  type 'a t =
    | Expr : Expr.t * Name.Set.t -> Expr.t t
    | Named : Flambda_kind.t * Named.t * Name.Set.t -> Named.t t

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
