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
end

module Apply = struct
  type t = {
    func : Name.t;
    continuation : Continuation.t;
    exn_continuation : Continuation.t;
    args : Simple.t list;
    call_kind : Call_kind.t;
    dbg : Debuginfo.t;
    inline : Inline_attribute.t;
    specialise : Specialise_attribute.t;
  }

  let print ppf { func; continuation; exn_continuation; args; call_kind;
        dbg; inline; specialise; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(func %a)@]@ \
        @[<hov 1>(continuation %a)@]@ \
        @[<hov 1>(exn_continuation %a)@]@ \
        @[<hov 1>(args %a)@]@ \
        @[<hov 1>(call_kind %a)@]@ \
        @[<hov 1>(dbg %a)@]@ \
        @[<hov 1>(inline %a)@]@ \
        @[<hov 1>(specialise %a)@])@]"
      Name.print func
      Continuation.print continuation
      Continuation.print exn_continuation
      Simple.List.print args
      Call_kind.print call_kind
      Debuginfo.print_compact dbg
      Inline_attribute.print inline
      Specialise_attribute.print specialise
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

type invalid_term_semantics =
  | Treat_as_unreachable
  | Halt_and_catch_fire

let print_invalid_term_semantics ppf (sem : invalid_term_semantics) =
  match sem with
  | Treat_as_unreachable -> Format.pp_print_string ppf "Treat_as_unreachable"
  | Halt_and_catch_fire -> Format.pp_print_string ppf "Halt_and_catch_fire"

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
    | Invalid of invalid_term_semantics

  val create_let : Variable.t -> Flambda_kind.t -> Named.t -> t -> t
  val create_switch
     : scrutinee:Name.t
    -> arms:Continuation.t Discriminant.Map.t
    -> Expr.t
  val create_switch'
     : scrutinee:Name.t
    -> arms:Continuation.t Discriminant.Map.t
    -> Expr.t * bool
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

  let create_switch ~scrutinee ~arms : t =
    if Discriminant.Map.cardinal arms < 1 then begin
      Misc.fatal_error "Cannot create zero-arity [Switch]; use [Invalid]"
    end;
    Switch (scrutinee, arms)

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

  let name_usage ?ignore_uses_in_project_var (t : t) =
    match t with
    | Simple simple ->
      Name_occurrences.create_from_set_in_terms (Simple.free_names simple)
    | _ ->
      let free = ref (Name_occurrences.create ()) in
      let free_names names =
        free := Name_occurrences.union !free names
      in
      let free_names_in_term names =
        let new_free = Name_occurrences.add_set !free names In_terms in
        free := new_free
      in
      begin match t with
      | Simple simple -> free_names_in_term (Simple.free_names simple)
      | Set_of_closures set ->
        free_names (Set_of_closures.free_names set)
      | Prim (Unary (Project_var _, x0), _dbg) ->
        begin match ignore_uses_in_project_var with
        | None -> free_names_in_term (Simple.free_names x0)
        | Some () -> ()
        end
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

  let free_names ?ignore_uses_in_project_var t =
    name_usage ?ignore_uses_in_project_var t

  let used_names ?ignore_uses_in_project_var named =
    name_usage ?ignore_uses_in_project_var named

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
end and Let0 : sig
  type t

  val create
     : Flambda_kind.t
    -> Named.t
    -> Expr.t
    -> Name_permutation.t
    -> t
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
    body_without_permutation : Expr.t;
    permutation : Name_permutation.t;
    body_with_permutation_and_free_names
       : (Expr.t * Name_occurrences.t) Lazy.t;
  }

  let create kind defining_expr body perm =
    let defining_expr = Named.apply_name_permutation defining_expr perm in
    let body_with_free_names =
      lazy (
        let body = Expr.apply_name_permutation body perm in
        let free_names = Expr.free_names body in
        body, free_names)
    in
    { kind;
      defining_expr;
      body_with_free_names;
    }

  let kind t = t.kind
  let defining_expr t = t.defining_expr

  let body t = fst (Lazy.force t.body_with_free_names)
  let free_names_of_body t = snd (Lazy.force t.body_with_free_names)

  let free_names { kind = _; defining_expr; body_with_free_names; } =
    let from_defining_expr = Named.free_names defining_expr in
    let _body, from_body = Lazy.force body_with_free_names in
    Name_occurrences.union from_defining_expr from_body

  let apply_name_permutation
        { kind;
          defining_expr;
          body_without_permutation;
          permutation;
          body_with_permutation_and_free_names;
        } perm =
    let permutation = Name_permutation.compose perm permutation in
    { kind;
      defining_expr = Named.apply_name_permutation defining_expr perm;
      body_without_permutation;
      permutation;
      body_with_permutation_and_free_names =
        lazy (
          let body = Expr.apply_name_permutation body permutation in
          let free_names = Expr.free_names body in
          body, free_names);
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
     : bound_var:(Variable.t * Flambda_kind.t)
    -> defining_expr:Named.t
    -> body:Expr.t
    -> t

  val map_defining_expr : t -> f:(Named.t -> Named.t) -> Expr.t
end = struct
  include Name_abstraction.Make (Bound_variable) (Let0)

  let create ~bound_var:(var, kind) ~defining_expr ~body =
    let let0 : Let0.t =
      { kind;
        defining_expr;
        body;
        free_names_of_body = Expr.free_names body;
      }
    in
    create var let0

  let apply_name_permutation 
end and Let_cont : sig
  type t = {
    body : Expr.t;
    handlers : Let_cont_handlers.t;
  }
end = struct
  include Let_cont
end and Let_cont_handlers : sig
  type t =
    | Non_recursive of {
        name : Continuation.t;
        handler : Continuation_handler.t;
      }
    | Recursive of Continuation_handlers.t

  val free_names : t -> Name_occurrences.t
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
end and Continuation_handlers : sig
  type t = Continuation_handler.t Continuation.Map.t
end = struct
  include Continuation_handlers
end and Continuation_handler : sig
  type t = {
    params : Flambda_type.Parameters.t;
    stub : bool;
    is_exn_handler : bool;
    handler : Expr.t;
  }
  val print : Format.formatter -> t -> unit
(*
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
*)
end = struct
  include Continuation_handler

  let print_with_cache ~cache ppf { params; stub; handler; is_exn_handler; } =
    fprintf ppf "%s%s%a@ =@ %a"
      (if stub then "*stub* " else "")
      (if is_exn_handler then "*exn* " else "")
      (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
      Expr.print handler

  let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t
end and Set_of_closures : sig
  type t = {
    function_decls : Function_declarations.t;
    closure_elements : Simple.t Var_within_closure.Map.t;
    direct_call_surrogates : Closure_id.t Closure_id.Map.t;
  }

  val create
     : function_decls:Function_declarations.t
    -> closure_elements:Simple.t Var_within_closure.Map.t
    -> direct_call_surrogates:Closure_id.t Closure_id.Map.t
    -> t
  val free_names : t -> Name_occurrences.t
  val has_empty_environment : t -> bool
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
end = struct
  include Set_of_closures

  let create ~(function_decls : Function_declarations.t) ~closure_elements
        ~direct_call_surrogates =
    { function_decls;
      closure_elements;
      direct_call_surrogates;
    }

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
    let in_decls =
      Function_declarations.free_names function_decls
    in
    let in_closure_elements =
      Name_occurrences.create_from_set_in_terms
        (Simple.List.free_names
          (Var_within_closure.Map.data closure_elements))
    in
    Name_occurrences.union in_decls in_closure_elements
end and Function_declarations : sig
  type t = {
    set_of_closures_origin : Set_of_closures_origin.t;
    funs : Function_declaration.t Closure_id.Map.t;
  }

  val create : funs:Function_declaration.t Closure_id.Map.t -> t
  val find : Closure_id.t -> t -> Function_declaration.t
  val update : t -> funs:Function_declaration.t Closure_id.Map.t -> t
  val import_for_pack
     : t
    -> (Set_of_closures_origin.t -> Set_of_closures_origin.t)
    -> t
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
  val free_names : t -> Name_occurrences.t
end = struct
  include Function_declarations

  let create ~funs =
    let compilation_unit = Compilation_unit.get_current_exn () in
    let set_of_closures_origin =
      Set_of_closures_origin.create compilation_unit
    in
    { set_of_closures_origin;
      funs;
    }

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

  let free_names t =
    Closure_id.Map.fold
      (fun _closure_id (func_decl : Function_declaration.t) syms ->
        Name_occurrences.union syms (Function_declaration.free_names func_decl))
      t.funs
      (Name_occurrences.create ())
end and Function_declaration : sig
  type t = {
    closure_origin : Closure_origin.t;
    continuation_param : Continuation.t;
    exn_continuation_param : Continuation.t;
    params : Flambda_type.Parameters.t;
    body : Expr.t;
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
  val update_body : t -> body:Expr.t -> t
  val update_params : t -> params:Flambda_type.Parameters.t -> t
  val update_params_and_body
     : t
    -> params:Flambda_type.Parameters.t
    -> body:Expr.t
    -> t
  val used_params : t -> Variable.Set.t
  val free_names : t -> Name_occurrences.t
  val print_with_cache
     : cache:Printing_cache.t
    -> Closure_id.t
    -> Format.formatter
    -> t
    -> unit
  val print : Closure_id.t -> Format.formatter -> t -> unit
end = struct
  include Function_declaration

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
      body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
      free_names_in_body = Expr.free_names body;
      result_arity;
      stub;
      dbg;
      inline;
      specialise;
      is_a_functor;
      my_closure;
    }

  (* CR mshinwell: use record pattern match *)
  let update_body t ~body : t =
    { closure_origin = t.closure_origin;
      params = t.params;
      continuation_param = t.continuation_param;
      exn_continuation_param = t.exn_continuation_param;
      body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
      free_names_in_body = Expr.free_names body;
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
      body;
      code_id = Code_id.create (Compilation_unit.get_current_exn ());
      free_names_in_body = Expr.free_names body;
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
          free_names_in_body = _;
          result_arity = _;
          stub = _;
          dbg = _;
          inline = _;
          specialise = _;
          is_a_functor = _;
          my_closure = _;
        } =
    Flambda_type.Parameters.free_names params

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
