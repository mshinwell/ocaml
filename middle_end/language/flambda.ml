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

module F0 = Flambda0
module K = Flambda_kind

type assign = F0.assign
type mutable_or_immutable = Flambda0.mutable_or_immutable
type inline_attribute = F0.inline_attribute =
  | Always_inline
  | Never_inline
  | Unroll of int
  | Default_inline
type specialise_attribute = F0.specialise_attribute =
  | Always_specialise
  | Never_specialise
  | Default_specialise
type recursive = F0.recursive

module Free_var = F0.Free_var
module Let = F0.Let
module Let_cont = F0.Let_cont
module Let_mutable = F0.Let_mutable
module Switch = F0.Switch
module Trap_action = F0.Trap_action
module With_free_names = F0.With_free_names

module Call_kind = struct
  include F0.Call_kind

  let rename_variables t ~f =
    match t with
    | Function (Direct _)
    | Function Indirect_unknown_arity
    | Function (Indirect_known_arity _)
    | C_call _ -> t
    | Method { kind; obj; } ->
      Method {
        kind;
        obj = Name.map_var obj ~f;
      }
end

module Apply = struct
  include F0.Apply

  let rename_variables t ~f =
    { func = Name.map_var t.func ~f;
      continuation = t.continuation;
      exn_continuation = t.exn_continuation;
      args = List.map (fun arg -> Simple.map_var arg ~f) t.args;
      call_kind = Call_kind.rename_variables t.call_kind ~f;
      dbg = t.dbg;
      inline = t.inline;
      specialise = t.specialise;
    }
end

module Free_vars = struct
  include F0.Free_vars

  (* let clean_projections (t : Closure_id.t) = *)
  (*   Closure_id.Map.map (fun (free_var : Free_var.t) -> *)
  (*       match free_var.projection with *)
  (*       | None -> free_var *)
  (*       | Some projection -> *)
  (*         let from = Projection.projecting_from projection in *)
  (*         if Closure_id.Map.mem from t then free_var *)
  (*         else ({ free_var with projection = None; } : Free_var.t)) *)
  (*     t *)
end

module Reachable = struct
  type t =
    | Reachable of Flambda0.Named.t
    | Invalid of Flambda0.invalid_term_semantics

  let reachable named = Reachable named

  let invalid () =
    if !Clflags.treat_invalid_code_as_unreachable then
      Invalid Treat_as_unreachable
    else
      Invalid Halt_and_catch_fire

  let print ppf t =
    match t with
    | Reachable named -> Flambda0.Named.print ppf named
    | Invalid sem -> Flambda0.print_invalid_term_semantics ppf sem
end

module Typed_parameter = struct
  include Flambda0.Typed_parameter
end

module rec Expr : sig
  include module type of F0.Expr

  val invariant : Invariant_env.t -> t -> unit
  val if_then_else
     : scrutinee:Name.t
    -> if_true:Continuation.t
    -> if_false:Continuation.t
    -> t
  val no_effects_or_coeffects : t -> bool
  val make_closure_declaration
     : id:Variable.t
    -> free_variable_kind:(Variable.t -> K.t)
    -> body:t
    -> params:Typed_parameter.t list
    -> continuation_param:Continuation.t
    -> exn_continuation_param:Continuation.t
    (* CR mshinwell: update comment. *)
    -> stub:bool
    -> continuation:Continuation.t
    -> return_arity:Flambda_arity.t
    -> dbg:Debuginfo.t
    -> t
  val toplevel_substitution
     : Variable.t Variable.Map.t
    -> t
    -> t
  val description_of_toplevel_node : t -> string
  val bind
     : bindings:(Variable.t * K.t * Named.t) list
    -> body:t
    -> t
  val all_defined_continuations_toplevel : t -> Continuation.Set.t
  val count_continuation_uses_toplevel : t -> int Continuation.Map.t
  type with_wrapper =
    | Unchanged of { handler : Continuation_handler.t; }
    | With_wrapper of {
        new_cont : Continuation.t;
        new_handler : Continuation_handler.t;
        wrapper_handler : Continuation_handler.t;
      }
  val build_let_cont_with_wrappers
     : body:t
    -> recursive:Flambda0.recursive
    -> with_wrappers:with_wrapper Continuation.Map.t
    -> t
  module Iterators : sig
    val iter : (t -> unit) -> (Named.t -> unit) -> t -> unit
    val iter_subexpressions : (t -> unit) -> (Named.t -> unit) -> t -> unit
    val iter_expr : (t -> unit) -> t -> unit
    val iter_named : (Named.t -> unit) -> t -> unit
    val iter_all_immutable_let_and_let_rec_bindings
       : t
      -> f:(Variable.t -> Named.t -> unit)
      -> unit
    val iter_sets_of_closures : (Set_of_closures.t -> unit) -> t -> unit
    val iter_function_bodies
       : t
      -> f:(continuation_arity:Flambda_arity.t
        -> Continuation.t
        -> t
        -> unit)
      -> unit
    val iter_lets
        : t
      -> for_defining_expr:(Variable.t -> K.t -> Named.t -> unit)
      -> for_last_body:(t -> unit)
      -> for_each_let:(t -> unit)
      -> unit
    module Toplevel_only : sig 
      val iter : (t -> unit) -> (Named.t -> unit) -> t -> unit
      val iter_all_immutable_let_and_let_rec_bindings
         : t
        -> f:(Variable.t -> Named.t -> unit)
        -> unit
    end
  end
  module Mappers : sig
    val map : (t -> t) -> (Named.t -> Named.t) -> t -> t
    val map_lets
       : t
      -> for_defining_expr:(Variable.t -> K.t -> Named.t -> Named.t)
      -> for_last_body:(t -> t)
      -> after_rebuild:(t -> t)
      -> t
    val map_subexpressions
       : (t -> t)
      -> (Variable.t -> Named.t -> Named.t)
      -> t
      -> t
    val map_expr : (t -> t) -> t -> t
    val map_named : (Named.t -> Named.t) -> t -> t
    val map_named_with_id : (Variable.t -> Named.t -> Named.t) -> t -> t
    val map_symbols : t -> f:(Symbol.t -> Symbol.t) -> t
    val map_sets_of_closures
       : t
      -> f:(Set_of_closures.t -> Set_of_closures.t)
      -> t
    val map_apply : t -> f:(Apply.t -> Apply.t) -> t
    val map_all_immutable_let_and_let_rec_bindings
       : t
      -> f:(Variable.t -> Named.t -> Named.t)
      -> t
    val map_function_bodies
       : ?ignore_stubs:unit
      -> t
      -> f:(continuation_arity:Flambda_arity.t
        -> Continuation.t
        -> t
        -> t)
      -> t
    module Toplevel_only : sig 
      val map : (t -> t) -> (Named.t -> Named.t) -> t -> t
      val map_expr : (t -> t) -> t -> t
      val map_named : (Named.t -> Named.t) -> t -> t
      val map_sets_of_closures
         : t
        -> f:(Set_of_closures.t -> Set_of_closures.t)
        -> t
    end
  end
  module Folders : sig
    val fold_lets_option
        : t
      -> init:'a
      -> for_defining_expr:(
          'a
        -> Variable.t
        -> K.t
        -> Named.t
        -> 'a
          * (Variable.t * K.t * Named.t) list
          * Variable.t
          * K.t
          * Reachable.t)
      -> for_last_body:('a -> t -> t * 'b)
      (* CR-someday mshinwell: consider making [filter_defining_expr]
        optional *)
      -> filter_defining_expr:(
          'b
        -> Variable.t
        -> Flambda_kind.t
        -> Named.t
        -> Name_occurrences.t
        -> 'b * Variable.t * Flambda_kind.t * (Named.t option))
      -> t * 'b
  end
end = struct
  include F0.Expr

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
    | Let_mutable { body; _ } -> no_effects_or_coeffects body
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
    | Let_mutable _ -> "let_mutable"
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
      | Let_mutable { body; _ } -> f body
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
          | Simple _ | Read_mutable _ | Assign _ | Prim _ -> ())
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
            | Let_mutable mutable_let ->
              let new_body = aux mutable_let.body in
              if new_body == mutable_let.body then
                tree
              else
                Let_mutable { mutable_let with body = new_body }
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
          | Simple _ | Read_mutable _ | Assign _ | Prim _ -> named
          | Set_of_closures ({ function_decls; free_vars;
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
                  Set_of_closures.create ~function_decls ~in_closure:free_vars
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
      | Let_mutable mutable_let ->
        let new_body = f mutable_let.body in
        if new_body == mutable_let.body then
          tree
        else
          Let_mutable { mutable_let with body = new_body }
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
          | (Set_of_closures _ | Read_mutable _ | Prim _ | Assign _)
              as named ->
            named)
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
          | (Simple _ | Assign _ | Prim _ | Read_mutable _) as named -> named)
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
            | (Simple _ | Read_mutable _ | Prim _ | Assign _) as named -> named)
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
        | Let { var; kind; defining_expr; body; _ } ->
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
          end
        | t ->
          let last_body, acc = for_last_body acc t in
          finish ~last_body ~acc ~rev_lets
      in
      loop t ~acc:init ~rev_lets:[]
  end

  (* CR-soon mshinwell: this should use the explicit ignore functions *)
  let toplevel_substitution sb tree =
    let sb' = sb in
    let sb v = try Variable.Map.find v sb with Not_found -> v in
    let substitute_type ty =
      Flambda_type.rename_variables ty sb'
    in
    let substitute_params_list params =
      List.map (fun param ->
          Typed_parameter.map_type param ~f:(fun ty ->
            substitute_type ty))
        params
    in
    let substitute_args_list args =
      List.map (fun arg -> Simple.map_var arg ~f:sb) args
    in
    let aux (expr : t) : t =
      (* Note that this does not have to traverse subexpressions; the call to
         [map_toplevel] below will deal with that. *)
      match expr with
      | Let_mutable mutable_let ->
        let initial_value = Simple.map_var mutable_let.initial_value ~f:sb in
        Let_mutable { mutable_let with initial_value }
      | Apply apply ->
        Apply (Apply.rename_variables apply ~f:sb)
      | Switch (cond, sw) ->
        let cond = Name.map_var cond ~f:sb in
        Switch (cond, sw)
      | Apply_cont (cont, trap_action, args) ->
        let args = substitute_args_list args in
        Apply_cont (cont, trap_action, args)
      | Let_cont { body; handlers; } ->
        let f handlers =
          Continuation.Map.map (fun (handler : Continuation_handler.t)
                  : Continuation_handler.t ->
              { handler with
                params = substitute_params_list handler.params;
              })
            handlers
        in
        Let_cont {
          body;
          handlers = Let_cont_handlers.map handlers ~f;
        }
      | Let _ | Invalid _ -> expr
    in
    let aux_named (named : Named.t) : Named.t =
      match named with
      | Simple simple ->
        let simple' = Simple.map_var simple ~f:sb in
        if simple == simple' then named
        else Simple simple'
      | Read_mutable _ -> named
      | Assign { being_assigned; new_value; } ->
        let new_value = Simple.map_var new_value ~f:sb in
        Assign { being_assigned; new_value; }
      | Set_of_closures set_of_closures ->
        let function_decls =
          Function_declarations.map_parameter_types
            set_of_closures.function_decls
            ~f:(fun ty -> substitute_type ty)
        in
        let set_of_closures =
          Set_of_closures.create
            ~function_decls
            ~in_closure:
              (Var_within_closure.Map.map (fun (free_var : Free_var.t) ->
                  { free_var with var = sb free_var.var; })
                set_of_closures.free_vars)
            ~direct_call_surrogates:set_of_closures.direct_call_surrogates
        in
        Set_of_closures set_of_closures
      | Prim (prim, dbg) ->
        Prim (Flambda_primitive.rename_variables prim ~f:sb, dbg)
    in
    if Variable.Map.is_empty sb' then tree
    else Mappers.Toplevel_only.map aux aux_named tree

  let make_closure_declaration ~id
        ~(free_variable_kind : Variable.t -> K.t) ~body ~params
        ~continuation_param ~exn_continuation_param ~stub ~continuation
        ~return_arity ~dbg : Expr.t =
    let my_closure = Variable.rename id in
    let closure_id = Closure_id.wrap id in
    let free_variables =
      Name_occurrences.variables_only (Expr.free_names body)
    in
    let in_types = Name_occurrences.in_types free_variables in
    let in_debug_only = Name_occurrences.in_debug_only free_variables in
    if not (Name.Set.is_empty in_types) then begin
      Misc.fatal_errorf "Cannot create closure declaration with free \
          [In_types] names %a: %a"
        Name.Set.print in_types
        Expr.print body
    end;
    if not (Name.Set.is_empty in_debug_only) then begin
      Misc.fatal_errorf "Cannot create closure declaration with free \
          [In_debug_only] names %a: %a"
        Name.Set.print in_debug_only
        Expr.print body
    end;
    let free_variables =
      Name.set_to_var_set (Name_occurrences.in_terms free_variables)
    in
    let sb =
      Variable.Set.fold (fun id sb ->
          Variable.Map.add id (Variable.rename id) sb)
        free_variables
        Variable.Map.empty
    in
    (* CR-soon mshinwell: try to eliminate this [toplevel_substitution].  This
       function is only called from [Simplify], so we should be able
       to do something similar to what happens in [Inlining_transforms] now. *)
    let body = toplevel_substitution sb body in
    let param_set = Typed_parameter.List.var_set params in
    let vars_within_closure =
      Variable.Map.of_set Var_within_closure.wrap
        (Variable.Set.diff free_variables param_set)
    in
    let body =
      Variable.Map.fold (fun var var_within_closure body ->
          let new_var = Variable.Map.find var sb in
          let kind : K.t = free_variable_kind var in
          let projection : Named.t =
            let my_closure = Simple.var my_closure in
            Prim (Unary (Project_var (closure_id, var_within_closure),
              my_closure), dbg)
          in
          match kind with
          | Value ->
            Expr.create_let new_var kind projection body
          | Naked_number _ ->
            let boxed_var = Variable.rename new_var in
            let unbox, boxed_kind =
              Named.unbox_value (Name.var boxed_var) kind dbg
            in
            Expr.create_let boxed_var boxed_kind projection
              (Expr.create_let new_var kind unbox body)
          | Fabricated | Phantom _ ->
            Misc.fatal_error "Not yet implemented" (* XXX *) )
        vars_within_closure body
    in
    let subst var = Variable.Map.find var sb in
    let subst_param param = Typed_parameter.map_var param ~f:subst in
    let function_declaration : Function_declaration.t =
      Function_declaration.create
        ~params:(List.map subst_param params)
        ~continuation_param ~exn_continuation_param
        ~return_arity ~body ~stub ~dbg:Debuginfo.none
        ~inline:Default_inline ~specialise:Default_specialise
        ~is_a_functor:false
        ~closure_origin:(Closure_origin.create closure_id)
        ~my_closure
    in
    let in_closure, boxed_vars =
      Variable.Map.fold (fun id var_within_closure (in_closure, boxed_vars) ->
          let var, boxed_vars =
            let kind = free_variable_kind id in
            match kind with
            | Value ->
              id, boxed_vars
            | Naked_number _ ->
              let boxed_var = Variable.rename id in
              let box, boxed_kind = Named.box_value (Name.var id) kind dbg in
              boxed_var, (boxed_var, boxed_kind, box) :: boxed_vars
            | Phantom _ | Fabricated ->
              Misc.fatal_error "Not yet implemented" (* XXX *)
          in
          let free_var = Free_var.create var in
          let in_closure =
            Var_within_closure.Map.add var_within_closure free_var in_closure
          in
          in_closure, boxed_vars)
        vars_within_closure
        (Var_within_closure.Map.empty, [])
    in
    let current_compilation_unit = Compilation_unit.get_current_exn () in
    let set_of_closures_var =
      Variable.create "set_of_closures" ~current_compilation_unit
    in
    let set_of_closures =
      let function_decls =
        Function_declarations.create
          ~funs:(Closure_id.Map.singleton closure_id function_declaration)
      in
      Set_of_closures.create ~function_decls ~in_closure
        ~direct_call_surrogates:Closure_id.Map.empty
    in
    let project_closure : Named.t =
      let set_of_closures = Simple.var set_of_closures_var in
      Prim (Unary (Project_closure closure_id, set_of_closures), dbg)
    in
    let project_closure_var =
      Variable.create "project_closure" ~current_compilation_unit
    in
    let body =
      Expr.create_let set_of_closures_var (K.value ())
        (Set_of_closures set_of_closures)
        (Expr.create_let project_closure_var (K.value ())
          project_closure
          (Apply_cont (continuation, None, [Simple.var project_closure_var])))
    in
    Expr.bind ~bindings:boxed_vars ~body

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
        | Let _ | Let_mutable _ | Let_cont _ | Invalid _ -> ())
      (fun _named -> ())
      expr;
    Continuation.Tbl.to_map counts

  let invariant env expr =
    let module E = Invariant_env in
    let unbound_continuation cont reason =
      Misc.fatal_errorf "Unbound continuation %a in %s: %a"
        Continuation.print cont
        reason
        print expr
    in
    let add_typed_parameters t params =
      List.fold_left (fun t param ->
          let var = Typed_parameter.var param in
          let kind = Typed_parameter.kind param in
          E.add_variable t var kind)
        t
        params
    in
    let rec loop env (t : t) : unit =
      match t with
      | Let { var; kind; defining_expr; body; _ } ->
        let named_kind =
          match Named.invariant env defining_expr with
          | Singleton kind -> Some kind
          | Unit -> Some (K.value ())
          | Never_returns -> None
        in
        begin match named_kind with
        | None -> ()
        | Some named_kind ->
          if not (K.compatible_allowing_phantom named_kind ~if_used_at:kind)
          then begin
            Misc.fatal_errorf "[Let] expression inferred kind (%a)@ is not \
                compatible with the annotated kind (%a);@ [Let] \
                expression is:@ %a"
              K.print named_kind
              K.print kind
              print t
          end
        end;
        let env = E.add_variable env var kind in
        loop env body
      | Let_mutable _ ->
        Misc.fatal_errorf "Let_mutable not yet supported"
(* { var; initial_value; body; contents_type; } ->
        let initial_value_kind = E.kind_of_simple env initial_value in
        let contents_kind =
          Flambda_type.kind ~type_of_name:
            (fun ?local_env (id : Flambda_type.Name_or_export_id.t) ->
              ignore local_env;
              match id with
              | Name name -> E.type_of_name_option env name
              | Export_id _ -> None)
            contents_type
        in
        if not (K.equal initial_value_kind contents_kind) then begin
          Misc.fatal_errorf "Initial value of [Let_mutable] term has kind %a \
              whereas %a was expected: %a"
            K.print initial_value_kind
            Flambda_type.print contents_type
            print t
        end;
        let contents_ty = Flambda_type.unknown contents_kind in
        let env = E.add_mutable_variable env var contents_ty in
        loop env body
*)
      | Let_cont { body; handlers; } ->
        let handler_stack = E.Continuation_stack.var () in
        let env =
          match handlers with
          | Non_recursive { name; handler; } ->
            let kind : E.continuation_kind =
              if handler.is_exn_handler then Exn_handler else Normal
            in
            let params = handler.params in
            let arity = Typed_parameter.List.arity params in
            let env = add_typed_parameters env params in
            let env = E.set_current_continuation_stack env handler_stack in
            loop env handler.handler;
            E.add_continuation env name arity kind handler_stack
          | Recursive handlers ->
            let recursive_env =
              Continuation.Map.fold
                (fun cont (handler : Continuation_handler.t) env ->
                  let arity = Typed_parameter.List.arity handler.params in
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
                let env = add_typed_parameters recursive_env params in
                let env = E.set_current_continuation_stack env handler_stack in
                loop env handler;
                ignore (stub : bool))
              handlers;
            recursive_env
        in
        loop env body
      | Apply_cont (cont, trap_action, args) ->
        let args_arity = List.map (fun arg -> E.kind_of_simple env arg) args in
        let arity, kind, cont_stack =
          match E.find_continuation_opt env cont with
          | Some result -> result
          | None -> unbound_continuation cont "[Apply_cont] term"
        in
        let stack = E.current_continuation_stack env in
        E.Continuation_stack.unify cont stack cont_stack;
        if not (Flambda_arity.compatible args_arity ~if_used_at:arity)
        then begin
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
(* XXX Move into [Switch]
        if Targetint.OCaml.Map.cardinal arms < 1 then begin
          Misc.fatal_errorf "Empty switch:@ %a" print t
        end;
*)
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
        Switch.iter switch ~f:check
      | Invalid _ -> ()
    in
    loop env expr
end and Named : sig
  include module type of F0.Named

  val invariant
     : Invariant_env.t
    -> t
    -> Flambda_primitive.result_kind
  val toplevel_substitution
     : Variable.t Variable.Map.t
    -> t
    -> t
  val no_effects_or_coeffects : t -> bool
  val at_most_generative_effects : t -> bool
  val dummy_value : Flambda_kind.t -> t
  module Iterators : sig
    val iter : (Expr.t -> unit) -> (t -> unit) -> t -> unit
    val iter_named : (t -> unit) -> t -> unit
    module Toplevel_only : sig
      val iter : (Expr.t -> unit) -> (t -> unit) -> t -> unit
    end
  end
end = struct
  include F0.Named

  let no_effects_or_coeffects (t : t) =
    match t with
    | Simple _ -> true
    | Prim (prim, _) -> Flambda_primitive.no_effects_or_coeffects prim
    | Set_of_closures _ -> true
    | Assign _ | Read_mutable _ -> false

  let at_most_generative_effects (t : t) =
    match t with
    | Simple _ -> true
    | Prim (prim, _) -> Flambda_primitive.at_most_generative_effects prim
    | Set_of_closures _ -> true
    | Assign _ | Read_mutable _ -> false

  let dummy_value (kind : K.t) : t =
    let simple = 
      match kind with
      | Value | Phantom (_, Value) -> Simple.const_zero
      | Naked_number Naked_immediate
      | Phantom (_, Naked_number Naked_immediate) ->
        Simple.const (Untagged_immediate Immediate.zero)
      | Naked_number Naked_float
      | Phantom (_, Naked_number Naked_float) ->
        Simple.const (Naked_float Numbers.Float_by_bit_pattern.zero)
      | Naked_number Naked_int32
      | Phantom (_, Naked_number Naked_int32) ->
        Simple.const (Naked_int32 Int32.zero)
      | Naked_number Naked_int64
      | Phantom (_, Naked_number Naked_int64) ->
        Simple.const (Naked_int64 Int64.zero)
      | Naked_number Naked_nativeint
      | Phantom (_, Naked_number Naked_nativeint) ->
        Simple.const (Naked_nativeint Targetint.zero)
      | Fabricated | Phantom (_, Fabricated) ->
        Simple.discriminant Discriminant.zero
    in
    Simple simple

  (* CR mshinwell: Implement this properly. *)
  let toplevel_substitution sb (t : t) =
    let var = Variable.create "subst" in
    let cont = Continuation.create () in
    let expr : Expr.t =
      Expr.create_let var
        (K.value () (* arbitrary *)) t
        (Apply_cont (cont, None, []))
    in
    match Expr.toplevel_substitution sb expr with
    | Let let_expr -> let_expr.defining_expr
    | _ -> assert false

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
      | Read_mutable mut_var ->
        Singleton (E.kind_of_mutable_variable env mut_var)
      | Assign { being_assigned; new_value; } ->
        let being_assigned_kind =
          E.kind_of_mutable_variable env being_assigned
        in
        let new_value_kind = E.kind_of_simple env new_value in
        if not (K.equal new_value_kind being_assigned_kind) then begin
          Misc.fatal_errorf "Cannot put value %a of kind %a into mutable \
              variable %a with contents kind %a"
            Simple.print new_value
            K.print new_value_kind
            Mutable_variable.print being_assigned
            K.print being_assigned_kind
        end;
        Singleton (K.unit ())
      | Set_of_closures set_of_closures ->
        Set_of_closures.invariant env set_of_closures;
        Singleton (K.fabricated ())
      | Prim (prim, dbg) ->
        primitive_invariant env prim;
        ignore (dbg : Debuginfo.t);
        Flambda_primitive.result_kind prim
    with Misc.Fatal_error ->
      Misc.fatal_errorf "(during invariant checks) Context is:@ %a" print t
end and Let_cont_handlers : sig
  include module type of F0.Let_cont_handlers

  val no_effects_or_coeffects : t -> bool
end = struct
  include F0.Let_cont_handlers

  let no_effects_or_coeffects (t : t) =
    match t with
    | Non_recursive { name = _; handler; } ->
      Continuation_handler.no_effects_or_coeffects handler
    | Recursive handlers ->
      Continuation_handlers.no_effects_or_coeffects handlers
end and Continuation_handler : sig
  include module type of F0.Continuation_handler

  val no_effects_or_coeffects : t -> bool
  val param_arity : t -> Flambda_arity.t
end = struct
  include F0.Continuation_handler

  let no_effects_or_coeffects t = Expr.no_effects_or_coeffects t.handler
  let param_arity t = List.map Typed_parameter.kind t.params
end and Continuation_handlers : sig
  include module type of F0.Continuation_handlers

  val no_effects_or_coeffects : t -> bool
end = struct
  include F0.Continuation_handlers

  let no_effects_or_coeffects t =
    Continuation.Map.for_all (fun _cont handler ->
        Continuation_handler.no_effects_or_coeffects handler)
      t
end and Set_of_closures : sig
  include module type of F0.Set_of_closures

  val invariant : Invariant_env.t -> t -> unit

  val variables_bound_by_the_closure : t -> Var_within_closure.Set.t

  (* CR mshinwell: swap parameters and add "_exn" suffix or similar *)
  val find_free_variable : Var_within_closure.t -> t -> Variable.t

  module Iterators : sig
    val iter_function_bodies
       : t
      -> f:(continuation_arity:Flambda_arity.t
        -> Continuation.t
        -> Expr.t
        -> unit)
      -> unit
  end

  module Mappers : sig
    val map_symbols : t -> f:(Symbol.t -> Symbol.t) -> t
    val map_function_bodies
       : ?ignore_stubs:unit
      -> t
      -> f:(continuation_arity:Flambda_arity.t
        -> Continuation.t
        -> Expr.t
        -> Expr.t)
      -> t
  end
  module Folders : sig
    val fold_function_decls_ignoring_stubs
       : t
      -> init:'a
      -> f:(closure_id:Closure_id.t
        -> function_decl:Function_declaration.t
        -> 'a
        -> 'a)
      -> 'a
  end
end = struct
  include F0.Set_of_closures

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
          f ~continuation_arity:function_decl.return_arity
            function_decl.continuation_param function_decl.body)
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
                f ~continuation_arity:function_decl.return_arity
                  function_decl.continuation_param body
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
        ({ function_decls; free_vars; direct_call_surrogates = _; } as t) =
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
          let { Function_declaration.params; body; stub; dbg; my_closure;
                continuation_param = return_cont;
                exn_continuation_param; return_arity; _ } =
            function_decl
          in
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
          (* Check that every variable free in the body of the function is
             either the distinguished "own closure" variable or one of the
             function's parameters. *)
          let allowed_free_variables =
            Variable.Set.add my_closure
              (Typed_parameter.List.var_set params)
          in
          let parameters_with_kinds =
            List.map (fun param ->
                let var = Typed_parameter.var param in
                let kind = Typed_parameter.kind param in
                var, kind)
              params
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
*)
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
  include module type of F0.Function_declarations

  val find_declaration_variable : Closure_id.t -> t -> Variable.t
  val fun_vars_referenced_in_decls
     : t
    -> backend:(module Backend_intf.S)
    -> Closure_id.Set.t Closure_id.Map.t
  val closures_required_by_entry_point
     : entry_point:Closure_id.t
    -> backend:(module Backend_intf.S)
    -> t
    -> Closure_id.Set.t
  val all_functions_parameters : t -> Variable.Set.t
  val contains_stub : t -> bool
  val map_parameter_types : t -> f:(Flambda_type.t -> Flambda_type.t) -> t
  val freshen : t -> Freshening.t -> t * Freshening.t
end = struct
  include F0.Function_declarations

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
        Variable.Set.union set (Typed_parameter.List.var_set params))
      function_decls.funs Variable.Set.empty

  let contains_stub (fun_decls : t) =
    let number_of_stub_functions =
      Closure_id.Map.cardinal
        (Closure_id.Map.filter
          (fun _ ({ stub; _ } : Function_declaration.t) -> stub)
          fun_decls.funs)
    in
    number_of_stub_functions > 0

  let map_parameter_types t ~f =
    let funs =
      Closure_id.Map.map (fun (decl : Function_declaration.t) ->
          Function_declaration.map_parameter_types decl ~f)
        t.funs
    in
    update t ~funs

  let freshen (func_decls : t) freshening =
    let freshen_func_decl (func_decl : Function_declaration.t)
          freshening =
      let params_rev, freshening =
        List.fold_left (fun (params_rev, freshening) param ->
            let var = Typed_parameter.var param in
            let fresh_var, freshening =
              Freshening.add_variable freshening var
            in
            let param =
              (* CR mshinwell: Add [Typed_parameter.replace_var] *)
              Typed_parameter.map_var ~f:(fun _var -> fresh_var) param
            in
            param :: params_rev, freshening)
          ([], freshening)
          func_decl.params
      in
      let params = List.rev params_rev in
      (* Since all parameters are distinct, even between functions, we can
         just use a single substitution. *)
      (* CR mshinwell: Why does this [toplevel_substitution] need to happen?
         Can't this freshening be put into the environment and then applied
         as needed? *)
      let body =
        Expr.toplevel_substitution (Freshening.variable_substitution freshening)
          func_decl.body
      in
      let function_decl =
        Function_declaration.update_params_and_body func_decl ~params ~body
      in
      function_decl, freshening
    in
    let funs, freshening =
      Closure_id.Map.fold (fun closure_id func_decl (funs, freshening) ->
          let func_decl, freshening =
            freshen_func_decl func_decl freshening
          in
          Closure_id.Map.add closure_id func_decl funs, freshening)
        func_decls.funs
        (Closure_id.Map.empty, freshening)
    in
    let function_decls = update func_decls ~funs in
    function_decls, freshening
end and Function_declaration : sig
  include module type of F0.Function_declaration

  val function_arity : t -> int
  (* val num_variables_in_closure *)
  (*    : t *)
  (*   -> function_decls:Function_declarations.t *)
  (*   -> int *)
  val map_parameter_types : t -> f:(Flambda_type.t -> Flambda_type.t) -> t
end = struct
  include F0.Function_declaration

  let function_arity t = List.length t.params

  let map_parameter_types t ~f =
    let params =
      List.map (fun param -> Typed_parameter.map_type param ~f) t.params
    in
    Function_declaration.update_params t ~params
end
