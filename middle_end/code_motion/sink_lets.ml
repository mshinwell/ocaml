(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2017 OCamlPro SAS                                    *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module W = Flambda.With_free_names

let rec join_continuation_stacks stack1 stack2 =
  match stack1, stack2 with
  | [], [] | _, [] | [], _ -> []
  | (cont1, rec1)::stack1, (cont2, _rec2)::stack2 ->
    if Continuation.equal cont1 cont2 then
      match (rec1 : Flambda.recursive) with
      | Non_recursive ->
        (cont1, rec1) :: join_continuation_stacks stack1 stack2
      | Recursive -> []  (* Don't sink lets into recursive continuations. *)
    else []

module State : sig
  type t

  val create : unit -> t

  val should_sink_let : t -> Variable.t -> bool

  val sunken_lets_for_handler
     : t
    -> Continuation.t
    -> (Variable.t * Flambda.Named.t W.t) list

  val add_candidates_to_sink
     : t
    -> sink_into:(Continuation.t * Flambda.recursive) list
    -> candidates_to_sink:Variable.Set.t
    -> t

  val add_candidates_to_sink_from_handler_state
     : t
    -> current_continuation:(Continuation.t * Flambda.recursive)
    -> handler_state:t
    -> except:Variable.Set.t
    -> t

  val is_candidate_to_sink
     : t
    -> Variable.t
    -> (Continuation.t * Flambda.recursive) list option

  val remove_candidate_to_sink
     : t
    -> Variable.t
    -> (Continuation.t * Flambda.recursive) list option * t

  val sink_let
     : t
    -> Variable.t
    -> sink_into:Continuation.t
    -> defining_expr:Flambda.Named.t W.t
    -> t

  val add_to_sink_from_state : t -> from:t -> t
end = struct
  type t = {
    to_sink :
      (Variable.t * Flambda.Named.t W.t) list Continuation.Map.t;
    variables_to_sink : Variable.Set.t;
    candidates_to_sink :
      (Continuation.t * Flambda.recursive) list Variable.Map.t;
  }

  let create () =
    { to_sink = Continuation.Map.empty;
      variables_to_sink = Variable.Set.empty;
      candidates_to_sink = Variable.Map.empty;
    }

  let should_sink_let t var =
    Variable.Set.mem var t.variables_to_sink

  let sunken_lets_for_handler t cont =
    match Continuation.Map.find cont t.to_sink with
    | exception Not_found -> []
    | to_sink -> to_sink

  let add_candidates_to_sink t ~sink_into ~candidates_to_sink =
    let candidates_to_sink =
      Variable.Set.fold (fun candidate candidates_to_sink ->
          Variable.Map.add candidate sink_into
            candidates_to_sink)
        candidates_to_sink
        t.candidates_to_sink
    in
    { t with
      candidates_to_sink;
    }

  let add_candidates_to_sink_from_handler_state t ~current_continuation
        ~handler_state ~except =
    let candidates_to_sink =
      Variable.Map.filter_map (fun var sink_to ->
          if Variable.Set.mem var except then
            None
          else begin
            Some (current_continuation :: sink_to)
          end)
        handler_state.candidates_to_sink
    in
    let candidates_to_sink =
      Variable.Map.union (fun _var sink_to1 sink_to2 ->
          Some (join_continuation_stacks sink_to1 sink_to2))
        candidates_to_sink
        t.candidates_to_sink
    in
    { t with
      candidates_to_sink;
    }

  let is_candidate_to_sink t var =
    match Variable.Map.find var t.candidates_to_sink with
    | exception Not_found -> None
    | sink_to -> Some sink_to

  let remove_candidate_to_sink t var =
    let sink_to =
      match Variable.Map.find var t.candidates_to_sink with
      | exception Not_found -> None
      | sink_to -> Some sink_to
    in
    let candidates_to_sink =
      Variable.Map.remove var t.candidates_to_sink
    in
    let t =
      { t with
        candidates_to_sink;
      }
    in
    sink_to, t

  let sink_let t var ~sink_into ~defining_expr =
    let to_sink =
      let to_sink =
        match Continuation.Map.find sink_into t.to_sink with
        | exception Not_found -> []
        | to_sink -> to_sink
      in
      Continuation.Map.add sink_into ((var, defining_expr) :: to_sink)
        t.to_sink
    in
    let variables_to_sink = Variable.Set.add var t.variables_to_sink in
    { t with
      to_sink;
      variables_to_sink;
    }

  let add_to_sink_from_state t ~from =
    let to_sink = Continuation.Map.disjoint_union t.to_sink from.to_sink in
    let variables_to_sink =
      Variable.Set.union t.variables_to_sink from.variables_to_sink
    in
    { t with
      to_sink;
      variables_to_sink;
    }
end

let rec sink_expr (expr : Flambda.Expr.t) ~state : Flambda.Expr.t * State.t =
  match expr with
  | Let ({ var; kind; defining_expr; body; } as let_expr) ->
    let body, state = sink_expr body ~state in
    let defining_expr, state =
      match defining_expr with
      | Set_of_closures set_of_closures ->
        let set_of_closures = sink_set_of_closures set_of_closures in
        let defining_expr : Flambda.Named.t = Set_of_closures set_of_closures in
        W.of_named kind defining_expr, state
      | _ -> W.of_defining_expr_of_let let_expr, state
    in
    let sink_into, state = State.remove_candidate_to_sink state var in
    let state =
      match sink_into with
      | Some sink_into
        when Flambda.Named.at_most_generative_effects
          (W.to_named defining_expr) ->
        begin match List.rev sink_into with
        | [] -> state
        | (sink_into, _recursive)::_ ->
          State.sink_let state var ~sink_into ~defining_expr
        end
      | _ -> state
    in
    let add_candidates ~sink_into =
      Name_occurrences.fold_everything (W.free_names defining_expr)
        ~init:state
        ~f:(fun state (name : Name.t)->
          match name with
          | Symbol _ -> state
          | Var var ->
            let sink_into =
              match State.is_candidate_to_sink state var with
              | None -> sink_into
              | Some sink_into' -> join_continuation_stacks sink_into sink_into'
            in
            State.add_candidates_to_sink state
              ~sink_into
              ~candidates_to_sink:(Variable.Set.singleton var))
    in
    let keep_let () =
      W.create_let_reusing_defining_expr var defining_expr body
    in
    let only_generative_effects =
      Flambda.Named.at_most_generative_effects (W.to_named defining_expr)
    in
    (* CR mshinwell: Try to improve the structure of the code here and
        above *)
    begin match sink_into with
    | Some sink_into when only_generative_effects ->
      keep_let (), add_candidates ~sink_into
    | Some _sink_into ->
      keep_let (), add_candidates ~sink_into:[]
    | None ->
      if only_generative_effects then begin
        body, state
      end else begin
        keep_let (), add_candidates ~sink_into:[]
      end
    end
  | Let_mutable { var; initial_value; contents_type; body; }->
    let body, state = sink_expr body ~state in
    let state =
      match initial_value with
      | Name (Var initial_value) ->
        State.add_candidates_to_sink state
          ~sink_into:[]
          ~candidates_to_sink:(Variable.Set.singleton initial_value)
      | Name (Symbol _) | Const _ | Discriminant _ -> state
    in
    Let_mutable { var; initial_value; contents_type; body; }, state
  | Let_cont { body; handlers = Recursive handlers; } ->
    let body = sink body in
    let handlers, state =
      Continuation.Map.fold (fun name (handler : Flambda.Continuation_handler.t)
              (handlers, state) ->
          (* We don't sink anything into a recursive continuation. *)
          (* CR mshinwell: This is actually required for correctness at the
             moment since e.g. mutable block creation is deemed as "no
             generative effects" but cannot unconditionally be moved into
             loops. *)
          let new_handler = sink handler.handler in
          let fvs =
            Name.set_to_var_set (Name_occurrences.everything (
              Flambda.Expr.free_names new_handler))
          in
          let state =
            State.add_candidates_to_sink state
              ~sink_into:[]
              ~candidates_to_sink:fvs
          in
          let handler =
            { handler with
              handler = new_handler;
            }
          in
          Continuation.Map.add name handler handlers, state)
        handlers
        (Continuation.Map.empty, State.create ())
    in
    let candidates_to_sink =
      (* CR mshinwell: This pattern comes up sufficiently often that we
         should add it to [Flambda.Expr], but the name needs to make it clear
         exactly what it does. *)
      Name.set_to_var_set (Name_occurrences.everything (
        Flambda.Expr.free_names body))
    in
    let state =
      State.add_candidates_to_sink state
        ~sink_into:[]
        ~candidates_to_sink
    in
    Let_cont { body; handlers = Recursive handlers; }, state
  | Let_cont { body; handlers =
      Non_recursive { name; handler = {
        params; stub; is_exn_handler; handler; }; }; } ->
    let params_set = Flambda.Typed_parameter.List.var_set params in
    let body, state = sink_expr body ~state in
    let handler, handler_state =
      sink_expr handler ~state:(State.create ())
    in
    let state =
      State.add_candidates_to_sink_from_handler_state state
        ~current_continuation:(name, Non_recursive)
        ~handler_state
        ~except:params_set
    in
    let state = State.add_to_sink_from_state state ~from:handler_state in
    let state =
      let free_names = Flambda.Typed_parameter.List.free_names params in
      let candidates_to_sink =
        Name.set_to_var_set (Name_occurrences.everything free_names)
      in
      State.add_candidates_to_sink state
        ~sink_into:[]
        ~candidates_to_sink
    in
    Let_cont { body; handlers =
      Non_recursive { name; handler = {
        params; stub; is_exn_handler; handler; }; }; }, state
  | Apply _ | Apply_cont _ | Switch _ | Invalid _ ->
    let candidates_to_sink =
      Name.set_to_var_set (Name_occurrences.everything (
        Flambda.Expr.free_names expr))
    in
    let state =
      State.add_candidates_to_sink state
        ~sink_into:[]
        ~candidates_to_sink
    in
    expr, state

and sink_set_of_closures (set_of_closures : Flambda.Set_of_closures.t) =
  let funs =
    Closure_id.Map.map (fun
            (function_decl : Flambda.Function_declaration.t) ->
        Flambda.Function_declaration.update_body function_decl
          ~body:(sink function_decl.body))
      set_of_closures.function_decls.funs
  in
  let function_decls =
    Flambda.Function_declarations.update
      set_of_closures.function_decls ~funs
  in
  Flambda.Set_of_closures.create ~function_decls
    ~in_closure:set_of_closures.free_vars
    ~direct_call_surrogates:set_of_closures.direct_call_surrogates

and sink (expr : Flambda.Expr.t) =
  let expr, state = sink_expr expr ~state:(State.create ()) in
  let rec sink (expr : Flambda.Expr.t) : Flambda.Expr.t =
    match expr with
    | Let ({ var; body; } as let_expr) ->
      let body = sink body in
      if State.should_sink_let state var then
        body (* The let is to be moved into a handler. *)
      else
        let defining_expr = W.of_defining_expr_of_let let_expr in
        W.create_let_reusing_defining_expr var defining_expr body
    | Let_mutable { var; initial_value; contents_type; body; } ->
      let body = sink body in
      Let_mutable { var; initial_value; contents_type; body; }
    | Let_cont { body; handlers = Non_recursive { name; handler = {
        params; stub; is_exn_handler; handler; }; }; } ->
      let body = sink body in
      let handler =
        let handler = sink handler in
        let bindings = State.sunken_lets_for_handler state name in
        List.fold_left (fun handler (var, defining_expr) ->
            W.create_let_reusing_defining_expr var defining_expr handler)
          handler
          (List.rev bindings)
      in
      Let_cont { body; handlers = Non_recursive { name; handler =
        { params; stub; is_exn_handler; handler; }; }; }
    | Let_cont { body; handlers = Recursive handlers; } ->
      let body = sink body in
      let handlers =
        Continuation.Map.map (fun (handler : Flambda.Continuation_handler.t) ->
            { handler with
              handler = sink handler.handler;
            })
          handlers
      in
      Let_cont { body; handlers = Recursive handlers; }
    | Apply _ | Apply_cont _ | Switch _ | Invalid _ -> expr
  in
  sink expr

and sink_top ~continuation_arity:_ _continuation expr =
  sink expr

let run program =
  Flambda_static.Program.Mappers.map_toplevel_exprs program ~f:sink_top
