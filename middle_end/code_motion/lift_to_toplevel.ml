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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module K = Flambda_kind
module Program_body = Flambda_static.Program_body

let static_structure name ~vars_with_kinds =
  let dbg = Debuginfo.none in
  let make_symbol index =
    let linkage_name =
      Linkage_name.create (Format.asprintf "lifted_%s%d" name index)
    in
    Symbol.create (Compilation_unit.get_current_exn ()) linkage_name
  in
  let rec assign_symbols index ~vars_with_kinds
        resulting_static_part resulting_bindings =
    (* Even when it looks like we could produce a single symbol with multiple
       fields (say when all of the parameters are of kind [Value] or
       [Naked_float]), we still produce multiple symbols, since unused
       definitions (e.g. arising as a result of unboxing) will then be cleared
       away by the straightforward "unused symbol" analysis rather than
       requiring something more complicated. *)
    match vars_with_kinds with
    | [] -> (List.rev resulting_static_part), (List.rev resulting_bindings)
    | (var, (kind : K.t))::vars_with_kinds ->
      let symbol = make_symbol index in
      let static_part : Flambda_static.Static_part.t =
        match kind with
        | Value _ | Fabricated _->
          Block (Tag.Scannable.zero, Immutable,
            [Flambda_static.Of_kind_value.Dynamically_computed var])
        | Naked_number Naked_float -> Boxed_float (Var var)
        | Naked_number Naked_int32 -> Boxed_int32 (Var var)
        | Naked_number Naked_int64 -> Boxed_int64 (Var var)
        | Naked_number Naked_nativeint -> Boxed_nativeint (Var var)
        | Naked_number Naked_immediate ->
          Misc.fatal_errorf "Not yet implemented"
        | Phantom _ ->
          (* CR mshinwell: Think about what to do here *)
          Misc.fatal_error "Not yet implemented"
      in
      let prim : Flambda_primitive.t =
        let var = Simple.var var in
        match kind with
        | Value _ | Fabricated _ ->
          Binary (Block_load (Block Any_value, Immutable), var,
            Simple.const (Tagged_immediate Immediate.zero))
        | Naked_number Naked_float -> Unary (Unbox_number Naked_float, var)
        | Naked_number Naked_int32 -> Unary (Unbox_number Naked_int32, var)
        | Naked_number Naked_int64 -> Unary (Unbox_number Naked_int64, var)
        | Naked_number Naked_nativeint ->
          Unary (Unbox_number Naked_nativeint, var)
        | Naked_number Naked_immediate ->
          Misc.fatal_errorf "Not yet implemented"
        | Phantom _ ->
          (* CR mshinwell: Think about what to do here *)
          Misc.fatal_error "Not yet implemented"
      in
      let binding : Flambda.Named.t = Prim (prim, dbg) in
      assign_symbols (index + 1) ~vars_with_kinds
        ((symbol, static_part) :: resulting_static_part)
        ((var, kind, binding) :: resulting_bindings)
  in
  assign_symbols 0 ~vars_with_kinds [] []

type or_do_not_lift =
  | Lift of Program_body.static_structure
      * ((Variable.t * Flambda_kind.t * Flambda.Named.t) list)
  | Do_not_lift

let rec lift (expr : Flambda.Expr.t) ~to_copy =
  match expr with
  | Let_cont ({ body; handlers = Non_recursive { name; handler = ({
      params; handler; is_exn_handler; _ } as handler_record); }; })
      when (not is_exn_handler) ->
    let free_conts_body, lifted, body = lift body ~to_copy in
    let our_cont = Continuation.Set.singleton name in
    if Continuation.Set.is_empty free_conts_body then begin
      (* The continuation is unused; delete it. *)
      free_conts_body, lifted, body
    end else if Continuation.Set.equal free_conts_body our_cont then begin
      (* The body of this [Let_cont] can only return through [name], which
         means that [handler] postdominates [body].  As such we can cut off
         [body] and put it inside a [computation] whose continuation
         is [handler].
         We augment [to_copy] to ensure that the bindings of the variables
         currently serving as parameters to [handler] is/are restated at the
         top of each subsequent lifted expression. *)
      let vars = Flambda.Typed_parameter.List.vars params in
      let arity = Flambda.Typed_parameter.List.arity params in
      let vars_with_kinds = List.combine vars arity in
      let static_structure, to_copy' =
        let name = Format.asprintf "%a" Continuation.print name in
        static_structure name ~vars_with_kinds
      in
      let to_copy = to_copy' @ to_copy in
      let free_conts_handler, lifted', handler =
        lift handler ~to_copy
      in
      let computation : Program_body.computation =
        { expr = body;
          return_cont = name;
          (* Since there is only one free continuation in the body of the
             [Let_cont] and we know that to be a non-exception-handling
             continuation, the body cannot raise any exceptions. *)
          exception_cont = Continuation.create ();
          computed_values = vars_with_kinds;
        }
      in
      let defn : Program_body.definition =
        { computation = Some computation;
          static_structure;
        }
      in
      let lifted = lifted @ [defn, to_copy] @ lifted' in
      let expr = Flambda.Expr.bind ~bindings:to_copy' ~body:handler in
      free_conts_handler, lifted, expr
    end else begin
      let handlers : Flambda.Let_cont_handlers.t =
        Non_recursive {
          name;
          handler = handler_record;
        };
      in
      let expr : Flambda.Expr.t =
        Let_cont {
          body;
          handlers;
        }
      in
      let free_conts =
        Continuation.Set.union
          (Continuation.Set.remove name free_conts_body)
          (Flambda.Expr.free_continuations handler_record.handler)
      in
      free_conts, lifted, expr
    end
  | Let { var; kind; defining_expr; body; _ } ->
    let vars_with_kinds = [var, kind] in
    let static_part_or_do_not_lift =
      match defining_expr with
      | Simple _ -> Do_not_lift
      (* CR mshinwell: We should probably lift some primitives (e.g.
         Project_var, and maybe closed sets of closures, directly to toplevel
         here. *)
      | Prim _
      | Set_of_closures _
      | Assign _
      | Read_mutable _ ->
        let name = Format.asprintf "%a" Variable.print var in
        let static_structure, bindings =
          static_structure name ~vars_with_kinds
        in
        Lift (static_structure, bindings)
    in
    begin match static_part_or_do_not_lift with
    | Do_not_lift ->
      (* This let-expression is not to be lifted, but instead restated at the
         top of each lifted expression. *)
      let to_copy = (var, kind, defining_expr)::to_copy in
      let free_conts, lifted, body = lift body ~to_copy in
      let body =
        if Variable.Set.mem var (Flambda.Expr.free_variables body) then
          Flambda.Expr.create_let var kind defining_expr body
        else
          body
      in
      free_conts, lifted, body
    | Lift (static_structure, to_copy') ->
      let to_copy = to_copy' @ to_copy in
      let free_conts, lifted, body = lift body ~to_copy in
      let return_cont, expr =
        let var' = Variable.rename var in
        let return_cont = Continuation.create () in
        let expr : Flambda.Expr.t =
          Flambda.Expr.create_let var' kind defining_expr
            (Apply_cont (return_cont, None, [Simple.var var']))
        in
        return_cont, expr
      in
      let computation : Program_body.computation =
        { expr;
          return_cont;
          exception_cont = Continuation.create ();
          computed_values = vars_with_kinds;
        }
      in
      let defn : Program_body.definition =
        { computation = Some computation;
          static_structure;
        }
      in
      let lifted = (defn, to_copy) :: lifted in
      let body = Flambda.Expr.bind ~bindings:to_copy' ~body in
      free_conts, lifted, body
    end
  | Let_cont { body; handlers; } ->
    (* Note that we do not recurse into the continuation [handlers]. *)
    let free_conts_body, lifted, body = lift body ~to_copy in
    let expr : Flambda.Expr.t =
      Let_cont {
        body;
        handlers;
      }
    in
    let free_and_bound_conts_handlers =
      Flambda.Let_cont_handlers.free_and_bound_continuations handlers
    in
    let free_conts =
      Continuation.Set.diff
        (Continuation.Set.union free_conts_body
          free_and_bound_conts_handlers.free)
        free_and_bound_conts_handlers.bound
    in
    free_conts, lifted, expr
  | Let_mutable _ | Apply _ | Apply_cont _ | Switch _ | Invalid _ ->
    let free_conts = Flambda.Expr.free_continuations expr in
    free_conts, [], expr

(* CR-someday mshinwell: Try to avoid having a separate substitution phase
   (so long as it doesn't complicate the code too much; the function above
   is already quite tricky). *)
let introduce_symbols (defn : Program_body.definition) =
  let computation_and_lifted =
    Misc.Stdlib.Option.map (fun (computation : Program_body.computation) ->
      let _free_conts, lifted, expr = lift computation.expr ~to_copy:[] in
      let lifted =
        List.map (fun ((defn : Program_body.definition), to_copy) ->
            let to_copy, subst =
              List.fold_left (fun (to_copy, subst)
                      (var, kind, defining_expr) ->
                  let var' = Variable.rename var in
                  let to_copy = (var', kind, defining_expr) :: to_copy in
                  to_copy, Variable.Map.add var var' subst)
                ([], Variable.Map.empty)
                to_copy
            in
            let to_copy =
              List.map (fun (var, kind, defining_expr) ->
                  let defining_expr =
                    Flambda.Named.toplevel_substitution subst
                      defining_expr
                  in
                  var, kind, defining_expr)
                to_copy
            in
            let computation =
              Misc.Stdlib.Option.map
                (fun (computation : Program_body.computation) ->
                  let expr =
                    Flambda.Expr.toplevel_substitution subst expr
                  in
                  { computation with expr; })
                defn.computation
            in
            let defn = { defn with computation; } in
            { defn with computation; }, to_copy)
          lifted
      in
      lifted, { computation with expr; })
    defn.computation
  in
  match computation_and_lifted with
  | None -> [], defn
  | Some (lifted, computation) ->
    lifted, { defn with computation = Some computation; }

let add_extracted lifted program_body =
  List.fold_left
    (fun acc ((defn : Flambda_static.Program_body.definition), to_copy)
        : Flambda_static.Program_body.t ->
      let computation =
        Misc.Stdlib.Option.map
          (fun (computation : Flambda_static.Program_body.computation) ->
            let expr =
              List.fold_left (fun expr (var, kind, defining_expr) ->
                  let fvs = Flambda.Expr.free_variables expr in
                  if Variable.Set.mem var fvs then
                    Flambda.Expr.create_let var kind defining_expr expr
                  else
                    expr)
                computation.expr
                to_copy
            in
            { computation with expr; })
          defn.computation
      in
      Define_symbol ({ defn with computation; }, acc))
    program_body
    (List.rev lifted)

let rec lift_program_body (body : Flambda_static.Program_body.t)
      : Flambda_static.Program_body.t =
  let lift_define_symbol defn body ~rebuild =
    let introduced, defn = introduce_symbols defn in
    let body = lift_program_body body in
    add_extracted introduced (rebuild defn body)
  in
  match body with
  | Define_symbol (defn, body) ->
    lift_define_symbol defn body ~rebuild:(fun defn body ->
      Program_body.Define_symbol (defn, body))
  | Define_symbol_rec (defn, body) ->
    lift_define_symbol defn body ~rebuild:(fun defn body ->
      Program_body.Define_symbol_rec (defn, body))
  | Root _ -> body

let lift (program : Flambda_static.Program.t) =
  { program with
    body = lift_program_body program.body;
  }
