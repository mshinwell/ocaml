(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* XXX Commented out for the moment.  The continuation part of this is
   mostly ok (except for changes relating to [Name]) but the function part
   needs overhauling as a result of the closure change

(* CR-someday pchambart to pchambart: in fact partial application doesn't
   work because there are no 'known' partial application left: they are
   converted to applications new partial function declaration.
   That can be improved (and many other cases) by keeping track of aliases in
   closure of functions. *)

(* These analyses are computed in two steps:
   * accumulate the atomic <- relations
   * compute the least-fixed point

  The <- relation is represented by the type

     t CF.Name_and_variable.Map.t

  if [CF.Name_and_variable.Map.find (f, x) relation = Top] then (f, x) <- Top
  is in the relation.

  if [CF.Name_and_variable.Map.find (f, x) relation = Implication s] and
  [CF.Name_and_variable.Set.mem (g, y) s] then (f, x) <- (g, y) is in the
  relation.
*)

module type Continuations_or_functions = sig
  module Identifier : sig
    type t
    include Hashtbl.With_map with type t := t

    (* Conversions to and from variables, for first-class entities only. *)
    val as_variable : t -> Variable.t option
    val of_variable : Variable.t -> t option
  end

  module Identifier_and_variable : sig
    include Hashtbl.With_map with type t = Identifier.t * Variable.t
  end

  module Declaration : sig
    type t

    val params : t -> Variable.t list
    val body : t -> Flambda.Expr.t

    val free_names_of_body_excluding_callees_and_args
       : t
      -> Variable.Set.t
  end

  module Declarations : sig
    type t

    val declarations : t -> Declaration.t Name.Map.t

    val function_variable_aliases
       : t
      -> backend:(module Backend_intf.S)
      -> Identifier.t Variable.Map.t
  end

  type application = {
    callee : Identifier.t;
    args : Simple.t list;
  }

  val check_application : Flambda.Expr.t -> application option
end

(*
module For_functions = struct
  module Name = struct
    include Variable
    let as_variable t = Some t
    let of_variable t = Some t
  end

  module Name_and_variable = struct
    type t = Variable.t * Variable.t
    include Variable.Pair
  end

  module Declaration = struct
    type t = Flambda.Function_declaration.t
    let params (t : t) = Flambda.Typed_parameter.List.vars t.params
    let body (t : t) = t.body

    let free_names_of_body_excluding_callees_and_args (t : t) =
      Flambda.Expr.free_names ~ignore_uses_as_callee:()
        ~ignore_uses_as_argument:() t.body
  end

  module Declarations = struct
    type t = Flambda.Function_declarations.t
    let declarations (t : t) = t.funs

    (* CR-soon pchambart: to move to Flambda_utils and document
      mshinwell: I think this calculation is basically the same as
      [Flambda_utils.fun_vars_referenced_in_decls], so we should try
      to share code.  However let's defer until after 4.03.  (And note CR
      below.)
    *)
    (* Finds variables that represent the functions.
      In a construction like:
        let f x =
          let g = Symbol f_closure in
          ..
      the variable g is bound to the symbol f_closure which
      is the current closure.
      The result of [function_variable_alias] will contain
      the association [g -> f]
    *)
    let function_variable_aliases
        (function_decls : Flambda.Function_declarations.t) ~backend =
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
      let fun_var_bindings = ref Variable.Map.empty in
      Variable.Map.iter (fun _ (function_decl : Flambda.Function_declaration.t) ->
          Flambda.Expr.Iterators.Toplevel_only.iter_all_immutable_let_and_let_rec_bindings
            ~f:(fun var named ->
              (* CR-soon mshinwell: consider having the body passed to this
                  function and using fv calculation instead of used_variables.
                  Need to be careful of "let rec" *)
              match named with
              | Symbol sym ->
                begin match Symbol.Map.find sym symbols_to_fun_vars with
                | exception Not_found -> ()
                | fun_var ->
                  fun_var_bindings :=
                    Variable.Map.add var fun_var !fun_var_bindings
                end
              | _ -> ())
            function_decl.body)
        function_decls.funs;
      !fun_var_bindings
  end

  type application = {
    callee : Name.t;
    args : Variable.t list;
  }

  let check_application (expr : Flambda.Expr.t) : application option =
    match expr with
    | Apply { func; args; } -> Some { callee = func; args; }
    | _ -> None
end
*)

module For_continuations = struct
  module Identifier = struct
    include Continuation

    (* Continuations are not first class. *)
    let as_variable _ = None
    let of_variable _ = None
  end

  module Identifier_and_variable = struct
    type t = Continuation.t * Variable.t

    include Hashtbl.Make_with_map (struct
      type nonrec t = t

      let compare (cont1, var1) (cont2, var2) =
        let c = Continuation.compare cont1 cont2 in
        if c <> 0 then c
        else Variable.compare var1 var2

      let equal t1 t2 = (compare t1 t2 = 0)

      let hash (cont, var) =
        Hashtbl.hash (Continuation.hash cont, Variable.hash var)

      let print ppf (cont, var) =
        Format.fprintf ppf "(%a, %a)"
          Continuation.print cont
          Variable.print var
    end)
  end

  module Declaration = struct
    type t = Flambda.Continuation_handler.t

    let params (t : t) = Flambda.Typed_parameter.List.vars t.params
    let body (t : t) = t.handler

    let free_names_of_body_excluding_callees_and_args (t : t) =
      Flambda.Expr.free_names_advanced
        ~ignore_uses_as_continuation_argument:() t.handler
  end

  module Declarations = struct
    type t = Flambda.Continuation_handlers.t
    let declarations (t : t) = t

    let function_variable_aliases _t ~backend:_ = Variable.Map.empty
  end

  type application = {
    callee : Identifier.t;
    args : Simple.t list;
  }

  let check_application (expr : Flambda.Expr.t) : application option =
    match expr with
    | Apply_cont (cont, _trap_action, args) -> Some { callee = cont; args; }
    | Apply { continuation; _ } -> Some { callee = continuation; args = []; }
    | _ -> None
end

module Analyse (CF : Continuations_or_functions) = struct
  type t =
    | Top
    | Implication of CF.Identifier_and_variable.Set.t

  let _print ppf = function
    | Top -> Format.fprintf ppf "Top"
    | Implication args ->
        Format.fprintf ppf "Implication: @[<hv>%a@]"
          CF.Identifier_and_variable.Set.print args

  let top relation p =
    CF.Identifier_and_variable.Map.add p Top relation

  let implies relation from to_ =
    match CF.Identifier_and_variable.Map.find to_ relation with
    | Top -> relation
    | Implication set ->
      CF.Identifier_and_variable.Map.add to_
        (Implication (CF.Identifier_and_variable.Set.add from set))
        relation
    | exception Not_found ->
      CF.Identifier_and_variable.Map.add to_
        (Implication (CF.Identifier_and_variable.Set.singleton from))
        relation

  let transitive_closure state =
    let union s1 s2 =
      match s1, s2 with
      | Top, _ | _, Top -> Top
      | Implication s1, Implication s2 ->
        Implication (CF.Identifier_and_variable.Set.union s1 s2)
    in
    let equal s1 s2 =
      match s1, s2 with
      | Top, Implication _ | Implication _, Top -> false
      | Top, Top -> true
      | Implication s1, Implication s2 -> CF.Identifier_and_variable.Set.equal s1 s2
    in
    let update arg state =
      let original_set =
        try CF.Identifier_and_variable.Map.find arg state with
        | Not_found -> Implication CF.Identifier_and_variable.Set.empty
      in
      match original_set with
      | Top -> state
      | Implication arguments ->
        let set =
          CF.Identifier_and_variable.Set.fold (fun orig acc ->
              let set =
                try CF.Identifier_and_variable.Map.find orig state with
                | Not_found -> Implication CF.Identifier_and_variable.Set.empty in
              union set acc)
            arguments original_set
        in
        CF.Identifier_and_variable.Map.add arg set state
    in
    let once state =
      CF.Identifier_and_variable.Map.fold (fun arg _ state -> update arg state) state state
    in
    let rec fp state =
      let state' = once state in
      if CF.Identifier_and_variable.Map.equal equal state state'
      then state
      else fp state'
    in
    fp state

  let analyse_functions ~backend ~param_to_param
        ~anything_to_param ~param_to_anywhere
        (decls : CF.Declarations.t) =
    let function_variable_alias =
      CF.Declarations.function_variable_aliases decls ~backend
    in
    let param_indexes_by_fun_vars =
      CF.Identifier.Map.map (fun (decl : CF.Declaration.t) ->
          Array.of_list (CF.Declaration.params decl))
        (CF.Declarations.declarations decls)
    in
    let find_callee_arg ~callee ~callee_pos =
      match CF.Identifier.Map.find callee param_indexes_by_fun_vars with
      | exception Not_found -> None (* not a recursive call *)
      | arr ->
        (* Ignore overapplied parameters: they are applied to a different
           function. *)
        if callee_pos < Array.length arr then Some arr.(callee_pos)
        else None
    in
    let escaping_functions = CF.Identifier.Tbl.create 13 in
    let escaping_function fun_var =
      let fun_var =
        match Variable.Map.find fun_var function_variable_alias with
        | exception Not_found -> CF.Identifier.of_variable fun_var
        | fun_var -> Some fun_var
      in
      match fun_var with
      | None -> ()
      | Some fun_var ->
        if CF.Identifier.Map.mem fun_var (CF.Declarations.declarations decls)
        then CF.Identifier.Tbl.add escaping_functions fun_var ()
    in
    let used_variables = Variable.Tbl.create 42 in
    let used_variable var = Variable.Tbl.add used_variables var () in
    let relation = ref CF.Identifier_and_variable.Map.empty in
    (* If the called closure is in the current set of closures, record the
       relation (callee, callee_arg) <- (caller, caller_arg) *)
    let check_argument ~caller ~callee ~callee_pos ~caller_arg =
      escaping_function caller_arg;
      match find_callee_arg ~callee ~callee_pos with
      | None ->
        used_variable caller_arg (* not a recursive call *)
      | Some callee_arg ->
        match CF.Identifier.Map.find caller (CF.Declarations.declarations decls) with
        | exception Not_found ->
          assert false
        | decl ->
          let new_relation =
            (* We only track dataflow for parameters of functions and
               continuations, not arbitrary variables. *)
            (* CR mshinwell: remove use of polymorphic comparison *)
            let params = CF.Declaration.params decl in
            if List.mem caller_arg params then begin
              param_to_param ~caller ~caller_arg ~callee ~callee_arg !relation
            end else begin
              used_variable caller_arg;
              anything_to_param ~callee ~callee_arg !relation
            end
          in
          relation := new_relation
    in
    let arity ~callee =
      match CF.Identifier.Map.find callee (CF.Declarations.declarations decls) with
      | exception Not_found -> 0
      | func -> List.length (CF.Declaration.params func)
    in
    let check_expr ~caller (expr : Flambda.Expr.t) =
      match CF.check_application expr with
      | Some { callee; args; } ->
        let callee_var = CF.Identifier.as_variable callee in
        Misc.Stdlib.Option.iter used_variable callee_var;
        let callee =
          match callee_var with
          | None -> callee
          | Some callee_var ->
            match Variable.Map.find callee_var function_variable_alias with
            | exception Not_found -> callee
            | callee -> callee
        in
        let num_args = List.length args in
        for callee_pos = num_args to (arity ~callee) - 1 do
          (* If a function is partially applied, consider all missing
             arguments as "anything". *)
          match find_callee_arg ~callee ~callee_pos with
          | None -> ()
          | Some callee_arg ->
            relation := anything_to_param ~callee ~callee_arg !relation
        done;
        List.iteri (fun callee_pos caller_arg ->
            check_argument ~caller ~callee ~callee_pos ~caller_arg)
          args
      | None -> ()
    in
    CF.Identifier.Map.iter (fun caller (decl : CF.Declaration.t) ->
        Flambda.Expr.Iterators.iter (check_expr ~caller)
          (fun (_ : Flambda.Identifierd.t) -> ())
          (CF.Declaration.body decl);
        Variable.Set.iter
          (fun var -> escaping_function var; used_variable var)
          (* CR-soon mshinwell: we should avoid recomputing this, cache in
             [function_declaration].  See also comment on
             [only_via_symbols] in [Flambda_utils]. *)
          (CF.Declaration.free_names_of_body_excluding_callees_and_args
            decl))
      (CF.Declarations.declarations decls);
    CF.Identifier.Map.iter
      (fun func_var decl ->
        List.iter
          (fun param ->
              if Variable.Tbl.mem used_variables param then
                relation :=
                  param_to_anywhere ~caller:func_var ~caller_arg:param !relation;
              if CF.Identifier.Tbl.mem escaping_functions func_var then
                relation :=
                  anything_to_param ~callee:func_var ~callee_arg:param
                    !relation)
          (CF.Declaration.params decl))
      (CF.Declarations.declarations decls);
    transitive_closure !relation

  (* A parameter [x] of the function [f] is considered as unchanging if
     during an 'external' (call from outside the set of closures) call of
     [f], every recursive call of [f] all the instances of [x] are aliased
     to the original one.  This function computes an underapproximation of
     that set by computing the flow of parameters between the different
     functions of the set of closures.
 
     We record [(f, x) <- (g, y)] when the function g calls f and
     the y parameter of g is used as argument for the x parameter of f. For
     instance in
 
       let rec f x = ...
       and g y = f x
 
     We record [(f, x) <- Top] when some unknown values can flow to the
     [y] parameter.
 
       let rec f x = f 1
 
     We record also [(f, x) <- Top] if [f] could escape. This is over
     approximated by considering that a function escape when its variable is used
     for something else than an application:
 
       let rec f x = (f, f)
 
     [x] is not unchanging if either
         (f, x) <- Top
     or (f, x) <- (f, y) with x != y
 
     Notice that having (f, x) <- (g, a) and (f, x) <- (g, b) does not make
     x not unchanging. This is because (g, a) and (g, b) represent necessarily
     different values only if g is the externaly called function. If some
     value where created during the execution of the function that could
     flow to (g, a), then (g, a) <- Top, so (f, x) <- Top.
 
  *)

  let invariant_params_in_recursion (decls : CF.Declarations.t) ~backend =
    let param_to_param ~caller ~caller_arg ~callee ~callee_arg relation =
      implies relation (caller, caller_arg) (callee, callee_arg)
    in
    let anything_to_param ~callee ~callee_arg relation =
      top relation (callee, callee_arg)
    in
    let param_to_anywhere ~caller:_ ~caller_arg:_ relation = relation in
    let relation =
      analyse_functions ~backend ~param_to_param
        ~anything_to_param ~param_to_anywhere
        decls
    in
    let not_unchanging =
      CF.Identifier_and_variable.Map.fold (fun (func, var) set not_unchanging ->
          match set with
          | Top -> Variable.Set.add var not_unchanging
          | Implication set ->
            if CF.Identifier_and_variable.Set.exists (fun (func', var') ->
                CF.Identifier.equal func func' && not (Variable.equal var var'))
              set
            then Variable.Set.add var not_unchanging
            else not_unchanging)
        relation Variable.Set.empty
    in
    let params =
      CF.Identifier.Map.fold (fun _ decl set ->
          let params = CF.Declaration.params decl in
          Variable.Set.union (Variable.Set.of_list params) set)
        (CF.Declarations.declarations decls)
        Variable.Set.empty
    in
    let unchanging = Variable.Set.diff params not_unchanging in
    let aliased_to =
      CF.Identifier_and_variable.Map.fold (fun (_, var) set aliases ->
          match set with
          | Implication set
            when Variable.Set.mem var unchanging ->
              CF.Identifier_and_variable.Set.fold (fun (_, caller_args) aliases ->
                  if Variable.Set.mem caller_args unchanging then
                    let alias_set =
                      match Variable.Map.find caller_args aliases with
                      | exception Not_found ->
                        Variable.Set.singleton var
                      | alias_set ->
                        Variable.Set.add var alias_set
                    in
                    Variable.Map.add caller_args alias_set aliases
                  else
                    aliases)
                set aliases
          | Top | Implication _ -> aliases)
        relation Variable.Map.empty
    in
    (* We complete the set of aliases such that there does not miss any
       unchanging param *)
    Variable.Map.of_set (fun var ->
        match Variable.Map.find var aliased_to with
        | exception Not_found -> Variable.Set.empty
        | set -> set)
      unchanging

  let invariant_param_sources decls ~backend =
    let param_to_param ~caller ~caller_arg ~callee ~callee_arg relation =
      implies relation (caller, caller_arg) (callee, callee_arg)
    in
    let anything_to_param ~callee:_ ~callee_arg:_ relation = relation in
    let param_to_anywhere ~caller:_ ~caller_arg:_ relation = relation in
    let relation =
      analyse_functions ~backend ~param_to_param
        ~anything_to_param ~param_to_anywhere
        decls
    in
    CF.Identifier_and_variable.Map.fold (fun (_, var) set relation ->
        match set with
        | Top -> relation
        | Implication set -> Variable.Map.add var set relation)
      relation Variable.Map.empty

  let pass_name = "unused-arguments"
  let () = Clflags.all_passes := pass_name :: !Clflags.all_passes

  let unused_arguments (decls : CF.Declarations.t) ~backend =
    let dump = Clflags.dumped_pass pass_name in
    let param_to_param ~caller ~caller_arg ~callee ~callee_arg relation =
      implies relation (callee, callee_arg) (caller, caller_arg)
    in
    let anything_to_param ~callee:_ ~callee_arg:_ relation = relation in
    let param_to_anywhere ~caller ~caller_arg relation =
      top relation (caller, caller_arg)
    in
    let relation =
      analyse_functions ~backend ~param_to_param
        ~anything_to_param ~param_to_anywhere
        decls
    in
    let arguments =
      CF.Identifier.Map.fold (fun fun_var decl acc ->
          List.fold_left (fun acc param ->
              match CF.Identifier_and_variable.Map.find (fun_var, param) relation with
              | exception Not_found -> Variable.Set.add param acc
              | Implication _ -> Variable.Set.add param acc
              | Top -> acc)
            acc
            (CF.Declaration.params decl))
        (CF.Declarations.declarations decls)
        Variable.Set.empty
    in
    if dump then begin
      Format.printf "Unused arguments: %a@." Variable.Set.print arguments
    end;
    arguments
end

(*
module Functions = Analyse (For_functions)
*)

module Continuations = struct
  module Continuation_and_variable = For_continuations.Name_and_variable
  include Analyse (For_continuations)
end

*)
