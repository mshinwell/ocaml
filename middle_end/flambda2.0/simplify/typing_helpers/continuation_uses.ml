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

[@@@ocaml.warning "+a-30-40-41-42"]

module T = Flambda_type
module TE = Flambda_type.Typing_env
module U = One_continuation_use

type t = {
  continuation : Continuation.t;
  arity : Flambda_arity.t;
  uses : U.t list;
}

let create continuation arity =
  { continuation;
    arity;
    uses = [];
  }

let print ppf { continuation; arity; uses; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuation@ %a)@]@ \
      @[<hov 1>(arity@ %a)@]@ \
      @[<hov 1>(uses@ %a)@]\
      )@]"
    Continuation.print continuation
    Flambda_arity.print arity
    (Format.pp_print_list ~pp_sep:Format.pp_print_space U.print) uses

let add_use t kind ~typing_env_at_use id ~arg_types =
  try
    let arity = T.arity_of_list arg_types in
    if not (Flambda_arity.equal arity t.arity) then begin
      Misc.fatal_errorf "Arity of use (%a) doesn't match continuation's \
          arity (%a)"
        Flambda_arity.print arity
        Flambda_arity.print t.arity
    end;
    let use = U.create kind ~typing_env_at_use id ~arg_types in
    { t with
      uses = use :: t.uses;
    }
  with Misc.Fatal_error -> begin
    if !Clflags.flambda2_context_on_error then begin
      Format.eprintf "\n%sContext is:%s adding use of %a with \
            arg types@ (%a);@ existing uses:@ %a; environment:@ %a"
        (Flambda_colours.error ())
        (Flambda_colours.normal ())
        Continuation.print t.continuation
        (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) arg_types
        print t
        TE.print typing_env_at_use
    end;
    raise Misc.Fatal_error
  end

let number_of_uses t = List.length t.uses

let arity t = t.arity

(* CR mshinwell: Four possible stages of join (turn into proper comment):

   1. Simple erasure policy
     - If the type has no free variables, propagate it
     - For each x in the free variables of the type, resolve x using [Aliases].
       If it resolves to a name in scope in the destination env then keep it.
       Otherwise unknown.
     - Don't produce any existentials in the resulting extension if there is
       more than one path
   2. For the "=x" case, if no name can be found in scope in the destination
      env equal to x, then expand the head of x recursively, to obtain a
      better type.  Propagate this.
   3. Support existentials from multiple paths.  This probably requires
      something like [Join_env] from the prototype.
   4. Path sensitivity.
*)

(* CR mshinwell: Move to [Generic_simplify_let_cont]? *)
let compute_handler_env t
      ~definition_typing_env_with_params_defined:typing_env
      ~params : Continuation_env_and_param_types.t =
(*
Format.eprintf "uses for %a\n%!" Continuation.print t.continuation;
*)
  match t.uses with
  | [] -> No_uses
  | uses ->
    let definition_scope_level = TE.current_scope typing_env in
    let use_envs_with_ids =
      List.map (fun use ->
(*
          Format.eprintf "For use, parameters: %a, arg types: %a, env:@ %a\n%!"
            Kinded_parameter.List.print params
            (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
            (U.arg_types use) TE.print (U.typing_env_at_use use);
*)
          let use_env =
            U.typing_env_at_use use
            |> TE.add_equations_on_params ~params
                 ~param_types:(U.arg_types use)
          in
          use_env, U.id use, U.use_kind use,
            Variable.Set.empty (* CR mshinwell: remove *) )
        uses
    in
(*
Format.eprintf "Unknown at or later than %a\n%!"
  Scope.print (Scope.next definition_scope_level);
*)
    let handler_typing_env, extra_params_and_args, is_single_inlinable_use =
      match use_envs_with_ids with
      | [use_env, _, Inlinable, _] ->
        (* A single inlinable use will be inlined out by the simplifier, so
           avoid any join-related computations. *)
        use_env, Continuation_extra_params_and_args.empty, true
      | [] | [_, _, Non_inlinable, _]
      | (_, _, (Inlinable | Non_inlinable), _) :: _ ->
        let env_extension, extra_params_and_args =
          TE.cut_and_n_way_join typing_env use_envs_with_ids
            ~unknown_if_defined_at_or_later_than:
              (Scope.next definition_scope_level)
        in
(*
Format.eprintf "handler env extension for %a is:@ %a\n%!"
  Continuation.print t.continuation
  T.Typing_env_extension.print env_extension;
Format.eprintf "The extra params and args are:@ %a\n%!"
  Continuation_extra_params_and_args.print extra_params_and_args;
*)
        let handler_env =
          typing_env
          |> TE.add_definitions_of_params
            ~params:extra_params_and_args.extra_params
          |> TE.add_env_extension ~env_extension
        in
        handler_env, extra_params_and_args, false
    in
    let arg_types_by_use_id =
      List.fold_left (fun args use ->
          List.map2 (fun arg_map arg_type ->
              Apply_cont_rewrite_id.Map.add (U.id use)
                (U.typing_env_at_use use, arg_type)
                arg_map)
            args
            (U.arg_types use))
        (List.map (fun _ -> Apply_cont_rewrite_id.Map.empty) t.arity)
        uses
    in
    Uses {
      handler_typing_env;
      arg_types_by_use_id;
      extra_params_and_args;
      is_single_inlinable_use;
    }
