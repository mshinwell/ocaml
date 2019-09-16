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

module KP = Kinded_parameter
module T = Flambda_type
module TE = Flambda_type.Typing_env

module Use = struct
  type t = {
    id : Apply_cont_rewrite_id.t;
    kind : Continuation_use_kind.t;
    arg_types : T.t list;
    typing_env : TE.t;
  }

  let create kind ~typing_env_at_use:typing_env id ~arg_types =
    { id;
      kind;
      arg_types;
      typing_env;
    }

  let print ppf { typing_env = _; id = _; kind = _; arg_types; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(arg_types@ %a)@]@ \
        )@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Flambda_type.print)
      arg_types

  let id t = t.id
  let arg_types t = t.arg_types
  let typing_env_at_use t = t.typing_env
end

type t = {
  continuation : Continuation.t;
  arity : Flambda_arity.t;
  uses : Use.t list;
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
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Use.print) uses

let add_use t kind ~typing_env_at_use id ~arg_types =
  try
    let arity = T.arity_of_list arg_types in
    if not (Flambda_arity.equal arity t.arity) then begin
      Misc.fatal_errorf "Arity of use (%a) doesn't match continuation's \
          arity (%a)"
        Flambda_arity.print arity
        Flambda_arity.print t.arity
    end;
    let use = Use.create kind ~typing_env_at_use id ~arg_types in
(*
Format.eprintf "For %a, recording use:@ %a\n%!"
  Continuation.print t.continuation
  Use.print use;
*)
    { t with
      uses = use :: t.uses;
    }
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sContext is:%s adding use of %a with \
          arg types@ (%a);@ existing uses:@ %a; environment:@ %a"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Continuation.print t.continuation
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) arg_types
      print t
      TE.print typing_env_at_use;
    raise Misc.Fatal_error
  end

let number_of_uses t = List.length t.uses

let arity t = t.arity

(* CR mshinwell: Naming of this function still isn't ideal. *)
let cannot_change_continuation's_arity t =
  match Continuation.sort t.continuation with
  | Return -> true
  | Toplevel_return -> false
  | Exn -> true (* CR mshinwell: this should go to [false] *)
  | Normal -> false

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

let introduce_params typing_env ~params =
  List.fold_left (fun env param ->
      let name =
        Name_in_binding_pos.create (KP.name param) Name_occurrence_kind.normal
      in
      TE.add_definition typing_env name (KP.kind param))
    typing_env
    params

let compute_handler_env t
      ~definition_typing_env_with_params_defined:handler_typing_env
      ~params : Continuation_env_and_param_types.t =
  match t.uses with
  | [] -> No_uses
  | uses ->
    let definition_scope_level = TE.current_scope handler_typing_env in
    let use_envs_with_ids =
      List.map (fun use ->
          let typing_env =
            List.fold_left2 (fun env param arg_type ->
                TE.add_equation typing_env (KP.name param) arg_type)
              typing_env
              params (Use.arg_types use)
          in
          typing_env, Use.id use)
        uses
    in
    let joined_env_extension, extra_params_and_args =
      TE.cut_and_n_way_join handler_typing_env use_envs_with_ids
        ~unknown_if_defined_at_or_later_than:definition_scope_level
    in
    let handler_typing_env =
      TE.add_env_extension
        (introduce_params handler_typing_env
          ~params:extra_params_and_args.extra_params)
        joined_env_extension
    in
    let arg_types_by_use_id =
      List.fold_left (fun args use ->
          List.map2 (fun arg_map arg_type ->
              Apply_cont_rewrite_id.Map.add (Use.id use)
                (Use.typing_env_at_use use, arg_type)
                arg_map)
            args
            (Use.arg_types use))
        (List.map (fun _ -> Apply_cont_rewrite_id.Map.empty) t.arity)
        uses
    in
    Uses {
      handler_typing_env;
      arg_types_by_use_id;
      extra_params_and_args;
    }
