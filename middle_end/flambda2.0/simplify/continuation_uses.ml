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

module T = Flambda_type
module TE = Flambda_type.Typing_env

module Use = struct
  type t = {
    arg_types : T.t list;
    typing_env : TE.t;
  }

  let create ~typing_env_at_use:typing_env ~arg_types =
    { arg_types;
      typing_env;
    }

  let print ppf { typing_env = _; arg_types; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(arg_types@ %a)@]@ \
        )@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Flambda_type.print)
      arg_types

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

let add_use t ~typing_env_at_use ~arg_types =
  try
    let arity = T.arity_of_list arg_types in
    if not (Flambda_arity.equal arity t.arity) then begin
      Misc.fatal_errorf "Arity of use (%a) doesn't match continuation's \
          arity (%a)"
        Flambda_arity.print arity
        Flambda_arity.print t.arity
    end;
    let use = Use.create ~typing_env_at_use ~arg_types in
    { t with
      uses = use :: t.uses;
    }
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sContext is:%s adding use of %a with arg types@ (%a);@ \
          existing uses:@ %a; environment:@ %a"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Continuation.print t.continuation
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print) arg_types
      print t
      TE.print typing_env_at_use;
    raise Misc.Fatal_error
  end

let env_and_arg_types t ~definition_typing_env =
  let definition_scope_level =
    T.Typing_env.current_scope definition_typing_env
  in
  let cut_point = Scope.next definition_scope_level in
  let process_use use =
    let env = Use.typing_env_at_use use in
    let env_extension, vars_in_scope_at_cut =
      TE.cut env ~unknown_if_defined_at_or_later_than:cut_point
    in
    let arg_types = Use.arg_types use in
    let arg_types =
      List.map (fun ty ->
          T.erase_aliases env ~bound_name:None
            ~allowed:vars_in_scope_at_cut ty)
        arg_types
    in
    env_extension, arg_types
  in
  match t.uses with
  | [] -> definition_typing_env, List.map (fun kind -> T.unknown kind) t.arity
  | [use] ->
    let env_extension, use_arg_types = process_use use in
    let env = TE.add_env_extension definition_typing_env env_extension in
    env, use_arg_types
  | use::uses ->
    let env_extension, use_arg_types = process_use use in
    List.fold_left (fun (env, arg_types) use ->
        let env_extension, use_arg_types = process_use use in
        let arg_types =
          List.map2 (fun arg_type arg_type' ->
              (* CR mshinwell: Which environment should be used here? *)
              T.join env arg_type arg_type')
            arg_types
            use_arg_types
        in
        let env = TE.add_env_extension definition_typing_env env_extension in
        env, arg_types)
      (TE.add_env_extension definition_typing_env env_extension, use_arg_types)
      uses

let number_of_uses t = List.length t.uses

let arity t = t.arity
