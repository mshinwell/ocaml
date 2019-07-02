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
module TEE = Flambda_type.Typing_env_extension

module Use = struct
  type t = {
    env_extension : TEE.t;
    arg_types : T.t list;
  }

  let create env_extension ~arg_types =
    { env_extension;
      arg_types;
    }

  let print ppf { env_extension; arg_types; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(env_extension@ %a)@]@ \
        @[<hov 1>(arg_types@ %a)@]@ \
        )@]"
      TEE.print env_extension
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Flambda_type.print)
      arg_types

  let env_extension t = t.env_extension
  let arg_types t = t.arg_types
end

type t = {
  continuation : Continuation.t;
  definition_scope_level : Scope.t;
  arity : Flambda_arity.t;
  uses : Use.t list;
}

let create continuation arity ~definition_scope_level =
  { continuation;
    definition_scope_level;
    arity;
    uses = [];
  }

let print ppf { continuation; definition_scope_level; arity; uses; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuation@ %a)@]@ \
      @[<hov 1>(definition_scope_level@ %a)@]@ \
      @[<hov 1>(arity@ %a)@]@ \
      @[<hov 1>(uses@ %a)@]\
      )@]"
    Continuation.print continuation
    Scope.print definition_scope_level
    Flambda_arity.print arity
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Use.print) uses

let add_use t typing_env ~arg_types =
  try
    let arity = T.arity_of_list arg_types in
    if not (Flambda_arity.equal arity t.arity) then begin
      Misc.fatal_errorf "Arity of use (%a) doesn't match continuation's \
          arity (%a)"
        Flambda_arity.print arity
        Flambda_arity.print t.arity
    end;
    let cut_point = Scope.next t.definition_scope_level in
(*
  Format.eprintf "**** Unknown >= %a, env:@ %a\n%!"
    Scope.print cut_point
    TE.print typing_env;
*)
    let env_extension, vars_in_scope_at_cut =
      TE.cut typing_env ~unknown_if_defined_at_or_later_than:cut_point
    in
    let arg_types =
      List.map (fun ty ->
          T.erase_aliases typing_env ~bound_name:None
            ~allowed:vars_in_scope_at_cut ty)
        arg_types
    in
    let use = Use.create env_extension ~arg_types in
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
      TE.print typing_env;
    raise Misc.Fatal_error
  end

let env_and_arg_types t env =
  match t.uses with
  | [] -> env, List.map (fun kind -> T.unknown kind) t.arity
  | [use] ->
    let env_extension = Use.env_extension use in
    let arg_types = Use.arg_types use in
    let env = TE.add_env_extension env env_extension in
    env, arg_types
  | use::uses ->
    List.fold_left (fun (env, arg_types) use ->
        let arg_types =
          List.map2 (fun arg_type arg_type' ->
              (* CR mshinwell: Which environment should be used here? *)
              T.join env arg_type arg_type')
            arg_types (Use.arg_types use)
        in
        let env = TE.add_env_extension env (Use.env_extension use) in
        env, arg_types)
      (TE.add_env_extension env (Use.env_extension use), Use.arg_types use)
      uses

let definition_scope_level t = t.definition_scope_level

let number_of_uses t = List.length t.uses

let arity t = t.arity
