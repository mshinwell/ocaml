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
  type t = T.t list

  let create typs = typs

  let print ppf t =
    Format.fprintf ppf "@[<hov 1>(%a)@]"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Flambda_type.print)
      t

  let arg_types t = t
end

type t = {
  definition_scope_level : Scope_level.t;
  arity : Flambda_arity.t;
  uses : Use.t list;
}

let create arity ~definition_scope_level =
  { definition_scope_level;
    arity;
    uses = [];
  }

let print ppf { definition_scope_level; arity; uses; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(definition_scope_level %a)@]@ \
      @[<hov 1>(arity %a)@]@ \
      @[<hov 1>(uses %a)@]\
      )@]"
    Scope_level.print definition_scope_level
    Flambda_arity.print arity
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Use.print) uses

let add_use t typing_env ~arg_types =
  let arity = T.arity_of_list arg_types in
  if not (Flambda_arity.equal arity t.arity) then begin
    Misc.fatal_errorf "Arity of use (%a) doesn't match continuation's \
        arity (%a)"
      Flambda_arity.print arity
      Flambda_arity.print t.arity
  end;
  let cut_point = Scope_level.next t.definition_scope_level in
  let allowed = TE.defined_variables typing_env ~at_or_previous_to:cut_point in
  let arg_types =
    List.map (fun typ -> T.erase_aliases typ ~allowed) arg_types
  in
  let use = Use.create arg_types in
  { t with
    uses = use :: t.uses;
  }

let arg_types t typing_env =
  match t.uses with
  | [] -> List.map (fun kind -> T.unknown kind) t.arity
  | [use] -> Use.arg_types use
  | use::uses ->
    List.fold_left (fun arg_types use ->
        let arg_types' = Use.arg_types use in
        List.map2 (fun arg_type arg_type' ->
            T.join typing_env arg_type arg_type')
          arg_types arg_types')
      (Use.arg_types use)
      uses
