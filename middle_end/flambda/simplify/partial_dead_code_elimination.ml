(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(* [Simplify_import] cannot be used owing to a circular dependency. *)
module EA = Continuation_extra_params_and_args.Extra_arg
module EP = Flambda_primitive.Eligible_for_pdce
module EPA = Continuation_extra_params_and_args
module K = Flambda_kind
module KP = Kinded_parameter
module NM = Name_mode
module P = Flambda_primitive
module RI = Apply_cont_rewrite_id
module T = Flambda_type
module TE = Flambda_type.Typing_env

module List = ListLabels

module For_downwards_env = struct
  type t = {
    by_scope : EP.t Variable.Map.t Scope.Map.t;
    combined : EP.t Variable.Map.t;
  }

  let print ppf { by_scope; combined; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(by_scope@ %a)@]@ \
        @[<hov 1>(combined@ %a)@]\
        @]"
      (Scope.Map.print (Variable.Map.print EP.print)) by_scope
      (Variable.Map.print EP.print) combined
  let empty =
    { by_scope = Scope.Map.empty;
      combined = Variable.Map.empty;
    }

  let consider_primitive t ~bound_to prim scope =
    match P.Eligible_for_pdce.create prim with
    | None -> t
    | Some prim ->
      let level =
        match Scope.Map.find scope t.by_scope with
        | exception Not_found -> Variable.Map.singleton bound_to prim
        | level -> Variable.Map.add bound_to prim level
      in
      let by_scope = Scope.Map.add (* replace *) scope level t.by_scope in
      let combined = Variable.Map.add bound_to prim t.combined in
      let lhs_with_aliases = Variable.Set.add bound_to t.lhs_with_aliases in
      { equations;
        lhs_with_aliases;
      }

  let join ~typing_env_at_fork ~use_info ~get_pdce =
    ...
end

module For_downwards_acc = struct
  type t = {
    lhs_with_aliases : Variable.Set.t;
  }

  let print ppf { lhs_with_aliases; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(lhs_with_aliases@ %a)@]\
        @]"
      Variable.Set.print lhs_with_aliases
end

module For_upwards_acc = struct
  type t = {
    still_to_be_placed : EP.t Variable.Map.t;
    lhs_with_aliases : Variable.Set.t;
  }
end

module For_upwards_env = struct
  type t = {
    uses_of_lhs_or_aliases : Variable.Set.t Continuation.Map.t;
  }
end
