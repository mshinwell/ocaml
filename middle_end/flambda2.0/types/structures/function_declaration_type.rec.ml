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

type inlinable = {
  function_decl : Term_language_function_declaration.t;
  rec_info : Rec_info.t;
}

type t =
  | Non_inlinable of {
      param_arity : Flambda_arity.t;
      result_arity : Flambda_arity.t;
      recursive : Recursive.t;
    }
  | Inlinable of inlinable

let print_inlinable_with_cache ~cache ppf
      ({ function_decl; rec_info; } as decl) =
  Printing_cache.with_cache cache ppf "inlinable_fundecl" decl (fun ppf () ->
    Format.fprintf ppf
    "@[<hov 1>(Inlinable@ \
        @[<hov 1>(function_decl@ %a)@]@ \
        @[<hov 1>(rec_info@ %a)@]\
        )@]"
    Term_language_function_declaration.print_compact function_decl
    Rec_info.print rec_info)

let print_with_cache ~cache ppf t =
  match t with
  | Inlinable decl ->
    print_inlinable_with_cache ~cache ppf decl
  | Non_inlinable { param_arity; result_arity; recursive; } ->
    Format.fprintf ppf
      "@[<hov 1>(Non_inlinable@ \
       @[<hov 1>(param_arity@ %a)@]@ \
       @[<hov 1>(result_arity@ %a)@] \
       @[<hov 1>(recursive@ %a)@]\
       )@]"
      Flambda_arity.print param_arity
      Flambda_arity.print result_arity
      Recursive.print recursive
