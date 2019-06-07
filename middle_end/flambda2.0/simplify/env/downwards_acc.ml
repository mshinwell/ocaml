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

module CUE = Continuation_uses_env
module DE = Simplify_env_and_result.Downwards_env
module R = Simplify_env_and_result.Result

type t = {
  denv : DE.t;
  continuation_uses_env : CUE.t;
  r : R.t;
}

let print ppf { denv; continuation_uses_env; r; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(denv@ %a)@]@ \
      @[<hov 1>(continuation_uses_env@ %a)@] \
      @[<hov 1>(r@ %a)@]\
      )@]"
    DE.print denv
    CUE.print continuation_uses_env
    R.print r

let create ~round ~backend = {
  denv = DE.create ~round ~backend;
  continuation_uses_env = CUE.empty;
}

let denv t = t.denv

let map_denv t ~f =
  { t with
    denv = f t.denv;
  }

let with_denv t denv =
  { t with
    denv;
  }

let r t = t.r

let map_r t ~f =
  { t with
    r = f t.r;
  }

let with_continuation_uses_env t continuation_uses_env =
  { t with
    continuation_uses_env;
  }

let add_continuation t cont arity =
  with_continuation_uses_env (
    CUE.add_continuation t.continuation_uses_env cont arity)

let add_exn_continuation t exn_cont =
  with_continuation_uses_env (
    CUE.add_exn_continuation t.continuation_uses_env exn_cont)

let record_continuation_use t cont ~arg_types =
  with_continuation_uses_env (
    CUE.record_continuation_use t.continuation_uses_env t cont ~arg_types)

let num_continuation_uses t cont =
  CUE.num_continuation_uses t.continuation_uses_env cont

let continuation_uses_env t = t.continuation_uses_env
