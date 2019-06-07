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

module DE = Simplify_env_and_result.Downwards_env
module R = Simplify_env_and_result.Result

type t = {
  denv : DE.t;
  r : R.t;
}

let print ppf { denv; r; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[(denv@ %a)@]@ \
      @[(r@ %a)@]\
      )@]"
    DE.print denv
    R.print r

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
