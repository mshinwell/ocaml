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

module DA = Downwards_acc
module LCS = Simplify_env_and_result.Lifted_constant_state
module R = Simplify_env_and_result.Result
module TE = Flambda_type.Typing_env
module UE = Simplify_env_and_result.Upwards_env

type t = {
  uenv : UE.t;
  code_age_relation : Code_age_relation.t;
  lifted_constants : LCS.t;
  r : R.t;
}

let print ppf { uenv; code_age_relation; lifted_constants; r; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[(uenv@ %a)@]@ \
      @[(code_age_relation@ %a)@]@ \
      @[(lifted_constants@ %a)@]@ \
      @[(r@ %a)@]\
      )@]"
    UE.print uenv
    Code_age_relation.print code_age_relation
    LCS.print lifted_constants
    R.print r

let create uenv code_age_relation r =
  { uenv;
    code_age_relation;
    lifted_constants = LCS.empty;
    r;
  }

let of_dacc dacc =
  { uenv = UE.empty;
    code_age_relation = TE.code_age_relation (DA.typing_env dacc);
    lifted_constants = LCS.empty;
    r = DA.r dacc;
  }

let uenv t = t.uenv
let code_age_relation t = t.code_age_relation
let lifted_constants_still_to_be_placed t = t.lifted_constants
let with_lifted_constants_still_to_be_placed t lifted_constants =
  { t with lifted_constants; }
let no_lifted_constants_still_to_be_placed t =
  LCS.is_empty t.lifted_constants

let map_uenv t ~f =
  { t with
    uenv = f t.uenv;
  }

let with_uenv t uenv =
  { t with
    uenv;
  }

let r t = t.r

let with_r t r =
  { t with
    r;
  }

let map_r t ~f =
  with_r t (f t.r)
