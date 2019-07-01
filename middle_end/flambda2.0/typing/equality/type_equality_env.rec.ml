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

type t = {
  typing_env_left : Typing_env.t;
  typing_env_right : Typing_env.t;
  already_comparing : Simple.Pair.Set.t;
}

let print ppf { typing_env_left; typing_env_right; already_comparing; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(typing_env_left@ %a)@]@ \
      @[<hov 1>(typing_env_right@ %a)@]@ \
      @[<hov 1>(already_comparing@ %a)@]\
      @]"
    Typing_env.print typing_env_left
    Typing_env.print typing_env_right
    Simple.Pair.Set.print already_comparing

let create ~typing_env_left ~typing_env_right =
  { typing_env_left;
    typing_env_right;
    already_comparing = Simple.Pair.Set.empty;
  }

let typing_env_left t = t.typing_env_left
let typing_env_right t = t.typing_env_right

let already_comparing t simple1 simple2 =
  Simple.Pair.Set.mem (simple1, simple2) t.already_comparing
    || Simple.Pair.Set.mem (simple2, simple1) t.already_comparing

let now_comparing t simple1 simple2 =
  if already_comparing t simple1 simple2 then begin
    Misc.fatal_errorf "Already testing %a and %a for equality:@ %a"
      Simple.print simple1
      Simple.print simple2
      print t
  end;
  let already_comparing =
    Simple.Pair.Set.add (simple1, simple2) t.already_comparing
  in
  { t with
    already_comparing;
  }
