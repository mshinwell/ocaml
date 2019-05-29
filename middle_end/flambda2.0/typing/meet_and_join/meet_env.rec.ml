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
  env : Typing_env.t;
  already_meeting : Simple.Pair.Set.t;
}

let print ppf { env; already_meeting; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(env@ %a)@]@ \
      @[<hov 1>(already_meeting@ %a)@])@]"
    Typing_env.print env
    Simple.Pair.Set.print already_meeting

let create env =
  { env;
    already_meeting = Simple.Pair.Set.empty;
  }

let env t = t.env

let already_meeting t simple1 simple2 =
  Simple.Pair.Set.mem (simple1, simple2) t.already_meeting
    || Simple.Pair.Set.mem (simple2, simple1) t.already_meeting

let now_meeting t simple1 simple2 =
  if already_meeting t simple1 simple2 then begin
    Misc.fatal_errorf "Already meeting %a and %a:@ %a"
      Simple.print simple1
      Simple.print simple2
      print t
  end;
  let already_meeting =
    Simple.Pair.Set.add (simple1, simple2) t.already_meeting
  in
  { t with
    already_meeting;
  }
