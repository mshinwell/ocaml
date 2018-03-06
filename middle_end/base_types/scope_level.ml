(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Numbers.Int

let for_symbols = 0
let initial = for_symbols

let next t =
(*
  if t < 0 then begin
    Misc.fatal_error "Cannot increment scope level from [for_symbols]"
  end;
*)
  t + 1

let prev t =
  if t <= initial then begin
    Misc.fatal_error "Cannot decrement scope level past the initial level"
  end;
  t - 1

let (>=) (t1 : t) t2 = t1 >= t2

let to_int t = t
