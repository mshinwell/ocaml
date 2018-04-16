(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017--2018 OCamlPro SAS                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type S = sig
  type t

  include Hashtbl.With_map with type t := t

  val initial : t

  val prev : t -> t
  val next : t -> t

  val (<): t -> t -> bool
  val (>): t -> t -> bool
  val (>=): t -> t -> bool

  val to_int : t -> int
end

module T0 = struct
  include Numbers.Int

  let for_symbols = 0
  let initial = for_symbols

  let next t =
    t + 1

  let prev t =
    if t <= initial then begin
      Misc.fatal_error "Cannot decrement continuation level past the \
        initial level"
    end;
    t - 1

  let (<) (t1 : t) t2 = t1 < t2
  let (>) (t1 : t) t2 = t1 > t2
  let (>=) (t1 : t) t2 = t1 >= t2
  let (=) (t1 : t) t2 = t1 = t2

  let to_int t = t
end

include T0

module Sublevel = struct
  include Numbers.Int

  let for_symbols = 0
  let initial = for_symbols

  let next t =
    t + 1

  let prev t =
    if t <= initial then begin
      Misc.fatal_error "Cannot decrement sublevel past the initial level"
    end;
    t - 1

  let (<) (t1 : t) t2 = t1 < t2
  let (>) (t1 : t) t2 = t1 > t2
  let (>=) (t1 : t) t2 = t1 >= t2

  let to_int t = t
end

module With_sublevel = struct
  type with_sublevel = t * Sublevel.t

  let create level sublevel = level, sublevel

  let level (level, _sublevel) = level
  let sublevel (_level, sublevel) = sublevel

  let (>) (level1, sublevel1) (level2, sublevel2) =
    (level1 = level2 && Sublevel.(>) sublevel1 sublevel2)
      || level1 > level2

  include Hashtbl.Make_with_map_pair (T0) (Sublevel)
end
