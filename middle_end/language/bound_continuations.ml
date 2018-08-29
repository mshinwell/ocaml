(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = Continuation.t list

let rename t =
  List.map (fun _k -> Continuation.create ()) t

let permutation_to_swap t1 t2 =
  if List.compare_lengths t1 t2 <> 0 then begin
    Misc.fatal_error "Bound_continuations.create with differing lengths"
  end;
  List.fold_left2 (fun perm k1 k2 ->
      Name_permutation.add_continuation perm k1 k2)
    (Name_permutation.create ())
    t1 t2
