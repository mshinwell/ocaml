(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module T = struct
  type t =
    | Continuation of Continuation.t
    | Name of Name.t

  let compare t1 t2 =
    match t1, t2 with
    | Continuation k1, Continuation k2 -> Continuation.compare k1 k2
    | Name name1, Name name2 -> Name.compare name1 name2
    | Name _, Continuation _ -> 1
    | Continuation _, Name _ -> -1

  let print ppf t =
    match t with
    | Continuation k -> Continuation.print k
    | Name name -> Name.print name
end

include Map.Make_with_set (T)
