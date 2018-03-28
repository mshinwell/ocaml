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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = Continuation.t * (Flambda_type.t list)

include Hashtbl.Make_with_map (struct
  type nonrec t = t

  let compare t1 t2 =
    let c = Continuation.compare (fst t1) (fst t2) in
    if c <> 0 then c
    else assert false
(* XXX need to think about what to do here
      (Variable.Map.compare Flambda.compare_specialised_to) (snd t1) (snd t2)
*)

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash _t = assert false
(*
    Hashtbl.hash (Continuation.hash (fst t),
      Hashtbl.hash (Variable.Map.bindings (snd t)))
*)

  let print ppf (cont, tys) =
    Format.fprintf ppf "@[(%a, %a)@]"
      Continuation.print cont
      (Format.pp_print_list Flambda_type.print) tys
end)
