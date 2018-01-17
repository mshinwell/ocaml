(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = Flambda_kind.t list

let length t = List.length t

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = Misc.Stdlib.List.compare Flambda_kind.compare t1 t2
  let equal t1 t2 = (compare t1 t2) = 0
  let hash = Hashtbl.hash

  let print ppf t =
    Format.fprintf ppf "@[%a@]"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        Flambda_kind.print)
      t
end)

let is_all_values t =
  List.for_all Flambda_kind.is_value t

let is_all_naked_floats t =
  List.for_all Flambda_kind.is_naked_float t

let compatible t ~if_used_at =
  if List.compare_lengths t if_used_at <> 0 then begin
    Misc.fatal_errorf "Flambda_arity.compatible: mismatching lengths: %a vs %a"
      print t
      print if_used_at
  end;
  List.for_all2 (fun kind if_used_at ->
      Flambda_kind.compatible kind ~if_used_at)
    t if_used_at
