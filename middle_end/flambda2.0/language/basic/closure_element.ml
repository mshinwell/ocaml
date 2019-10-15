(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* CR mshinwell: Delete things we don't need and review this whole thing.
   And Variable. *)

type t = {
  compilation_unit : Compilation_unit.t;
  name : string;
  name_stamp : int;
  (** [name_stamp]s are unique within any given compilation unit. *)
}

module Self = Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    if t1 == t2 then 0
    else
      let c = t1.name_stamp - t2.name_stamp in
      if c <> 0 then c
      else Compilation_unit.compare t1.compilation_unit t2.compilation_unit

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t = t.name_stamp lxor (Compilation_unit.hash t.compilation_unit)

  let print ppf t =
    Format.fprintf ppf "@<0>%s" (Flambda_colours.closure_element ());
    if Compilation_unit.equal t.compilation_unit
        (Compilation_unit.get_current_exn ())
    then begin
      Format.fprintf ppf "%s/%d"
        t.name t.name_stamp
    end else begin
      Format.fprintf ppf "%a.%s/%d"
        Compilation_unit.print t.compilation_unit
        t.name t.name_stamp
    end;
    Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

include Self

let in_compilation_unit t cu =
  Compilation_unit.equal cu t.compilation_unit

let get_compilation_unit t = t.compilation_unit

let unique_name t =
  t.name ^ "_" ^ (string_of_int t.name_stamp)

let unwrap t =
  Variable.create ~current_compilation_unit:t.compilation_unit
    (unique_name t)

let wrap var =
  { compilation_unit = Variable.get_compilation_unit var;
    name = Variable.raw_name var;
    name_stamp = Variable.raw_name_stamp var;
  }
