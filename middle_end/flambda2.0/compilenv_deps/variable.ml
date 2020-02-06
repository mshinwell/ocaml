(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = Table_by_int_id.Id.t

module Data = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    name : string;
    name_stamp : int;
    user_visible : bool;
  }

  let print ppf { compilation_unit; name; name_stamp; user_visible; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(name@ %s)@]@ \
        @[<hov 1>(name_stamp@ %d)@]@ \
        @[<hov 1>(user_visible@ %b)@]\
        )@]"
      Compilation_unit.print compilation_unit
      name
      name_stamp
      user_visible

  let hash { compilation_unit; name; name_stamp; user_visible = _; } =
    Hashtbl.hash (Compilation_unit.hash compilation_unit, name, name_stamp)

  let equal
      { compilation_unit = compilation_unit1; name = name1;
        name_stamp = name_stamp1; user_visible = _; }
      { compilation_unit = compilation_unit2; name = name2;
        name_stamp = name_stamp2; user_visible = _; }
      =
    Int.equal name_stamp1 name_stamp2
      && String.equal name1 name2
      && Compilation_unit.equal compilation_unit1 compilation_unit2
end

module Table = Table_by_int_id.Make (Data)

let global_table = ref (Table.create ())

let initialise () =
  global_table := Table.create ()

let compilation_unit t = (Table.find !global_table t).compilation_unit
let name t = (Table.find !global_table t).name
let name_stamp t = (Table.find !global_table t).name_stamp
let user_visible t = (Table.find !global_table t).user_visible

module Self = Identifiable.Make (struct
  include Table_by_int_id.Id

  let print ppf t = Format.fprintf ppf "%s/%d" (name t) (name_stamp t)

  let output chan t = print (Format.formatter_of_out_channel chan) t
end)

include Self

let previous_name_stamp = ref (-1)

let create ?user_visible name =
  let name_stamp =
    incr previous_name_stamp;
    !previous_name_stamp
  in
  let data : Data.t =
    { compilation_unit = Compilation_unit.get_current_exn ();
      name;
      name_stamp;
      user_visible = Option.is_some user_visible;
    }
  in
  Table.add !global_table data

let create_with_same_name_as_ident ?user_visible ident : t =
  create ?user_visible (Ident.name ident)

let rename ?append t =
  let name =
    match append with
    | None -> (name t)
    | Some s -> (name t) ^ s
  in
  let user_visible = if user_visible t then Some () else None in
  create ?user_visible name

let with_user_visible t ~user_visible =
  (* CR mshinwell: duplicate code with above *)
  let name_stamp =
    incr previous_name_stamp;
    !previous_name_stamp
  in
  let data : Data.t =
    { compilation_unit = compilation_unit t;
      name = name t;
      name_stamp;
      user_visible;
    }
  in
  Table.add !global_table data

let raw_name = name
let raw_name_stamp = name_stamp

let unique_name t =
  (name t) ^ "_" ^ (string_of_int (name_stamp t))

let print_list ppf ts =
  let pp_sep ppf () = Format.fprintf ppf "@ " in
  Format.pp_print_list ~pp_sep print ppf ts

let debug_when_stamp_matches t ~stamp ~f =
  if (name_stamp t) = stamp then f ()

let print_opt ppf = function
  | None -> Format.fprintf ppf "<no var>"
  | Some t -> print ppf t

type pair = t * t
module Pair = struct
  include Identifiable.Make_pair
    (struct type nonrec t = t include Self end)
    (struct type nonrec t = t include Self end)
end

let compare_lists l1 l2 =
  Misc.Stdlib.List.compare compare l1 l2

module List = struct
  type nonrec t = t list

  let rename ?append t =
    List.map (fun var -> rename ?append var) t
end
