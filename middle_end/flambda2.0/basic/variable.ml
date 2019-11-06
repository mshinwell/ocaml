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

type t = {
  name : string;
  name_stamp : int;
  (** [name_stamp]s are unique within any given compilation unit.
      We don't need to store the compilation unit because no variables are
      free in a complete program.  Any variables seen (which might e.g. have
      come from another compilation unit) will always be freshened first. *)
  user_visible : bool;
}

let compare0 t1 t2 =
  if t1 == t2 then 0
  else t1.name_stamp - t2.name_stamp

let equal0 t1 t2 = (compare0 t1 t2 = 0)

module Self = Identifiable.Make (struct
  type nonrec t = t

  let compare = compare0

  let equal = equal0

  let hash t = t.name_stamp

  let print ppf t = Format.fprintf ppf "%s/%d" t.name t.name_stamp

  let output chan t = print (Format.formatter_of_out_channel chan) t
end)

include Self

let compare = compare0

let equal = equal0

let previous_name_stamp = ref (-1)

let create ?current_compilation_unit ?user_visible name =
  ignore current_compilation_unit;
(*
  let compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
*)
  let name_stamp =
    incr previous_name_stamp;
    !previous_name_stamp
  in
(*
if name_stamp = 285366 then begin
  Format.eprintf "Creation of variable 285366:\n%s\n%!"
    (Printexc.raw_backtrace_to_string (Printexc.get_callstack 10))
end;
*)
  { name;
    name_stamp;
    user_visible = Option.is_some user_visible;
  }

let create_with_same_name_as_ident ?user_visible ident : t =
  create ?user_visible (Ident.name ident)

let fresh () : t = create "fresh"

let rename ?current_compilation_unit ?append t =
  let current_compilation_unit =
    match current_compilation_unit with
    | Some compilation_unit -> compilation_unit
    | None -> Compilation_unit.get_current_exn ()
  in
  let name =
    match append with
    | None -> t.name
    | Some s -> t.name ^ s
  in
  let user_visible = if t.user_visible then Some () else None in
  create ~current_compilation_unit ?user_visible name

let user_visible t = t.user_visible

let with_user_visible t ~user_visible =
  { t with user_visible; }

let raw_name t = t.name
let raw_name_stamp t = t.name_stamp

let unique_name t =
  t.name ^ "_" ^ (string_of_int t.name_stamp)

let print_list ppf ts =
  let pp_sep ppf () = Format.fprintf ppf "@ " in
  Format.pp_print_list ~pp_sep print ppf ts

let debug_when_stamp_matches t ~stamp ~f =
  if t.name_stamp = stamp then f ()

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

  let rename ?current_compilation_unit ?append t =
    List.map (fun var -> rename ?current_compilation_unit ?append var) t
end
