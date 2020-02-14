(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module C = struct
  include Cmm_helpers
  include Un_cps_helper
end

type t = {
  gc_roots : Symbol.t list;
  data_list : Cmm.phrase list;
  functions : Cmm.phrase list;
  current_data : Cmm.data_item list;
}

let empty = {
  gc_roots = [];
  data_list = [];
  functions = [];
  current_data = [];
}

let add_to_data_list x l =
  match x with
  | [] -> l
  | _ :: _ -> C.cdata x :: l

let archive_data r =
  { r with current_data = [];
           data_list = add_to_data_list r.current_data r.data_list; }

let update_data r f =
  { r with current_data = f r.current_data; }

let set_data r l =
  update_data r (function
      | [] -> l
      | _ ->
        Misc.fatal_errorf
          "Un_cps_result.set_data: %s"
          "about to loose some translated static data items"
    )

let add_gc_roots r l =
  { r with gc_roots = l @ r.gc_roots; }

let add_function r f =
  { r with functions = f :: r.functions; }

let to_cmm r =
  let r = archive_data r in
  r.data_list, r.gc_roots, r.functions

