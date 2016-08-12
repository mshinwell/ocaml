(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 2013--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let die_name_from_function_name fun_name =
  "camlDIE__" ^ fun_name

let base_type_die_name_for_ident ~(ident : Ident.t) ~output_path =
  let ident_name = Ident.name ident in
  assert (try ignore (String.index ident_name ' '); false
      with Not_found -> true);
  Printf.sprintf "__ocaml %s %d %s" ident_name ident.stamp output_path

type split_base_type_die_name_result = {
  ident_name : string;
  ident_stamp : int;
  output_path : string;
}

let split_base_type_die_name name =
  match String.split_on_char ' ' name with
  | "__ocaml"::ident_name::ident_stamp::output_path ->
    let output_path = String.concat " " output_path in
    let ident_stamp = int_of_string ident_stamp in
    Some { ident_name; ident_stamp; output_path; }
  | _ -> None

let fun_name_to_symbol fun_name =
  (* This is disgusting, see CR in the .mli *)
  let rec loop ~from =
    if from >= String.length fun_name - 2 then begin
      None
    end else begin
      let underscore =
        try Some (String.index_from fun_name (from + 1) '_')
        with Not_found -> None
      in
      match underscore with
      | None -> None
      | Some underscore ->
        if fun_name.[underscore] = '_' && fun_name.[underscore + 1] = '_'
        then begin
          let compilation_unit = String.sub fun_name 0 underscore in
          let linkage_name =
            String.sub fun_name (underscore + 2)
              ((String.length fun_name) - (underscore + 2))
          in
          assert (String.length compilation_unit >= 5);
          let compilation_unit =
            let ident = Ident.create_persistent compilation_unit in
            let linkage_name = Linkage_name.create compilation_unit in
            Compilation_unit.create ident linkage_name
          in
          let linkage_name = Linkage_name.create linkage_name in
          Some (Symbol.create compilation_unit linkage_name)
      end else begin
        loop ~from:underscore
      end
    end
  in
  match loop ~from:0 with
  | Some symbol -> symbol
  | None ->
    Symbol.create (Lazy.force Compilation_unit.startup)
      (Linkage_name.create fun_name)
