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

let die_name_from_function_name fun_name =
  "camlDIE__" ^ fun_name

(* CR-soon mshinwell: remove is_parameter if not needed *)
let base_type_die_name_for_ident ~ident_name ~is_parameter:_ ~output_path =
  assert (try ignore (String.index ident_name ' '); false
      with Not_found -> true);
  Printf.sprintf "__ocaml %s %s"
    ident_name
    output_path

type split_base_type_die_name_result = {
  ident_name : string;
  output_path : string;
}

let split_base_type_die_name name =
  match Misc.Stdlib.String.split name ~on:' ' with
  | "__ocaml"::ident_name::output_path ->
    let output_path = String.concat " " output_path in
    Some { ident_name; output_path; }
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

(*
let dots_to_double_underscores path_as_string =
  let num_dots = ref 0 in
  let length = String.length path_as_string in
  for i = 0 to length - 1 do
    if String.get path_as_string i = '.' then incr num_dots
  done;
  let num_dots = !num_dots in
  if num_dots < 1 then
    path_as_string
  else begin
    let new_path_as_string = Bytes.create (length + num_dots) in
    let pos = ref 0 in
    for i = 0 to length - 1 do
      let chr = String.get path_as_string i in
      if chr <> '.' then
        Bytes.set new_path_as_string !pos chr
      else begin
        Bytes.set new_path_as_string !pos '_';
        incr pos;
        Bytes.set new_path_as_string !pos '_'
      end;
      incr pos
    done;
    Bytes.to_string new_path_as_string
  end

let path_to_mangled_name path =
  let rec traverse_path = function
    | Path.Pident ident -> Some (Ident.name ident)
    (* CR-someday mshinwell: handle [Papply] *)
    | Path.Papply _ -> None
    | Path.Pdot (path, component, _) ->
      match traverse_path path with
      | None -> None
      | Some path -> Some (path ^ "__" ^ component)
  in
  match traverse_path path with
  | None -> None
  | Some path ->
    match !Clflags.for_package with
    | None -> Some ("caml" ^ path)
    | Some pack ->
      (* [pack] may contain a dot; if so, it must be replaced by the
         double underscore encoding. *)
      let pack = dots_to_double_underscores pack in
      Some (Printf.sprintf "caml%s__%s" pack path)

(*
  match path_to_mangled_name path with
  | None -> ()
  | Some path ->
    match path_to_mangled_name (Path.Pident global) with
    | None -> ()
    | Some global ->
      let name = path ^ "__" ^ (Ident.unique_name ident) in
*)

let partially_mangle name =
  let components = String.split name ~on:'.' in
  "caml" ^ (String.concat "__" components)

let demangle mangled =
  let str = Bytes.copy mangled in
  let rec loop i j =
    if j >= Bytes.length str then
      i
    else if str.[j] = '_' && j + 1 < Bytes.length str && str.[j + 1] = '_' then (
      (* So, here is the funny part: there's no way to distinguish between "__" inserted
          by [Compilenv.make_symbol] (see asmcomp/compilenv.ml) and names containing
          "__".
          We are just going to assume that people never use "__" in their name (although
          we know for a fact that this happens in Core.) *)
      (* CR mshinwell: fix the above *)
      Bytes.set str i '.';
      loop (i + 1) (j + 2)
    ) else (
      Bytes.set str i str.[j];
      loop (i + 1) (j + 1)
    )
  in
  let len = loop 0 0 in
  Bytes.sub str 0 len

(* CR mshinwell: fix the compiler so that [code_begin] and friends gain the
   [__ocaml] prefix. *)
let is_legacy_name = function
  | "code_begin" | "code_end"
  | "data_begin" | "data_end"
  | "frametable" -> true
  | _ -> false

let is_probably_ocaml_name ~mangled_name =
  String.length mangled_name > 4
    && String.is_prefix mangled_name ~prefix:"caml"
    (* Beware: really naive *)
    && mangled_name.[4] <> '_'
    && mangled_name.[4] = (Char.uppercase mangled_name.[4])

let demangle mangled_name =
  if not (is_probably_ocaml_name ~mangled_name) then begin
    let without_stamp = String.drop_stamp mangled_name in
    if without_stamp <> mangled_name then
      Some without_stamp (* just assume it's a parameter; see CR above *)
    else
      Some mangled_name
  end else
    let maybe_stamped =
      demangle (String.sub mangled_name ~pos:4 ~len:(String.length mangled_name - 4))
    in
    let after_last_significant_dot =
      match try Some (String.rindex maybe_stamped '.') with Not_found -> None with
      | None -> None
      | Some dot when dot > 0 && maybe_stamped.[dot - 1] = '.' ->
        Some (String.sub maybe_stamped
          ~pos:dot
          ~len:(String.length maybe_stamped - dot))
      | Some dot ->
        Some (String.sub maybe_stamped
          ~pos:(dot + 1)
          ~len:(String.length maybe_stamped - dot - 1))
    in
    match after_last_significant_dot with
    | None -> None
    | Some after_last_significant_dot ->
      if String.is_prefix after_last_significant_dot ~prefix:".ocaml"
         || is_legacy_name after_last_significant_dot
         || (* A module component starting with a number must be a compiler-generated
               symbol (e.g. one for structured data). *)
            (String.length after_last_significant_dot > 0
              && let c = String.get after_last_significant_dot 0 in
                 Char.code c >= Char.code '0' && Char.code c <= Char.code '9') then
        None
      else begin
        let unstamped = String.drop_stamp maybe_stamped in
        let demangled_name =
          if String.is_suffix unstamped ~suffix:".fun" then
            maybe_stamped
          else
            unstamped
        in
        Some demangled_name
      end
*)
