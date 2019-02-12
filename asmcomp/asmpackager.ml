(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Misc

module CU = Compilation_unit
module UI = Cmx_format.Unit_info

type error =
  | Illegal_renaming of {
      name_in_cmx : CU.Name.t;
      file : string;
      desired_name : CU.Name.t;
    }
  | Forward_reference of string * CU.Name.t
  | Wrong_for_pack of string * string
  | Linking_error
  | File_not_found of string

exception Error of error

(* Read the unit information from a .cmx file. *)

type pack_member_kind =
  | PM_intf
  | PM_impl of UI.t * Cmx_format.Unit_info_link_time.t

type pack_member =
  { pm_file: filepath;
    pm_name: CU.Name.t;
    pm_kind: pack_member_kind }

let read_member_info pack_path file =
  let name =
    CU.Name.of_string (String.capitalize_ascii (
      Filename.basename (chop_extensions file)))
  in
  let kind =
    if Filename.check_suffix file ".cmi" then
      PM_intf
    else begin
      let info, link_info, crc = Cmx_format.load ~filename:file in
      let name_in_cmx = CU.name (UI.unit info) in
      if not (CU.Name.equal name_in_cmx name) then begin
        raise (Error (Illegal_renaming {
          name_in_cmx;
          file;
          desired_name = name;
        }))
      end;
      let cmx_file_for_pack_prefix = CU.for_pack_prefix (UI.unit info) in
      let full_path_current_unit = CU.full_path (CU.get_current_exn ()) in
      if not (Misc.Stdlib.List.equal CU.Name.equal
        cmx_file_for_pack_prefix full_path_current_unit)
      then begin
        raise (Error (Wrong_for_pack (file, pack_path)))
      end;
      Asmlink.check_consistency file info crc;
      Compilation_state.cache_unit_info info;
      PM_impl (info, link_info)
    end
  in
  { pm_file = file; pm_name = name; pm_kind = kind }

(* Check absence of forward references *)

let check_units members =
  let rec check forbidden = function
    [] -> ()
  | mb :: tl ->
      begin match mb.pm_kind with
      | PM_intf -> ()
      | PM_impl (info, _link_info) ->
          CU.Map.iter
            (fun unit _crc ->
              let name = CU.name unit in
              if CU.Name.Set.mem name forbidden
              then raise(Error(Forward_reference(mb.pm_file, name))))
            (UI.imports_cmx info)
      end;
      check (CU.Name.Set.remove mb.pm_name forbidden) tl
  in
  let forbidden =
    CU.Name.Set.of_list (List.map (fun mb -> mb.pm_name) members)
  in
  check forbidden members

(* Make the .o file for the package *)

let make_package_object ~ppf_dump members targetobj targetname coercion
      ~backend =
  Profile.record_call (Printf.sprintf "pack(%s)" targetname) (fun () ->
    let objtemp =
      if !Clflags.keep_asm_file
      then Filename.remove_extension targetobj ^ ".pack" ^ Config.ext_obj
      else
        (* Put the full name of the module in the temporary file name
           to avoid collisions with MSVC's link /lib in case of successive
           packs *)
        let comp_unit = CU.get_current_exn () in
        let symbol = Symbol.for_module_block comp_unit in
        let backend_sym = Backend_sym.of_symbol symbol in
        Filename.temp_file (Backend_sym.to_string backend_sym) Config.ext_obj
    in
    let components =
      List.map
        (fun m ->
          match m.pm_kind with
          | PM_intf -> None
          | PM_impl _ ->
            Some (Ident.create_persistent (CU.Name.to_string m.pm_name)))
        members
    in
    let module_ident = Ident.create_persistent targetname in
    let prefixname = Filename.remove_extension objtemp in
    let required_globals = Ident.Set.empty in
    if Config.flambda then begin
      let main_module_block_size, code =
        Translmod.transl_package_flambda components coercion
      in
      let program =
        { Lambda.
          code;
          main_module_block_size;
          module_ident;
          required_globals;
        }
      in
      Asmgen.compile_implementation ~backend
        ~filename:targetname
        ~prefixname
        ~middle_end:Flambda_middle_end.lambda_to_clambda
        ~ppf_dump
        program
    end else begin
      let main_module_block_size, code =
        Translmod.transl_store_package components
          (Ident.create_persistent targetname) coercion
      in
      let program =
        { Lambda.
          code;
          main_module_block_size;
          module_ident;
          required_globals;
        }
      in
      Asmgen.compile_implementation ~backend
        ~filename:targetname
        ~prefixname
        ~middle_end:Closure_middle_end.lambda_to_clambda
        ~ppf_dump
        program
    end;
    let objfiles =
      List.map
        (fun m -> Filename.remove_extension m.pm_file ^ Config.ext_obj)
        (List.filter (fun m -> m.pm_kind <> PM_intf) members)
    in
    let ok =
      Ccomp.call_linker Ccomp.Partial targetobj (objtemp :: objfiles) ""
    in
    remove_file objtemp;
    if not ok then raise(Error Linking_error)
  )

(* Make the .cmx file for the package *)

let build_package_cmx members cmxfile =
  let module UI = Cmx_format.Unit_info in
  let unit_names_in_pack =
    CU.Name.Set.of_list (List.map (fun m -> m.pm_name) members)
  in
  let units, unit_link_infos =
    List.split (
      List.fold_right (fun m accu ->
          match m.pm_kind with
          | PM_intf -> accu
          | PM_impl (info, link_info) -> (info, link_info) :: accu)
        members [])
  in
  let compilation_state = Compilation_state.Snapshot.create () in
  let current_unit = CU.get_current_exn () in
  let current_unit_name = CU.name current_unit in
  let current_unit_crc =
    Env.crc_of_unit (CU.Name.to_string current_unit_name)
  in
  let imports_cmi =
    let imports_cmi =
      CU.Name.Map.filter (fun name _crc ->
          not (CU.Name.Set.mem name unit_names_in_pack))
        (Asmlink.extract_crc_interfaces ())
    in
    CU.Name.Map.add current_unit_name (Some current_unit_crc) imports_cmi
  in
  let imports_cmx =
    CU.Map.filter (fun imported_unit _crc ->
        let name = CU.name imported_unit in
        not (CU.Name.Set.mem name unit_names_in_pack))
      (Asmlink.extract_crc_implementations ())
  in
  let defines =
    (List.flatten (List.map UI.defines units)) @ [current_unit]
  in
  let export_info : UI.export_info =
    match compilation_state.export_info with
    | Closure approx -> Closure approx
    | Flambda export_info ->
      let export_info =
        List.fold_left (fun acc info ->
            match UI.export_info info with
            | Flambda export_info -> Export_info.merge acc export_info
            | Closure _ ->
              Misc.fatal_errorf "%a contains Closure approximations yet \
                  Flambda export info was found"
                CU.print (UI.unit info))
          export_info
          units
      in
      Flambda export_info
  in
  let pkg_infos =
    UI.create ~unit:current_unit ~defines ~imports_cmi ~imports_cmx
      ~export_info
  in
  let pkg_link_infos = Cmx_format.Unit_info_link_time.join unit_link_infos in
  Cmx_format.save pkg_infos pkg_link_infos ~filename:cmxfile

(* Make the .cmx and the .o for the package *)

let package_object_files ~ppf_dump files targetcmx
                         targetobj targetname coercion ~backend =
  let pack_path =
    match !Clflags.for_package with
    | None -> targetname
    | Some p -> p ^ "." ^ targetname in
  let members = map_left_right (read_member_info pack_path) files in
  check_units members;
  make_package_object ~ppf_dump members targetobj targetname coercion ~backend;
  build_package_cmx members targetcmx

(* The entry point *)

let package_files ~ppf_dump initial_env files targetcmx ~backend =
  let files =
    List.map
      (fun f ->
        try Load_path.find f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let prefix = chop_extensions targetcmx in
  let targetcmi = prefix ^ ".cmi" in
  let targetobj = Filename.remove_extension targetcmx ^ Config.ext_obj in
  let targetname = String.capitalize_ascii(Filename.basename prefix) in
  (* Set the name of the current "input" *)
  Location.input_name := targetcmx;
  (* Set the name of the current compunit *)
  let for_pack_prefix =
    match !Clflags.for_package with
    | None | Some "" -> []
    | Some for_pack_prefix ->
      List.map CU.Name.of_string (String.split_on_char '.' for_pack_prefix)
  in
  let comp_unit = CU.create ~for_pack_prefix (CU.Name.of_string targetname) in
  Compilation_unit.set_current comp_unit;
  Compilation_state.reset comp_unit;
  Misc.try_finally (fun () ->
      let coercion =
        Typemod.package_units initial_env files targetcmi targetname in
      package_object_files ~ppf_dump files targetcmx targetobj targetname
        coercion ~backend
    )
    ~exceptionally:(fun () -> remove_file targetcmx; remove_file targetobj)

(* Error report *)

open Format

let report_error ppf = function
  | Illegal_renaming { name_in_cmx; file; desired_name; } ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %a when %a was expected"
        Location.print_filename file CU.Name.print name_in_cmx
        CU.Name.print desired_name
  | Forward_reference(file, cu_name) ->
      fprintf ppf "Forward reference to %a in file %a" CU.Name.print cu_name
        Location.print_filename file
  | Wrong_for_pack(file, path) ->
      fprintf ppf "File %a@ was not compiled with the `-for-pack %s' option"
              Location.print_filename file path
  | File_not_found file ->
      fprintf ppf "File %s not found" file
  | Linking_error ->
      fprintf ppf "Error during partial linking"

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
