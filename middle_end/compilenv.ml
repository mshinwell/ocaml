(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compilation environments for compilation units *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Config
open Cmx_format

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string

exception Error of error

let global_infos_table =
  (Hashtbl.create 17 : (string, unit_infos option) Hashtbl.t)

(* The type of the contents of the first .cmx file section (the one that
   comes immediately after the marshalled value of type
   [Cmx_format.unit_infos]). *)

type section_contents =
  | Closure of Clambda.value_approximation
  | Flambda of Obj.t

module CstMap =
  Map.Make(struct
    type t = Clambda.ustructured_constant
    let compare = Clambda.compare_structured_constants
    (* PR#6442: it is incorrect to use Stdlib.compare on values of type t
       because it compares "0.0" and "-0.0" equal. *)
  end)

module SymMap = Misc.Stdlib.String.Map

type structured_constants =
  {
    strcst_shared: string CstMap.t;
    strcst_all: Clambda.ustructured_constant SymMap.t;
  }

let structured_constants_empty  =
  {
    strcst_shared = CstMap.empty;
    strcst_all = SymMap.empty;
  }

let structured_constants = ref structured_constants_empty

let exported_constants = Hashtbl.create 17

let current_unit =
  { ui_name = "";
    ui_symbol = "";
    ui_defines = [];
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_curry_fun = [];
    ui_apply_fun = [];
    ui_send_fun = [];
    ui_force_link = false;
    ui_section_toc = [];
    ui_channel = None;
    ui_sections = [| |];
    ui_index_of_next_section_to_write = 0;
    ui_sections_to_write_rev = [];
  }

(* CR mshinwell: When are we going to close the .cmx files? *)

let symbolname_for_pack pack name =
  match pack with
  | None -> name
  | Some p ->
      let b = Buffer.create 64 in
      for i = 0 to String.length p - 1 do
        match p.[i] with
        | '.' -> Buffer.add_string b "__"
        |  c  -> Buffer.add_char b c
      done;
      Buffer.add_string b "__";
      Buffer.add_string b name;
      Buffer.contents b

let unit_id_from_name name = Ident.create_persistent name

let concat_symbol unitname id =
  unitname ^ "__" ^ id

let make_symbol ?(unitname = current_unit.ui_symbol) idopt =
  let prefix = "caml" ^ unitname in
  match idopt with
  | None -> prefix
  | Some id -> concat_symbol prefix id

let current_unit_linkage_name () =
  Linkage_name.create (make_symbol ~unitname:current_unit.ui_symbol None)

let reset ?packname name =
  Hashtbl.clear global_infos_table;
  let symbol = symbolname_for_pack packname name in
  current_unit.ui_name <- name;
  current_unit.ui_symbol <- symbol;
  current_unit.ui_defines <- [symbol];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_send_fun <- [];
  current_unit.ui_force_link <- !Clflags.link_everything;
  Hashtbl.clear exported_constants;
  structured_constants := structured_constants_empty;
  current_unit.ui_section_toc <- [];
  current_unit.ui_channel <- None;
  current_unit.ui_sections <- [| |];
  current_unit.ui_index_of_next_section_to_write <- 0;
  current_unit.ui_sections_to_write_rev <- [];
  let compilation_unit =
    Compilation_unit.create
      (Ident.create_persistent name)
      (current_unit_linkage_name ())
  in
  Compilation_unit.set_current compilation_unit

let clear_export_info_for_current_unit () =
  current_unit.ui_section_toc <- []

let current_unit_infos () =
  current_unit

let current_unit_name () =
  current_unit.ui_name

let symbol_in_current_unit name =
  let prefix = "caml" ^ current_unit.ui_symbol in
  name = prefix ||
  (let lp = String.length prefix in
   String.length name >= 2 + lp
   && String.sub name 0 lp = prefix
   && name.[lp] = '_'
   && name.[lp + 1] = '_')

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = really_input_string ic (String.length cmx_magic_number) in
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let ui = (input_value ic : unit_infos) in
    let offset_after_cmx_infos = pos_in ic in
    Printf.printf "loading cmx %s: offset after cmx_infos = %d\n%!"
      ui.ui_symbol offset_after_cmx_infos;
    ui.ui_channel <- Some ic;
    let last_byte_offset_in_cmx = ref 0 in
    let sections =
      Array.of_list ui.ui_section_toc
      |> Array.map (fun byte_offset_in_cmx ->
        last_byte_offset_in_cmx := byte_offset_in_cmx;
        { byte_offset_in_cmx = byte_offset_in_cmx + offset_after_cmx_infos;
          section_contents = None;
        })
    in
    ui.ui_sections <- sections;
    seek_in ic (offset_after_cmx_infos + !last_byte_offset_in_cmx);
    let crc = Digest.input ic in
    ui, crc
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

(* XXX *)
let _close_cmx_file ui =
  match ui.ui_channel with
  | None -> ()
  | Some ic ->
    close_in ic;
    ui.ui_channel <- None

let read_library_info filename =
  let ic = open_in_bin filename in
  let buffer = really_input_string ic (String.length cmxa_magic_number) in
  if buffer <> cmxa_magic_number then
    raise(Error(Not_a_unit_info filename));
  let infos = (input_value ic : library_infos) in
  close_in ic;
  infos

(* Management of .cmx file sections *)

let add_section ui (section : section_contents) =
  match ui.ui_section_toc with
  | [] ->
    let index = ui.ui_index_of_next_section_to_write in
    ui.ui_sections_to_write_rev
      <- (Obj.repr section) :: ui.ui_sections_to_write_rev;
    ui.ui_index_of_next_section_to_write <- index + 1;
    index
  | _::_ ->
    Misc.fatal_errorf "unit_infos record for unit %s already has sections \
        from when it was read from a .cmx file.  It is not expected that \
        more sections are added to such units."
      ui.ui_symbol

let prepare_sections_for_export ui =
  Printf.eprintf "prepare_sections_for_export\n%!";
  let _, section_toc_rev, marshalled_rev =
    ListLabels.fold_left (List.rev ui.ui_sections_to_write_rev)
      ~init:(0, [], [])
      ~f:(fun (byte_offset, section_toc_rev, marshalled_rev) contents ->
        Printf.eprintf "--> next section at byte_offset %d\n%!" byte_offset;
        let section_toc_rev = byte_offset :: section_toc_rev in
        let marshalled_data = Marshal.to_string contents [] in
        let marshalled_rev = marshalled_data :: marshalled_rev in
        let byte_offset = byte_offset + String.length marshalled_data in
        byte_offset, section_toc_rev, marshalled_rev)
  in
  ui.ui_sections <- [| |];
  ui.ui_index_of_next_section_to_write <- 0;
  ui.ui_sections_to_write_rev <- [];
  ui.ui_section_toc <- List.rev section_toc_rev;
  List.rev marshalled_rev

let num_sections_read_from_cmx_file ui =
  Array.length ui.ui_sections

let read_section_from_cmx_file ui ~index =
  Printf.eprintf "read_section_from_cmx_file %s: index %d\n%!"
    ui.ui_symbol index;
  match ui.ui_channel with
  | None -> None
  | Some ic ->
    let num_sections = num_sections_read_from_cmx_file ui in
    if index < 0 || index >= num_sections then
      Misc.fatal_errorf ".cmx file for unit %s only has %d sections, but \
          the section at index %d was requested"
        ui.ui_symbol
        num_sections
        index
    else
      let { byte_offset_in_cmx; section_contents; } = ui.ui_sections.(index) in
      match section_contents with
      | Some section_contents -> Some section_contents
      | None ->
        Printf.eprintf "--> seeking to %d\n%!" byte_offset_in_cmx;
        seek_in ic byte_offset_in_cmx;
        let section_contents : Obj.t option = Some (input_value ic) in
        ui.ui_sections.(index) <- { byte_offset_in_cmx; section_contents; };
        section_contents

(* Read and cache info on global identifiers *)

let get_global_info global_ident = (
  let modname = Ident.name global_ident in
  if modname = current_unit.ui_name then
    Some current_unit
  else begin
    try
      Hashtbl.find global_infos_table modname
    with Not_found ->
      let (infos, crc) =
        if Env.is_imported_opaque modname then (None, None)
        else begin
          try
            let filename =
              Load_path.find_uncap (modname ^ ".cmx") in
            let (ui, crc) = read_unit_info filename in
            if ui.ui_name <> modname then
              raise(Error(Illegal_renaming(modname, ui.ui_name, filename)));
            (Some ui, Some crc)
          with Not_found ->
            let warn = Warnings.No_cmx_file modname in
              Location.prerr_warning Location.none warn;
              (None, None)
          end
      in
      current_unit.ui_imports_cmx <-
        (modname, crc) :: current_unit.ui_imports_cmx;
      Hashtbl.add global_infos_table modname infos;
      infos
  end
)

let cache_unit_info ui =
  Hashtbl.add global_infos_table ui.ui_name (Some ui)

(* Return the approximation of a global identifier *)

let get_clambda_approx ui =
  assert(not Config.flambda);
  if num_sections_read_from_cmx_file ui <> 1 then
    Misc.fatal_error "Not a Closure approx (wrong number of sections)"
  else
    match read_section_from_cmx_file ui ~index:0 with
    | Some info ->
      let (info : section_contents) = Obj.obj info in
      begin match info with
      | Closure approx -> approx
      | Flambda _ -> Misc.fatal_error "Not a Closure approx"
    end
    | None -> Misc.fatal_error "Clambda section could not be read"

let toplevel_approx :
  (string, Clambda.value_approximation) Hashtbl.t = Hashtbl.create 16

let record_global_approx_toplevel () =
  Hashtbl.add toplevel_approx current_unit.ui_name
    (get_clambda_approx current_unit)

let global_approx_for_unit = get_clambda_approx

let global_approx id =
  if Ident.is_predef id then Clambda.Value_unknown
  else try Hashtbl.find toplevel_approx (Ident.name id)
  with Not_found ->
    match get_global_info id with
      | None -> Clambda.Value_unknown
      | Some ui -> get_clambda_approx ui

(* Return the symbol used to refer to a global identifier *)

let symbol_for_global id =
  if Ident.is_predef id then
    "caml_exn_" ^ Ident.name id
  else begin
    let unitname = Ident.name id in
    match
      try ignore (Hashtbl.find toplevel_approx unitname); None
      with Not_found -> get_global_info id
    with
    | None -> make_symbol ~unitname:(Ident.name id) None
    | Some ui -> make_symbol ~unitname:ui.ui_symbol None
  end

(* Register the approximation of the module being compiled *)

let unit_for_global id =
  let sym_label = Linkage_name.create (symbol_for_global id) in
  Compilation_unit.create id sym_label

let predefined_exception_compilation_unit =
  Compilation_unit.create (Ident.create_persistent "__dummy__")
    (Linkage_name.create "__dummy__")

let is_predefined_exception sym =
  Compilation_unit.equal
    predefined_exception_compilation_unit
    (Symbol.compilation_unit sym)

let symbol_for_global' id =
  let sym_label = Linkage_name.create (symbol_for_global id) in
  if Ident.is_predef id then
    Symbol.create predefined_exception_compilation_unit sym_label
  else
    Symbol.create (unit_for_global id) sym_label

(* Exporting and importing cross module information for Closure *)

let set_global_approx_for_unit ui approx =
  assert(not Config.flambda);
  match ui.ui_sections_to_write_rev with
  | [] -> ignore ((add_section ui (Closure approx)) : int)
  | _::_ -> Misc.fatal_error "Closure value approximation already set"

let set_global_approx approx =
  set_global_approx_for_unit current_unit approx

(* Handling of export information for Flambda 2 *)

let ensure_is_flambda_section ui section_contents =
  match section_contents with
  | None -> None
  | Some section_contents ->
    match ((Obj.obj section_contents) : section_contents) with
    | Flambda contents -> Some contents
    | Closure _ ->
      Misc.fatal_errorf "The .cmx file for module %s was written by the \
          Closure middle end, not Flambda 2.  Please recompile it."
        ui.ui_name

let read_flambda_section_from_cmx_file ui ~index =
  read_section_from_cmx_file ui ~index
  |> ensure_is_flambda_section ui

let read_flambda_header_section_for_unit_from_cmx_file ui =
  (* The header is always the last section. *)
  let index = num_sections_read_from_cmx_file ui - 1 in
  read_section_from_cmx_file ui ~index
  |> ensure_is_flambda_section ui
  |> Option.map (fun contents ->
      Flambda_cmx_format.associate_with_loaded_cmx_file
        ~header_contents:contents
        ~read_flambda_section_from_cmx_file:(fun ~index ->
          match read_flambda_section_from_cmx_file ui ~index with
          | None -> Misc.fatal_error "Could not read Flambda section"
          | Some contents -> contents))

let read_flambda_header_section_from_cmx_file id =
  match get_global_info id with
  | None -> None
  | Some ui -> read_flambda_header_section_for_unit_from_cmx_file ui

let set_flambda_export_info_for_unit ui flambda_cmx =
  match current_unit.ui_sections_to_write_rev with
  | [] ->
    (* First the subsidiary sections have their indexes (not byte offsets)
       assigned.  Then the remainder of the Flambda export info is updated
       to take account of these offsets, before being put out as the last
       section. *)
    let header_contents =
      Flambda_cmx_format.header_contents flambda_cmx
        ~add_code_section:(fun contents ->
          add_section ui (Flambda contents))
      |> Obj.repr
    in
    ignore ((add_section ui (Flambda header_contents)) : int)
  | _::_ -> Misc.fatal_error "Flambda export info already set"

let set_flambda_export_info flambda_cmx =
  set_flambda_export_info_for_unit current_unit flambda_cmx

let need_curry_fun n =
  if not (List.mem n current_unit.ui_curry_fun) then
    current_unit.ui_curry_fun <- n :: current_unit.ui_curry_fun

let need_apply_fun n =
  assert(n > 0);
  if not (List.mem n current_unit.ui_apply_fun) then
    current_unit.ui_apply_fun <- n :: current_unit.ui_apply_fun

let need_send_fun n =
  if not (List.mem n current_unit.ui_send_fun) then
    current_unit.ui_send_fun <- n :: current_unit.ui_send_fun

(* Write the description of the current unit *)

let write_unit_info info filename =
  let marshalled_sections = prepare_sections_for_export info in
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc info;
  List.iter (output_string oc) marshalled_sections;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

let save_unit_info filename =
  current_unit.ui_imports_cmi <- Env.imports();
  write_unit_info current_unit filename

let current_unit () =
  match Compilation_unit.get_current () with
  | Some current_unit -> current_unit
  | None -> Misc.fatal_error "Compilenv.current_unit"

let current_unit_symbol () =
  Symbol.create (current_unit ()) (current_unit_linkage_name ())

let const_label = ref 0

let new_const_symbol () =
  incr const_label;
  make_symbol (Some (Int.to_string !const_label))

let snapshot () = !structured_constants
let backtrack s = structured_constants := s

let new_structured_constant cst ~shared =
  let {strcst_shared; strcst_all} = !structured_constants in
  if shared then
    try
      CstMap.find cst strcst_shared
    with Not_found ->
      let lbl = new_const_symbol() in
      structured_constants :=
        {
          strcst_shared = CstMap.add cst lbl strcst_shared;
          strcst_all = SymMap.add lbl cst strcst_all;
        };
      lbl
  else
    let lbl = new_const_symbol() in
    structured_constants :=
      {
        strcst_shared;
        strcst_all = SymMap.add lbl cst strcst_all;
      };
    lbl

let add_exported_constant s =
  Hashtbl.replace exported_constants s ()

let clear_structured_constants () =
  structured_constants := structured_constants_empty

let structured_constant_of_symbol s =
  SymMap.find_opt s (!structured_constants).strcst_all

let structured_constants () =
  let provenance : Clambda.usymbol_provenance =
    { original_idents = [];
      module_path =
        Path.Pident (Ident.create_persistent (current_unit_name ()));
    }
  in
  SymMap.bindings (!structured_constants).strcst_all
  |> List.map
    (fun (symbol, definition) ->
       {
         Clambda.symbol;
         exported = Hashtbl.mem exported_constants symbol;
         definition;
         provenance = Some provenance;
       })

let require_global global_ident =
  if not (Ident.is_predef global_ident) then
    ignore (get_global_info global_ident : Cmx_format.unit_infos option)

(* Error report *)

open Format

let report_error ppf = function
  | Not_a_unit_info filename ->
      fprintf ppf "%a@ is not a compilation unit description."
        Location.print_filename filename
  | Corrupted_unit_info filename ->
      fprintf ppf "Corrupted compilation unit description@ %a"
        Location.print_filename filename
  | Illegal_renaming(name, modname, filename) ->
      fprintf ppf "%a@ contains the description for unit\
                   @ %s when %s was expected"
        Location.print_filename filename name modname

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
