#2 "otherlibs/dynlink/dynlink.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

open! Dynlinkaux  (* REMOVE_ME for ../../debugger/dynlink.ml *)

module DC = Dynlink_common

module Bytecode = struct
  external register_code_fragment : bytes -> int -> string -> unit
    = "caml_register_code_fragment"

  type filename = string

  module Unit_header = struct
    type t = Cmo_format.compilation_unit

    let name (t : t) = t.cu_name
    let crc _t = None

    let interface_imports (t : t) = t.cu_imports
    let implementation_imports _t = []

    let defined_symbols (t : t) =
      List.map (fun ident -> Ident.name ident)
        (Symtable.defined_globals t.cu_reloc)

    let unsafe_module (t : t) = t.cu_primitives <> []
  end

  type handle = Pervasives.in_channel * filename * Digest.t

  let default_crcs = ref []

  let init () =
    if !Sys.interactive then begin (* PR#6802 *)
      invalid_arg "The dynlink.cma library cannot be used \
        inside the OCaml toplevel"
    end;
    default_crcs := Symtable.init_toplevel()

  type state = Symtable.global_map

  let snapshot_state () = Symtable.current_state ()
  let restore_state state = Symtable.restore_state state

  let is_native = false
  let adapt_filename f = f

  let num_globals_inited () =
    Misc.fatal_error "Should never be called for bytecode dynlink"

  let iter_default_available_units f =
    List.iter (fun (comp_unit, crc_intf) ->
       match crc_intf with
        | None -> ()
        | Some crc ->
          f ~comp_unit ~interface:crc ~implementation:None
            (* CR mshinwell: not sure about this [defined_symbols] *)
            ~defined_symbols:[comp_unit])
      !default_crcs

  let run (ic, file_name, file_digest) ~unit_header =
    let compunit : Cmo_format.compilation_unit = unit_header in
    seek_in ic compunit.cu_pos;
    let code_size = compunit.cu_codesize + 8 in
    let code = Meta.static_alloc code_size in
    unsafe_really_input ic code 0 compunit.cu_codesize;
    Bytes.unsafe_set code compunit.cu_codesize (Char.chr Opcodes.opRETURN);
    Bytes.unsafe_set code (compunit.cu_codesize + 1) '\000';
    Bytes.unsafe_set code (compunit.cu_codesize + 2) '\000';
    Bytes.unsafe_set code (compunit.cu_codesize + 3) '\000';
    Bytes.unsafe_set code (compunit.cu_codesize + 4) '\001';
    Bytes.unsafe_set code (compunit.cu_codesize + 5) '\000';
    Bytes.unsafe_set code (compunit.cu_codesize + 6) '\000';
    Bytes.unsafe_set code (compunit.cu_codesize + 7) '\000';
    begin try
      Symtable.patch_object code compunit.cu_reloc;
      Symtable.check_global_initialized compunit.cu_reloc;
      Symtable.update_global_table()
    with Symtable.Error error ->
      let new_error =
        match error with
        | Symtable.Undefined_global s -> DC.Undefined_global s
        | Symtable.Unavailable_primitive s -> DC.Unavailable_primitive s
        | Symtable.Uninitialized_global s -> DC.Uninitialized_global s
        | _ -> assert false in
      raise(DC.Error(Linking_error (file_name, new_error)))
    end;
    (* PR#5215: identify this code fragment by
       digest of file contents + unit name.
       Unit name is needed for .cma files, which produce several code
       fragments. *)
    let digest = Digest.string (file_digest ^ compunit.cu_name) in
    register_code_fragment code code_size digest;
    let events =
      if compunit.cu_debug = 0 then [| |]
      else begin
        seek_in ic compunit.cu_debug;
        [| input_value ic |]
      end in
    Meta.add_debug_info code code_size events;
    ignore((Meta.reify_bytecode code code_size) ())

  let load ~filename:file_name ~priv:_ =
    let ic = open_in_bin file_name in
    let file_digest = Digest.channel ic (-1) in
    seek_in ic 0;
    try
      let buffer =
        try really_input_string ic (String.length Config.cmo_magic_number)
        with End_of_file -> raise (DC.Error (Not_a_bytecode_file file_name))
      in
      let handle = ic, file_name, file_digest in
      if buffer = Config.cmo_magic_number then begin
        let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
        seek_in ic compunit_pos;
        let cu = (input_value ic : Cmo_format.compilation_unit) in
        handle, [cu]
      end else
      if buffer = Config.cma_magic_number then begin
        let toc_pos = input_binary_int ic in  (* Go to table of contents *)
        seek_in ic toc_pos;
        let lib = (input_value ic : Cmo_format.library) in
        begin try
          Dll.open_dlls Dll.For_execution
                        (List.map Dll.extract_dll_name lib.lib_dllibs)
        with exn ->
          raise(DC.Error(Cannot_open_dynamic_library exn))
        end;
        handle, lib.lib_units
      end else begin
        raise(DC.Error(Not_a_bytecode_file file_name))
      end
    with exc ->
      close_in ic; raise exc

  let finish (ic, _filename, _digest) =
    close_in ic
end

include DC.Make (Bytecode)

type linking_error = DC.linking_error =
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error = DC.error =
  | Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | Cannot_open_dynamic_library of exn
  | Library's_module_initializers_failed of exn
  | Inconsistent_implementation of string
  | Module_already_loaded of string
  | Private_library_cannot_implement_interface of string

exception Error = DC.Error
let error_message = DC.error_message
