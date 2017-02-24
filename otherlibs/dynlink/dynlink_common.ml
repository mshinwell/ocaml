(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*             Mark Shinwell and Leo White, Jane Street Europe            *)
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

[@@@ocaml.warning "+a-4-9-30-40-41-42-58"]

module String = struct
  include String

  module Set = Set.Make (struct
    type t = string
    let compare = Pervasives.compare
  end)

  module Map = struct
    include Map.Make (struct
      type t = string
      let compare = Pervasives.compare
    end)

    let keys t =
      fold (fun key _data keys -> Set.add key keys) t Set.empty
  end
end

type filename = string

type implem_state =
  | Loaded
  | Not_initialized
  | Check_inited of int

type linking_error =
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error =
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

exception Error of error

(* Error report *)

let error_message = function
  | Not_a_bytecode_file name ->
      name ^ " is not an object file"
  | Inconsistent_import name ->
      "interface mismatch on " ^ name
  | Unavailable_unit name ->
      "no implementation available for " ^ name
  | Unsafe_file ->
      "this object file uses unsafe features"
  | Linking_error (name, Undefined_global s) ->
      "error while linking " ^ name ^ ".\n" ^
      "Reference to undefined global `" ^ s ^ "'"
  | Linking_error (name, Unavailable_primitive s) ->
      "error while linking " ^ name ^ ".\n" ^
      "The external function `" ^ s ^ "' is not available"
  | Linking_error (name, Uninitialized_global s) ->
      "error while linking " ^ name ^ ".\n" ^
      "The module `" ^ s ^ "' is not yet initialized"
  | Corrupted_interface name ->
      "corrupted interface file " ^ name
  | Cannot_open_dynamic_library exn ->
      "error loading shared library: " ^ (Printexc.to_string exn)
  | Inconsistent_implementation name ->
      "implementation mismatch on " ^ name
  | Library's_module_initializers_failed exn ->
      "execution of module initializers in the shared library failed: "
        ^ (Printexc.to_string exn)
  | Module_already_loaded name ->
      "The module `" ^ name ^ "' is already loaded \
        (either by the main program or a previously-dynlinked library)"
  | Private_library_cannot_implement_interface name ->
      "The interface `" ^ name ^ "' cannot be implemented by a \
        library loaded privately"

let () =
  Printexc.register_printer
    (function
      | Error err ->
          let msg = match err with
          | Not_a_bytecode_file s ->
              Printf.sprintf "Not_a_bytecode_file %S" s
          | Inconsistent_import s ->
              Printf.sprintf "Inconsistent_import %S" s
          | Unavailable_unit s ->
              Printf.sprintf "Unavailable_unit %S" s
          | Unsafe_file ->
              "Unsafe_file"
          | Linking_error (s, Undefined_global s') ->
              Printf.sprintf "Linking_error (%S, Dynlink.Undefined_global %S)"
                             s s'
          | Linking_error (s, Unavailable_primitive s') ->
              Printf.sprintf "Linking_error (%S, Dynlink.Unavailable_primitive \
                              %S)" s s'
          | Linking_error (s, Uninitialized_global s') ->
              Printf.sprintf "Linking_error (%S, Dynlink.Uninitialized_global \
                              %S)" s s'
          | Corrupted_interface s ->
              Printf.sprintf "Corrupted_interface %S" s
          | Cannot_open_dynamic_library exn ->
              Printf.sprintf "Cannot_open_dll %S" (Printexc.to_string exn)
          | Inconsistent_implementation s ->
              Printf.sprintf "Inconsistent_implementation %S" s
          | Library's_module_initializers_failed exn ->
              Printf.sprintf "Library's_module_initializers_failed %S"
                (Printexc.to_string exn)
          | Module_already_loaded name ->
              Printf.sprintf "Module_already_loaded %S" name
          | Private_library_cannot_implement_interface name ->
              Printf.sprintf "Private_library_cannot_implement_interface %S"
                name
          in
          Some (Printf.sprintf "Dynlink.Error(Dynlink.%s)" msg)
      | _ -> None)

module type Dynlink_platform = sig
  type handle

  module Unit_header : sig
    type t

    val name : t -> string
    val crc : t -> Digest.t option

    val interface_imports : t -> (string * Digest.t option) list
    val implementation_imports : t -> (string * Digest.t option) list

    val defined_symbols : t -> string list
    val unsafe_module : t -> bool
  end

  val init : unit -> unit

  val is_native : bool
  val adapt_filename : filename -> filename

  val num_globals_inited : unit -> int

  type state

  val snapshot_state : unit -> state
  val restore_state : state -> unit

  val iter_default_available_units
     : (comp_unit:string
      -> interface:Digest.t
      -> implementation:(Digest.t * implem_state) option
      -> defined_symbols:string list
      -> unit)
    -> unit

  val load : filename:string -> priv:bool -> handle * (Unit_header.t list)
  val run : handle -> unit_header:Unit_header.t -> unit
  val finish : handle -> unit
end

module Make (P : Dynlink_platform) = struct
  module UH = P.Unit_header

  type handle

  type interface_dep =
    | Name  (* the only use of the interface can be via a module alias *)
    | Contents of Digest.t

  type state = {
    ifaces: (interface_dep * filename) String.Map.t;
    (* Interfaces that have been depended upon. *)
    implems: (Digest.t option * filename * implem_state) String.Map.t;
    (* Implementations that exist in the main program or have been dynamically
       loaded. *)
    defined_symbols : String.Set.t;
    (* Symbols corresponding to compilation units or packed modules (cf.
       [Asmpackager.build_package_cmx]).  Used as a sanity check. *)
    allowed_units : String.Set.t;
    (* Units that are allowed to be referenced by a subsequently-loaded
       dynamic library. *)
    main_program_units : String.Set.t;
    (* Units forming part of the main program (i.e. not dynamically linked). *)
    public_dynamically_loaded_units : String.Set.t;
    (* All units that have been dynamically linked, not including those that
       were privately loaded. *)
  }

  let invariant state =
    let ifaces = String.Map.keys state.ifaces in
    let implems = String.Map.keys state.implems in
    assert (String.Set.subset implems ifaces);
    assert (String.Set.subset state.main_program_units ifaces);
    assert (String.Set.subset state.main_program_units implems)

  let empty_state = {
    ifaces = String.Map.empty;
    implems = String.Map.empty;
    defined_symbols = String.Set.empty;
    allowed_units = String.Set.empty;
    main_program_units = String.Set.empty;
    public_dynamically_loaded_units = String.Set.empty;
  }

  let global_state = ref empty_state

  let inited = ref false

  let unsafe_allowed = ref false

  let allow_unsafe_modules b =
    unsafe_allowed := b

  let check_symbols_disjoint ~descr syms1 syms2 =
    let exe = Sys.executable_name in
    let overlap = String.Set.inter syms1 syms2 in
    if not (String.Set.is_empty overlap) then begin
      let msg =
        Format.asprintf "%s: symbols multiply-defined %s: %a"
          exe (Lazy.force descr)
          (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
            Format.pp_print_string)
          (String.Set.elements overlap)
      in
      failwith msg
    end

  let default_available_units () =
    let exe = Sys.executable_name in
    let ifaces = ref String.Map.empty in
    let implems = ref String.Map.empty in
    let defined_symbols = ref String.Set.empty in
    P.iter_default_available_units
      (fun ~comp_unit ~interface ~implementation ~defined_symbols:symbols ->
        ifaces := String.Map.add comp_unit (Contents interface, exe) !ifaces;
        let crc, state =
          match implementation with
          | None -> None, Loaded
          | Some (crc, state) -> Some crc, state
        in
        implems := String.Map.add comp_unit (crc, exe, state) !implems;
        let symbols = String.Set.of_list symbols in
        check_symbols_disjoint ~descr:(lazy "in the executable file")
          symbols !defined_symbols;
        defined_symbols := String.Set.union symbols !defined_symbols);
    let implems = !implems in
    let main_program_units = String.Map.keys implems in
    let state =
      { ifaces = !ifaces;
        implems;
        defined_symbols = !defined_symbols;
        allowed_units = main_program_units;
        main_program_units;
        public_dynamically_loaded_units = String.Set.empty;
      }
    in
    global_state := state

  let init () =
    if not !inited then begin
      P.init ();
      default_available_units ();
      inited := true
    end

  let check_ifaces filename ui ifaces ~allowed_units =
    List.fold_left (fun ifaces (name, crc) ->
        let add_interface crc =
          if name = UH.name ui
            || String.Set.mem name allowed_units
          then
            String.Map.add name (crc, filename) ifaces
          else
            raise (Error (Unavailable_unit name))
        in
        match String.Map.find name ifaces with
        | exception Not_found ->
          begin match crc with
          | None -> add_interface Name
          | Some crc -> add_interface (Contents crc)
          end
        | old_crc, _old_src ->
          match old_crc, crc with
          | (Name | Contents _), None -> ifaces
          | Name, Some crc -> add_interface (Contents crc)
          | Contents old_crc, Some crc ->
            if old_crc <> crc then raise (Error (Inconsistent_import name))
            else ifaces)
      ifaces
      (UH.interface_imports ui)

  let check_implems filename ui ifaces implems ~priv =
    List.iter (fun (name, crc) ->
        match String.Map.find name implems with
        | exception Not_found -> raise (Error (Unavailable_unit name))
        | old_crc, _old_src, unit_state ->
          begin match old_crc, crc with
          | (None | Some _), None -> ()
          | None, Some _crc ->
            (* The [None] behaves like a CRC different from every other. *)
            raise (Error (Inconsistent_implementation name))
          | Some old_crc, Some crc ->
            if old_crc <> crc then begin
              raise (Error (Inconsistent_implementation name))
            end
          end;
          match unit_state with
          | Not_initialized ->
            raise (Error (Linking_error (filename, Uninitialized_global name)))
          | Check_inited i ->
            if P.num_globals_inited () < i then begin
              raise (Error (Linking_error (
                filename, Uninitialized_global name)))
            end
          | Loaded -> ())
      (UH.implementation_imports ui);
    let name = UH.name ui in
    if priv && String.Map.mem name ifaces then begin
      raise (Error (Private_library_cannot_implement_interface name))
    end;
    if not priv then begin
      (* This relies on the semantics of RTLD_LOCAL (or equivalent). *)
      if String.Map.mem name implems then begin
        raise (Error (Module_already_loaded name))
      end;
    end;
    String.Map.add name (UH.crc ui, filename, Not_initialized) implems

  let check_unsafe_module ui =
    if (not !unsafe_allowed) && UH.unsafe_module ui then begin
      raise (Error Unsafe_file)
    end

  let check filename (units : UH.t list) state ~priv =
    List.iter (fun ui -> check_unsafe_module ui) units;
    let new_ifaces =
      List.fold_left (fun ifaces ui ->
          check_ifaces filename ui ifaces ~allowed_units:state.allowed_units)
        state.ifaces units
    in
    let new_implems =
      List.fold_left (fun implems ui ->
          check_implems filename ui new_ifaces implems ~priv)
        state.implems units
    in
    let defined_symbols =
      List.fold_left (fun defined_symbols ui ->
          let descr =
            lazy (Printf.sprintf "between the executable file (and any \
                existing dynamically-loaded units) and the unit '%s' being \
                dynamically loaded from %s"
              (UH.name ui)
              filename)
          in
          let symbols = String.Set.of_list (UH.defined_symbols ui) in
          check_symbols_disjoint ~descr symbols defined_symbols;
          if priv then defined_symbols
          else String.Set.union symbols defined_symbols)
        state.defined_symbols
        units
    in
    let new_units =
      String.Set.of_list (List.map (fun ui -> UH.name ui) units)
    in
    let allowed_units =
      if priv then state.allowed_units
      else String.Set.union state.allowed_units new_units
    in
    let public_dynamically_loaded_units =
      if priv then state.public_dynamically_loaded_units
      else String.Set.union state.public_dynamically_loaded_units new_units
    in
    let state =
      { state with
        implems = new_implems;
        ifaces = new_ifaces;
        defined_symbols;
        allowed_units;
        public_dynamically_loaded_units;
      }
    in
    invariant state;
    state

  let set_allowed_units allowed_units =
    let allowed_units = String.Set.of_list allowed_units in
    let state =
      { !global_state with
        allowed_units;
      }
    in
    global_state := state

  let allow_only units =
    let allowed_units =
      String.Set.inter (!global_state).allowed_units
        (String.Set.of_list units)
    in
    let state =
      { !global_state with
        allowed_units;
      }
    in
    global_state := state

  let prohibit units =
    let allowed_units =
      String.Set.diff (!global_state).allowed_units
        (String.Set.of_list units)
    in
    let state =
      { !global_state with
        allowed_units;
      }
    in
    global_state := state

  let main_program_units () =
    String.Set.elements (!global_state).main_program_units

  let public_dynamically_loaded_units () =
    String.Set.elements (!global_state).public_dynamically_loaded_units

  let all_units () =
    String.Set.elements (String.Set.union
      (!global_state).main_program_units
      (!global_state).public_dynamically_loaded_units)

  let dll_filename fname =
    if Filename.is_implicit fname then Filename.concat (Sys.getcwd ()) fname
    else fname

  let load priv filename =
    init ();
    let filename = dll_filename filename in
    let old_platform_state = P.snapshot_state () in
    match P.load ~filename ~priv with
    | exception exn -> raise (Error (Cannot_open_dynamic_library exn))
    | handle, units ->
      let old_global_state = !global_state in
      global_state := check filename units !global_state ~priv;
      let exn =
        try
          List.iter (fun unit_header -> P.run handle ~unit_header) units;
          None
        with exn -> Some exn
      in
      P.finish handle;
      if priv then begin
        P.restore_state old_platform_state;
        global_state := old_global_state
      end;
      match exn with
      | None ->
        if not priv then begin
          let implems =
            List.fold_left (fun implems ui ->
                String.Map.add (UH.name ui) (UH.crc ui, filename, Loaded)
                  implems)
              (!global_state).implems units
          in
          global_state := { !global_state with implems; }
        end
      | Some exn ->
        raise (Error (Library's_module_initializers_failed exn))

  let loadfile filename = load false filename
  let loadfile_private filename = load true filename

  let is_native = P.is_native
  let adapt_filename = P.adapt_filename
end
