(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* From lambda to assembly code *)

open Format
open Config
open Clflags
open Misc
open Cmm

type error = Assembler_error of string

exception Error of error

let liveness ppf phrase =
  Liveness.fundecl ppf phrase; phrase

let dump_if ppf flag message phrase =
  if !flag then Printmach.phase message ppf phrase

let pass_dump_if ppf flag message phrase =
  dump_if ppf flag message phrase; phrase

let pass_dump_linear_if ppf flag message phrase =
  if !flag then fprintf ppf "*** %s@.%a@." message Printlinear.fundecl phrase;
  phrase

let clambda_dump_if ppf ulambda =
  if !dump_clambda then Printclambda.clambda ppf ulambda; ulambda

let rec regalloc ppf round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
(*  dump_if ppf dump_live "Liveness analysis" fd;*)
  Interf.build_graph fd;
  if !dump_interf then Printmach.interferences ppf ();
  if !dump_prefer then Printmach.preferences ppf ();
  Coloring.allocate_registers();
  dump_if ppf dump_regalloc "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  dump_if ppf dump_reload "After insertion of reloading code" newfd;
  if redo_regalloc then begin
    Reg.reinit(); Liveness.fundecl ppf newfd; regalloc ppf (round + 1) newfd
  end else newfd

let available_ranges_and_emit ppf fundecl ~dwarf =
  let fundecl =
    match dwarf with
    | None -> fundecl
    | Some dwarf -> Dwarf.pre_emission_dwarf_for_function dwarf ~fundecl
  in
  let _ = pass_dump_linear_if ppf dump_linear "Available subranges" fundecl in
  Emit.fundecl fundecl ~dwarf

let (++) x f = f x

let compile_fundecl (ppf : formatter) ~dwarf fd_cmm =
  Proc.init ();
  Reg.reset();
  fd_cmm
  ++ Selection.fundecl
  ++ pass_dump_if ppf dump_selection "After instruction selection"
  ++ Comballoc.fundecl
  ++ pass_dump_if ppf dump_combine "After allocation combining"
  ++ CSE.fundecl
  ++ pass_dump_if ppf dump_cse "After CSE"
  ++ liveness ppf
  ++ Deadcode.fundecl
(*  ++ pass_dump_if ppf dump_live "Liveness analysis"*)
  ++ Spill.fundecl
  ++ liveness ppf
  ++ pass_dump_if ppf dump_spill "After spilling"
  ++ Split.fundecl
  ++ pass_dump_if ppf dump_split "After live range splitting"
  ++ liveness ppf
  ++ regalloc ppf 1
  ++ Available_regs.fundecl
  ++ pass_dump_if ppf dump_live "Liveness analysis"
  ++ Linearize.fundecl
  ++ pass_dump_linear_if ppf dump_linear "Linearized code"
  ++ Scheduling.fundecl
  ++ pass_dump_linear_if ppf dump_scheduling "After instruction scheduling"
  ++ available_ranges_and_emit ppf ~dwarf

let compile_phrase ppf ~dwarf p =
  if !dump_cmm then fprintf ppf "%a@." Printcmm.phrase p;
  match p with
  | Cfunction fd -> compile_fundecl ppf ~dwarf fd
  | Cdata dl -> Emit.data dl


(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns ppf f =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name ->
           compile_phrase ppf ~dwarf:None ph
       | _ -> ())
    (Cmmgen.generic_functions true [Compilenv.current_unit_infos ()])

let compile_implementation ?toplevel ~source_file_path prefixname ppf
      ((size, module_value_bindings), lam) =
(* CR mshinwell: remove
  List.iter (fun (path, ident, typ, global, pos) ->
    Printf.printf "global '%s' path '%s' ident '%s' pos=%d\n%!"
      (Ident.name global) (Path.name path) (Ident.unique_name ident)
      pos) module_value_bindings;
*)
  let asmfile =
    if !keep_asm_file
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm in
  let oc = open_out asmfile in
  begin try
    Emitaux.output_channel := oc;
    (* CR-soon mshinwell: use a proper type *)
    let start_of_code_symbol, end_of_code_symbol, start_of_data_symbol =
      Emit.begin_assembly ()
    in
    let dwarf =
      if !Clflags.debug_full then
        let macosx = (Config.system = "macosx") in
        let target =
          if macosx then `MacOS_X else `Other
        in
        let dwarf =
          (* CR mshinwell: How do we choose the DWARF format?  It must match
             whatever the CFI directives compile down to, I think. *)
          Dwarf_format.set_size `Thirty_two;
          Dwarf.create ~source_file_path
            ~emit_string:Emit.emit_string
            ~emit_symbol:Emit.emit_symbol
            ~emit_label:Emit.emit_label
            ~emit_label_declaration:Emit.emit_label_declaration
            ~emit_section_declaration:Emit.emit_section_declaration
            ~emit_switch_to_section:Emit.emit_switch_to_section
            ~start_of_code_symbol
            ~end_of_code_symbol
            ~start_of_data_symbol
            ~target
            ~module_value_bindings
        in
        Some dwarf
      else
        None
    in
    Closure.intro size lam
    ++ clambda_dump_if ppf
    ++ Cmmgen.compunit size
    ++ List.iter (compile_phrase ppf ~dwarf) ++ (fun () -> ());
    (match toplevel with None -> () | Some f -> compile_genfuns ppf f);

    (* We add explicit references to external primitive symbols.  This
       is to ensure that the object files that define these symbols,
       when part of a C library, won't be discarded by the linker.
       This is important if a module that uses such a symbol is later
       dynlinked. *)

    compile_phrase ppf ~dwarf
      (Cmmgen.reference_symbols
         (List.filter (fun s -> s <> "" && s.[0] <> '%')
            (List.map Primitive.native_name !Translmod.primitive_declarations))
      );

    Emit.end_assembly ~end_of_code_symbol;
    begin match dwarf with
    | None -> ()
    | Some dwarf -> Dwarf.emit dwarf
    end;
    close_out oc
  with x ->
    close_out oc;
    if !keep_asm_file then () else remove_file asmfile;
    raise x
  end;
  if Proc.assemble_file asmfile (prefixname ^ ext_obj) <> 0
  then raise(Error(Assembler_error asmfile));
  if !keep_asm_file then () else remove_file asmfile

(* Error report *)

let report_error ppf = function
  | Assembler_error file ->
      fprintf ppf "Assembler error, input left in file %a"
        Location.print_filename file

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
