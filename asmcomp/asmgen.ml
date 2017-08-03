(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* From lambda to assembly code *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

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

let flambda_raw_clambda_dump_if ppf
      ({ Flambda_to_clambda. expr = ulambda; preallocated_blocks = _;
        structured_constants; exported = _; } as input) =
  if !dump_rawclambda then
    begin
      Format.fprintf ppf "@.clambda (before Un_anf):@.";
      Printclambda.clambda ppf ulambda;
      Symbol.Map.iter (fun sym (cst, _provenance) ->
          Format.fprintf ppf "%a:@ %a@."
            Symbol.print sym
            Printclambda.structured_constant cst)
        structured_constants
    end;
  if !dump_cmm then Format.fprintf ppf "@.cmm:@.";
  input

type clambda_and_constants =
  Clambda.ulambda *
  Clambda.preallocated_block list *
  Clambda.preallocated_constant list

let raw_clambda_dump_if ppf
      ((ulambda, _, structured_constants):clambda_and_constants) =
  if !dump_rawclambda || !dump_clambda then
    begin
      Format.fprintf ppf "@.clambda:@.";
      Printclambda.clambda ppf ulambda;
      List.iter (fun {Clambda.symbol; definition} ->
          Format.fprintf ppf "%s:@ %a@."
            symbol
            Printclambda.structured_constant definition)
        structured_constants
    end;
  if !dump_cmm then Format.fprintf ppf "@.cmm:@."

let rec regalloc ppf round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if ppf dump_live "Liveness analysis" fd;
  if !use_linscan then begin
    (* Linear Scan *)
    Interval.build_intervals fd;
    if !dump_interval then Printmach.intervals ppf ();
    Linscan.allocate_registers()
  end else begin
    (* Graph Coloring *)
    Interf.build_graph fd;
    if !dump_interf then Printmach.interferences ppf ();
    if !dump_prefer then Printmach.preferences ppf ();
    Coloring.allocate_registers()
  end;
  dump_if ppf dump_regalloc "After register allocation" fd;
  let (newfd, redo_regalloc) = Reload.fundecl fd in
  dump_if ppf dump_reload "After insertion of reloading code" newfd;
  if redo_regalloc then begin
    Reg.reinit(); Liveness.fundecl ppf newfd; regalloc ppf (round + 1) newfd
  end else newfd

let available_regs ppf fundecl =
  if not !Clflags.debug then fundecl
  else begin
    Printmach.print_availability := true;
    (* CR mshinwell: add -d... *)
(*     dump_if ppf dump_regalloc "After register allocation" fd; *)
(* CR mshinwell: don't do Available_regs except with -g.  It seems we still
   get some available sets even without -g, though the pass doesn't run *)
    let fd = Available_regs.fundecl fundecl in
    dump_if ppf dump_regalloc "After Available_regs" fd;
    fd
  end

let emit ppf fundecl ~dwarf =
  let available_ranges, fundecl =
    Available_ranges.create ~fundecl:(Available_filtering.fundecl fundecl)
  in
  if !Clflags.dump_linear then begin
    Format.fprintf ppf
      "*** %s@.%a@." "Available subranges before coalescing of labels"
      (Printlinear.fundecl_with_available_ranges available_ranges) fundecl
  end;
  let label_rewriting, fundecl = Coalesce_labels.fundecl fundecl in
  let available_ranges =
    Available_ranges.rewrite_labels available_ranges ~env:label_rewriting
  in
  if !Clflags.dump_linear then begin
    Format.fprintf ppf
      "*** %s@.%a@." "Available subranges after coalescing of labels"
      (Printlinear.fundecl_with_available_ranges available_ranges) fundecl
  end;
  let emit_info = Emit.fundecl fundecl in
  match dwarf with
  | None -> ()
  | Some dwarf ->
    Dwarf.dwarf_for_function_definition dwarf ~fundecl
      ~available_ranges ~emit_info

let (++) x f = f x

let compile_fundecl (ppf : formatter) ~dwarf fd_cmm =
  Proc.init ();
  Reg.reset();
  Selectgen.reset ();
  fd_cmm
  ++ Profile.record ~accumulate:true "selection" Selection.fundecl
  ++ pass_dump_if ppf dump_selection "After instruction selection"
  ++ Profile.record ~accumulate:true "comballoc" Comballoc.fundecl
  ++ pass_dump_if ppf dump_combine "After allocation combining"
  ++ Profile.record ~accumulate:true "cse" CSE.fundecl
  ++ pass_dump_if ppf dump_cse "After CSE"
  ++ Profile.record ~accumulate:true "liveness" (liveness ppf)
  ++ Profile.record ~accumulate:true "deadcode" Deadcode.fundecl
  ++ pass_dump_if ppf dump_live "Liveness analysis"
  ++ Profile.record ~accumulate:true "spill" Spill.fundecl
  ++ Profile.record ~accumulate:true "liveness" (liveness ppf)
  ++ pass_dump_if ppf dump_spill "After spilling"
  ++ Profile.record ~accumulate:true "split" Split.fundecl
  ++ pass_dump_if ppf dump_split "After live range splitting"
  ++ Profile.record ~accumulate:true "liveness" (liveness ppf)
  ++ Profile.record ~accumulate:true "regalloc" (regalloc ppf 1)
  ++ Profile.record ~accumulate:true "available_regs" (available_regs ppf)
  ++ pass_dump_if ppf dump_live "Available regs"
  ++ Profile.record ~accumulate:true "linearize" Linearize.fundecl
  ++ pass_dump_linear_if ppf dump_linear "Linearized code"
  ++ Profile.record ~accumulate:true "scheduling" Scheduling.fundecl
  ++ pass_dump_linear_if ppf dump_scheduling "After instruction scheduling"
  ++ Profile.record ~accumulate:true "emit" (emit ppf ~dwarf)

let compile_phrase ppf ~dwarf p =
  if !dump_cmm then fprintf ppf "%a@." Printcmm.phrase p;
  match p with
  | Cfunction fd -> compile_fundecl ppf ~dwarf fd
  | Cdata dl -> Emit.data dl


(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns ppf ~dwarf f =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name ->
           compile_phrase ppf ~dwarf ph
       | _ -> ())
    (Cmmgen.generic_functions true [Compilenv.current_unit_infos ()])

let compile_unit _output_prefix asm_filename keep_asm
      obj_filename gen =
  let create_asm = keep_asm || not !Emitaux.binary_backend_available in
  Emitaux.create_asm_file := create_asm;
  try
    if create_asm then Emitaux.output_channel := open_out asm_filename;
    begin try
      gen ();
      if create_asm then close_out !Emitaux.output_channel;
    with exn when create_asm ->
      close_out !Emitaux.output_channel;
      if not keep_asm then remove_file asm_filename;
      raise exn
    end;
    let assemble_result =
      Profile.record "assemble"
        (Proc.assemble_file asm_filename) obj_filename
    in
    if assemble_result <> 0
    then raise(Error(Assembler_error asm_filename));
    if create_asm && not keep_asm then remove_file asm_filename
  with exn ->
    remove_file obj_filename;
    raise exn

let set_export_info (ulambda, prealloc, structured_constants, export) =
  Compilenv.set_export_info export;
  (ulambda, prealloc, structured_constants)

let end_gen_implementation ?toplevel ~source_provenance ppf
    (clambda:clambda_and_constants) =
  Emit.begin_assembly ();
  let dwarf =
    if not !Clflags.debug then None
    else begin
      let dwarf = Dwarf.create ~source_provenance in
      let _, toplevel_inconstants, toplevel_constants = clambda in
      Dwarf.dwarf_for_toplevel_constants dwarf toplevel_constants;
      Dwarf.dwarf_for_toplevel_inconstants dwarf toplevel_inconstants;
      Some dwarf
    end
  in
  let unit_name =
    (* CR mshinwell: find out how to fix this properly *)
    try
      Compilation_unit.get_persistent_ident
        (Compilation_unit.get_current_exn ())
    with _exn ->
      Ident.create_persistent "(unknown)"
  in
  clambda
  ++ Profile.record "cmm" (Cmmgen.compunit ~unit_name)
  ++ Profile.record "compile_phrases" (List.iter (compile_phrase ~dwarf ppf))
  ++ (fun () -> ());
  (match toplevel with None -> () | Some f -> compile_genfuns ppf ~dwarf f);

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
  Emit.end_assembly ~before_code_generation:(fun asm ->
    begin match dwarf with
    | None -> ()
    | Some dwarf -> Dwarf.emit dwarf asm
    end)

let flambda_gen_implementation ?toplevel ~backend ppf
    (program:Flambda.program) =
  let export = Build_export_info.build_export_info ~backend program in
  let (clambda, preallocated, constants) =
    Profile.record_call "backend" (fun () ->
      (program, export)
      ++ Flambda_to_clambda.convert
      ++ flambda_raw_clambda_dump_if ppf
      ++ (fun { Flambda_to_clambda. expr; preallocated_blocks;
                structured_constants; exported; } ->
             (* "init_code" following the name used in
                [Cmmgen.compunit_and_constants]. *)
           Un_anf.apply expr ~what:"init_code", preallocated_blocks,
           structured_constants, exported)
      ++ set_export_info)
  in
  let constants =
    List.map (fun (symbol, (definition, provenance)) ->
        { Clambda.symbol = Linkage_name.to_string (Symbol.label symbol);
          exported = true;
          provenance;
          definition })
      (Symbol.Map.bindings constants)
  in
  end_gen_implementation ?toplevel ppf
    (clambda, preallocated, constants)

let lambda_gen_implementation ?toplevel ppf
    (lambda:Lambda.program) =
  let clambda = Closure.intro lambda.main_module_block_size lambda.code in
  let preallocated_block =
    Clambda.{
      symbol = Compilenv.make_symbol None;
      exported = true;
      tag = 0;
      size = lambda.main_module_block_size;
      provenance = None;
    }
  in
  let clambda_and_constants =
    clambda, [preallocated_block], []
  in
  raw_clambda_dump_if ppf clambda_and_constants;
  end_gen_implementation ?toplevel ~source_provenance ppf
    clambda_and_constants

let compile_implementation_gen ?toplevel prefixname
    ~required_globals ppf gen_implementation program =
  let asmfile =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm
  in
  compile_unit prefixname asmfile !keep_asm_file
      (prefixname ^ ext_obj) (fun () ->
        Ident.Set.iter Compilenv.require_global required_globals;
        gen_implementation ?toplevel ppf program)

let compile_implementation_clambda ?toplevel prefixname
    ppf (program:Lambda.program) =
  compile_implementation_gen ?toplevel prefixname
    ~required_globals:program.Lambda.required_globals
    ppf lambda_gen_implementation program

let compile_implementation_flambda ?toplevel prefixname
    ~required_globals ~backend ppf (program:Flambda.program) =
  compile_implementation_gen ?toplevel prefixname
    ~required_globals ppf (flambda_gen_implementation ~backend) program

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
