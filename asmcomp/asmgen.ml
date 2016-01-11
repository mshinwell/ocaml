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

type lambda_program =
  { code : Lambda.lambda;
    main_module_block_size : int; }

type _ backend_kind =
  | Lambda : lambda_program backend_kind
  | Flambda : Flambda.program backend_kind

type error = Assembler_error of string

exception Error of error

let max_use_index = 9 + Printlambda.num_primitives

type what =
  | Direct_apply
  | Indirect_apply
  | Send
  | Switch
  | String_switch
  | Try_with
  | Conditional_branch
  | Unconditional_branch
  | Const
  | Var
  | Prim of int

let function_sizes = ref []

let calculate_function_sizes flam ~backend ~prefixname:_ =
  let results = ref [] in
  let module Backend = (val backend : Backend_intf.S) in
  Flambda_iterators.iter_on_set_of_closures_of_program flam
    ~f:(fun ~constant:_ (set_of_closures : Flambda.set_of_closures) ->
      Variable.Map.iter (fun fun_var
            (function_decl : Flambda.function_declaration) ->
          let closure_id = Closure_id.wrap fun_var in
          let function_label = Compilenv.function_label closure_id in
          let uses = Array.make (max_use_index + 1) 0 in
          let use what =
            let use_index =
              match what with
              | Direct_apply -> 0
              | Indirect_apply -> 1
              | Send -> 2
              | Switch -> 3
              | String_switch -> 4
              | Try_with -> 5
              | Const -> 6
              | Conditional_branch -> 7
              | Unconditional_branch -> 8
              | Var -> 9
              | Prim prim -> 10 + prim
            in
            uses.(use_index) <- uses.(use_index) + 1
          in
          let rec count (lam : Flambda.t) =
            match lam with
            | Var _ -> use Var
            | Apply ({ func = _; args; kind = Direct _ }) ->
              use Var;
              List.iter (fun _arg -> use Var) args;
              use Direct_apply
            | Apply ({ func = _; args; kind = Indirect }) ->
              use Var;
              List.iter (fun _arg -> use Var) args;
              use Indirect_apply
            | Assign _ ->
              use Var;
              use Var
            | Send { kind = _; meth = _; obj = _; args; dbg = _; } ->
              use Var;
              use Var;
              List.iter (fun _arg -> use Var) args;
              use Send
            | Proved_unreachable -> ()
            | Let { defining_expr; body; _ } ->
              count_named defining_expr;
              count body
            | Let_mutable (_, _, body) ->
              use Var;
              count body
            | Let_rec (bindings, body) ->
              List.iter (fun (_, lam) -> count_named lam) bindings;
              count body
            | Switch (_, sw) ->
              use Var;
              use Switch;
              List.iter (fun (_, lam) -> count lam; use Unconditional_branch)
                sw.consts;
              List.iter (fun (_, lam) -> count lam; use Unconditional_branch)
                sw.blocks
            | String_switch (_, sw, def) ->
              use Var;
              use String_switch;
              List.iter (fun (_, lam) -> count lam; use Unconditional_branch)
                sw;
              Misc.may count def
            | Static_raise _ -> use Unconditional_branch
            | Static_catch (_, _, body, handler) ->
              count body;
              use Unconditional_branch;
              count handler
            | Try_with (body, _, handler) ->
              use Try_with;
              count body;
              count handler
            | If_then_else (_, ifso, ifnot) ->
              use Var;
              use Conditional_branch;
              count ifso;
              use Unconditional_branch;
              count ifnot
            | While (cond, body) ->
              count cond;
              use Conditional_branch;
              count body;
              use Unconditional_branch
            | For { body; _ } ->
              (* Load starting value *)
              use Var;
              (* Initialise loop counter, expected to be in a register. *)
              use Var;
              (* Top of loop (with counter contents already loaded) *)
              (* Load finishing value *)
              use Var;
              (* Check condition *)
              use (Prim (Printlambda.number_of_primitive (Pintcomp Cle)));
              use Conditional_branch;
              count body;
              (* Increment counter contents (assume still loaded) *)
              use (Prim (Printlambda.number_of_primitive Paddint));
              (* Branch to top of loop *)
              use Unconditional_branch
          and count_named (named : Flambda.named) =
            match named with
            | Symbol _ -> use Const
            | Read_mutable _ -> use Var
            | Const _ -> use Const
            | Allocated_const _ -> () (* should have been lifted *)
            | Read_symbol_field _ ->
              use Const;
              use (Prim (Printlambda.number_of_primitive (Pfield 0)))
            | Set_of_closures set_of_closures ->
              (* This is always a non-constant set of closures by this stage. *)
              Variable.Map.iter (fun _fun_var
                    (function_decl : Flambda.function_declaration) ->
                  use Const;  (* code pointer *)
                  use Const;  (* arity *)
                  if List.length function_decl.params > 1 then begin
                    use Const  (* partial application code pointer *)
                  end)
                set_of_closures.function_decls.funs;
              let env_size =
                Variable.Map.cardinal set_of_closures.free_vars
              in
              for _var = 1 to env_size do
                use Var
              done;
              use (Prim (Printlambda.number_of_primitive
                (Pmakeblock (0, Immutable))))
            | Project_closure _
            | Project_var _ ->
              use (Prim (Printlambda.number_of_primitive (Pfield 0)))
            | Move_within_set_of_closures _ ->
              use (Prim (Printlambda.number_of_primitive Paddint))
            | Prim (prim, args, _) ->
              List.iter (fun _arg -> use Var) args;
              use (Prim (Printlambda.number_of_primitive prim))
            | Expr expr -> count expr
          in
          count function_decl.body;
          let string_of_symbol prefix s =
            let spec = ref false in
            for i = 0 to String.length s - 1 do
              match String.unsafe_get s i with
              | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' -> ()
              | _ -> spec := true;
            done;
            if not !spec then if prefix = "" then s else prefix ^ s
            else
              let b = Buffer.create (String.length s + 10) in
              Buffer.add_string b prefix;
              String.iter
                (function
                  | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c ->
                    Buffer.add_char b c
                  | c -> Printf.bprintf b "$%02x" (Char.code c)
                )
                s;
              Buffer.contents b
          in
          let function_label = string_of_symbol "" function_label in
          let num_params = List.length function_decl.params in
          results := (function_label, num_params, uses) :: !results)
        set_of_closures.function_decls.funs);
  !results

let extract_sizes_from_object_file object_file ~output_prefix =
  let function_sizes = !function_sizes in
  match function_sizes with
  | [] -> ()
  | function_sizes ->
    let temp_file = Printf.sprintf "%s.sizes-tmp" object_file in
    let command =
      Printf.sprintf "/bin/sh %s %s %s"
        "/mnt/local/sda1/mshinwell/function_symbol_sizes.sh"
        object_file
        temp_file
    in
    let result = Sys.command command in
    if result <> 0 then
      Misc.remove_file temp_file
    else begin
      let finished = ref false in
      let measured_sizes = ref [] in
      let chan = open_in temp_file in
      while not !finished do
        match input_line chan with
        | exception End_of_file -> finished := true
        | line ->
          let function_label, hex_size = Misc.cut_at line ' ' in
          let size = int_of_string hex_size in
          measured_sizes := (function_label, size) :: !measured_sizes
      done;
      close_in chan;
      Misc.remove_file temp_file;
      let measured_sizes = !measured_sizes in
      let output_file = Printf.sprintf "%s.sizes" output_prefix in
      let chan = open_out output_file in
      List.iter (fun (function_label, num_params, uses) ->
          try
            let measured_size = List.assoc function_label measured_sizes in
            Printf.fprintf chan "%s %d %d " function_label measured_size
              num_params;
            let num_uses = Array.length uses in
            for use = 0 to num_uses - 1 do
              Printf.fprintf chan "%d" uses.(use);
              if use >= num_uses - 1 then
                Printf.fprintf chan "\n%!"
              else
                Printf.fprintf chan " "
            done
          with Not_found -> ())
        function_sizes;
      close_out chan
    end

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
      Symbol.Map.iter (fun sym cst ->
          Format.fprintf ppf "%a:@ %a@."
            Symbol.print sym
            Printclambda.structured_constant cst)
        structured_constants
    end;
  if !dump_cmm then Format.fprintf ppf "@.cmm:@.";
  input

let raw_clambda_dump_if ppf (ulambda, _, structured_constants) =
  if !dump_rawclambda then
    begin
      Format.fprintf ppf "@.clambda (before Un_anf):@.";
      Printclambda.clambda ppf ulambda;
      List.iter (fun ((symbol, _), cst) ->
          Format.fprintf ppf "%a:@ %a@."
            Symbol.print symbol
            Printclambda.structured_constant cst)
        structured_constants
    end;
  if !dump_cmm then Format.fprintf ppf "@.cmm:@."

let rec regalloc ppf round fd =
  if round > 50 then
    fatal_error(fd.Mach.fun_name ^
                ": function too complex, cannot complete register allocation");
  dump_if ppf dump_live "Liveness analysis" fd;
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

let (++) x f = f x

let compile_fundecl (ppf : formatter) fd_cmm =
  Proc.init ();
  Reg.reset();
  let build = Compilenv.current_build () in
  fd_cmm
  ++ Timings.(accumulate_time (Selection build)) Selection.fundecl
  ++ pass_dump_if ppf dump_selection "After instruction selection"
  ++ Timings.(accumulate_time (Comballoc build)) Comballoc.fundecl
  ++ pass_dump_if ppf dump_combine "After allocation combining"
  ++ Timings.(accumulate_time (CSE build)) CSE.fundecl
  ++ pass_dump_if ppf dump_cse "After CSE"
  ++ Timings.(accumulate_time (Liveness build)) (liveness ppf)
  ++ Timings.(accumulate_time (Deadcode build)) Deadcode.fundecl
  ++ pass_dump_if ppf dump_live "Liveness analysis"
  ++ Timings.(accumulate_time (Spill build)) Spill.fundecl
  ++ Timings.(accumulate_time (Liveness build)) (liveness ppf)
  ++ pass_dump_if ppf dump_spill "After spilling"
  ++ Timings.(accumulate_time (Split build)) Split.fundecl
  ++ pass_dump_if ppf dump_split "After live range splitting"
  ++ Timings.(accumulate_time (Liveness build)) (liveness ppf)
  ++ Timings.(accumulate_time (Regalloc build)) (regalloc ppf 1)
  ++ Timings.(accumulate_time (Linearize build)) Linearize.fundecl
  ++ pass_dump_linear_if ppf dump_linear "Linearized code"
  ++ Timings.(accumulate_time (Scheduling build)) Scheduling.fundecl
  ++ pass_dump_linear_if ppf dump_scheduling "After instruction scheduling"
  ++ Timings.(accumulate_time (Emit build)) Emit.fundecl

let compile_phrase ppf p =
  if !dump_cmm then fprintf ppf "%a@." Printcmm.phrase p;
  match p with
  | Cfunction fd -> compile_fundecl ppf fd
  | Cdata dl -> Emit.data dl

(* CR mshinwell: add -d... option for this *)
let dump_sizes =
  try ignore (Sys.getenv "FUNCTION_SIZES"); true with Not_found -> false

(* For the native toplevel: generates generic functions unless
   they are already available in the process *)
let compile_genfuns ppf f =
  List.iter
    (function
       | (Cfunction {fun_name = name}) as ph when f name ->
           compile_phrase ppf ph
       | _ -> ())
    (Cmmgen.generic_functions true [Compilenv.current_unit_infos ()])

let compile_unit ~source_provenance output_prefix asm_filename keep_asm
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
      Timings.(time (Assemble source_provenance))
        (Proc.assemble_file asm_filename) obj_filename
    in
    if assemble_result <> 0
    then raise(Error(Assembler_error asm_filename));
    if dump_sizes then begin
      extract_sizes_from_object_file obj_filename ~output_prefix
    end;
    if create_asm && not keep_asm then remove_file asm_filename
  with exn ->
    remove_file obj_filename;
    raise exn

let set_export_info (ulambda, prealloc, structured_constants, export) =
  Compilenv.set_export_info export;
  (ulambda, prealloc, structured_constants)

type clambda_and_constants =
  Clambda.ulambda *
  Clambda.preallocated_block list *
  ((Symbol.t * bool (* exported *)) *
   Clambda.ustructured_constant) list

let end_gen_implementation ?toplevel ~source_provenance ppf
    (clambda:clambda_and_constants) =
  Emit.begin_assembly ();
  clambda
  ++ Timings.(time (Cmm source_provenance)) Cmmgen.compunit_and_constants
  ++ Timings.(time (Compile_phrases source_provenance))
       (List.iter (compile_phrase ppf))
  ++ (fun () -> ());
  (match toplevel with None -> () | Some f -> compile_genfuns ppf f);
  (* We add explicit references to external primitive symbols.  This
     is to ensure that the object files that define these symbols,
     when part of a C library, won't be discarded by the linker.
     This is important if a module that uses such a symbol is later
     dynlinked. *)
  compile_phrase ppf
    (Cmmgen.reference_symbols
       (List.filter (fun s -> s <> "" && s.[0] <> '%')
          (List.map Primitive.native_name !Translmod.primitive_declarations))
    );
  Emit.end_assembly ()

let flambda_gen_implementation ?toplevel ~source_provenance ~backend
    ~prefixname ppf (program:Flambda.program) =
  if dump_sizes then begin
    function_sizes := calculate_function_sizes program ~backend ~prefixname
  end;
  let export = Build_export_info.build_export_info ~backend program in
  let (clambda, preallocated, constants) =
    Timings.time (Flambda_pass ("backend", source_provenance)) (fun () ->
      (program, export)
      ++ Flambda_to_clambda.convert
      ++ flambda_raw_clambda_dump_if ppf
      ++ (fun { Flambda_to_clambda. expr; preallocated_blocks;
                structured_constants; exported; } ->
             (* "init_code" following the name used in
                [Cmmgen.compunit_and_constants]. *)
           Un_anf.apply expr ~what:"init_code", preallocated_blocks,
           structured_constants, exported)
      ++ set_export_info) ()
  in
  let constants =
    List.map (fun (symbol, const) -> (symbol, true), const)
      (Symbol.Map.bindings constants)
  in
  end_gen_implementation ?toplevel ~source_provenance ppf
    (clambda, preallocated, constants)

let lambda_gen_implementation ?toplevel ~source_provenance ppf
    (lambda:lambda_program) =
  let clambda = Closure.intro lambda.main_module_block_size lambda.code in
  let preallocated_block =
    Clambda.{
      symbol = Linkage_name.to_string (Compilenv.current_unit_linkage_name ());
      tag = 0;
      size = lambda.main_module_block_size;
    }
  in
  let compilation_unit = Compilenv.current_unit () in
  let constants =
    let constants = Compilenv.structured_constants () in
    Compilenv.clear_structured_constants ();
    List.map (fun (name, exported, const) ->
        (Symbol.unsafe_create compilation_unit
           (Linkage_name.create name),
         exported),
        const)
      constants
  in
  let clambda_and_constants =
    clambda, [preallocated_block], constants
  in
  raw_clambda_dump_if ppf clambda_and_constants;
  end_gen_implementation ?toplevel ~source_provenance ppf clambda_and_constants

let gen_implementation (type t) ?toplevel ~source_provenance ~backend
    (backend_kind: t backend_kind) ~prefixname ppf (code : t) =
  match backend_kind with
  | Flambda ->
    flambda_gen_implementation ?toplevel ~source_provenance ~backend
      ~prefixname ppf code
  | Lambda ->
    lambda_gen_implementation ?toplevel ~source_provenance ppf code

let compile_implementation (type t) ?toplevel ~source_provenance prefixname
    ~backend (backend_kind: t backend_kind) ppf (code : t) =
  let asmfile =
    if !keep_asm_file || !Emitaux.binary_backend_available
    then prefixname ^ ext_asm
    else Filename.temp_file "camlasm" ext_asm
  in
  compile_unit ~source_provenance prefixname asmfile !keep_asm_file
      (prefixname ^ ext_obj) (fun () ->
        gen_implementation ?toplevel ~source_provenance ~backend backend_kind
          ~prefixname ppf code)

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
