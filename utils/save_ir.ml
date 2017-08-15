(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Int = Numbers.Int

module Language = struct
  include Identifiable.Make (struct
    type lambda =
      | Transl
      | Simplif

    type clambda =
      | Closure
      | Flambda_to_clambda
      | Un_anf

    type flambda =
      | Closure_conversion
      | Lift_constants
      | Share_constants
      | Remove_unused_program_constructs
      | Lift_let_to_initialize_symbol
      | Lift_code
      | Inline_and_simplify
      | Remove_unused_closure_vars
      | Ref_to_variables
      | Initialize_symbol_to_let_symbol

    type mach =
      | Selection
      | Comballoc
      | CSE
      | Liveness_1
      | Deadcode
      | Spill
      | Liveness_2
      | Split
      | Liveness_3
      | Regalloc
      | Reload
      | Liveness_during_regalloc

    type linear =
      | Linearize
      | Scheduling

    type 'a pass =
      | After_all_passes
      | After of 'a

    type t =
      | Parsetree
      | Typedtree
      | Lambda of lambda pass
      | Clambda of clambda pass
      | Flambda of flambda pass
      | Cmm
      | Mach of mach pass
      | Linear of linear pass

    let compare = Pervasives.compare
    let equal t1 t2 = (compare t1 t2 = 0)
    let hash = Hashtbl.hash
  end)

  let all = [
    Parsetree;
    Typedtree;
    Lambda Transl;
    Lambda Simplif;
    Clambda Closure;
    Clambda Flambda_to_clambda;
    Clambda Un_anf;
    Flambda Closure_conversion;
    Flambda Lift_constants;
    Flambda Share_constants;
    Flambda Remove_unused_program_constructs;
    Flambda Lift_let_to_initialize_symbol;
    Flambda Lift_code;
    Flambda Inline_and_simplify;
    Flambda Remove_unused_closure_vars;
    Flambda Ref_to_variables;
    Flambda Initialize_symbol_to_let_symbol;
    Cmm;
    Mach Selection;
    Mach Comballoc;
    Mach CSE;
    Mach Liveness_1;
    Mach Deadcode;
    Mach Spill;
    Mach Liveness_2;
    Mach Split;
    Mach Liveness_3;
    Mach Regalloc;
    Mach Reload;
    Mach Liveness_during_regalloc;
    Linear Linearize;
    Linear Scheduling;
  ]

  let all_numbered =
    let _next_count, all_numbered =
    List.fold_left (fun (next_count, all_numbered) pass ->
        let all_numbered = Language.Map.add pass next_count in
        next_count + 1, all_numbered)
      (0, Language.Map.empty)
      all
    in
    all_numbered

  let number_of_pass pass =
    match Language.Map.find pass all_numbered with
    | exception Not_found -> assert false  (* see above *)
    | number -> number

  let mach_pass_name_human = function
    | Selection -> "instruction selection"
    | Comballoc -> "allocation combining"
    | CSE -> "CSE"
    | Liveness_1 -> "liveness analysis (1)"
    | Deadcode -> "dead code elimination"
    | Spill -> "insertion of suggested spills and reloads"
    | Liveness_2 -> "liveness analysis (2)"
    | Split -> "live range splitting"
    | Liveness_3 -> "liveness analysis (3)"
    | Regalloc -> "register allocation"
    | Liveness_during_regalloc -> "liveness analysis (during reg. alloc)"
    | Reload -> "insertion of remaining spills and reloads"

  let to_string = function
    | Parsetree -> "parsetree"
    | Typedtree -> "typedtree"
    | Lambda_before_simplif -> "lambda_before_simplif"
    | Lambda_after_simplif -> "lambda_after_simplif"
    | Clambda_before_un_anf -> "clambda_before_un_anf"
    | Clambda_after_un_anf -> "clambda_after_un_anf"
    | Flambda -> "flambda"
    | Flambda_all_passes -> "flambda_all_passes"
    | Cmm -> "cmm"
    | Mach -> "mach"
    | Linear -> "linear"

  let extension = function
    | Parsetree -> "parsetree"
    | Typedtree -> "typedtree"
    | Lambda of _ -> "lambda"
    | Clambda of _ -> "clambda"
    | Flambda of _ -> "flambda"
    | Cmm -> "cmm"
    | Mach of _ -> "mach"
    | Linear of _ -> "linear"
end

let should_save = ref Language.Set.empty

let should_save lang =
  should_save := Language.Set.add lang !should_save

let all_languages =
  List.map Language.to_string all_languages

let save lang ~output_prefix f term =
  if Language.Set.mem lang !should_save then begin
    let lang = Language.to_string lang in
    let number = Language.number_of_pass lang in
    let filename =
      Printf.sprintf "%s.%03d.%s.%s"
        output_prefix
        (Language.number_of_pass lang)
        (Language.to_string lang)
        (Language.extension lang)
    in
    match open_out filename with
    | exception exn ->
      Printf.eprintf "Cannot open intermediate language file \
          for writing: %s (%s)\n"
        filename (Printexc.to_string exn);
      exit 1
    | chan ->
      let formatter = Format.formatter_of_out_channel chan in
      f formatter term;
      close_out chan
  end
