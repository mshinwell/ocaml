(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The batch compiler *)

open Misc
open Compile_common

let tool_name = "ocamlopt"

let with_info =
  Compile_common.with_info ~native:true ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info

let (|>>) (x, y) f = (x, f y)

(** Native compilation backend for .ml files. *)

(* XXX *)
module Flambda2_backend = struct
  let symbol_for_global' id =
    Flambda2.Symbol.unsafe_create
      (Flambda2.Compilation_unit.get_current_exn ())
      (Flambda2.Linkage_name.create (Compilenv.symbol_for_global id))

  let closure_symbol _ = failwith "Not yet implemented"

  let really_import_approx _ = failwith "Not yet implemented"
  let import_symbol _ = failwith "Not yet implemented"

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    Proc.max_arguments_for_tailcalls - 1
end
let flambda2_backend =
  (module Flambda2_backend : Flambda2.Flambda2_backend_intf.S)

let flambda i backend typed =
  if !Clflags.classic_inlining then begin
    Clflags.default_simplify_rounds := 1;
    Clflags.use_inlining_arguments_set Clflags.classic_arguments;
    Clflags.unbox_free_vars_of_closures := false;
    Clflags.unbox_specialised_args := false
  end;
  typed
  |> Profile.(record transl)
      (Translmod.transl_implementation_flambda i.module_name)
  |> Profile.(record generate)
    (fun {Lambda.module_ident; main_module_block_size;
          required_globals; code } ->
    ((module_ident, main_module_block_size), code)
    |>> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
    |>> Simplif.simplify_lambda
    |>> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
    |> (fun ((module_ident, main_module_block_size), code) ->
      let program : Lambda.program =
        { Lambda.
          module_ident;
          main_module_block_size;
          required_globals;
          code;
        }
      in
      begin
        match Sys.getenv "FLAMBDA2" with
        | exception Not_found -> ()
        | _ ->
          let _ : Flambda2.Flambda_static.Program.t =
            Flambda2.Flambda2_middle_end.middle_end ~ppf_dump:i.ppf_dump
              ~prefixname:"<prefixname>"
              ~backend:flambda2_backend
              ~size:main_module_block_size
              ~filename:"<filename>"
              ~module_ident
              ~module_initializer:code
          in
          ()
      end;
      Asmgen.compile_implementation
        ~backend
        ~filename:i.source_file
        ~prefixname:i.output_prefix
        ~middle_end:Flambda_middle_end.lambda_to_clambda
        ~ppf_dump:i.ppf_dump
        program);
    Compilenv.save_unit_info (cmx i))

let clambda i backend typed =
  Clflags.use_inlining_arguments_set Clflags.classic_arguments;
  typed
  |> Profile.(record transl)
    (Translmod.transl_store_implementation i.module_name)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Profile.(record generate)
    (fun program ->
       let code = Simplif.simplify_lambda program.Lambda.code in
       { program with Lambda.code }
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
       |> Asmgen.compile_implementation
            ~backend
            ~filename:i.source_file
            ~prefixname:i.output_prefix
            ~middle_end:Closure_middle_end.lambda_to_clambda
            ~ppf_dump:i.ppf_dump;
       Compilenv.save_unit_info (cmx i))

let implementation ~backend ~source_file ~output_prefix =
  let backend info typed =
    Compilenv.reset ?packname:!Clflags.for_package info.module_name;
    if Config.flambda
    then flambda info backend typed
    else clambda info backend typed
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" @@ fun info ->
  Compile_common.implementation info ~backend
