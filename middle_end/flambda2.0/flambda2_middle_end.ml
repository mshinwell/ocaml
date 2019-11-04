(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

let check_invariants program =
  try Flambda_static.Program.invariant program
  with exn -> begin
    Format.eprintf "Program which failed invariant check:@ %a\n%!"
      Flambda_static.Program.print program;
    raise exn
  end

let print_prepared_lambda ppf lam =
  if !Clflags.dump_prepared_lambda then begin
    Format.fprintf ppf "%sAfter Prepare_lambda:%s@ %a@."
      (Flambda_colours.each_file ())
      (Flambda_colours.normal ())
      Printlambda.lambda lam
  end

let print_ilambda ppf (ilam : Ilambda.program) =
  if !Clflags.dump_ilambda then begin
    Format.fprintf ppf
      "\n%sAfter CPS conversion (return continuation %a) \
       (exception continuation %a):%s@ %a@."
      (Flambda_colours.each_file ())
      Continuation.print ilam.return_continuation
      Continuation.print ilam.exn_continuation.exn_handler
      (Flambda_colours.normal ())
      Ilambda.print ilam.expr
  end

let print_ilambda_after_mutable_variable_elimination ppf
      (ilam : Ilambda.program) =
  if !Clflags.dump_ilambda then begin
    Format.fprintf ppf
      "\n%sAfter mutable variable elimination (return continuation %a) \
       (exception continuation %a):%s@ %a@."
      (Flambda_colours.each_file ())
      Continuation.print ilam.return_continuation
      Continuation.print ilam.exn_continuation.exn_handler
      (Flambda_colours.normal ())
      Ilambda.print ilam.expr
  end

let print_rawflambda ppf program =
  if !Clflags.dump_rawflambda2 then begin
    Format.fprintf ppf "\n%sAfter closure conversion:%s@ %a@."
      (Flambda_colours.each_file ())
      (Flambda_colours.normal ())
      Flambda_static.Program.print program
  end

let print_flambda name ppf program =
  if !Clflags.dump_flambda then begin
    Format.fprintf ppf "\n%sAfter %s:%s@ %a@."
      (Flambda_colours.each_file ())
      name
      (Flambda_colours.normal ())
      Flambda_static.Program.print program
  end

let middle_end0 ppf ~prefixname:_ ~backend ~size ~filename
      ~module_ident ~module_initializer =
  Profile.record_call "flambda2.0" (fun () ->
    let prepared_lambda, recursive_static_catches =
      Profile.record_call "prepare_lambda" (fun () ->
        Prepare_lambda.run module_initializer)
    in
    print_prepared_lambda ppf prepared_lambda;
    let ilambda =
      Profile.record_call "cps_conversion" (fun () ->
        Cps_conversion.lambda_to_ilambda prepared_lambda
          ~recursive_static_catches)
    in
    print_ilambda ppf ilambda;
    let ilambda =
      if ilambda.uses_mutable_variables then begin
        let ilambda =
          Profile.record_call "eliminate_mutable_variables" (fun () ->
            Eliminate_mutable_vars.run ilambda)
        in
        print_ilambda_after_mutable_variable_elimination ppf ilambda;
        ilambda
      end else begin
        ilambda
      end
    in
    let flambda =
      Profile.record_call "closure_conversion" (fun () ->
        Closure_conversion.ilambda_to_flambda ~backend ~module_ident
          ~size ~filename ilambda)
    in
    print_rawflambda ppf flambda;
    check_invariants flambda;
    let flambda =
      Profile.record_call ~accumulate:true "simplify"
        (fun () -> Simplify.run ~backend ~round:1 flambda)
    in
    print_flambda "simplify" ppf flambda;
    let flambda =
      Profile.record_call ~accumulate:true "remove_unused_closure_vars"
        (fun () -> Remove_unused_closure_vars.run flambda)
    in
    print_flambda "remove_unused_closure_vars" ppf flambda;
    flambda)

let middle_end ~ppf_dump:ppf ~prefixname ~backend ~size ~filename ~module_ident
      ~module_initializer =
  try
    middle_end0 ppf ~prefixname ~backend ~size ~filename ~module_ident
      ~module_initializer
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sOriginal backtrace is:%s\n%s\n"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      (Printexc.raw_backtrace_to_string (Misc.fatal_error_callstack ()));
    raise Misc.Fatal_error
  end
