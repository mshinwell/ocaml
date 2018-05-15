(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]
[@@@ocaml.warning "-26"]

(* let _dump_function_sizes flam ~backend = *)
(*   let module Backend = (val backend : Backend_intf.S) in *)
(*   let than = max_int in *)
(*   Flambda_iterators.iter_on_set_of_closures_of_program flam *)
(*     ~f:(fun ~constant:_ (set_of_closures : Flambda.Set_of_closures.t) -> *)
(*       Variable.Map.iter (fun fun_var *)
(*             (function_decl : Flambda.Function_declaration.t) -> *)
(*           let closure_id = Closure_id.wrap fun_var in *)
(*           let symbol = Backend.closure_symbol closure_id in *)
(*           match Inlining_cost.lambda_smaller' function_decl.body ~than with *)
(*           | Some size -> Format.eprintf "%a %d\n" Symbol.print symbol size *)
(*           | None -> assert false) *)
(*         set_of_closures.function_decls.funs) *)

let middle_end0 ppf ~prefixname ~backend ~size ~filename ~module_ident
      ~module_initializer =
  Profile.record_call "flambda" (fun () ->
    let pass_number = ref 0 in
    let round_number = ref 0 in
    let check flam =
      try Flambda_static.Program.invariant flam
      with exn -> begin
        Format.eprintf "Term which failed invariant check:@ %a\n%!"
          Flambda_static.Program.print flam;
        raise exn
      end
    in
    let print_prepared_lambda (lam, recursive_static_catches) =
      if not !Clflags.dump_rawflambda then begin
        lam, recursive_static_catches
      end else begin
        Format.fprintf ppf "After Prepare_lambda:@ %a@." Printlambda.lambda lam;
        lam, recursive_static_catches
      end
    in
    let print_ilambda (ilam : Ilambda.program) =
      if not !Clflags.dump_rawflambda then begin
        ilam
      end else begin
        Format.fprintf ppf
          "After CPS conversion (return continuation %a) \
           (exception continuation %a):@ %a@."
          Continuation.print ilam.return_continuation
          Continuation.print ilam.exception_continuation
          Ilambda.print ilam.expr;
        ilam
      end
    in
    let (+-+) flam (name, pass) =
      incr pass_number;
      if !Clflags.dump_flambda_verbose then begin
        Format.fprintf ppf "@.PASS: %s@." name;
        Format.fprintf ppf "Before pass %d, round %d:@ %a@." !pass_number
          !round_number Flambda_static.Program.print flam;
        Format.eprintf "\n@?"
      end;
      let flam = Profile.record ~accumulate:true name pass flam in
      if !Clflags.flambda_invariant_checks then begin
        Profile.record ~accumulate:true "check" check flam
      end;
      flam
    in
    Profile.record_call ~accumulate:true "middle_end" (fun () ->
      let flam =
        Profile.record_call ~accumulate:true "cps_and_closure_conversion"
          (fun () ->
            module_initializer
            |> Prepare_lambda.run
            |> print_prepared_lambda
            |> Cps_conversion.lambda_to_ilambda
            |> print_ilambda
            |> Closure_conversion.ilambda_to_flambda ~backend ~module_ident
                  ~size ~filename)
      in
      if !Clflags.dump_rawflambda
      then
        Format.fprintf ppf "After closure conversion:@ %a@."
          Flambda_static.Program.print flam;
      check flam;
      let fast_mode flam =
        pass_number := 0;
        (* let round = 0 in *)
        flam
        (* +-+ ("Lift_let_cont 1", Lift_let_cont.run) *)
        (* +-+ ("Sink_lets 1", Sink_lets.run) *)
        (* +-+ ("Lift_constants", Lift_constants.lift_constants ~backend) *)
        (* +-+ ("Share_constants", Share_constants.share_constants) *)
        (* +-+ ("Lift_to_toplevel", *)
        (*      Lift_to_toplevel.lift ~backend) *)
        (* +-+ ("Simplify", *)
        (*      Simplify.run ~never_inline:false *)
        (*        ~allow_continuation_inlining:true *)
        (*        ~allow_continuation_specialisation:false *)
        (*        ~backend ~prefixname ~round) *)
        (* +-+ ("Remove_unused_closure_vars 2", *)
        (*      Remove_unused_closure_vars.remove_unused_closure_variables *)
        (*        ~remove_direct_call_surrogates:false) *)
        (* +-+ ("Ref_to_variables", *)
        (*      Ref_to_variables.eliminate_ref) *)
        (* +-+ ("Initialize_symbol_to_let_symbol", *)
        (*      Initialize_symbol_to_let_symbol.run) *)
      in
      let rec loop flam =
        pass_number := 0;
        let round = !round_number in
        incr round_number;
        if !round_number > (Clflags.rounds ()) then flam
        else
          flam
          (* Beware: [Lift_constants] must be run before any pass that might
             duplicate strings. *)
          +-+ ("Lift_let_cont 1", Lift_let_cont.run)
          +-+ ("Sink_lets 1", Sink_lets.run)
(*
          +-+ ("Lift_constants", Lift_constants.lift_constants ~backend)
          +-+ ("Share_constants", Share_constants.share_constants)
          +-+ ("Remove_unused_program_constructs",
               Remove_unused_program_constructs.
                 remove_unused_program_constructs)
*)
(*
          +-+ ("Lift_to_toplevel", Lift_to_toplevel.lift)
*)
          +-+ ("Lift_let_cont 2", Lift_let_cont.run)
          +-+ ("Sink_lets 2", Sink_lets.run)
(*
          +-+ ("Remove_unused_closure_vars 1",
               Remove_unused_closure_vars.remove_unused_closure_variables
                ~remove_direct_call_surrogates:false)
*)
          +-+ ("Simplify",
               Simplify.run ~never_inline:false
                 ~allow_continuation_inlining:false
                 ~allow_continuation_specialisation:false
                 ~backend ~prefixname ~round)
(*
          +-+ ("Remove_unused_closure_vars 2",
               Remove_unused_closure_vars.remove_unused_closure_variables
                ~remove_direct_call_surrogates:false)
*)
          +-+ ("Lift_let_cont 3", Lift_let_cont.run)
          +-+ ("Sink_lets 3", Sink_lets.run)
          +-+ ("Simplify continuation unboxing & specialisation",
              Simplify.run ~never_inline:true
                ~allow_continuation_inlining:false
                ~allow_continuation_specialisation:true
                ~backend ~prefixname ~round)
          +-+ ("Remove_unused_continuation_params",
                Remove_unused_continuation_params.run ~backend)
          +-+ ("Simplify (func. inlining off, cont. inlining on)",
                Simplify.run ~never_inline:true
                  ~allow_continuation_inlining:true
                  ~allow_continuation_specialisation:false
                  ~backend ~prefixname ~round)
(*
          +-+ ("Remove_unused_closure_vars 3",
               Remove_unused_closure_vars.remove_unused_closure_variables
                ~remove_direct_call_surrogates:false)
          +-+ ("Ref_to_variables",
               Ref_to_variables.eliminate_ref)
          +-+ ("Initialize_symbol_to_let_symbol",
               Initialize_symbol_to_let_symbol.run)
*)
          |> loop
      in
      let back_end flam =
        flam
        (* +-+ ("Remove_unused_closure_vars", *)
        (*      Remove_unused_closure_vars.remove_unused_closure_variables *)
        (*        ~remove_direct_call_surrogates:true) *)
        (* +-+ ("Lift_constants", Lift_constants.lift_constants ~backend) *)
        (* +-+ ("Share_constants", Share_constants.share_constants) *)
        (* +-+ ("Remove_unused_program_constructs", *)
        (*   Remove_unused_program_constructs.remove_unused_program_constructs) *)
      in
      let flam =
        if !Clflags.classic_inlining then fast_mode flam
        else loop flam
      in
      let flam = back_end flam in
      (* Check that there aren't any unused "always inline" attributes. *)
      Flambda_static.Program.Iterators.iter_apply flam ~f:(fun apply ->
          match apply.inline with
          | Default_inline | Never_inline -> ()
          | Always_inline ->
            (* CR-someday mshinwell: consider a different error message if
               this triggers as a result of the propagation of a user's
               attribute into the second part of an over application
               (inline_and_simplify.ml line 710). *)
            Location.prerr_warning (Debuginfo.to_location apply.dbg)
              (Warnings.Inlining_impossible "[@inlined] attribute was not \
                used on this function application (the optimizer did not \
                know what function was being applied)")
          | Unroll _ ->
            Location.prerr_warning (Debuginfo.to_location apply.dbg)
              (Warnings.Inlining_impossible "[@unroll] attribute was not \
                used on this function application (the optimizer did not \
                know what function was being applied)"));
      if !Clflags.dump_flambda
      then
        Format.fprintf ppf "End of middle end:@ %a@."
          Flambda_static.Program.print flam;
      check flam;
      (* CR-someday mshinwell: add -d... option for this *)
      (* dump_function_sizes flam ~backend; *)
      flam)
  )

let middle_end ppf ~prefixname ~backend ~size ~filename ~module_ident
      ~module_initializer =
  try
    middle_end0 ppf ~prefixname ~backend ~size ~filename ~module_ident
      ~module_initializer
  with Misc.Fatal_error -> begin
    Format.eprintf "\n%sOriginal backtrace is:%s\n%s\n"
      (Misc_color.bold_red ())
      (Misc_color.reset ())
      (Printexc.raw_backtrace_to_string (Misc.fatal_error_callstack ()));
    raise Misc.Fatal_error
  end
