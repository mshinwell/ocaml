

(*
  let hash_of_prev_frames = Ident.create "hash_of_prev_frames" in
  let backtrace_stack_init = Ident.create "backtrace_stack_init" in
  let backtrace_stack = Ident.create "backtrace_stack" in
  let backtrace_stack_limit = Ident.create "backtrace_stack_limit" in
  let return_address = Ident.create "return_address" in
  let one_word = Cconst_int Arch.size_addr in
  (* CR-someday mshinwell: Consider using a trap page under the stack with
     a signal handler to catch the stack overflow. *)
  Clet (backtrace_bucket,
    Clet (backtrace_stack_init, Cop (Cbacktrace_top_of_stack, []),
      Clet (hash_of_prev_frames, Cop (Cload Word, [backtrace_stack]),
        Clet (backtrace_stack, 
          Clet (backtrace_stack_limit, Cop (Cbacktrace_stack_limit, []),
            Cif (Cop (Ccmpa Cle,
                [Cvar backtrace_stack_init; Cvar backtrace_stack_limit])),
              (* Upon stack overflow, overwrite the most recent frame. *)
              backtrace_stack_init,
              (* Otherwise allocate one more word on the stack. *)
              Csequence (
                Cop (Cbacktrace_allocate_stack_word, []),
                Cop (Csuba, [Cvar backtrace_stack; one_word]))),
          (* [backtrace_stack] now points at the word where the new hash
             value must be saved.  [backtrace_stack + one_word] is where the
             current function's return address must be saved. *)
          Clet (return_address, Cop (Creturn_address, []),
            Clet (hash_of_all_frames, ...,
              Csequence (
                Csequence (
                  Cop (Cstore word, [return_address;
                      Cop (Cadda, [backtrace_stack; one_word])]),
                  Cop (Cstore word, [hash_of_all_frames; backtrace_stack])),
                ...

  Clet (backtrace_bucket, Cop (Cbacktrace_bucket, []),
*)

let top_of_stack =
  Ident.create_persistent "caml_allocation_profiling_backtrace_top_of_stack"


let code_for_exit_point expr ~saved_backtrace_stack_ptr =
  (* Upon exit from an OCaml function, the backtrace stack pointer must
     be restored.  To reduce the chance of error, we restore it to an
     absolute position, instead of using a relative offset. *)
  Csequence (
    Cop (Cstore Word, [saved_backtrace_stack_ptr; Cvar top_of_stack]),
    expr)

let add_epilogue ~function_body ~saved_backtrace_stack_ptr =
  let code_for_exit_point = code_for_exit_point ~saved_backtrace_stack_ptr in
  let rec instrument_exit_points expr =
    match expr with
    | (Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
      | Cconst_pointer _ | Cconst_natpointer _ | Cvar _) ->
      code_for_exit_point expr
    | Clet (id, e1, e2) ->
      Clet (id, e1, instrument_exit_points e2)
    | Cassign (id, e) -> code_for_exit_point (Cassign (id, e))
    | Ctuple es -> 
      begin match List.rev es with
      | [] -> code_for_exit_point expr
      | e_last::e_rest ->
        let e_last =
          let id = Ident.create "tuple_result" in
          Clet (id, e_last, code_for_exit_point (Cvar id))
        in
        Ctuple (List.rev (e_last::e_rest))
      end
    (* XXX: note: we could trap extcalls that are not "noalloc" and record
       things, if it helps the veneers *)
    | Cop _ -> expr
    | Csequence (e1, e2) -> Csequence (e1, instrument_exit_points e2)
    | Cifthenelse (e1, e2, e3) ->
      Cifthenelse (e1, instrument_exit_points e2, instrument_exit_points e3)
    | Cswitch (e, is, es) ->
      Cswitch (e, is, Array.map instrument_exit_points es)
    | Cloop _ -> expr
    | Ccatch (n, ids, e1, e2) ->
      Ccatch (n, ids, instrument_exit_points e1, instrument_exit_points e2)
    | Cexit _ -> expr
    | Ctrywith (e1, id, e2) ->
      Ctrywith (instrument_exit_points e1, id, instrument_exit_points e2)
  in
  instrument_exit_points function_body
