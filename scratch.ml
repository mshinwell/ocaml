

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
