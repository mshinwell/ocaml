open Cmm

let code_for_prologue ~backtrace_bucket ~function_body
      ~num_allocation_points =
  (* Upon entry to an OCaml function, the backtrace top of stack pointer
     points at the word holding the hash of all previous frames.  It is
     the responsibility of the callee to push their return address and to
     update the hash.  We make callees do this rather than callers to keep
     code size down; there should be fewer callees than call points.
     To reduce complexity, this is implemented in C as a "noalloc" function;
     the overhead should be small since it's a static branch and the
     C compiler should be able to apply high optimization to the code. *)
  Clet (backtrace_bucket,
    Cop (Cextcall ("caml_allocation_profiling_prologue",
        Int,   (* Actually a naked pointer, just don't tell anyone. *)
        false, (* = noalloc *)
        Debuginfo.none),
      [Cconst_int num_allocation_points;
       (* We pass the return address since it's easy to get hold of and
          avoids invoking libunwind. *)
       Creturn_address;
      ]),
    function_body)

let profinfo_counter =
  Ident.create_persistent "caml_allocation_profiling_profinfo"

let code_for_allocation_point ~value's_header ~alloc_point_number
      ~backtrace_bucket =
  let pc = Ident.create "pc" in
  let existing_profinfo = Ident.create "existing_profinfo" in
  let new_profinfo = Ident.create "new_profinfo" in
  let profinfo = Ident.create "profinfo" in
  let offset_into_backtrace_bucket =
    ((2 * Arch.size_addr) * alloc_point_number)
  in
  let address_of_profinfo =
    Cop (Cadda, [
      Cvar backtrace_bucket;
      Cconst_int offset_into_backtrace_bucket;
    ])
  in
  let address_of_pc =
    Cop (Cadda, [
      Cvar backtrace_bucket;
      Cconst_int (offset_into_backtrace_bucket + Arch.size_addr);
    ])
  in
  let generate_new_profinfo =
    Clet (pc, Cop (Cprogram_counter, []),
      Clet (new_profinfo, Cop (Cload Word, [profinfo_counter]),
        Csequence (
          Cop (Cstore Word, [
            Cop (Cadda, [new_profinfo; Cconst_int 1]);
            profinfo_counter;
          ]),
          Csequence (
            Csequence (
              Cop (Cstore Word, [pc, address_of_pc]);
              Cop (Cstore Word, [new_profinfo, address_of_profinfo])),
            new_profinfo))))
  in
  Clet (existing_profinfo, Cop (Cload Word, [address_of_profinfo]),
    Clet (profinfo,
      Cifthenelse (
        (* Check if we have already allocated a profinfo value for this
           allocation point with the current backtrace.  If so, use that
           value; if not, allocate a new one. *)
        Cop (Ccmpa Ceq, [Cvar existing_profinfo; Cconst_pointer 0]),
        generate_new_profinfo,
        existing_profinfo),
      Cop (Cor, [profinfo; value's_header])))

let instrument_code expr =
  let next_alloc_point_number = ref 0 in
  let backtrace_bucket = Ident.create "backtrace_bucket" in
  let rec instrument_code expr =
    match expr with
    | Cblockheader value's_header ->
      let alloc_point_number = !next_alloc_point_number in
      incr next_alloc_point_number;
      code_for_allocation_point ~value's_header ~alloc_point_number
        ~backtrace_bucket
    | (Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
      | Cconst_pointer _ | Cconst_natpointer _ | Cvar _) -> expr
    | Clet (id, e1, e2) ->
      Clet (id, instrument_code e1, instrument_code e2)
    | Cassign (id, e) ->
      Cassign (id, instrument_code e)
    | Ctuple es ->
      Ctuple (List.map instrument_code es)
    | Cop (op, es) ->
      Cop (op, List.map instrument_code es)
    | Csequence (e1, e2) ->
      Csequence (instrument_code e1, instrument_code e2)
    | Cifthenelse (e1, e2, e3) ->
      Cifthenelse (instrument_code e1, instrument_code e2,
        instrument_code e3)
    | Cswitch (e, is, es) ->
      Cswitch (instrument_code e, is, Array.map instrument_code es)
    | Cloop e ->
      Cloop (instrument_code e)
    | Ccatch (n, ids, e1, e2) ->
      Ccatch (n, ids, instrument_code e1, instrument_code e2)
    | Cexit (n, es) ->
      Cexit (n, List.map instrument_code es)
    | Ctrywith (e1, id, e2) ->
      Ctrywith (instrument_code e1, id, instrument_code e2)
  in
  let function_body = instrument_code expr in
  let might_allocate = !next_alloc_point_number > 0 in
  if not might_allocate then
    expr
  else
    code_for_prologue ~backtrace_bucket ~function_body
      ~num_allocation_points:!next_alloc_point_number

let fundecl decl =
  (* CR mshinwell: prevent [allocation_profiling] being set on 32 bit *)
  if not !Clflags.allocation_profiling then decl
  else
    { decl with
      expression = instrument_code decl.expression;
    }
