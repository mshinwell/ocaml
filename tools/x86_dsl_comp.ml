[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let map_instruction_arg f (insn : X86_ast.instruction) : X86_ast.instruction =
  match insn with
  | ADD (a1, a2) -> ADD (f a1, f a2)
  | ADDSD (a1, a2) -> ADDSD (f a1, f a2)
  | AND (a1, a2) -> AND (f a1, f a2)
  | ANDPD (a1, a2) -> ANDPD (f a1, f a2)
  | BSWAP arg -> BSWAP (f arg)
  | CALL arg -> CALL (f arg)
  | CDQ -> CDQ
  | CMOV (c, a1, a2) -> CMOV (c, f a1, f a2)
  | CMP (a1, a2) -> CMP (f a1, f a2)
  | COMISD (a1, a2) -> COMISD (f a1, f a2)
  | CQO -> CQO
  | CVTSD2SI (a1, a2) -> CVTSD2SI (f a1, f a2)
  | CVTSD2SS (a1, a2) -> CVTSD2SS (f a1, f a2)
  | CVTSI2SD (a1, a2) -> CVTSI2SD (f a1, f a2)
  | CVTSS2SD (a1, a2) -> CVTSS2SD (f a1, f a2)
  | CVTTSD2SI (a1, a2) -> CVTTSD2SI (f a1, f a2)
  | DEC arg -> DEC (f arg)
  | DIVSD (a1, a2) -> DIVSD (f a1, f a2)
  | FABS -> FABS
  | FADD arg -> FADD (f arg)
  | FADDP (a1, a2) -> FADDP (f a1, f a2)
  | FCHS -> FCHS
  | FCOMP arg -> FCOMP (f arg)
  | FCOMPP -> FCOMPP
  | FCOS -> FCOS
  | FDIV arg -> FDIV (f arg)
  | FDIVP (a1, a2) -> FDIVP (f a1, f a2)
  | FDIVR arg -> FDIVR (f arg)
  | FDIVRP (a1, a2) -> FDIVRP (f a1, f a2)
  | FILD arg -> FILD (f arg)
  | FISTP arg -> FISTP (f arg)
  | FLD arg -> FLD (f arg)
  | FLD1 -> FLD1
  | FLDCW arg -> FLDCW (f arg)
  | FLDLG2 -> FLDLG2
  | FLDLN2 -> FLDLN2
  | FLDZ -> FLDZ
  | FMUL arg -> FMUL (f arg)
  | FMULP (a1, a2) -> FMULP (f a1, f a2)
  | FNSTCW arg -> FNSTCW (f arg)
  | FNSTSW arg -> FNSTSW (f arg)
  | FPATAN -> FPATAN
  | FPTAN -> FPTAN
  | FSIN -> FSIN
  | FSQRT -> FSQRT
  | FSTP arg -> FSTP (f arg)
  | FSUB arg -> FSUB (f arg)
  | FSUBP (a1, a2) -> FSUBP (f a1, f a2)
  | FSUBR arg -> FSUBR (f arg)
  | FSUBRP (a1, a2) -> FSUBRP (f a1, f a2)
  | FXCH arg -> FXCH (f arg)
  | FYL2X -> FYL2X
  | HLT -> HLT
  | IDIV arg -> IDIV (f arg)
  | IMUL (a1, a2) -> IMUL (f a1, Misc.Stdlib.Option.map f a2)
  | INC arg -> INC (f arg)
  | J (c, arg) -> J (c, f arg)
  | JMP arg -> JMP (f arg)
  | LEA (a1, a2) -> LEA (f a1, f a2)
  | LEAVE -> LEAVE
  | MOV (a1, a2) -> MOV (f a1, f a2)
  | MOVAPD (a1, a2) -> MOVAPD (f a1, f a2)
  | MOVLPD (a1, a2) -> MOVLPD (f a1, f a2)
  | MOVSD (a1, a2) -> MOVSD (f a1, f a2)
  | MOVSS (a1, a2) -> MOVSS (f a1, f a2)
  | MOVSX (a1, a2) -> MOVSX (f a1, f a2)
  | MOVSXD (a1, a2) -> MOVSXD (f a1, f a2)
  | MOVZX (a1, a2) -> MOVZX (f a1, f a2)
  | MULSD (a1, a2) -> MULSD (f a1, f a2)
  | NEG arg -> NEG (f arg)
  | NOP -> NOP
  | OR (a1, a2) -> OR (f a1, f a2)
  | POP arg -> POP (f arg)
  | PUSH arg -> PUSH (f arg)
  | RET -> RET
  | ROUNDSD (r, a1, a2) -> ROUNDSD (r, f a1, f a2)
  | SAL (a1, a2) -> SAL (f a1, f a2)
  | SAR (a1, a2) -> SAR (f a1, f a2)
  | SET (c, a) -> SET (c, f a)
  | SHR (a1, a2) -> SHR (f a1, f a2)
  | SQRTSD (a1, a2) -> SQRTSD (f a1, f a2)
  | SUB (a1, a2) -> SUB (f a1, f a2)
  | SUBSD (a1, a2) -> SUBSD (f a1, f a2)
  | TEST (a1, a2) -> TEST (f a1, f a2)
  | UCOMISD (a1, a2) -> UCOMISD (f a1, f a2)
  | XCHG (a1, a2) -> XCHG (f a1, f a2)
  | XOR (a1, a2) -> XOR (f a1, f a2)
  | XORPD (a1, a2) -> XORPD (f a1, f a2)

let filter_directives (asm : X86_ast.asm_line list) =
  Misc.Stdlib.List.filter_map (fun directive ->
      match directive with
      | X86_ast.Ins insn -> Some insn
      | _ -> None)
    asm

let erase_symbols (asm : X86_ast.instruction list) =
  List.map (fun insn ->
      map_instruction_arg (fun (arg : X86_ast.arg) : X86_ast.arg ->
          match arg with
          | Sym _ -> Sym "<sym>"
          | Mem64_RIP (data_type, _, i) -> Mem64_RIP (data_type, "<sym>", i)
          | arg -> arg)
        insn)
    asm

let feed_asm_code asm =
  X86_proc.reset_asm_code ();
  List.iter X86_proc.directive asm

let compare_core ~correct ~bad ~correct_asm ~bad_asm =
  let _correct_asm_orig = correct_asm in
  let _bad_asm_orig = bad_asm in
  let correct_asm = filter_directives correct_asm in
  let bad_asm = filter_directives bad_asm in
  let correct_asm = erase_symbols correct_asm in
  let bad_asm = erase_symbols bad_asm in
  if Pervasives.compare correct_asm bad_asm <> 0 then begin
    Printf.printf "FAILURE: %s and %s\n%!" correct bad;
    let correct_out = Filename.temp_file "x86_dsl_comp" "correct" in
    let bad_out = Filename.temp_file "x86_dsl_comp" "bad" in
    let cmd = Printf.sprintf "patdiff %s %s" correct_out bad_out in
    let correct_out = open_out correct_out in
    let bad_out = open_out bad_out in
    let correct_asm_orig =
      List.map (fun insn : X86_ast.asm_line -> Ins insn)
        correct_asm
    in
    feed_asm_code correct_asm_orig;
    X86_proc.generate_code (Some (X86_gas.generate_asm correct_out));
    let bad_asm_orig =
      List.map (fun insn : X86_ast.asm_line -> Ins insn)
        bad_asm
    in
    feed_asm_code bad_asm_orig;
    X86_proc.generate_code (Some (X86_gas.generate_asm bad_out));
    close_out correct_out;
    close_out bad_out;
    ignore (Sys.command cmd)
  end else begin
(*
    Printf.printf "OK: %s and %s\n%!" correct bad
*)
()
  end

let compare ~correct ~bad =
  let correct_chan = open_in correct in
  let bad_chan = open_in bad in
  let correct_asm : X86_ast.asm_line list =
    Marshal.from_channel correct_chan
  in
  let bad_asm : X86_ast.asm_line list =
    Marshal.from_channel bad_chan
  in
  close_in correct_chan;
  close_in bad_chan;
  compare_core ~correct ~bad ~correct_asm ~bad_asm

let read_dir dir =
  let dir = Unix.opendir dir in
  let files = ref [] in
  try
    while true do
      let file = Unix.readdir dir in
      match file with
      | "." | ".." -> ()
      | _ -> files := file :: !files
    done;
    assert false
  with End_of_file -> begin
    Unix.closedir dir;
    List.sort Pervasives.compare !files
  end

let () =
  if Array.length Sys.argv <> 3 then begin
    failwith "syntax: x86_dsl_comp <correct dir> <bad dir>"
  end;
  let correct_dir = Sys.argv.(1) in
  let bad_dir = Sys.argv.(2) in
  let correct_files = read_dir correct_dir in
  let bad_files = read_dir bad_dir in
  List.iter (fun file ->
      let correct = correct_dir ^ "/" ^ file in
      let bad = bad_dir ^ "/" ^ file in
      if not (Sys.file_exists bad) then
        let underscore = String.rindex file '_' in
        let without_stamp = String.sub file 0 underscore in
        let candidates =
          List.filter (fun candidate ->
              String.length candidate >= underscore
                && String.sub candidate 0 underscore = without_stamp)
            bad_files
        in
        match candidates with
        | [candidate] ->
          let bad = bad_dir ^ "/" ^ candidate in
          compare ~correct ~bad
        | _ -> ()
(*
          Printf.printf "MISSING: %s has no or multiple `bad' version(s)\n%!" file
*)
      else
        compare ~correct ~bad)
    correct_files
