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

(* Pretty-printing of pseudo machine code *)

open Format
open Cmm
open Reg
open Mach
open Interval

module V = Backend_var

let reg = Reg.print ~register_name:Proc.register_name

let regs ppf v =
  match Array.length v with
  | 0 -> ()
  | 1 -> reg ppf v.(0)
  | n -> reg ppf v.(0);
         for i = 1 to n-1 do fprintf ppf " %a" reg v.(i) done

let regset ppf s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then begin first := false; fprintf ppf "%a" reg r end
      else fprintf ppf "@ %a" reg r)
    s

let regsetaddr ppf s =
  let first = ref true in
  Reg.Set.iter
    (fun r ->
      if !first then begin first := false; fprintf ppf "%a" reg r end
      else fprintf ppf "@ %a" reg r;
      match r.typ with
      | Val -> fprintf ppf "*"
      | Addr -> fprintf ppf "!"
      | _ -> ())
    s

let intcomp = function
  | Isigned c -> Printf.sprintf " %ss " (Printcmm.integer_comparison c)
  | Iunsigned c -> Printf.sprintf " %su " (Printcmm.integer_comparison c)

let floatcomp c =
    Printf.sprintf " %sf " (Printcmm.float_comparison c)

let intop = function
  | Iadd -> " + "
  | Isub -> " - "
  | Imul -> " * "
  | Imulh -> " *h "
  | Idiv -> " div "
  | Imod -> " mod "
  | Iand -> " & "
  | Ior ->  " | "
  | Ixor -> " ^ "
  | Ilsl -> " << "
  | Ilsr -> " >>u "
  | Iasr -> " >>s "
  | Icomp cmp -> intcomp cmp
  | Icheckbound { label_after_error; spacetime_index; } ->
    if not Config.spacetime then " check > "
    else
      Printf.sprintf "check[lbl=%s,index=%d] > "
        begin
          match label_after_error with
          | None -> ""
          | Some lbl -> string_of_int lbl
        end
        spacetime_index

let test tst ppf arg =
  match tst with
  | Itruetest -> reg ppf arg.(0)
  | Ifalsetest -> fprintf ppf "not %a" reg arg.(0)
  | Iinttest cmp -> fprintf ppf "%a%s%a" reg arg.(0) (intcomp cmp) reg arg.(1)
  | Iinttest_imm(cmp, n) -> fprintf ppf "%a%s%i" reg arg.(0) (intcomp cmp) n
  | Ifloattest cmp ->
      fprintf ppf "%a%s%a"
       reg arg.(0) (floatcomp cmp) reg arg.(1)
  | Ieventest -> fprintf ppf "%a & 1 == 0" reg arg.(0)
  | Ioddtest -> fprintf ppf "%a & 1 == 1" reg arg.(0)

let print_live = ref false

let operation op arg ppf res =
  if Array.length res > 0 then fprintf ppf "%a := " regs res;
  match op with
  | Imove -> regs ppf arg
  | Ispill -> fprintf ppf "%a (spill)" regs arg
  | Ireload -> fprintf ppf "%a (reload)" regs arg
  | Iconst_int n -> fprintf ppf "%s" (Nativeint.to_string n)
  | Iconst_float f -> fprintf ppf "%F" (Int64.float_of_bits f)
  | Iconst_symbol s -> fprintf ppf "\"%a\"" Backend_sym.print s
  | Icall_ind _ -> fprintf ppf "call %a" regs arg
  | Icall_imm { func; _ } ->
    fprintf ppf "call \"%a\" %a" Backend_sym.print func regs arg
  | Itailcall_ind _ -> fprintf ppf "tailcall %a" regs arg
  | Itailcall_imm { func; } ->
    fprintf ppf "tailcall \"%a\" %a" Backend_sym.print func regs arg
  | Iextcall { func; alloc; _ } ->
      fprintf ppf "extcall \"%a\" %a%s" Backend_sym.print func regs arg
      (if alloc then "" else " (noalloc)")
  | Istackoffset n ->
      fprintf ppf "offset stack %i" n
  | Iload(chunk, addr) ->
      fprintf ppf "%s[%a]"
       (Printcmm.chunk chunk) (Arch.print_addressing reg addr) arg
  | Istore(chunk, addr, is_assign) ->
      fprintf ppf "%s[%a] := %a %s"
       (Printcmm.chunk chunk)
       (Arch.print_addressing reg addr)
       (Array.sub arg 1 (Array.length arg - 1))
       reg arg.(0)
       (if is_assign then "(assign)" else "(init)")
  | Ialloc { bytes = n; _ } ->
    fprintf ppf "alloc %i" n;
    if Config.spacetime then begin
      fprintf ppf "(spacetime node = %a)" reg arg.(0)
    end
  | Iintop(op) -> fprintf ppf "%a%s%a" reg arg.(0) (intop op) reg arg.(1)
  | Iintop_imm(op, n) -> fprintf ppf "%a%s%i" reg arg.(0) (intop op) n
  | Inegf -> fprintf ppf "-f %a" reg arg.(0)
  | Iabsf -> fprintf ppf "absf %a" reg arg.(0)
  | Iaddf -> fprintf ppf "%a +f %a" reg arg.(0) reg arg.(1)
  | Isubf -> fprintf ppf "%a -f %a" reg arg.(0) reg arg.(1)
  | Imulf -> fprintf ppf "%a *f %a" reg arg.(0) reg arg.(1)
  | Idivf -> fprintf ppf "%a /f %a" reg arg.(0) reg arg.(1)
  | Ifloatofint -> fprintf ppf "floatofint %a" reg arg.(0)
  | Iintoffloat -> fprintf ppf "intoffloat %a" reg arg.(0)
  | Iname_for_debugger { ident; is_parameter; } ->
    fprintf ppf "name_for_debugger %a%s=%a"
      V.print ident
      (match is_parameter with
        | Is_parameter.Local -> ""
        | Is_parameter.Parameter { index; } -> sprintf "[P%d]" index)
      reg arg.(0)
  | Ispecific op ->
      Arch.print_specific_operation reg op ppf arg

let rec instr ppf i =
  if !print_live then begin
    fprintf ppf "@[<1>{%a" regsetaddr i.live;
    if Array.length i.arg > 0 then fprintf ppf "@ +@ %a" regs i.arg;
    fprintf ppf "}@]@,";
    if !Clflags.dump_avail then begin
      let module RAS = Reg_availability_set in
      (* CR mshinwell: Just have [Insn_debuginfo.print] or something *)
      fprintf ppf "@[<1>AB={%a},PAB={%a}"
        (RAS.print ~print_reg:reg) (Insn_debuginfo.available_before i.dbg)
        Backend_var.Set.print (Insn_debuginfo.phantom_available_before i.dbg);
      begin match Insn_debuginfo.available_across i.dbg with
      | None -> ()
      | Some available_across ->
        fprintf ppf ",AA={%a}" (RAS.print ~print_reg:reg) available_across
      end;
      fprintf ppf "@]@,"
    end
  end;
  begin match i.desc with
  | Iend -> ()
  | Iop op ->
      operation op i.arg ppf i.res
  | Ireturn ->
      fprintf ppf "return %a" regs i.arg
  | Iifthenelse(tst, ifso, ifnot) ->
      fprintf ppf "@[<v 2>if %a then@,%a" (test tst) i.arg instr ifso;
      begin match ifnot.desc with
      | Iend -> ()
      | _ -> fprintf ppf "@;<0 -2>else@,%a" instr ifnot
      end;
      fprintf ppf "@;<0 -2>endif@]"
  | Iswitch(index, cases) ->
      fprintf ppf "switch %a" reg i.arg.(0);
      for i = 0 to Array.length cases - 1 do
        fprintf ppf "@,@[<v 2>@[";
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then fprintf ppf "case %i:@," j
        done;
        fprintf ppf "@]@,%a@]" instr cases.(i)
      done;
      fprintf ppf "@,endswitch"
  | Iloop(body) ->
      fprintf ppf "@[<v 2>loop@,%a@;<0 -2>endloop@]" instr body
  | Icatch(flag, handlers, body) ->
      fprintf ppf "@[<v 2>catch%a@,%a@;<0 -2>with"
        Printcmm.rec_flag flag instr body;
      let h (nfail, handler) =
        fprintf ppf "(%d)@,%a@;" nfail instr handler in
      let rec aux = function
        | [] -> ()
        | [v] -> h v
        | v :: t ->
            h v;
            fprintf ppf "@ and";
            aux t
      in
      aux handlers
  | Iexit i ->
      fprintf ppf "exit(%d)" i
  | Itrywith(body, handler) ->
      fprintf ppf "@[<v 2>try@,%a@;<0 -2>with@,%a@;<0 -2>endtry@]"
             instr body instr handler
  | Iraise k ->
      fprintf ppf "%a %a" Printcmm.raise_kind k reg i.arg.(0)
  end;
  let dbg = Insn_debuginfo.dbg i.dbg in
  if not (Debuginfo.is_none dbg) then
    fprintf ppf "%a" Debuginfo.print dbg;
  begin match i.next.desc with
    Iend -> ()
  | _ -> fprintf ppf "@,%a" instr i.next
  end

let phantom_defining_expr ppf defining_expr =
  match defining_expr with
  | Iphantom_const_int i ->
    fprintf ppf "@[(const_int@ %a)@]" Targetint.print i
  | Iphantom_const_symbol sym ->
    fprintf ppf "@[(const_symbol@ %a)@]" Backend_sym.print sym
  | Iphantom_var var ->
    fprintf ppf "@[(var@ %a)@]" Backend_var.print var
  | Iphantom_offset_var { var; offset_in_words; } ->
    fprintf ppf "@[((var@ %a)@ (offset_in_words@ %d))@]"
      Backend_var.print var
      offset_in_words
  | Iphantom_read_field { var; field; } ->
    fprintf ppf "@[((var@ %a)@ (field@ %d))@]"
      Backend_var.print var
      field
  | Iphantom_read_symbol_field { sym; field; } ->
    fprintf ppf "@[((symbol@ %a)@ (field@ %d))@]"
      Backend_sym.print sym
      field
  | Iphantom_block { tag; fields; } ->
    fprintf ppf "@[((tag@ %d)@ (fields@ @[(%a)@]))@]"
      tag
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
        (Misc.Stdlib.Option.print Backend_var.print)) fields

let phantom_let ppf (provenance_opt, defining_expr) =
  match provenance_opt with
  | None ->
    fprintf ppf "@[<hov 1>((defining_expr@ %a))@]"
      phantom_defining_expr defining_expr
  | Some provenance ->
    fprintf ppf "@[<hov 1>(\
        @[<hov 1>(provenance@ %a)@]@ \
        @[<hov 1>(defining_expr@ %a)@]\
        )@]"
      Backend_var.Provenance.print provenance
      phantom_defining_expr defining_expr

let fundecl ppf f =
  let dbg =
    if not !Clflags.dump_mach_dbg then ""
    else Format.asprintf " %a" Debuginfo.Function.print f.fun_dbg
  in
  fprintf ppf "@[<v 2>%a(%a)%s@,%a"
    Backend_sym.print f.fun_name regs f.fun_args dbg instr f.fun_body;
  if not (Backend_var.Map.is_empty f.fun_phantom_lets) then begin
    fprintf ppf "@,With phantom lets:@,%a"
      (Backend_var.Map.print phantom_let) f.fun_phantom_lets
  end;
  fprintf ppf "@]"

let phase msg ppf f =
  fprintf ppf "*** %s@.%a@." msg fundecl f

let interference ppf r =
  let interf ppf =
   List.iter
    (fun r -> fprintf ppf "@ %a" reg r)
    r.interf in
  fprintf ppf "@[<2>%a:%t@]@." reg r interf

let interferences ppf () =
  fprintf ppf "*** Interferences@.";
  List.iter (interference ppf) (Reg.all_registers())

let interval ppf i =
  let interv ppf =
    List.iter
      (fun r -> fprintf ppf "@ [%d;%d]" r.rbegin r.rend)
      i.ranges in
  fprintf ppf "@[<2>%a:%t@]@." reg i.reg interv

let intervals ppf () =
  fprintf ppf "*** Intervals@.";
  List.iter (interval ppf) (Interval.all_fixed_intervals());
  List.iter (interval ppf) (Interval.all_intervals())

let preference ppf r =
  let prefs ppf =
    List.iter
      (fun (r, w) -> fprintf ppf "@ %a weight %i" reg r w)
      r.prefer in
  fprintf ppf "@[<2>%a: %t@]@." reg r prefs

let preferences ppf () =
  fprintf ppf "*** Preferences@.";
  List.iter (preference ppf) (Reg.all_registers())
