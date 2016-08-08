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

(* Pretty-printing of linearized machine code *)

open Format
open Mach
open Printmach
open Linearize

let label ppf l =
  Format.fprintf ppf "L%i" l

let available_ranges = ref None

let instr ppf i =
  begin match i.desc with
  | Lend -> ()
  | Lop op ->
      begin match op with
      | Imove (* CR mshinwell: remove *)
      | Ialloc _ | Icall_ind | Icall_imm _ | Iextcall(_, _) ->
          fprintf ppf "@[<1>LA={%a}@]@," regsetaddr i.live;
          if !Clflags.debug then begin
            fprintf ppf "@[<1>AB=%a@]@,"
              (Reg_availability.print ~print_reg:Printmach.reg)
              i.available_before;
            let phantom = Ident.Set.elements i.phantom_available_before in
            fprintf ppf "@[<1>PAB={%a}@]@,"
              (Format.pp_print_list Ident.print) phantom
          end
      | _ -> ()
      end;
      operation op i.arg ppf i.res
  | Lreloadretaddr ->
      fprintf ppf "reload retaddr"
  | Lreturn ->
      fprintf ppf "return %a" regs i.arg
  | Llabel lbl ->
      fprintf ppf "%a:" label lbl;
      begin match !available_ranges with
      | None -> ()
      | Some ranges ->
        List.iter (fun (start_or_end, ident, _subrange) ->
            let start_or_end' =
              match start_or_end with
              | Available_ranges.Start _ -> "avail."
              | Available_ranges.End -> "unavail."
            in
            fprintf ppf " (%a now %s"
              Ident.print ident start_or_end';
            begin match start_or_end with
            | Available_ranges.Start { end_pos; location; } ->
              fprintf ppf " until L%d" end_pos;
              let module AS = Available_ranges.Available_subrange in
              begin match location with
              | AS.Reg (r, _, _) ->
                fprintf ppf " in %a" reg r
              | AS.Phantom (_, _, AS.Const_symbol symbol) ->
                fprintf ppf " with known value %a" Symbol.print symbol
              | AS.Phantom (_, _, AS.Read_symbol_field { symbol; field; }) ->
                fprintf ppf " with known value %a.(%d)" Symbol.print symbol
                  field
              | AS.Phantom (_, _, AS.Const_int i) ->
                fprintf ppf " with known value %d" i
              | AS.Phantom (_, _, AS.Read_field _) ->
                (* CR mshinwell: fixme *)
                fprintf ppf " from <Read_field _>"
              | AS.Phantom (_, _, AS.Offset_pointer _) ->
                (* CR mshinwell: fixme *)
                fprintf ppf " from <Offset_pointer _>"
              end
            | Available_ranges.End -> ()
            end;
            fprintf ppf ")")
          (Available_ranges.classify_label ranges lbl)
      end
  | Lbranch lbl ->
      fprintf ppf "goto %a" label lbl
  | Lcondbranch(tst, lbl) ->
      fprintf ppf "if %a goto %a" (test tst) i.arg label lbl
  | Lcondbranch3(lbl0, lbl1, lbl2) ->
      fprintf ppf "switch3 %a" reg i.arg.(0);
      let case n = function
      | None -> ()
      | Some lbl ->
         fprintf ppf "@,case %i: goto %a" n label lbl in
      case 0 lbl0; case 1 lbl1; case 2 lbl2;
      fprintf ppf "@,endswitch"
  | Lswitch lblv ->
      fprintf ppf "switch %a" reg i.arg.(0);
      for i = 0 to Array.length lblv - 1 do
       fprintf ppf "case %i: goto %a" i label lblv.(i)
      done;
      fprintf ppf "@,endswitch"
  | Lsetuptrap lbl ->
      fprintf ppf "setup trap %a" label lbl
  | Lpushtrap ->
      fprintf ppf "push trap"
  | Lpoptrap ->
      fprintf ppf "pop trap"
  | Lraise k ->
      fprintf ppf "%s %a" (Lambda.raise_kind k) reg i.arg.(0)
  | Lprologue ->
      fprintf ppf "prologue"
  | Lcapture_stack_offset _ ->
      fprintf ppf "stack offset capture for reg %a" reg i.arg.(0)
  end;
  if not (Debuginfo.is_none i.dbg) then
    fprintf ppf " %s" (Debuginfo.to_string i.dbg)

let rec all_instr ppf i =
  match i.desc with
  | Lend -> ()
  | _ -> fprintf ppf "%a@,%a" instr i all_instr i.next

let fundecl ppf f =
  let dbg =
    if Debuginfo.is_none f.fun_dbg then
      ""
    else
      " " ^ Debuginfo.to_string f.fun_dbg in
  let path =
    match f.fun_module_path with
    | None -> ""
    | Some path -> "(" ^ Path.name path ^ ")"
  in
  fprintf ppf "@[<v 2>%s%s:%s@,%a@]" path f.fun_name dbg all_instr f.fun_body

let fundecl_with_available_ranges ranges ppf f =
  available_ranges := Some ranges;
  fundecl ppf f;
  available_ranges := None
