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

(* Pretty-printing of C-- code *)

open Format
open Cmm

module V = Backend_var
module VP = Backend_var.With_provenance

type Format.stag += Start_pos_tag of { placeholder_line : int; }
type Format.stag += End_pos_tag of { placeholder_line : int; }

module Make_mark_functions (P : sig
  type t

  val position : t -> Debuginfo.Code_range.t option
end) = struct
  let mark_start_location ppf dbg =
    match P.position dbg with
    | None -> ()
    | Some pos ->
      let placeholder_line = Debuginfo.Code_range.line pos in
      Format.pp_open_stag ppf (Start_pos_tag { placeholder_line; })

  let mark_end_location ppf dbg =
    match P.position dbg with
    | None -> ()
    | Some pos ->
      let placeholder_line = Debuginfo.Code_range.line pos in
      Format.pp_open_stag ppf (End_pos_tag { placeholder_line; })
end

include Make_mark_functions (Debuginfo)

module Mark_fun_dbg = Make_mark_functions (struct
  include Debuginfo.Function
  let position t = Some (position t)
end)

let parse_placeholder_line_start_tag (stag : Format.stag) =
  match stag with
  | Start_pos_tag { placeholder_line; } -> Some placeholder_line
  | _ -> None

let parse_placeholder_line_end_tag (stag : Format.stag) =
  match stag with
  | End_pos_tag { placeholder_line; } -> Some placeholder_line
  | _ -> None

let rec_flag ppf = function
  | Nonrecursive -> ()
  | Recursive -> fprintf ppf " rec"

let machtype_component ppf = function
  | Val -> fprintf ppf "val"
  | Addr -> fprintf ppf "addr"
  | Int -> fprintf ppf "int"
  | Float -> fprintf ppf "float"

let machtype ppf mty =
  match Array.length mty with
  | 0 -> fprintf ppf "unit"
  | n -> machtype_component ppf mty.(0);
         for i = 1 to n-1 do
           fprintf ppf "*%a" machtype_component mty.(i)
         done

let integer_comparison = function
  | Ceq -> "=="
  | Cne -> "!="
  | Clt -> "<"
  | Cle -> "<="
  | Cgt -> ">"
  | Cge -> ">="

let float_comparison = function
  | CFeq -> "=="
  | CFneq -> "!="
  | CFlt -> "<"
  | CFnlt -> "!<"
  | CFle -> "<="
  | CFnle -> "!<="
  | CFgt -> ">"
  | CFngt -> "!>"
  | CFge -> ">="
  | CFnge -> "!>="

let chunk = function
  | Byte_unsigned -> "unsigned int8"
  | Byte_signed -> "signed int8"
  | Sixteen_unsigned -> "unsigned int16"
  | Sixteen_signed -> "signed int16"
  | Thirtytwo_unsigned -> "unsigned int32"
  | Thirtytwo_signed -> "signed int32"
  | Word_int -> "int"
  | Word_val -> "val"
  | Single -> "float32"
  | Double -> "float64"
  | Double_u -> "float64u"

let raise_kind fmt = function
  | Raise_withtrace -> Format.fprintf fmt "raise_withtrace"
  | Raise_notrace -> Format.fprintf fmt "raise_notrace"

let phantom_defining_expr ppf defining_expr =
  match defining_expr with
  | Cphantom_const_int i -> Targetint.print ppf i
  | Cphantom_const_symbol sym -> Backend_sym.print ppf sym
  | Cphantom_var var -> V.print ppf var
  | Cphantom_offset_var { var; offset_in_words; } ->
    Format.fprintf ppf "%a+(%d)" V.print var offset_in_words
  | Cphantom_read_field { var; field; } ->
    Format.fprintf ppf "%a.(%d)" V.print var field
  | Cphantom_read_symbol_field { sym; field; } ->
    Format.fprintf ppf "%a.(%d)" Backend_sym.print sym field
  | Cphantom_block { tag; fields; } ->
    Format.fprintf ppf "[%d: " tag;
    List.iter (fun field ->
        Format.fprintf ppf "%a; " V.print field)
      fields;
    Format.fprintf ppf "]"

let phantom_defining_expr_opt ppf defining_expr =
  match defining_expr with
  | None -> Format.pp_print_string ppf "()"
  | Some defining_expr -> phantom_defining_expr ppf defining_expr

let operation ~print_dbg d = function
  | Capply (_ty, _callee_dbg) ->
      if print_dbg then Format.asprintf "app %a" Debuginfo.print d
      else "app"
  | Cextcall(lbl, _ty, _alloc, _) ->
      if print_dbg then
        Format.asprintf "extcall \"%a\" %a"
          Backend_sym.print lbl
          Debuginfo.print d
      else
        Format.asprintf "extcall \"%a\""
          Backend_sym.print lbl
  | Cload (c, Asttypes.Immutable) -> Printf.sprintf "load %s" (chunk c)
  | Cload (c, Asttypes.Mutable) -> Printf.sprintf "load_mut %s" (chunk c)
  | Calloc ->
      if print_dbg then Format.asprintf "alloc %a" Debuginfo.print d
      else "alloc"
  | Cstore (c, init) ->
    let init =
      match init with
      | Lambda.Heap_initialization -> "(heap-init)"
      | Lambda.Root_initialization -> "(root-init)"
      | Lambda.Assignment -> ""
    in
    Printf.sprintf "store %s%s" (chunk c) init
  | Caddi -> "+"
  | Csubi -> "-"
  | Cmuli -> "*"
  | Cmulhi -> "*h"
  | Cdivi -> "/"
  | Cmodi -> "mod"
  | Cand -> "and"
  | Cor -> "or"
  | Cxor -> "xor"
  | Clsl -> "<<"
  | Clsr -> ">>u"
  | Casr -> ">>s"
  | Ccmpi c -> integer_comparison c
  | Caddv -> "+v"
  | Cadda -> "+a"
  | Ccmpa c -> Printf.sprintf "%sa" (integer_comparison c)
  | Cnegf -> "~f"
  | Cabsf -> "absf"
  | Caddf -> "+f"
  | Csubf -> "-f"
  | Cmulf -> "*f"
  | Cdivf -> "/f"
  | Cfloatofint -> "floatofint"
  | Cintoffloat -> "intoffloat"
  | Ccmpf c -> Printf.sprintf "%sf" (float_comparison c)
  | Craise k ->
      if print_dbg then Format.asprintf "%a %a" raise_kind k Debuginfo.print d
      else Format.asprintf "%a" raise_kind k
  | Ccheckbound ->
      if print_dbg then Format.asprintf "checkbound %a" Debuginfo.print d
      else "checkbound"

let rec expr ~print_dbg ppf = function
  | Cconst_int n -> fprintf ppf "%i" n
  | Cconst_natint n ->
    fprintf ppf "%s" (Nativeint.to_string n)
  | Cblockheader(n, d) ->
      mark_start_location ppf d;
      if print_dbg then begin
        fprintf ppf "block-hdr(%s) %a"
          (Nativeint.to_string n) Debuginfo.print d
      end else begin
        fprintf ppf "block-hdr(%s)"
          (Nativeint.to_string n)
      end;
      mark_end_location ppf d
  | Cconst_float n -> fprintf ppf "%F" n
  | Cconst_symbol s -> fprintf ppf "\"%a\"" Backend_sym.print s
  | Cconst_pointer n -> fprintf ppf "%ia" n
  | Cconst_natpointer n -> fprintf ppf "%sa" (Nativeint.to_string n)
  | Cvar id -> V.print ppf id
  | Clet(id, def, (Clet(_, _, _) as body)) ->
      let print_binding id ppf def =
        fprintf ppf "@[<2>%a@ %a@]"
          VP.print id (expr ~print_dbg) def in
      let rec in_part ppf = function
        | Clet(id, def, body) ->
            fprintf ppf "@ %a" (print_binding id) def;
            in_part ppf body
        | exp -> exp in
      fprintf ppf "@[<2>(let@ @[<1>(%a" (print_binding id) def;
      let exp = in_part ppf body in
      fprintf ppf ")@]@ %a)@]" (sequence ~print_dbg) exp
  | Clet(id, def, body) ->
     fprintf ppf
      "@[<2>(let@ @[<2>%a@ %a@]@ %a)@]"
      VP.print id (expr ~print_dbg) def (sequence ~print_dbg) body
  | Cphantom_let(var, def, (Cphantom_let(_, _, _) as body)) ->
      let print_binding var ppf def =
        fprintf ppf "@[<2>%a@ %a@]" VP.print var
          phantom_defining_expr_opt def
      in
      let rec in_part ppf = function
        | Cphantom_let(var, def, body) ->
            fprintf ppf "@ %a" (print_binding var) def;
            in_part ppf body
        | exp -> exp in
      fprintf ppf "@[<2>(phantom_let@ @[<1>(%a" (print_binding var) def;
      let exp = in_part ppf body in
      fprintf ppf ")@]@ %a)@]" (sequence ~print_dbg) exp
  | Cphantom_let(var, def, body) ->
    fprintf ppf
      "@[<2>(phantom_let@ @[<2>%a@ %a@]@ %a)@]"
      VP.print var
      phantom_defining_expr_opt def
      (sequence ~print_dbg) body
  | Cassign(id, exp) ->
      fprintf ppf "@[<2>(assign @[<2>%a@ %a@])@]"
        V.print id (expr ~print_dbg) exp
  | Ctuple el ->
      let tuple ppf el =
       let first = ref true in
       List.iter
        (fun e ->
          if !first then first := false else fprintf ppf "@ ";
          (expr ~print_dbg) ppf e)
        el in
      fprintf ppf "@[<1>[%a]@]" tuple el
  | Cop(op, el, dbg) ->
      mark_start_location ppf dbg;
      fprintf ppf "@[<2>(%s" (operation ~print_dbg dbg op);
      List.iter (fun e -> fprintf ppf "@ %a" (expr ~print_dbg) e) el;
      begin match op with
      | Capply (mty, _callee_dbg) -> fprintf ppf "@ %a" machtype mty
      | Cextcall(_, mty, _, _) -> fprintf ppf "@ %a" machtype mty
      | _ -> ()
      end;
      fprintf ppf ")@]";
      mark_end_location ppf dbg;
  | Csequence(e1, e2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]"
        (sequence ~print_dbg) e1 (sequence ~print_dbg) e2
  | Cifthenelse(e1, ifso_dbg, e2, ifnot_dbg, e3, dbg) ->
      mark_start_location ppf dbg;
      fprintf ppf "@[<2>(if@ %a@ " (expr ~print_dbg) e1;
      mark_end_location ppf dbg;
      mark_start_location ppf ifso_dbg;
      fprintf ppf "%a@ " (expr ~print_dbg) e2;
      mark_end_location ppf ifso_dbg;
      mark_start_location ppf ifnot_dbg;
      fprintf ppf "%a)@]" (expr ~print_dbg) e3;
      mark_end_location ppf ifnot_dbg
  | Cswitch(e1, index, cases, dbg) ->
      mark_start_location ppf dbg;
      let print_case i ppf =
        for j = 0 to Array.length index - 1 do
          if index.(j) = i then fprintf ppf "case %i:" j
        done in
      let print_cases ppf =
       for i = 0 to Array.length cases - 1 do
        fprintf ppf "@ @[<2>%t@ %a@]"
          (print_case i) (sequence ~print_dbg) (fst cases.(i))
       done in
      fprintf ppf "@[<v 0>@[<2>(switch@ %a@ @]%t)@]"
        (expr ~print_dbg) e1 print_cases;
      mark_end_location ppf dbg
  | Cloop (e, _) ->
      fprintf ppf "@[<2>(loop@ %a)@]" (sequence ~print_dbg) e
  | Ccatch(flag, handlers, e1) ->
      let print_handler ppf (i, ids, e2, handler_dbg) =
        mark_start_location ppf handler_dbg;
        fprintf ppf "(%d%a)@ %a"
          i
          (fun ppf ids ->
             List.iter
               (fun (id, ty) ->
                 fprintf ppf "@ %a: %a"
                   VP.print id machtype ty)
               ids) ids
          (sequence ~print_dbg) e2;
        mark_end_location ppf handler_dbg
      in
      let print_handlers ppf l =
        List.iter (print_handler ppf) l
      in
      fprintf ppf
        "@[<2>(catch%a@ %a@;<1 -2>with%a)@]"
        rec_flag flag
        (sequence ~print_dbg) e1
        print_handlers handlers
  | Cexit (i, el) ->
      fprintf ppf "@[<2>(exit %d" i;
      List.iter (fun e -> fprintf ppf "@ %a" (expr ~print_dbg) e) el;
      fprintf ppf ")@]"
  | Ctrywith(e1, id, e2, handler_dbg) ->
      fprintf ppf "@[<2>(try@ %a@;<1 -2>with@ %a@ "
        (sequence ~print_dbg) e1 VP.print id;
      mark_start_location ppf handler_dbg;
      fprintf ppf "%a)@]"
        (sequence ~print_dbg) e2;
      mark_end_location ppf handler_dbg

and sequence ~print_dbg ppf = function
  | Csequence(e1, e2) ->
      fprintf ppf "%a@ %a" (sequence ~print_dbg) e1 (sequence ~print_dbg) e2
  | e -> expression ~print_dbg ppf e

and expression ~print_dbg ppf e = fprintf ppf "%a" (expr ~print_dbg) e

let fundecl ~print_dbg ppf f =
  let print_cases ppf cases =
    let first = ref true in
    List.iter
     (fun (id, ty) ->
       if !first then first := false else fprintf ppf "@ ";
       fprintf ppf "%a: %a" VP.print id machtype ty)
     cases in
  Mark_fun_dbg.mark_start_location ppf f.fun_dbg;
  fprintf ppf "@[<1>(function ";
  if print_dbg then begin
    fprintf ppf "%a " Debuginfo.Function.print f.fun_dbg
  end;
  fprintf ppf "%a@;<1 4>@[<1>(%a)@]@ @[%a@])@]@."
    Backend_sym.print f.fun_name
    print_cases f.fun_args (sequence ~print_dbg) f.fun_body;
  Mark_fun_dbg.mark_end_location ppf f.fun_dbg

let data_item ppf = function
  | Cdefine_symbol s -> fprintf ppf "\"%a\":" Backend_sym.print s
  | Cglobal_symbol s -> fprintf ppf "global \"%a\"" Backend_sym.print s
  | Cint8 n -> fprintf ppf "byte %i" n
  | Cint16 n -> fprintf ppf "int16 %i" n
  | Cint32 n -> fprintf ppf "int32 %s" (Nativeint.to_string n)
  | Cint n -> fprintf ppf "int %s" (Nativeint.to_string n)
  | Csingle f -> fprintf ppf "single %F" f
  | Cdouble f -> fprintf ppf "double %F" f
  | Csymbol_address s -> fprintf ppf "addr \"%a\"" Backend_sym.print s
  | Cstring s -> fprintf ppf "string \"%s\"" s
  | Cskip n -> fprintf ppf "skip %i" n
  | Calign n -> fprintf ppf "align %i" n

let data ppf dl =
  let items ppf = List.iter (fun d -> fprintf ppf "@ %a" data_item d) dl in
  fprintf ppf "@[<hv 1>(data%t)@]" items

let phrase' ?no_debuginfo ppf = function
  | Cfunction f ->
      begin match no_debuginfo with
      | None -> fundecl ~print_dbg:true ppf f
      | Some () -> fundecl ~print_dbg:false ppf f
      end
  | Cdata dl -> data ppf dl

let operation ppf op = operation ~print_dbg:true ppf op

let expression ppf expr = expression ~print_dbg:true ppf expr

let fundecl ppf decl = fundecl ~print_dbg:true ppf decl

let phrase ppf cmm_phrase = phrase' ?no_debuginfo:None ppf cmm_phrase
