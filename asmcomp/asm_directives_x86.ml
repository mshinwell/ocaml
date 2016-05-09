(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                      Elias Boutaleb, OCamlPro                          *)
(*                                                                        *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*   Copyright 2016 OCamlPro                                              *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module D = X86_dsl.D
module X = X86_dsl

let section_declaration ~section_name ~is_dwarf =
  match X86_proc.system with
  | S_macosx ->
    if is_dwarf then
      let section_name = "__" ^ section_name in
      D.section ["__DWARF"; section_name] None ["regular"; "debug"]
    else
      D.section [section_name] (Some "") []
  | S_gnu
  | S_cygwin
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw
  | S_win64
  | S_linux
  | S_mingw64
  | S_unknown -> D.section ["." ^ section_name] (Some "") ["%progbits"]

let switch_to_section ~section_name ~is_dwarf =
  match X86_proc.system with
  | S_macosx -> section_declaration ~section_name ~is_dwarf
  | S_gnu
  | S_cygwin
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw
  | S_win64
  | S_linux
  | S_mingw64
  | S_unknown -> D.section ["." ^ section_name] None []

let symbol_prefix = if X86_proc.system = X86_proc.S_macosx then "_" else ""

let escape_symbol s = X86_proc.string_of_symbol symbol_prefix s

let symbol sym =
  let sym = Linkage_name.to_string (Symbol.label sym) in
  D.qword (ConstLabel (escape_symbol sym))

let symbol_plus_offset sym ~offset_in_bytes =
  let sym = Linkage_name.to_string (Symbol.label sym) in
  D.qword (ConstAdd (ConstLabel (escape_symbol sym), Const offset_in_bytes))

let between_symbols ~upper ~lower =
  let upper = Linkage_name.to_string (Symbol.label upper) in
  let lower = Linkage_name.to_string (Symbol.label lower) in
  D.qword (ConstSub (
    ConstLabel (escape_symbol upper),
    ConstLabel (escape_symbol lower)))

let define_symbol sym =
  let name = Linkage_name.to_string (Symbol.label sym) in
  D.qword (ConstLabel (escape_symbol name));
  D.global name

let string_of_label label_name =
  match X86_proc.system with
  | S_macosx | S_win64 -> "L" ^ string_of_int label_name
  | S_gnu
  | S_cygwin
  | S_solaris
  | S_win32
  | S_linux_elf
  | S_bsd_elf
  | S_beos
  | S_mingw
  | S_linux
  | S_mingw64
  | S_unknown -> ".L" ^ string_of_int label_name

let label label_name =
  D.qword (ConstLabel (string_of_label label_name))

let label_declaration ~label_name =
  (* CR mshinwell: should this always be QWORD?  (taken from emit.mlp) *)
  D.label (string_of_label label_name) ~typ:QWORD

let between_symbol_and_label_offset ~upper ~lower ~offset_upper =
  let upper = string_of_label upper in
  let lower = Linkage_name.to_string (Symbol.label lower) in
  D.qword (ConstSub (
    ConstAdd (ConstLabel upper, Const (Int64.of_int offset_upper)),
    ConstLabel (escape_symbol lower)))

let temp_var_counter = ref 0
let new_temp_var () =
  let id = !temp_var_counter in
  incr temp_var_counter;
  Printf.sprintf "L$set$%d" id

(* XXX the base cannot be ".debug_foo" on the mac *)

let offset_into_section_label ~base ~label:upper
      ~(width : Asm_directives.width) =
  let upper = string_of_label upper in
  let expr : X86_ast.constant =
    match X86_proc.system with
    | S_macosx ->
      let temp = new_temp_var () in
      D.setvar (temp,
        ConstSub (ConstLabel upper, ConstLabel base));
      ConstLabel temp
    | _ ->
      ConstLabel upper
  in
  match width with
  (* CR mshinwell: make sure this behaves properly on 32-bit platforms.
     This width is independent of the natural machine width.
     Same below. *)
  | Thirty_two -> D.long expr
  | Sixty_four -> D.qword expr

let offset_into_section_symbol ~base ~symbol
      ~(width : Asm_directives.width) =
  let upper = Linkage_name.to_string (Symbol.label symbol) in
  let expr : X86_ast.constant =
    match X86_proc.system with
    | S_macosx ->
      let temp = new_temp_var () in
      D.setvar (temp,
        ConstSub (ConstLabel (escape_symbol upper), ConstLabel base));
      ConstLabel temp
    | _ ->
      ConstLabel (escape_symbol upper)
  in
  match width with
  (* CR mshinwell: make sure this behaves properly on 32-bit platforms.
     This width is independent of the natural machine width. *)
  | Thirty_two -> D.long expr
  | Sixty_four -> D.qword expr

let int8 i =
  D.byte (X.const (Int8.to_int i))

(* CR mshinwell: check these are the correct directives for both 64 and 32
   (same for the symbol case above). *)

(* CR mshinwell: Apple's doc says that ".word" is i386-specific.  Should
   maybe use ".short" instead everywhere.  Needs X86_dsl fixing *)
let int16 i =
  D.word (X.const (Int16.to_int i))

let int32 i =
  D.long (X.const_32 i)

let int64 i =
  D.qword (Const i)

let nativeint n =
  D.qword (X.const_nat n)

let uleb128 i =
  D.uleb128 (Const i)

let sleb128 i =
  D.sleb128 (Const i)

let string str =
  D.bytes str

let cached_strings = ref ([] : (string * Linearize.label) list)

let cache_string str =
  match List.assoc str !cached_strings with
  | label -> label
  | exception Not_found ->
    let label = Linearize.new_label () in
    cached_strings := (str, label) :: !cached_strings;
    label

let emit_cached_strings () =
  List.iter (fun (str, label_name) ->
      label_declaration ~label_name;
      string str;
      int8 Int8.zero)
    !cached_strings;
  cached_strings := []

let reset () =
  cached_strings := []

(*
    begin match Emitter.target emitter with
    | `Other ->
      Emitter.emit_offset_into_section emitter
        (`Label label) ".debug_str"
    | `MacOS_X -> assert false
    (*
      let count = !set_counter in
      let name = Printf.sprintf "Ldwarf_value%d" count in
      incr set_counter;
      Emitter.emit_string emitter name;
      Emitter.emit_string emitter " = ";
      Emitter.emit_label emitter label;
      Emitter.emit_string emitter "-";
      Emitter.emit_label emitter
        (Section_names.starting_label Section_names.debug_str);
      Emitter.emit_string emitter "\n";
      emit (Offset_from_var name) asm
    *)
    end
*)

(*
  | Absolute_offset o ->
    (* CR mshinwell: share with initial_length.ml *)
    if Int64.compare o 0xfffffff0L >= 0 then begin
      failwith "Absolute offset is too large for 32-bit DWARF"
    end;
    emit_directive_for_offset asm;
    Emitter.emit_string emitter (sprintf "0x%Lx\n" o);
  | Offset_from_label (label, section) ->
  | Reference_from_label label ->
    emit_directive_for_offset asm;
    Emitter.emit_label emitter label;
    Emitter.emit_string emitter "\n"
  | Code_address_from_label_diff (s2, s1) ->
    let count = !set_counter in
    let name = Printf.sprintf "Ldwarf_value%d" count in
    incr set_counter;
    begin match Emitter.target emitter with
    | `Other -> emit_directive_for_nativeint asm
    | `MacOS_X ->
      Emitter.emit_string emitter name;
      Emitter.emit_string emitter " = "
    end;
    begin match s2 with
    | `Symbol s2 -> Emitter.emit_symbol emitter s2
    | `Symbol_plus_offset_in_bytes (s2, offset) ->
      Emitter.emit_symbol emitter s2;
      Emitter.emit_string emitter " + ";
      Emitter.emit_string emitter (Printf.sprintf "%d" offset)
    | `Label s2 -> Emitter.emit_label emitter s2
    end;
    begin match Emitter.target emitter with
    | `Other -> Emitter.emit_string emitter " - "
    | `MacOS_X -> Emitter.emit_string emitter "-"
    end;
    begin match s1 with
    | `Symbol s1 -> Emitter.emit_symbol emitter s1
    | `Label s1 -> Emitter.emit_label emitter s1
    end;
    Emitter.emit_string emitter "\n";
    begin match Emitter.target emitter with
    | `Other -> ()
    | `MacOS_X ->
      emit_as_native_int (`String name) asm
    end
  | Code_address_from_label_diff_minus_8 (s2, s1) ->
    (* XXX fix this nonsense *)
    let count = !set_counter in
    let name = Printf.sprintf "Ldwarf_value%d" count in
    incr set_counter;
    begin match Emitter.target emitter with
    | `Other -> emit_directive_for_nativeint asm
    | `MacOS_X ->
      Emitter.emit_string emitter name;
      Emitter.emit_string emitter " = "
    end;
    begin match s2 with
    | `Symbol s2 -> Emitter.emit_symbol emitter s2
    | `Label s2 -> Emitter.emit_label emitter s2
    end;
    begin match Emitter.target emitter with
    | `Other -> Emitter.emit_string emitter " - 1 - "
    | `MacOS_X -> Emitter.emit_string emitter "-1-"
    end;
    Emitter.emit_symbol emitter s1;
    Emitter.emit_string emitter "\n";
    begin match Emitter.target emitter with
    | `Other -> ()
    | `MacOS_X ->
      emit_as_native_int (`String name) asm
    end
*)
