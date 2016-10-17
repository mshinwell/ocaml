(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2016 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16
module TS = Target_system

type constant =
  | Const of int64
  | This
  | Label of string
  | Numeric_label of Linearize.label
  | Add of constant * constant
  | Sub of constant * constant

type width =
  | Thirty_two
  | Sixty_four

type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_loc
  | Debug_str
  | Debug_line

type section =
  | Text
  | Data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Dwarf of dwarf_section

let text_label = Cmm.new_label ()
let data_label = Cmm.new_label ()
let eight_byte_literals_label = Cmm.new_label ()
let sixteen_byte_literals_label = Cmm.new_label ()
let debug_info_label = Cmm.new_label ()
let debug_abbrev_label = Cmm.new_label ()
let debug_aranges_label = Cmm.new_label ()
let debug_loc_label = Cmm.new_label ()
let debug_str_label = Cmm.new_label ()
let debug_line_label = Cmm.new_label ()

let label_for_section = function
  | Text -> text_label
  | Data -> data_label
  | Eight_byte_literals -> eight_byte_literals_label
  | Sixteen_byte_literals -> sixteen_byte_literals_label
  | Dwarf Debug_info -> debug_info_label
  | Dwarf Debug_abbrev -> debug_abbrev_label
  | Dwarf Debug_aranges -> debug_aranges_label
  | Dwarf Debug_loc -> debug_loc_label
  | Dwarf Debug_str -> debug_str_label
  | Dwarf Debug_line -> debug_line_label

module Directive = struct
  type directive =
    | Align of bool * int
    | Byte of constant
    | Bytes of string
    | Comment of string
    | Global of string
    | Long of constant
    | NewLabel of string
    | Quad of constant
    | Section of string list * string option * string list
    | Space of int
    | Word of constant
    | Cfi_adjust_cfa_offset of int
    | Cfi_endproc
    | Cfi_startproc
    | File of { file_num : int; filename : int; }
    | Indirect_symbol of string
    | Loc of { file_num : int; line : int; col : int; }
    | Private_extern of string
    | Set of string * constant
    | Size of string * constant
    | Sleb128 of constant
    | Type of string * string
    | Uleb128 of constant
    | Direct_assignment of string * constant

  let rec cst b = function
    | Label _ |  _ | This as c -> scst b c
    | Add (c1, c2) -> bprintf b "%a + %a" scst c1 scst c2
    | Sub (c1, c2) -> bprintf b "%a - %a" scst c1 scst c2

  and scst b = function
    | This -> Buffer.add_string b "."
    | Label l -> Buffer.add_string b l
    | n when n <= 0x7FFF_FFFFL && n >= -0x8000_0000L ->
      Buffer.add_string b (Int64.to_string n)
    | n -> bprintf b "0x%Lx" n
    | Add (c1, c2) -> bprintf b "(%a + %a)" scst c1 scst c2
    | Sub (c1, c2) -> bprintf b "(%a - %a)" scst c1 scst c2

  let print_gas b = function
    | Align { bytes = n; } ->
      (* Mac OS X's assembler interprets the integer n as a 2^n alignment *)
      let n = if TS.system = S_macosx then Misc.log2 n else n in
      bprintf b "\t.align\t%d" n
    | Byte n -> bprintf b "\t.byte\t%a" cst n
    | Bytes s ->
      if TS.system = S_solaris then buf_bytes_directive b ".byte" s
      else bprintf b "\t.ascii\t\"%s\"" (string_of_string_literal s)
    | Comment s -> bprintf b "\t\t\t\t/* %s */" s
    | Global s -> bprintf b "\t.globl\t%s" s;
    | Long n -> bprintf b "\t.long\t%a" cst n
    | NewLabel s -> bprintf b "%s:" s
    | Quad n -> bprintf b "\t.quad\t%a" cst n
    | Section ([".data" ], _, _) -> bprintf b "\t.data"
    | Section ([".text" ], _, _) -> bprintf b "\t.text"
    | Section (name, flags, args) ->
      bprintf b "\t.section %s" (String.concat "," name);
      begin match flags with
      | None -> ()
      | Some flags -> bprintf b ",%S" flags
      end;
      begin match args with
      | [] -> ()
      | _ -> bprintf b ",%s" (String.concat "," args)
      end
    | Space n ->
      if TS.system = S_solaris then bprintf b "\t.zero\t%d" n
      else bprintf b "\t.space\t%d" n
    | Word n ->
      (* Apple's documentation says that ".word" is i386-specific, so we use
          ".short" instead. *)
      if TS.system = S_solaris then bprintf b "\t.value\t%a" cst n
      else bprintf b "\t.short\t%a" cst n
    | Cfi_adjust_cfa_offset n -> bprintf b "\t.cfi_adjust_cfa_offset %d" n
    | Cfi_endproc -> bprintf b "\t.cfi_endproc"
    | Cfi_startproc -> bprintf b "\t.cfi_startproc"
    | File { file_num; filename; } ->
      bprintf b "\t.file\t%d\t\"%s\""
        file_num (X86_proc.string_of_string_literal filename)
    | Indirect_symbol s -> bprintf b "\t.indirect_symbol %s" s
    | Loc { file_num; line; col; } ->
      (* PR#7726: Location.none uses column -1, breaks LLVM assembler *)
      if col >= 0 then bprintf b "\t.loc\t%d\t%d\t%d" file_num line col
      else bprintf b "\t.loc\t%d\t%d" file_num line
    | Private_extern s -> bprintf b "\t.private_extern %s" s
    | Set (arg1, arg2) -> bprintf b "\t.set %s, %a" arg1 cst arg2
    | Size (s, c) -> bprintf b "\t.size %s,%a" s cst c
    | Sleb128 c -> bprintf b "\t.sleb128 %a" cst c
    | Type (s, typ) -> bprintf b "\t.type %s,%s" s typ
    | Uleb128 c -> bprintf b "\t.uleb128 %a" cst c
    | Direct_assignment (var, const) ->
      if TS.system <> S_macosx then failwith "Cannot emit Direct_assignment";
      bprintf b "%s = %a" var cst const

  let print_masm b = function
    | Align { bytes; } -> bprintf b "\tALIGN\t%d" bytes
    | Byte n -> bprintf b "\tBYTE\t%a" cst n
    | Bytes s -> buf_bytes_directive b "BYTE" s
    | Comment s -> bprintf b " ; %s " s
    | Global s -> bprintf b "\tPUBLIC\t%s" s
    | Long n -> bprintf b "\tDWORD\t%a" cst n
    | Quad n -> bprintf b "\tQUAD\t%a" cst n
    | Section ([".data"], None, []) -> bprintf b "\t.DATA"
    | Section ([".text"], None, []) -> bprintf b "\t.CODE"
    | Section _ -> assert false
    | Space n -> bprintf b "\tBYTE\t%d DUP (?)" n
    | Word n -> bprintf b "\tWORD\t%a" cst n
    | External (s, ptr) -> bprintf b "\tEXTRN\t%s: %s" s (string_of_datatype ptr)
    | Mode386 -> bprintf b "\t.386"
    | Model name -> bprintf b "\t.MODEL %s" name (* name = FLAT *)
    | Cfi_adjust_cfa_offset _
    | Cfi_endproc
    | Cfi_startproc
    | File _
    | Indirect_symbol _
    | Loc _
    | Private_extern _
    | Set _
    | Size _
    | Sleb128 _
    | Type _
    | Uleb128 _
    | Direct_assignment _ ->
      Misc.fatal_error "Unsupported asm directive for MASM"

  let print b t =
    if TS.masm then print_masm b t
    else print_gas b t
end

let emit_ref = ref None

let emit d =
  match !emit_ref with
  | Some emit -> emit d
  | None -> Misc.fatal_error "initialize not called"

let section segment flags args = emit (Section (segment, flags, args))
let align ~bytes = emit (Align (false, bytes))
let byte n = emit (Byte n)
let cfi_adjust_cfa_offset ~bytes = emit (Cfi_adjust_cfa_offset bytes)
let cfi_endproc () = emit Cfi_endproc
let cfi_startproc () = emit Cfi_startproc
let comment s = emit (Comment s)
let data () = section [ ".data" ] None []
let direct_assignment var const = emit (Direct_assignment (var, const))
let extrn s ptr = emit (External (s, ptr))
let file ~file_num ~file_name = emit (File (file_num, file_name))
let global s = emit (Global s)
let indirect_symbol s = emit (Indirect_symbol s)
let label s = emit (NewLabel s)
let loc ~file_num ~line ~col = emit (Loc (file_num, line, col))
let long cst = emit (Long cst)
let quad cst = emit (Quad cst)
let set x y = emit (Set (x, y))
let size name cst = emit (Size (name, cst))
let sleb128 cst = emit (Sleb128 (Const i))
let space ~bytes = emit (Space bytes)
let string s = emit (Bytes s)
let text () = section [ ".text" ] None []
let type_ name typ = emit (Type (name, typ))
let uleb128 cst = emit (Uleb128 (Const i))
let word cst = emit (Word cst)

let string_of_label label_name =
  match TS.system with
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
  quad (Label (string_of_label label_name))

let label_declaration ~label_name =
  label (string_of_label label_name)

let sections_seen = ref []

let switch_to_section (section : section) =
  let first_occurrence =
    if List.mem section !sections_seen then false
    else begin
      sections_seen := section::!sections_seen;
      true
    end
  in
  let section_name, middle_part, attrs =
    let text () = [".text"], None, [] in
    let data () = [".data"], None, [] in
    match section, TS.system with
    | Text -> text ()
    | Data -> data ()
    | Dwarf dwarf, S_macosx ->
      let name =
        match dwarf with
        | Debug_info -> "__debug_info"
        | Debug_abbrev -> "__debug_abbrev"
        | Debug_aranges -> "__debug_aranges"
        | Debug_loc -> "__debug_loc"
        | Debug_str -> "__debug_str"
        | Debug_line -> "__debug_line"
      in
      ["__DWARF"; name], None, ["regular"; "debug"]
    | Dwarf dwarf, _ ->
      let name =
        match dwarf with
        | Debug_info -> ".debug_info"
        | Debug_abbrev -> ".debug_abbrev"
        | Debug_aranges -> ".debug_aranges"
        | Debug_loc -> ".debug_loc"
        | Debug_str -> ".debug_str"
        | Debug_line -> ".debug_line"
      in
      let middle_part =
        if first_occurrence then
          Some ""
        else
          None
      in
      let attrs =
        if first_occurrence then
          ["%progbits"]
        else
          []
      in
      [name], middle_part, attrs
    | Sixteen_byte_literals, S_macosx ->
      ["__TEXT";"__literal16"], None, ["16byte_literals"]
    | Sixteen_byte_literals, (S_mingw64 | S_cygwin) ->
      [".rdata"], Some "dr", []
    | Sixteen_byte_literals, S_win64 ->
      data ()
    | Sixteen_byte_literals, _ ->
      [".rodata.cst8"], Some "a", ["@progbits"]
    | Eight_byte_literals, S_macosx ->
      ["__TEXT";"__literal8"], None, ["8byte_literals"]
    | Eight_byte_literals, (S_mingw64 | S_cygwin) ->
      [".rdata"], Some "dr", []
    | Eight_byte_literals, S_win64 ->
      data ()
    | Eight_byte_literals, _ ->
      [".rodata.cst8"], Some "a", ["@progbits"]
    | Jump_tables, (S_mingw64 | S_cygwin) ->
      [".rdata"], Some "dr", []
    | Jump_tables, (S_macosx | S_win64) ->
      text () (* with LLVM/OS X and MASM, use the text segment *)
    | Jump_tables, _ ->
      [".rodata"], None, []
  in
  section section_name middle_part attrs;
  if first_occurrence then begin
    label_declaration ~label_name:(label_for_section section)
  end

let cached_strings = ref ([] : (string * Linearize.label) list)

let reset () =
  cached_strings := [];
  sections_seen := []

let initialize ~emit =
  emit_ref := emit;
  reset ();
  match TS.system with
  | S_macosx -> ()
  | _ ->
    if !Clflags.debug then begin
      (* Forward label references are illegal in gas. *)
      switch_to_section Text;
      switch_to_section Data;
      switch_to_section Eight_byte_literals;
      switch_to_section Sixteen_byte_literals;
      switch_to_section (Dwarf Debug_info);
      switch_to_section (Dwarf Debug_abbrev);
      switch_to_section (Dwarf Debug_aranges);
      switch_to_section (Dwarf Debug_loc);
      switch_to_section (Dwarf Debug_str);
      switch_to_section (Dwarf Debug_line)
    end

let symbol_prefix = if TS.system = X86_proc.S_macosx then "_" else ""

let escape_symbol s = X86_proc.string_of_symbol symbol_prefix s

let symbol sym =
  let sym = Linkage_name.to_string (Symbol.label sym) in
  quad (Label (escape_symbol sym))

let symbol_plus_offset sym ~offset_in_bytes =
  let sym = Linkage_name.to_string (Symbol.label sym) in
  let offset_in_bytes = TS.Address.to_int64 offset_in_bytes in
  quad (Add (Label (escape_symbol sym), Const offset_in_bytes))

(* To avoid callers of this module having to worry about whether operands
   involved in displacement calculations are or are not relocatable, and to
   guard against clever linkers doing e.g. branch relaxation at link time, we
   always force such calculations to be done in a relocatable manner at
   link time.  On Mac OS X this requires use of the "direct assignment"
   syntax rather than ".set": the latter forces expressions to be evaluated
   as absolute assembly-time constants. *)

let force_relocatable expr =
  match TS.system with
  | S_macosx ->
    let temp = new_temp_var () in
    direct_assignment temp expr;
    Label temp
  | _ ->
    expr

let between_symbols ~upper ~lower =
  let upper = Linkage_name.to_string (Symbol.label upper) in
  let lower = Linkage_name.to_string (Symbol.label lower) in
  let expr = Sub (Label (escape_symbol upper), Label (escape_symbol lower)) in
  const64 (force_relocatable expr)

let between_labels_32bit ~upper ~lower =
  let expr = Sub (Numeric_label upper, Numeric_label lower) in
  const32 (force_relocatable expr)

let define_symbol sym =
  let name = Linkage_name.to_string (Symbol.label sym) in
  quad (Label (escape_symbol name));
  global name

let between_symbol_and_label_offset ~upper ~lower ~offset_upper =
  let lower = Linkage_name.to_string (Symbol.label lower) in
  let offset_upper = TS.Address.to_int64 offset_upper in
  let expr =
    Sub (
      Add (Numeric_label upper, Const offset_upper),
      Label (escape_symbol lower))
  in
  const64 (force_relocatable expr)

let between_this_and_label_offset_32bit ~upper ~offset_upper =
  let offset_upper = TS.Address.to_int64 offset_upper in
  let expr =
    Sub (Add (Numeric_label upper, Const offset_upper), This)
  in
  const32 (force_relocatable expr)

let temp_var_counter = ref 0
let new_temp_var () =
  let id = !temp_var_counter in
  incr temp_var_counter;
  Printf.sprintf "Ltemp%d" id

let constant_with_width expr ~(width : width) =
  match width with
  (* CR mshinwell: make sure this behaves properly on 32-bit platforms.
     This width is independent of the natural machine width. *)
  | Thirty_two -> long expr
  | Sixty_four -> quad expr

let offset_into_section_label ~section ~label:upper ~width =
  let lower = string_of_label (label_for_section section) in
  let expr : constant =
    (* The meaning of a label reference depends on the assembler:
       - On Mac OS X, it appears to be the distance from the label back to
         the start of the assembly file.
       - On gas, it is the distance from the label back to the start of the
         current section. *)
    match TS.system with
    | S_macosx ->
      let temp = new_temp_var () in
      direct_assignment temp (Sub (Numeric_label upper, Label lower));
      Label temp
    | _ ->
      Numeric_label upper
  in
  constant_with_width expr width

let offset_into_section_symbol ~section ~symbol ~width =
  let lower = string_of_label (label_for_section section) in
  let upper = escape_symbol (Linkage_name.to_string (Symbol.label symbol)) in
  let expr : constant =
    (* The same thing as for [offset_into_section_label] applies here. *)
    match TS.system with
    | S_macosx ->
      let temp = new_temp_var () in
      direct_assignment temp (Sub (Label upper, Label lower));
      Label temp
    | _ -> Label upper
  in
  constant_with_width expr width

let int8 i =
  byte (Const (Int64.of_int (Int8.to_int i)))

let int16 i =
  word (Const (Int64.of_int (Int16.to_int i)))

let int32 i =
  long (Const (Int64.of_int i))

let int64 i =
  quad (Const i)

let target_address addr =
  match Targetint.repr addr with
  | Int32 i -> int32 i
  | Int64 i -> int64 i

let cache_string str =
  match List.assoc str !cached_strings with
  | label -> label
  | exception Not_found ->
    let label = Cmm.new_label () in
    cached_strings := (str, label) :: !cached_strings;
    label

let emit_cached_strings () =
  List.iter (fun (str, label_name) ->
      label_declaration ~label_name;
      string str;
      int8 Int8.zero)
    !cached_strings;
  cached_strings := []

let mark_stack_non_executable () =
  match TS.system with
  | Linux -> section [".note.GNU-stack"] (Some "") [ "%progbits" ]
  | _ -> ()
