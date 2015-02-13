    let rec parse_file_names () =
      Stream.peek_ubyte stream
      >>= function
      | 0 -> Ok []
      | _ ->
        parse_file_names ()
        >>| fun remainder ->
        file_name :: remainder
    in
    parse_file_names ()

module File_name : sig
  type t

  val parse : Stream.t -> t Or_error.t
end = struct
  (* DWARF-4 specification section 6.2.4 (item 12). *)

  type t = {
    full_or_relative_path_name : string;
    dir_index : int;
    mtime : int option;
    length : int option;
  }

  let create ~full_or_relative_path_name ~dir_index ~mtime ~length =
    let mtime = if mtime = 0 then None else Some mtime in
    let length = if length = 0 then None else Some length in
    { full_or_relative_path_name; dir_index; mtime; length; }

  let parse stream =
    Stream.parse_null_terminated_string stream
    >>= fun full_or_relative_path_name ->
    Stream.parse_uleb128 stream
    >>= fun dir_index ->
    Stream.parse_uleb128 stream
    >>= fun mtime ->
    Stream.parse_uleb128 stream
    >>| fun length ->
    create ~full_or_relative_path_name ~dir_index ~mtime ~length
end

module Line_number_program_header : sig
  type t

  val parse : Stream.t -> t Or_error.t

  val default_is_stmt : t -> bool
end = struct
  (* DWARF-4 specification section 6.2.4. *)

  type t = {
    unit_length : int;
    version : int;
    header_length : int;
    minimum_instruction_length : int;
    maximum_operations_per_instruction : int;
    default_is_stmt : bool;
    line_base : int;
    line_range : int;
    opcode_base : int;
    standard_opcode_length : int array;
    include_directories : string array;
    file_names : File_name.t array;
  }

  let parse stream ~dwarf_format =
    Stream.parse_initial_length stream
    >>= fun unit_length ->
    Stream.parse_uhalf stream
    >>= fun version ->
    begin match Dwarf_format.size dwarf_format with
    | `Thirty_two -> Stream.parse_four_byte_unsigned_length stream
    | `Sixty_four -> Stream.parse_eight_byte_unsigned_length stream
    end
    >>= fun header_length ->
    Stream.parse_ubyte stream
    >>= fun minimum_instruction_length ->
    Stream.parse_ubyte stream
    >>= fun maximum_operations_per_instruction ->
    Stream.parse_ubyte_as_bool stream
    >>= fun default_is_stmt ->
    Stream.parse_sbyte stream
    >>= fun line_base ->
    Stream.parse_ubyte stream
    >>= fun line_range ->
    Stream.parse_ubyte stream
    >>= fun opcode_base ->
    Stream.parse_ubyte_array stream
    >>= fun standard_opcode_lengths ->
    Stream.parse_null_byte_terminated_sequence stream
      Stream.parse_null_terminated_string
    >>= fun include_directories ->
    Stream.parse_null_byte_terminated_sequence stream File_name.parse
    >>| fun file_names ->
    { unit_length; version; header_length; minimum_instruction_length;
      maximum_operations_per_instruction; default_is_stmt; line_base;
      line_range; opcode_base; standard_opcode_length; include_directories;
      file_names;
    }
end

module Row : sig

end = struct
  (* DWARF-4 specification section 6.2.2 (described as part of the "state"). *)
  type t = {
    mutable file : File_name.t;
    mutable address : Address.t;
    mutable line : int;
    mutable column : int;
    mutable is_stmt : bool;
    mutable basic_block : bool;
    mutable end_sequence : bool;
    mutable prologue_end : bool;
    mutable epilogue_begin : bool;
    mutable isa : int;
    mutable discriminator : int;
  }
end

module Matrix : sig
  type t

  val append : t -> row:Row.t -> unit
end = struct
  type t = {
    mutable rows : Row.t list;
  }

  let append t ~row =
    t.rows <- row::t.rows


end

module State : sig
  type t

  val create : header:Line_number_program_header.t -> t
  val reset : t -> unit

  val append_row_using_current_values : t -> unit

  val delta_address : t -> operation_advance:int -> int
  val delta_op_index : t -> operation_advance:int -> int
  val delta_address' : t -> special_opcode:int -> int
  val delta_op_index' : t -> special_opcode:int -> int

  val add_to_address : t -> delta_address:int -> unit
  val add_to_op_index : t -> delta_op_index:int -> unit
  val add_to_line : t -> delta_line:int -> unit

  val set_address : t -> address:Address.t -> unit
  val set_file : t -> file:int -> unit
  val set_column : t -> column:int -> unit
  val set_is_stmt : t -> is_stmt:bool -> unit
  val set_basic_block : t -> basic_block:bool -> unit
  val set_op_index : t -> op_index:int -> unit
  val set_prologue_end : t -> prologue_end:bool -> unit
  val set_epilogue_begin : t -> epilogue_begin:bool -> unit
  val set_isa : t -> isa:int -> unit
  val set_end_sequence : t -> end_sequence:bool -> unit
  val set_discriminator : t -> discriminator:int -> unit

  val is_stmt : t -> bool
end = struct
  type t = {
    (* DWARF-4 specification section 6.2.2. *)
    row : Row.t;
    mutable op_index : int;
  }

  let create ~header =
    { address = 0;
      op_index = 0;
      file = 1;
      line = 1;
      column = 0;
      is_stmt = Line_number_program_header.default_is_stmt header;
      basic_block = false;
      end_sequence = false;
      prologue_end = false;
      epilogue_begin = false;
      isa = 0;
      discriminator = 0;
    }

  let reset t =
    t.address <- 0;
    t.op_index <- 0;
    t.file <- 1;
    t.line <- 1;
    t.column <- 0;
    t.is_stmt <- default_is_stmt;
    t.basic_block <- false;
    t.end_sequence <- false;
    t.prologue_end <- false;
    t.epilogue_begin <- false;
    t.isa <- 0;
    t.discriminator <- 0

  let apply_special_opcode t ~delta_line ~delta_address ~delta_op_index
        ~row =
    t.line <- t.line + delta_line;
    t.address <- t.address + delta_address;
    t.op_index <- t.op_index + delta_op_index;
    (* append row *)
    t.basic_block <- false;
    t.prologue_end <- false;
    t.epilogue_begin <- false;
    t.discriminator <- 0

  (* DWARF-4 specification section 6.2.5.1. *)

  let delta_address ~operation_advance =
    minimum_instruction_length
      * ((op_index + operation_advance)
          / maximum_operations_per_instruction)

  let delta_op_index ~operation_advance =
    (op_index + operation_advance)
      mod maximum_operations_per_instruction

  let adjusted_opcode ~special_opcode =
    special_opcode - opcode_base

  let operation_advance ~special_opcode =
    (adjusted_opcode_of_special_opcode ~special_opcode) / line_range

  let delta_address' ~special_opcode =
    minimum_instruction_length
      * ((op_index + (operation_advance ~special_opcode))
          / maximum_operations_per_instruction)

  let delta_op_index' ~special_opcode =
    (op_index + (operation_advance ~special_opcode))
      mod maximum_operations_per_instruction

  let delta_line ~special_opcode =
    line_base + ((adjusted_opcode ~special_opcode) mod line_range)
end

module Standard_opcode : sig
  type t

  val parse : Stream.t -> t Or_error.t
  val apply : t -> State.t -> unit
end = struct
  (* DWARF-4 specification section 6.2.5.2. *)

  type t =
    | DW_LNS_copy
    | DW_LNS_advance_pc of int
    | DW_LNS_advance_line of int
    | DW_LNS_set_file of int
    | DW_LNS_set_column of int
    | DW_LNS_negate_stmt
    | DW_LNS_set_basic_block
    | DW_LNS_const_add_pc
    | DW_LNS_fixed_advance_pc of int
    | DW_LNS_set_prologue_end
    | DW_LNS_set_epilogue_begin
    | DW_LNS_set_isa of int

  let parse stream =
    Stream.parse_??? stream
    >>= function
    | 0x01 -> Ok DW_LNS_copy
    | 0x02 ->
      Stream.parse_uleb128 stream
      >>= fun operation_advance ->
      Ok (DW_LNS_advance_pc operation_advance)
    | 0x03 ->
      Stream.parse_leb128 stream
      >>= fun delta_line ->
      Ok (DW_LNS_advance_line delta_line)
    | 0x04 ->
      Stream.parse_uleb128 stream
      >>= fun file ->
      Ok (DW_LNS_set_file file)
    | 0x05 ->
      Stream.parse_uleb128 stream
      >>= fun column ->
      Ok (DW_LNS_set_column column)
    | 0x06 -> Ok DW_LNS_negate_stmt
    | 0x07 -> Ok DW_LNS_set_basic_block
    | 0x08 -> Ok DW_LNS_const_add_pc
    | 0x09 ->
      Stream.parse_uhalf stream
      >>= fun delta_address ->
      Ok (DW_LNS_fixed_advance_pc delta_address)
    | 0x0a -> Ok DW_LNS_set_prologue_end
    | 0x0b -> Ok DW_LNS_set_epilogue_begin
    | 0x0c ->
      Stream.parse_uleb128 stream
      >>= fun isa ->
      Ok (DW_LNS_set_isa isa)
    | opcode -> Error (Printf.sprintf "unknown standard opcode %x" opcode)

  let apply t state =
    match t with
    | DW_LNS_copy ->
      State.append_row_using_current_values state;
      State.set_basic_block state ~basic_block:false;
      State.set_prologue_end state ~prologue_end:false;
      State.set_epilogue_begin state ~epilogue_begin:false;
      State.set_discriminator state ~discriminator:0
    | DW_LNS_advance_pc operation_advance ->
      let delta_address = State.delta_address state ~operation_advance in
      let delta_op_index = State.delta_op_index state ~operation_advance in
      State.add_to_address state ~delta_address;
      State.add_to_op_index state ~delta_op_index
    | DW_LNS_advance_line delta_line ->
      State.add_to_line state ~delta_line
    | DW_LNS_set_file file ->
      State.set_file state ~file
    | DW_LNS_set_column column ->
      State.set_column state ~column
    | DW_LNS_negate_stmt ->
      State.set_is_stmt state ~is_stmt:(lnot (State.is_stmt state))
    | DW_LNS_set_basic_block ->
      State.set_basic_block state ~basic_block:true
    | DW_LNS_const_add_pc ->
      let special_opcode = 255 in
      let delta_address = State.delta_address' state ~special_opcode in
      let delta_op_index = State.delta_op_index' state ~special_opcode in
      State.add_to_address state ~delta_address;
      State.add_to_op_index state ~delta_op_index
    | DW_LNS_fixed_advance_pc delta_address ->
      State.add_to_address state ~delta_address;
      State.set_op_index state ~op_index:0
    | DW_LNS_set_prologue_end ->
      State.set_prologue_end state ~prologue_end:true
    | DW_LNS_set_epilogue_begin ->
      State.set_epilogue_begin state ~epilogue_begin:true
    | DW_LNS_set_isa isa ->
      State.set_isa state ~isa
end

module Extended_opcode : sig
  type t

  val parse : Stream.t -> t Or_error.t
  val apply : t -> State.t -> unit
end = struct
  (* DWARF-4 specification section 6.2.5.3. *)

  type t =
    | DW_LNE_end_sequence
    | DW_LNE_set_address of Relocatable_address.t
    | DW_LNE_define_file of string * int * int * int
    | DW_LNE_set_discriminator * int

  let parse_exn stream
    Stream.parse_??? stream
    >>= function
    | 0x01 -> Ok DW_LNE_end_sequence
    | 0x02 ->
      Stream.parse_relocatable_address stream
      >>| fun addr ->
      DW_LNE_set_address addr
    | 0x03 ->
      Stream.parse_null_terminated_string stream
      >>= fun path ->
      Stream.parse_uleb128 stream
      >>= fun dir_index ->
      Stream.parse_uleb128 stream
      >>= fun mtime ->
      Stream.parse_leb128 stream
      >>| fun length ->
      DW_LNE_define_file (path, dir_index, mtime, length)
    | 0x04 ->
      Stream.parse_uleb128 stream
      >>| fun discriminator ->
      DW_LNE_set_discriminator discriminator
    | opcode -> Error (Printf.sprintf "unknown extended opcode %x" opcode)

  let apply t state =
    match t with
    | DW_LNE_end_sequence ->
      State.set_end_sequence state ~end_sequence:true;
      State.append_row_using_current_values state;
      State.reset state
    | DW_LNE_set_address address ->
      State.set_address state ~address;
      State.set_op_index state ~op_index:0
    | DW_LNE_define_file (file, dir_index, mtime, length) ->
      ...
    | DW_LNE_set_discriminator discriminator ->
      State.set_discriminator state ~discriminator
end

module Special_standard_or_extended_opcode : sig
  type t

  val apply : t -> State.t -> unit
end = struct
  (* DWARF-4 specification section 6.2.3. *)
end
