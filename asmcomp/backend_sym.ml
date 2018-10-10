(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Names of object file symbols. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = string
type backend_sym = t

let print ppf t = Format.pp_print_string ppf t

let to_string t = t

let of_symbol sym = Linkage_name.to_string (Symbol.label sym)

let of_external_name name = name

let create ~base_name =
  let unit_name =
    Linkage_name.to_string (Compilation_unit.get_linkage_name (
      Compilation_unit.get_current_exn ()))
  in
  unit_name ^ "__" ^ base_name

let add_suffix t new_suffix = t ^ new_suffix

let add_suffixes t new_suffixes =
  List.fold_left (fun t new_suffix -> add_suffix t new_suffix) t new_suffixes

let add_int_suffix t new_suffix =
  add_suffix t (string_of_int new_suffix)

let to_escaped_string ?suffix ~symbol_prefix ~escape t =
  let suffix =
    match suffix with
    | None -> ""
    | Some suffix -> suffix
  in
  symbol_prefix ^ (escape t) ^ suffix

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = String.compare (to_string t1) (to_string t2)

  let equal t1 t2 = (compare t1 t2 = 0)

  let hash t = Hashtbl.hash (to_string t)

  let print t = print t

  let output chn t = output_string chn (to_string t)
end)

module Names = struct
  let mcount = of_external_name "mcount"
  let _mcount = of_external_name "_mcount"
  let __gnu_mcount_nc = of_external_name "__gnu_mcount_nc"
  let sqrt = of_external_name "sqrt"

  let __dummy__ = of_external_name "__dummy__"

  let _GLOBAL_OFFSET_TABLE_ = of_external_name "_GLOBAL_OFFSET_TABLE_"

  let caml_young_ptr = of_external_name "caml_young_ptr"
  let caml_young_limit = of_external_name "caml_young_limit"
  let caml_exception_pointer = of_external_name "caml_exception_pointer"
  let caml_negf_mask = of_external_name "caml_negf_mask"
  let caml_absf_mask = of_external_name "caml_absf_mask"
  let caml_backtrace_pos = of_external_name "caml_backtrace_pos"
  let caml_exn_Division_by_zero = of_external_name "caml_exn_Division_by_zero"
  let caml_nativeint_ops = of_external_name "caml_nativeint_ops"
  let caml_int32_ops = of_external_name "caml_int32_ops"
  let caml_int64_ops = of_external_name "caml_int64_ops"

  let caml_send n = add_int_suffix (of_external_name "caml_send") n
  let caml_curry_n n =
    add_int_suffix (of_external_name "caml_curry") n
  let caml_curry_m_to_n m n =
    add_suffixes (of_external_name "caml_curry")
      [string_of_int m; "_"; string_of_int n]
  let caml_curry_m_to_n_app m n =
    add_suffixes (of_external_name "caml_curry")
      [string_of_int m; "_"; string_of_int n; "_app"]
  let caml_tuplify n = add_int_suffix (of_external_name "caml_tuplify") n
  let caml_apply n = add_int_suffix (of_external_name "caml_apply") n

  let caml_ba_get n = add_int_suffix (of_external_name "caml_ba_get_") n
  let caml_ba_set n = add_int_suffix (of_external_name "caml_ba_set_") n

  let caml_exn name = add_suffix (of_external_name "caml_exn_") name

  let caml_call_gc = of_external_name "caml_call_gc"
  let caml_modify = of_external_name "caml_modify"
  let caml_initialize = of_external_name "caml_initialize"
  let caml_get_public_method = of_external_name "caml_get_public_method"
  let caml_c_call = of_external_name "caml_c_call"
  let caml_alloc = of_external_name "caml_alloc"
  let caml_alloc1 = of_external_name "caml_alloc1"
  let caml_alloc2 = of_external_name "caml_alloc2"
  let caml_alloc3 = of_external_name "caml_alloc3"
  let caml_allocN = of_external_name "caml_allocN"
  let caml_ml_array_bound_error = of_external_name "caml_ml_array_bound_error"
  let caml_raise_exn = of_external_name "caml_raise_exn"
  let caml_make_array = of_external_name "caml_make_array"
  let caml_bswap16_direct = of_external_name "caml_bswap16_direct"
  let caml_direct_bswap ty =
    of_external_name (Printf.sprintf "caml_%s_direct_bswap" ty)
  let caml_alloc_dummy = of_external_name "caml_alloc_dummy"
  let caml_alloc_dummy_float = of_external_name "caml_alloc_dummy_float"
  let caml_update_dummy = of_external_name "caml_update_dummy"
  let caml_program = of_external_name "caml_program"
  let caml_startup = of_external_name "caml_startup"
  let caml_globals_inited = of_external_name "caml_globals_inited"
  let caml_globals = of_external_name "caml_globals"
  let caml_plugin_header = of_external_name "caml_plugin_header"
  let caml_globals_map = of_external_name "caml_globals_map"
  let caml_code_segments = of_external_name "caml_code_segments"
  let caml_data_segments = of_external_name "caml_data_segments"

  let caml_frametable = of_external_name "caml_frametable"
  let caml_spacetime_shapes = of_external_name "caml_spacetime_shapes"

  let caml_afl_area_ptr = of_external_name "caml_afl_area_ptr"
  let caml_afl_prev_loc = of_external_name "caml_afl_prev_loc"
  let caml_setup_afl = of_external_name "caml_setup_afl"

  let caml_spacetime_allocate_node =
    of_external_name "caml_spacetime_allocate_node"
  let caml_spacetime_indirect_node_hole_ptr =
    of_external_name "caml_spacetime_indirect_node_hole_ptr"
  let caml_spacetime_generate_profinfo =
    of_external_name "caml_spacetime_generate_profinfo"
end
