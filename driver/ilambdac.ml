module Flambda2_backend = struct
  let symbol_for_global' id =
    Flambda2.Symbol.unsafe_create
      (Flambda2.Compilation_unit.get_current_exn ())
      (Flambda2.Linkage_name.create (Compilenv.symbol_for_global id))

  let division_by_zero =
    symbol_for_global' Predef.ident_division_by_zero

  let invalid_argument =
    symbol_for_global' Predef.ident_invalid_argument

  let closure_symbol _ = failwith "Not yet implemented"

  let really_import_approx _ = failwith "Not yet implemented"
  let import_symbol _ = failwith "Not yet implemented"

  let all_predefined_exception_symbols = Flambda2.Symbol.Set.empty (* XXX *)

  let size_int = Arch.size_int
  let big_endian = Arch.big_endian

  let max_sensible_number_of_arguments =
    Proc.max_arguments_for_tailcalls - 1
end
let flambda2_backend =
  (module Flambda2_backend : Flambda2.Flambda2_backend_intf.S)

let () = Flambda2.Parse_ilambda.go ~backend:flambda2_backend ()
