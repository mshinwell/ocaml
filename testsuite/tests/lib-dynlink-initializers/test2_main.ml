(* Check that a module in a loaded shared library whose initializer has not
   executed completely cannot be depended upon by another shared library being
   loaded. *)

let () =
  if Dynlink.is_native then
    Dynlink.loadfile "test2_plugin.cmxs"
  else
    Dynlink.loadfile "test2_plugin.cmo"
