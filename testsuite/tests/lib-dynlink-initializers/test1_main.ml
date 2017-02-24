(* Check that a module in the main program whose initializer has not
   executed completely cannot be depended upon by a shared library being
   loaded. *)

let f x = x + 1 [@@inline never]

let () =
  try
    if Dynlink.is_native then
      Dynlink.loadfile "test1_plugin.cmxs"
    else
      Dynlink.loadfile "test1_plugin.cmo"
  with
  | Dynlink.Error (
      Dynlink.Linking_error (_,
        Dynlink.Uninitialized_global "Test1_inited_second")) -> ()
  | _ -> exit 1
