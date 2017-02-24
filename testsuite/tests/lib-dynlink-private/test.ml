let () =
  Sheep.baa Sheep.s;
  try
    if Dynlink.is_native then
      Dynlink.loadfile_private "plugin/sheep.cmxs"
    else
      Dynlink.loadfile_private "plugin/sheep.cmo"
  with
  | Dynlink.Error (
      Dynlink.Private_library_cannot_implement_interface "Sheep") -> ()
  | exn -> raise exn
