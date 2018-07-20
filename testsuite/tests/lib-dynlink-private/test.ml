let () = Sheep.baa Sheep.s (* Use Sheep module *)
let _ = fun (x : Pig.t) -> x (* Reference Pig module *)

(* Test that a privately loaded module cannot have the same name as a
   module in the program *)
let test_sheep () =
  match
    if Dynlink.is_native then
      Dynlink.loadfile_private "plugin1/sheep.cmxs"
    else
      Dynlink.loadfile_private "plugin1/sheep.cmo"
  with
  | () -> assert false
  | exception Dynlink.Error (
      Dynlink.Module_already_loaded "Sheep") -> ()

(* Test that a privately loaded module can have the same name as a
   previous privately loaded module *)
let test_cow () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin2/cow.cmxs"
  else
    Dynlink.loadfile_private "plugin2/cow.cmo"

(* Test that a privately loaded module cannot have the same name as an
   interface depended on by modules the program *)
let test_pig () =
  match
    if Dynlink.is_native then
      Dynlink.loadfile_private "plugin3/pig.cmxs"
    else
      Dynlink.loadfile_private "plugin3/pig.cmo"
  with
  | () -> assert false
  | exception Dynlink.Error (
      Dynlink.Private_library_cannot_implement_interface "Pig") -> ()

(* Test that a privately loaded module can recursively load a module of
   the same name *)
let test_chicken () =
  if Dynlink.is_native then
    Dynlink.loadfile_private "plugin4/chicken.cmxs"
  else
    Dynlink.loadfile_private "plugin4/chicken.cmo"

let () =
  test_sheep ();
  test_cow ();
  test_cow ();
  test_pig ();
  test_chicken ()
