(* PR#4229 *)

let () =
  (* Dynlink.init (); *)  (* this function has been removed from the API *)
    Dynlink.loadfile "client.cmxs"; (* utilise abstract.cmo *)
      Dynlink.loadfile "sub/abstract.cmxs";
        Dynlink.loadfile "client.cmxs" (* utilise sub/abstract.cmo *)
