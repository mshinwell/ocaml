(* PR#4229 *)

let () =
  (* Dynlink.init (); *)  (* this function has been removed from the API *)
    Dynlink.loadfile "client.cmo"; (* utilise abstract.cmo *)
      Dynlink.loadfile "sub/abstract.cmo";
        Dynlink.loadfile "client.cmo" (* utilise sub/abstract.cmo *)
