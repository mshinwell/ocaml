(* Reduced from stdlib/genlex.ml *)

let foo f g =
  match f with
  | 'a' ->
    let _ =
      try g () with
      | Stream.Failure -> raise (Stream.Error "")
    in
    raise (Stream.Error "")
  | _ -> None
