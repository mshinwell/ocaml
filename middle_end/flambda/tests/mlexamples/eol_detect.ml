type 'a ref = { mutable contents : 'a }

external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external ( <> ) : 'a -> 'a -> bool = "%notequal"

type eol =
  | LF 
  | CRLF 

let get_eol_for_file ~read =
  let c = ref ' ' in
  let prev = ref None in
  try
    while (c := read (); !c <> '\n') do
      () (*prev := (Some (!c))*)
      done;
    (match !prev with
     | None -> LF
     | Some '\r' -> CRLF
     | Some _ -> LF)
  with | _ -> LF
