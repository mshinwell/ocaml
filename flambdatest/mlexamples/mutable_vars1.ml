(* Reduced from Oprint.escape_string *)

external length : string -> int = "%string_length"
external unsafe_get : string -> int -> char = "%string_unsafe_get"

let escape_string s s' =
  let n = ref 0 in
  for i = 0 to length s do
    begin match unsafe_get s i with
    | '\x00' -> incr n
    | c -> Bytes.unsafe_set s' !n c
    end;
    incr n
  done
