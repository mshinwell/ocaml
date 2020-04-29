(* TEST *)

(* Basic tests *)

let () =
  for x = -2 to 2 do
    print_int x;
    print_newline ()
  done

let () =
  for x = 2 downto -2 do
    print_int x;
    print_newline ()
  done

(* Test that compilation of for-loops has not miscompiled the
   case where incrementing (resp. decrementing) the upper
   (resp. lower) bound, in the case of increasing and decreasing
   for-loops respectively, would cause overflow and hence
   infinite looping. *)

let start = max_int - 1
let stop = max_int

let () =
  for x = start to stop do
    assert (x >= start);
    assert (x <= stop)
  done

let start = min_int + 1
let stop = min_int

let () =
  for x = start downto stop do
    assert (x <= start);
    assert (x >= stop)
  done

let () = print_endline "End"
