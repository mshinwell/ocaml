type t = A of string * int | B of int

let rec f x =
  if x = 0 then f 47
  else if x = 1 then g x
  else if x < 42 then A ("foo", x + 3)
  else B (x * 2)

and g _x =
  f (-1)

let () =
  Printf.printf "start\n";
  begin match f 9 with
  | A (s, i) -> print_string s; print_int i; print_newline ()
  | B x -> print_int x; print_newline ()
  end;
  begin match f 50 with
  | A (s, i) -> print_string s; print_int i; print_newline ()
  | B x -> print_int x; print_newline ()
  end;
  begin match f 0 with
  | A (s, i) -> print_string s; print_int i; print_newline ()
  | B x -> print_int x; print_newline ()
  end;
  begin match f 1 with
  | A (s, i) -> print_string s; print_int i; print_newline ()
  | B x -> print_int x; print_newline ()
  end;
  let module A = Allocation_profiling in
  let module H = A.Heap_snapshot in
  let pathname_prefix = "/tmp/heap_snapshot" in
  let writer = H.Writer.create ~pathname_prefix in
  A.Trace.debug ();
  Printf.printf "taking snapshot\n%!";
  H.take writer;
  Printf.printf "saving trace\n%!";
  H.Writer.save_trace_and_close writer;
  Printf.printf "done\n%!";
  let series = H.Series.read ~pathname_prefix in
  Printf.printf "read %d snapshot(s)\n" (H.Series.num_snapshots series);
  let trace = H.Series.trace series in
  A.Trace.debug_ocaml trace;
  Printf.printf "done"
