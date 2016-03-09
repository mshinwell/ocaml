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
  let module O = AProf in
  let module H = A.Heap_snapshot in
  let pathname_prefix = "/tmp/heap_snapshot" in
  let writer = H.Writer.create ~pathname_prefix in
  A.Trace.debug ();
  Printf.printf "taking snapshot\n%!";
  H.take writer;
  Printf.printf "saving trace\n%!";
  H.Writer.save_trace_and_close writer;
  Printf.printf "done\n%!";
  let module H = RawAProf.Heap_snapshot in
  let series = H.Series.read ~pathname_prefix in
  Printf.printf "read %d snapshot(s)\n" (H.Series.num_snapshots series);
  let trace =
    match
      H.Series.trace series
        ~kind:H.Series.Normal
        ~thread_index:0
    with
    | None -> failwith "No trace"
    | Some trace -> trace
  in
  let snapshot0 = H.Series.snapshot series ~index:0 in
  Printf.printf "minor heap: %d blocks, %d instrumented\n%!"
    (H.num_blocks_in_minor_heap snapshot0)
    (H.num_blocks_in_minor_heap_with_profinfo snapshot0);
  Printf.printf "major heap: %d blocks, %d instrumented\n%!"
    (H.num_blocks_in_major_heap snapshot0)
    (H.num_blocks_in_major_heap_with_profinfo snapshot0);
  let frame_table = H.Series.frame_table series in
  let resolve_return_address loc =
    match RawAProf.Frame_table.find_exn frame_table loc with
    | exception Not_found -> None
    | slot ->
      match Printexc.Slot.location slot with
      | None ->
        Some (Printf.sprintf "*0x%Lx"
          (RawAProf.Program_counter.OCaml.to_int64 loc))
      | Some loc ->
        let loc =
          Printf.sprintf "%s:%d(%d--%d)"
            loc.Printexc.filename
            loc.Printexc.line_number
            loc.Printexc.start_char
            loc.Printexc.end_char
        in
        Some loc
  in
  RawAProf.Trace.debug_ocaml trace ~resolve_return_address;
  Printf.printf "done"
