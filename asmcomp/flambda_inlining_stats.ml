open Abstract_identifiers

let vim_trailer = "vim:fdm=expr:filetype=plain:\
  foldexpr=getline(v\\:lnum)=~'^\\\\s*$'&&getline(v\\:lnum+1)=~'\\\\S'?'<1'\\:1"

let toplevel_name = "*toplevel*"

module Closure_stack = struct
  type t
    = (Closure_id.t * Flambda_inlining_stats_types.where_entering_closure
        * Debuginfo.t) list

  let last_compilation_unit = ref None
  let last_toplevel = ref None

  let toplevel () =
    let changed_unit =
      match !last_compilation_unit with
      | None -> true
      | Some compilation_unit ->
        compilation_unit <> Compilenv.current_unit ()
    in
    if changed_unit then begin
      let var =
        Variable.create toplevel_name
          ~current_compilation_unit:(Compilenv.current_unit ())
      in
      last_compilation_unit := Some (Compilenv.current_unit ());
      last_toplevel := Some (Closure_id.wrap var)
    end;
    match !last_toplevel with
    | Some toplevel -> toplevel
    | None -> assert false

  let create () = []

  let note_entering_closure t ~closure_id ~where ~debuginfo =
    if not !Clflags.inlining_stats then t
    else t @ [closure_id, where, debuginfo]

  let pop_exn = function
    | [] -> failwith "Closure_stack.pop on empty stack"
    | (closure_id, _, debuginfo)::tl -> closure_id, debuginfo, tl

  let debuginfo = function
    | [] -> None
    | (_, _, debuginfo)::tl -> Some debuginfo

  let add_toplevel_if_required (t : t) =
    match t with
    | [] -> []
    | (_, Inline_by_copying_function_body, debuginfo)::_
    | (_, Inline_by_copying_function_declaration, debuginfo)::_
    | (_, Inlining_decision, debuginfo)::_ ->
      (* The closure stack started not inside a declaration, so we must add
         the dummy toplevel element. *)
      (toplevel (), (Transform_set_of_closures_expression
        : Flambda_inlining_stats_types.where_entering_closure), debuginfo)::t
    | (closure_id, Transform_set_of_closures_expression, debuginfo)::_ ->
      (* Unfortunately we also have to check for this special case. *)
      (* CR mshinwell: share the name of [partial_fun] with flambdainline.ml *)
      if Closure_id.name closure_id = "partial_fun" then
        (toplevel (), (Transform_set_of_closures_expression
          : Flambda_inlining_stats_types.where_entering_closure), debuginfo)::t
      else
        t

  let save t ~out_channel =
    let print_elt (closure_id, where, _debuginfo) =
      let start_delim, end_delim, arrow =
        match
          (where : Flambda_inlining_stats_types.where_entering_closure)
        with
        | Transform_set_of_closures_expression -> "decl(", ")", ": "
        | Inline_by_copying_function_body
        | Inline_by_copying_function_declaration
        | Inlining_decision -> "", "", " <= "
      in
      let output =
        let current_unit = Compilenv.current_unit () in
        if Closure_id.in_compilation_unit current_unit closure_id then
          Closure_id.output
        else
          Closure_id.output_full
      in
      Printf.fprintf out_channel "%s%a%s" start_delim output closure_id
        end_delim;
      arrow
    in
    let rec loop = function
      | [] -> Printf.fprintf out_channel "[]"
      | [elt] -> let (_ : string) = print_elt elt in ()
      | ((_, where, _) as elt)::elts ->
        let arrow = print_elt elt in
        Printf.fprintf out_channel "%s" arrow;
        loop elts
    in
    loop t
end

let time = ref 0

module Sorted_closure_id = struct
  module T = struct
    type t = Closure_id.t

    let compare t1 t2 =
      let t1_toplevel = Closure_id.name t1 = toplevel_name in
      let t2_toplevel = Closure_id.name t2 = toplevel_name in
      begin match t1_toplevel, t2_toplevel with
      | true, true -> 0
      | true, false -> -1
      | false, true -> 1
      | false, false ->
        Pervasives.compare (Closure_id.name t1) (Closure_id.name t2)
      end
  end

  module Map = Map.Make (T)
end

module Line_number_then_time = struct
  type t = Debuginfo.t * int

  let compare_fst (((dbg1, t1) : t), _) (((dbg2, t2) : t), _) =
    match compare dbg1.dinfo_line dbg2.dinfo_line with
    | -1 -> -1
    | 1 -> 1
    | _ -> compare t1 t2

  let create ~debuginfo ~time = debuginfo, time
  let line_number (t : t) = (fst t).dinfo_line
end

let decisions :
  (Line_number_then_time.t
      * (Closure_stack.t * Flambda_inlining_stats_types.Decision.t)) list
    Sorted_closure_id.Map.t ref = ref Sorted_closure_id.Map.empty

let record_decision decision ~closure_stack ~debuginfo:_ =
  if !Clflags.inlining_stats then begin
    let closure_stack =
      (* When inlining within a toplevel declaration, e.g.
           let foo = bar 1 2 3
         then we need to add a dummy "toplevel" element to the closure stack
         in order to prevent (using this example) inlining of [bar] at this
         location from being conflated with inlining of [bar] within another
         function. *)
      Closure_stack.add_toplevel_if_required closure_stack
    in
    let closure_id, _, closure_stack = Closure_stack.pop_exn closure_stack in
    let debuginfo =
      match Closure_stack.debuginfo closure_stack with
      | None -> Debuginfo.none
      | Some debuginfo -> debuginfo
    in
    let bucket =
      let found =
        Sorted_closure_id.Map.fold (fun closure_id' bucket found ->
            if closure_id = closure_id' then bucket::found else found)
          !decisions []
      in
      match found with
      | [bucket] -> bucket
      | [] -> []
      | _multiple -> assert false
    in
    let key = Line_number_then_time.create ~debuginfo ~time:!time in
    let data = closure_stack, decision in
    (* The order here is important so that the "time rebasing" works
       properly, below. *)
    decisions :=
      Sorted_closure_id.Map.add closure_id
        ((key, data) :: bucket) !decisions;
    incr time
  end

let really_save_then_forget_decisions ~output_prefix =
  let out_channel = open_out (output_prefix ^ ".i") in
  Sorted_closure_id.Map.iter (fun closure_id bucket ->
      begin if Closure_id.name closure_id = toplevel_name then
        Printf.fprintf out_channel "*toplevel*\n"
      else
        Printf.fprintf out_channel "%a\n" Closure_id.output closure_id;
      end;
      let bucket =
        (* Rebase timestamps to start at zero within each bucket. *)
        List.mapi (fun rebased_time (key, (closure_stack, decision)) ->
            key, (rebased_time, closure_stack, decision))
          (List.rev bucket)
      in
      let bucket = List.sort Line_number_then_time.compare_fst bucket in
      List.iter (fun (key, (time, closure_stack, decision)) ->
          let line = Line_number_then_time.line_number key in
          Printf.fprintf out_channel "  %5d: (%5d) " line time;
          Closure_stack.save closure_stack ~out_channel;
          Printf.fprintf out_channel ": %s\n"
            (Flambda_inlining_stats_types.Decision.to_string decision))
        bucket;
      Printf.fprintf out_channel "\n") !decisions;
  Printf.fprintf out_channel "# %s\n" vim_trailer;
  close_out out_channel;
  decisions := Sorted_closure_id.Map.empty;
  time := 0

let save_then_forget_decisions ~output_prefix =
  if !Clflags.inlining_stats then begin
    really_save_then_forget_decisions ~output_prefix
  end
