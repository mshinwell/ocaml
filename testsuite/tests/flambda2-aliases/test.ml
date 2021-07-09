(* TEST
   include ocamlcommon
   include ocamlmiddleend
   * bytecode
*)

let () = Clflags.flambda_invariant_checks := true
let () = Clflags.dump_rawflambda := true
let () = Misc.Color.setup (Some Never)

let compilation_unit =
  let dummy = "compilation_unit" in
  Compilation_unit.create
     (Ident.create_persistent dummy)
     (Linkage_name.create dummy)

let () = Compilation_unit.set_current compilation_unit

let next_time : ?mode:Name_mode.t -> unit -> Binding_time.With_name_mode.t =
  let next = ref Binding_time.earliest_var in
  fun ?(mode = Name_mode.normal) () ->
    let time = !next in
    next := Binding_time.succ time;
    Binding_time.With_name_mode.create time mode

let mk_var name = Variable.create name
let mk_simple name = Simple.var (mk_var name)
let mk_coercion from to_ =
  let unrolling = Rec_info_expr.Unrolling_state.not_unrolling in
  let from = Rec_info_expr.const ~depth:(Finite from) ~unrolling in
  let to_ = Rec_info_expr.const ~depth:(Finite to_) ~unrolling in
  Coercion.change_depth ~from ~to_

let mk_symbol : string -> Symbol.t =
  let next = ref 0 in
  fun name ->
    let tag = !next in
    incr next;
    let full_name = Format.sprintf "%s_%d" name tag in
    Symbol.create compilation_unit (full_name |> Linkage_name.create)

let of_ok = function
  | Or_bottom.Ok a -> a
  | Or_bottom.Bottom -> Misc.fatal_errorf "of_ok Bottom"

let get_canonical aliases simple name_mode ~min_name_mode =
  match
    (* CR lmaurer: Should also be testing binding time *)
    Aliases.get_canonical_element_exn aliases simple name_mode ~min_name_mode
      ~min_binding_time:Binding_time.consts_and_discriminants
  with
  | exception Not_found -> None
  | canonical -> Some canonical

let add_alias
      ppf
      aliases
      ~element1
      ~binding_time_and_mode1
      ~coercion_from_element2_to_element1
      ~element2
      ~binding_time_and_mode2 =
  let element2 =
    Simple.with_coercion element2 coercion_from_element2_to_element1
  in
  let { Aliases.t; canonical_element; alias_of_demoted_element } =
    Aliases.add
      aliases
      ~element1
      ~binding_time_and_mode1
      ~element2
      ~binding_time_and_mode2
  in
  let coercion_from_demoted_alias_to_canonical_element =
    Simple.coercion alias_of_demoted_element
  in
  let demoted_alias = Simple.without_coercion alias_of_demoted_element in
  let pp_name_mode ppf binding_time_and_mode =
    let name_mode =
      Binding_time.With_name_mode.name_mode binding_time_and_mode
    in
    if Name_mode.is_normal name_mode then () else
      Format.fprintf ppf ("@ (%a)")
      Name_mode.print name_mode
  in
  Format.fprintf ppf "[added] @[<hov 1>%a%a@] <--[%a]-- @[<hov 1>%a%a@]@."
    Simple.print canonical_element
    pp_name_mode binding_time_and_mode1
    Coercion.print coercion_from_demoted_alias_to_canonical_element
    Simple.print demoted_alias
    pp_name_mode binding_time_and_mode2;
  t

let pp_opt_or_none f ppf = function
  | Some a -> f ppf a
  | None -> Format.pp_print_string ppf "<none>"

let test msg ~f =
  Format.printf "*** %s@." msg;
  f Format.std_formatter;
  Format.pp_print_newline Format.std_formatter ();
  Format.pp_print_newline Format.std_formatter ()

let () = test "empty" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  Aliases.print ppf aliases)

let () = test "single alias" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () in
  let t_y = next_time () in
  let x = mk_simple "x" in
  let y = mk_simple "y" in
  (* x <--[f]-- y *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  Aliases.print ppf aliases)

let () = test "single alias (inverse)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_y = next_time () in
  let t_x = next_time () in
  let x = mk_simple "x" in
  let y = mk_simple "y" in
  (* x <--[f]-- y
     ~>
     y <--[F]-- x *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  Aliases.print ppf aliases)

let () = test "two aliases (independent)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_z = next_time () in
  let x = mk_simple "x" in
  let y = mk_simple "y" in
  let z = mk_simple "z" in
  (* x <--[f]-- y
     +
     x <--[g]-- z
     ~>
     x <--[f]-- y
     ^--[g]-- z *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 2 0)
      ~element2:z
      ~binding_time_and_mode2:t_z
  in
  Aliases.print ppf aliases)

let () = test "two aliases (simple chain)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_z = next_time () in
  let x = mk_simple "x" in
  let y = mk_simple "y" in
  let z = mk_simple "z" in
  (* x <--[f]-- y
     +
     y <--[g]-- z
     ~>
     x <--[f]-- y
     ^--[fg]-- z *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
   let aliases =
    add_alias
      ppf
      aliases
      ~element1:y
      ~binding_time_and_mode1:t_y
      ~coercion_from_element2_to_element1:(mk_coercion 2 1)
      ~element2:z
      ~binding_time_and_mode2:t_z
  in
  Aliases.print ppf aliases)

let () = test "two aliases (two inverses)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_z = next_time () in
  let t_x = next_time () in
  let t_y = next_time () in
  let x = mk_simple "x" in
  let y = mk_simple "y" in
  let z = mk_simple "z" in
  (* x <--[f]-- y
     +
     y <--[g]-- z
     ~>
     z <--[GF]-- x
     ^--[G]-- y *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:y
      ~binding_time_and_mode1:t_y
      ~coercion_from_element2_to_element1:(mk_coercion 2 1)
      ~element2:z
      ~binding_time_and_mode2:t_z
  in
  Aliases.print ppf aliases)

let () = test "two aliases (one inverse)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_z = next_time () in
  let t_x = next_time () in
  let t_y = next_time () in
  let x = mk_simple "x" in
  let y = mk_simple "y" in
  let z = mk_simple "z" in
  (* x <--[f]-- y
     +
     z <--[g]-- y
     ~>
     z <--[gF]-- x
     ^--[g]-- y *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:z
      ~binding_time_and_mode1:t_z
      ~coercion_from_element2_to_element1:(mk_coercion 1 2)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  Aliases.print ppf aliases)

let () = test "two aliases (one inverse)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_z = next_time () in
  let x = mk_simple "x" in
  let y = mk_simple "y" in
  let z = mk_simple "z" in
  (* x <--[f]-- y
     +
     z <--[g]-- y
     ~>
     x <--[f]-- y
     ^--[fG]-- z *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:z
      ~binding_time_and_mode1:t_z
      ~coercion_from_element2_to_element1:(mk_coercion 1 2)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  Aliases.print ppf aliases)

let () = test "three aliases (one inverse)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_z = next_time () in
  let t_t = next_time () in
  let x = mk_simple "x" in
  let y = mk_simple "y" in
  let z = mk_simple "z" in
  let t = mk_simple "t" in
  (* x <--[f]-- y
     +
     z <--[g]-- t
     +
     y <--[h]-- t
     ~>
     x <--[f]-- y
     ^^--[fhG]-- z
      \--[fh]-- t *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:z
      ~binding_time_and_mode1:t_z
      ~coercion_from_element2_to_element1:(mk_coercion 3 2)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:y
      ~binding_time_and_mode1:t_y
      ~coercion_from_element2_to_element1:(mk_coercion 3 1)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  Aliases.print ppf aliases)

let () = test "three aliases (two inverses)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_z = next_time () in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_t = next_time () in
  let x = mk_simple "x" in
  let y = mk_simple "y" in
  let z = mk_simple "z" in
  let t = mk_simple "t" in
  (* x <--[f]-- y
     +
     z <--[g]-- t
     +
     y <--[h]-- t
     ~>
     z <--[gHF]-- x
     ^^--[gH]-- y
     \--[g]-- t *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:z
      ~binding_time_and_mode1:t_z
      ~coercion_from_element2_to_element1:(mk_coercion 3 2)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:y
      ~binding_time_and_mode1:t_y
      ~coercion_from_element2_to_element1:(mk_coercion 3 1)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  Aliases.print ppf aliases)

let () = test "three aliases of const (two inverses)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_z =
    Binding_time.With_name_mode.create
      Binding_time.consts_and_discriminants
      Name_mode.normal
  in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_t = next_time () in
  let x = mk_simple "x" in
  let y = mk_simple "y" in
  let z = Reg_width_things.Const.untagged_const_true |> Simple.const in
  let t = mk_simple "t" in
  (* x <--[f]-- y
     +
     z <--[g]-- t
     +
     y <--[h]-- t
     ~>
     z <--[gHF]-- x
     ^^--[gH]-- y
     \--[g]-- t *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:z
      ~binding_time_and_mode1:t_z
      ~coercion_from_element2_to_element1:(mk_coercion 3 2)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:y
      ~binding_time_and_mode1:t_y
      ~coercion_from_element2_to_element1:(mk_coercion 3 1)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  Aliases.print ppf aliases)


let two_var_name_mode_test name mode_x mode_y min_name_mode =
  test name ~f:(fun ppf ->
    let aliases = Aliases.empty in
    let t_x = next_time () ~mode:mode_x in
    let t_y = next_time () ~mode:mode_y in
    let v_x = mk_var "x" in
    let v_y = mk_var "y" in
    let x = Simple.var v_x in
    let y = Simple.var v_y in
    (* x <--[f]-- y *)
    let aliases =
      add_alias
        ppf
        aliases
        ~element1:x
        ~binding_time_and_mode1:t_x
        ~coercion_from_element2_to_element1:(mk_coercion 1 0)
        ~element2:y
        ~binding_time_and_mode2:t_y
    in
    Format.fprintf ppf
      "Canonical for %a: %a@.\
       ... with mode >= %a: %a@.\
       Aliases of %a: %a"
       Simple.print y
       Simple.print
        (Aliases.get_canonical_ignoring_name_mode aliases (Name.var v_y))
       Name_mode.print min_name_mode
       (pp_opt_or_none Simple.print)
         (get_canonical aliases y mode_y ~min_name_mode)
       Simple.print x
       Aliases.Alias_set.print (Aliases.get_aliases aliases x))

let () =
  two_var_name_mode_test "name mode: phantom = phantom"
    Name_mode.phantom Name_mode.phantom Name_mode.phantom

let () =
  two_var_name_mode_test "name mode: normal > phantom"
    Name_mode.normal Name_mode.phantom Name_mode.phantom

let () =
  two_var_name_mode_test "name mode: phantom !>= in_types (fail)"
    Name_mode.phantom Name_mode.phantom Name_mode.in_types

let () =
  two_var_name_mode_test "name mode: phantom !>= in_types (return same)"
    Name_mode.phantom Name_mode.in_types Name_mode.in_types

let () =
  two_var_name_mode_test "name mode: normal > in_types"
    Name_mode.normal Name_mode.phantom Name_mode.in_types

let () =
  two_var_name_mode_test "name mode: phantom !>= normal"
    Name_mode.phantom Name_mode.phantom Name_mode.normal

let () = test "three aliases (one inverse) /w modes" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () ~mode:Name_mode.in_types in
  let t_y = next_time () ~mode:Name_mode.in_types in
  let t_z = next_time () ~mode:Name_mode.phantom in
  let t_t = next_time () ~mode:Name_mode.normal in
  let v_x = mk_var "x" in
  let v_y = mk_var "y" in
  let v_z = mk_var "z" in
  let v_t = mk_var "t" in
  let x = Simple.var v_x in
  let y = Simple.var v_y in
  let z = Simple.var v_z in
  let t = Simple.var v_t in
  let z' = Simple.with_coercion z (mk_coercion 2 4) in
  (* x <--[f]-- y
     +
     z <--[g]-- t
     +
     y <--[h]-- t
     ~>
     x <--[f]-- y
     ^^--[fhG]-- z
      \--[fh]-- t *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:z
      ~binding_time_and_mode1:t_z
      ~coercion_from_element2_to_element1:(mk_coercion 3 2)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:y
      ~binding_time_and_mode1:t_y
      ~coercion_from_element2_to_element1:(mk_coercion 3 1)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  let show_canonicals simple var mode =
    (* CR lmaurer: This is a bit awkward because [Aliases] happens to lack a
       function for the canonical of a general [Simple.t] ignoring name
       modes. *)
    Format.fprintf ppf
      "@.Canonical for %a: %a@.\
       Canonical for %a@.\
       ... with mode >= Phantom: %a@.\
       ...           >= In_types: %a@.\
       ...           >= Normal: %a@.\
      "
      Variable.print var
      Simple.print
       (Aliases.get_canonical_ignoring_name_mode aliases (Name.var var))
      Simple.print simple
      (pp_opt_or_none Simple.print)
        (get_canonical aliases simple mode ~min_name_mode:Name_mode.phantom)
      (pp_opt_or_none Simple.print)
        (get_canonical aliases simple mode ~min_name_mode:Name_mode.in_types)
      (pp_opt_or_none Simple.print)
        (get_canonical aliases simple mode ~min_name_mode:Name_mode.normal)
  in
  let show_aliases simple =
    Format.fprintf ppf "@.Aliases of %a: %a"
      Simple.print simple
      Aliases.Alias_set.print (Aliases.get_aliases aliases simple)
  in
  show_canonicals x v_x Name_mode.in_types;
  show_canonicals y v_y Name_mode.in_types;
  show_canonicals z v_z Name_mode.phantom;
  show_canonicals z' v_z Name_mode.phantom;
  show_canonicals t v_t Name_mode.normal;
  show_aliases x;
  show_aliases z;
  show_aliases z';
  Format.fprintf ppf "@.@.%a" Aliases.print aliases)

let () = print_endline "OK."
