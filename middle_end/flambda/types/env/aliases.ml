(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Const = Reg_width_things.Const

let map_inter map1 map2 =
  Name.Map.merge (fun _elt a b ->
    match a, b with
    | None, None | Some _, None | None, Some _ -> None
    | Some (), Some () -> Some ()
  )
    map1
    map2

let map_union map1 map2 =
  Name.Map.union (fun _elt a b ->
    match a, b with
    | (), () -> Some ()
  )
    map1
    map2

module Aliases_of_canonical_element : sig
  type t

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : t
  val is_empty : t -> bool

  val add : t -> Name.t -> Name_mode.t -> t

  val find_earliest_candidates
     : t
    -> filter_by_scope:(Name_mode.t -> unit Name.Map.t -> unit Name.Map.t)
    -> min_name_mode:Name_mode.t
    -> unit Name.Map.t option

  val all : t -> unit Name.Map.t

  val mem : t -> Name.t -> bool

  val union : t -> t -> t
  val inter : t -> t -> t

  val rename : (Name.t -> Name.t) -> t -> t

  val merge : t -> t -> t
end = struct
  type t = {
    aliases : unit Name.Map.t Name_mode.Map.t;
    all : unit Name.Map.t;
  }

  let invariant _t = ()

  let print ppf { aliases; all = _; } =
    Name_mode.Map.print (Name.Map.print Unit.print) ppf aliases

  let empty = {
    aliases = Name_mode.Map.empty;
    all = Name.Map.empty;
  }

  let is_empty t = Name.Map.is_empty t.all

  let add t elt name_mode =
    if Name.Map.mem elt t.all then begin
      Misc.fatal_errorf "%a already added to [Aliases_of_canonical_element]: \
          %a"
        Name.print elt
        print t
    end;
    let aliases =
      Name_mode.Map.update name_mode
        (function
          | None -> Some (Name.Map.singleton elt ())
          | Some elts ->
            if !Clflags.flambda_invariant_checks then begin
              assert (not (Name.Map.mem elt elts))
            end;
            Some (Name.Map.add elt () elts))
        t.aliases
    in
    let all = Name.Map.add elt () t.all in
    { aliases;
      all;
    }

  let find_earliest_candidates t ~filter_by_scope ~min_name_mode =
    Name_mode.Map.fold (fun order aliases res_opt ->
        match res_opt with
        | Some _ -> res_opt
        | None ->
          begin match
            Name_mode.compare_partial_order
              order min_name_mode
          with
          | None -> None
          | Some result ->
            if result >= 0 then
              let aliases = filter_by_scope order aliases in
              if Name.Map.is_empty aliases then None else Some aliases
            else None
          end)
      t.aliases
      None

  let mem t elt =
    Name.Map.mem elt t.all

  let all t = t.all

  let union t1 t2 =
    let aliases : unit Name.Map.t Name_mode.Map.t =
      Name_mode.Map.union (fun _order elts1 elts2 ->
          Some (map_union elts1 elts2))
        t1.aliases t2.aliases
    in
    let t =
      { aliases;
        all = map_union t1.all t2.all;
      }
    in
    (* Should pass the invariant if the arguments did, since we assume that
       the maps are consistent (any keys in common map to the same coercion). *)
    invariant t;
    t

  let inter t1 t2 =
    let aliases =
      Name_mode.Map.merge (fun _order elts1 elts2 ->
          match elts1, elts2 with
          | None, None | Some _, None | None, Some _ -> None
          | Some elts1, Some elts2 -> Some (map_inter elts1 elts2))
        t1.aliases t2.aliases
    in
    let t =
      { aliases;
        all = map_inter t1.all t2.all;
      }
    in
    invariant t;
    t

  let rename rename_name { aliases; all } =
    let map_name elts =
      Name.Map.fold (fun elt coercion acc ->
        Name.Map.add (rename_name elt) coercion acc)
        elts
        Name.Map.empty
    in
    let aliases = Name_mode.Map.map map_name aliases in
    let all = map_name all in
    let t = { aliases; all } in
    invariant t;
    t

  let merge t1 t2 =
    let aliases =
      Name_mode.Map.union (fun _mode map1 map2 ->
        Some (map_union map1 map2)
      )
        t1.aliases
        t2.aliases
    in
    let all = map_union t1.all t2.all in
    let t = { aliases; all; } in
    invariant t;
    t
end

module Alias_set = struct
  type t = {
    const : Const.t option;
    names : unit Name.Map.t;
  }

  let empty = { const = None; names = Name.Map.empty; }

  let create ~canonical_element ~alias_names =
    Simple.pattern_match canonical_element
      ~const:(fun canonical_const ->
        let const = Some canonical_const in
        let names = alias_names in
        { const; names })
      ~name:(fun canonical_name ->
        let const = None in
        let names = Name.Map.add canonical_name () alias_names in
        { const; names })

  let singleton simple =
    Simple.pattern_match simple
      ~const:(fun const ->
        { const = Some const; names = Name.Map.empty; })
      ~name:(fun name ->
        { const = None; names = Name.Map.singleton name () })

  let get_singleton { const; names; } =
    match const with
    | Some const ->
      if Name.Map.is_empty names then Some (Simple.const const) else None
    | None ->
      Name.Map.get_singleton names
      |> Option.map (fun (s, _) -> Simple.name s)

  let print ppf { const; names; } =
    let none ppf () =
      Format.fprintf ppf "@<0>%s()" (Flambda_colours.elide ())
    in
    Format.fprintf ppf
      "@[<hov 1>(\
           @[<hov 1>(const@ %a)@]@ \
           @[<hov 1>(names@ %a)@]\
       @]"
       (Format.pp_print_option Const.print ~none) const
       (Name.Map.print Unit.print) names

  let inter
        { const = const1; names = names1; }
        { const = const2; names = names2; } =
    let const =
      match const1, const2 with
      | Some const1, Some const2 when Const.equal const1 const2 -> Some const1
      | _, _ -> None
    in
    let names = map_inter names1 names2 in
    { const; names; }

  let filter { const; names; } ~f =
    let const =
      match const with
      | Some const when f (Simple.const const) -> Some const
      | _ -> None
    in
    let names =
      Name.Map.filter (fun name _ -> f (Simple.name name)) names
    in
    { const; names; }

  let find_best { const; names; } =
    match const with
    | Some const -> Some (Simple.const const)
    | None ->
      let key_is_symbol key _data = Name.is_symbol key in
      let (symbols, vars) = Name.Map.partition key_is_symbol names in
      match Name.Map.min_binding_opt symbols with
      | Some (symbol, _) ->
        Some (Simple.name symbol)
      | None ->
        match Name.Map.min_binding_opt vars with
        | Some (var, _) ->
          Some (Simple.name var)
        | None ->
          None
end


type t = {
  canonical_elements : Simple.t Name.Map.t;
  (* Canonical elements that have no known aliases are not included in
     [canonical_elements]. *)
  aliases_of_canonical_names : Aliases_of_canonical_element.t Name.Map.t;
  (* For [elt |-> aliases] in [aliases_of_canonical_names], then
     [aliases] never includes [elt]. *)
  (* CR mshinwell: check this always holds *)
  aliases_of_consts : Aliases_of_canonical_element.t Const.Map.t;
  binding_times_and_modes : Binding_time.With_name_mode.t Name.Map.t;
  (* Binding times and name modes define an order on the elements.
     The canonical element for a set of aliases is always the minimal
     element for this order, which is different from the order used
     for creating sets and maps. *)
}

let print ppf { canonical_elements; aliases_of_canonical_names;
                aliases_of_consts; binding_times_and_modes; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(canonical_elements@ %a)@]@ \
      @[<hov 1>(aliases_of_canonical_names@ %a)@]@ \
      @[<hov 1>(aliases_of_consts@ %a)@]@ \
      @[<hov 1>(binding_times_and_modes@ %a)@]\
      )@]"
    (Name.Map.print Simple.print) canonical_elements
    (Name.Map.print Aliases_of_canonical_element.print)
    aliases_of_canonical_names
    (Const.Map.print Aliases_of_canonical_element.print)
    aliases_of_consts
    (Name.Map.print Binding_time.With_name_mode.print)
    binding_times_and_modes

let name_defined_earlier t alias ~than =
  let info1 = Name.Map.find alias t.binding_times_and_modes in
  let info2 = Name.Map.find than t.binding_times_and_modes in
  Binding_time.strictly_earlier
    (Binding_time.With_name_mode.binding_time info1)
    ~than:(Binding_time.With_name_mode.binding_time info2)

let defined_earlier t alias ~than =
  Simple.pattern_match than
    ~const:(fun _ -> false)
    ~name:(fun than ->
      Simple.pattern_match alias
        ~const:(fun _ -> true)
        ~name:(fun alias -> name_defined_earlier t alias ~than))

let binding_time_and_name_mode t elt =
  Simple.pattern_match elt
    ~const:(fun _ ->
      Binding_time.With_name_mode.create
        Binding_time.consts_and_discriminants
        Name_mode.normal)
    ~name:(fun elt -> Name.Map.find elt t.binding_times_and_modes)

let name_mode_unscoped t elt =
  Binding_time.With_name_mode.name_mode (binding_time_and_name_mode t elt)

let name_mode t elt ~min_binding_time =
  Binding_time.With_name_mode.scoped_name_mode
    (binding_time_and_name_mode t elt)
    ~min_binding_time

let invariant t =
  if !Clflags.flambda_invariant_checks then begin
    let all_aliases_of_names : unit Name.Map.t =
      Name.Map.fold (fun canonical_element aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases;
          let aliases = Aliases_of_canonical_element.all aliases in
          if not (Name.Map.for_all (fun elt _ ->
              name_defined_earlier t canonical_element ~than:elt) aliases)
          then begin
            Misc.fatal_errorf "Canonical element %a is not earlier than \
                all of its aliases:@ %a"
              Name.print canonical_element
              print t
          end;
          if Name.Map.mem canonical_element aliases then begin
            Misc.fatal_errorf "Canonical element %a occurs in alias set:@ %a"
              Name.print canonical_element
              (Name.Map.print Unit.print) aliases
          end;
          if Name.Map.inter_domain_is_non_empty aliases all_aliases then
          begin
            Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
          end;
          map_union aliases all_aliases)
        t.aliases_of_canonical_names
        Name.Map.empty
    in
    let _all_aliases : unit Name.Map.t =
      Const.Map.fold (fun _const aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases;
          let aliases = Aliases_of_canonical_element.all aliases in
          if Name.Map.inter_domain_is_non_empty aliases all_aliases then
          begin
            Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
          end;
          map_union aliases all_aliases)
        t.aliases_of_consts
        all_aliases_of_names
    in
    ()
  end

let empty = {
  (* CR mshinwell: Rename canonical_elements, maybe to
     aliases_to_canonical_elements. *)
  canonical_elements = Name.Map.empty;
  aliases_of_canonical_names = Name.Map.empty;
  aliases_of_consts = Const.Map.empty;
  binding_times_and_modes = Name.Map.empty;
}

type canonical =
  | Is_canonical of Simple.t
  | Alias_of_canonical of { element : Name.t; canonical_element : Simple.t; }

let canonical t element : canonical =
  Simple.pattern_match element
    ~const:(fun _ -> Is_canonical element)
    ~name:(fun name ->
      match Name.Map.find name t.canonical_elements with
      | exception Not_found -> Is_canonical element
      | canonical_element ->
        if !Clflags.flambda_invariant_checks then begin
          assert (not (Simple.equal element canonical_element))
        end;
        Alias_of_canonical { element = name; canonical_element; })

let get_aliases_of_canonical_element t ~canonical_element =
  let name name =
    Name.Map.find name t.aliases_of_canonical_names
  in
  let const const =
    Const.Map.find const t.aliases_of_consts
  in
  match Simple.pattern_match canonical_element ~name ~const with
  | exception Not_found -> Aliases_of_canonical_element.empty
  | aliases -> aliases

let add_alias_between_canonical_elements t ~canonical_element ~to_be_demoted =
  if Simple.equal canonical_element to_be_demoted then
    t
  else
    let name_to_be_demoted =
      Simple.pattern_match to_be_demoted
        ~const:(fun c ->
          Misc.fatal_errorf
            "Can't demote const %a@ (while adding alias to@ %a)"
          Const.print c
          Simple.print canonical_element)
        ~name:(fun name -> name)
    in
    let aliases_of_to_be_demoted =
      get_aliases_of_canonical_element t ~canonical_element:to_be_demoted
    in
    if !Clflags.flambda_invariant_checks then begin
      Simple.pattern_match canonical_element
        ~const:(fun _ -> ())
        ~name:(fun canonical_element ->
          assert (not (Aliases_of_canonical_element.mem
            aliases_of_to_be_demoted canonical_element)))
    end;
    let canonical_elements =
      t.canonical_elements
      |> Name.Map.fold (fun alias _ canonical_elements ->
          Name.Map.add alias canonical_element canonical_elements)
        (Aliases_of_canonical_element.all aliases_of_to_be_demoted)
      |> Name.Map.add name_to_be_demoted canonical_element
    in
    let aliases_of_canonical_element =
      get_aliases_of_canonical_element t ~canonical_element
    in
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Aliases_of_canonical_element.mem
        aliases_of_canonical_element name_to_be_demoted));
      assert (Aliases_of_canonical_element.is_empty (
        Aliases_of_canonical_element.inter
          aliases_of_canonical_element aliases_of_to_be_demoted))
    end;
    let aliases =
      Aliases_of_canonical_element.add
        (Aliases_of_canonical_element.union aliases_of_to_be_demoted
          aliases_of_canonical_element)
        name_to_be_demoted (name_mode_unscoped t to_be_demoted)
    in
    let aliases_of_canonical_names =
      Name.Map.remove name_to_be_demoted t.aliases_of_canonical_names
    in
    let aliases_of_canonical_names, aliases_of_consts =
      Simple.pattern_match canonical_element
        ~name:(fun name ->
          Name.Map.add (* replace *) name aliases aliases_of_canonical_names,
          t.aliases_of_consts)
        ~const:(fun const ->
          aliases_of_canonical_names,
          Const.Map.add (* replace *) const aliases t.aliases_of_consts)
    in
    { canonical_elements;
      aliases_of_canonical_names;
      aliases_of_consts;
      binding_times_and_modes = t.binding_times_and_modes;
    }

type to_be_demoted = Demote_canonical_element1 | Demote_canonical_element2

let choose_canonical_element_to_be_demoted t ~canonical_element1
      ~canonical_element2 =
  if defined_earlier t canonical_element1 ~than:canonical_element2
  then Demote_canonical_element2 else Demote_canonical_element1

(* CR mshinwell: add submodule *)
type add_result = {
  t : t;
  canonical_element : Simple.t;
  alias_of_demoted_element : Simple.t;
}

let invariant_add_result ~original_t { canonical_element; alias_of_demoted_element; t; } =
  if !Clflags.flambda_invariant_checks then begin
    invariant t;
    if not (defined_earlier t canonical_element ~than:alias_of_demoted_element) then begin
      Misc.fatal_errorf "Canonical element %a should be defined earlier \
          than %a after alias addition.@ Original alias tracker:@ %a@ \
          Resulting alias tracker:@ %a"
        Simple.print canonical_element
        Simple.print alias_of_demoted_element
        print original_t
        print t
    end;
    match canonical t alias_of_demoted_element with
    | Is_canonical _ ->
        Misc.fatal_errorf "Alias %a must not be must not be canonical \
            anymore.@ \
            Original alias tracker:@ %a@ \
            Resulting alias tracker:@ %a"
          Simple.print alias_of_demoted_element
          print original_t
          print t
    | Alias_of_canonical _ -> ()
  end

let add_alias t element1 element2 =
  match canonical t element1, canonical t element2 with
  | Is_canonical canonical_element1, Is_canonical canonical_element2
  | Alias_of_canonical
        { element = _; canonical_element = canonical_element1; },
      Is_canonical canonical_element2
  | Is_canonical canonical_element1,
      Alias_of_canonical
        { element = _; canonical_element = canonical_element2; }
  | Alias_of_canonical
        { element = _; canonical_element = canonical_element1; },
      Alias_of_canonical
        { element = _; canonical_element = canonical_element2; }
      ->
    if Simple.equal canonical_element1 canonical_element2
    then
      let canonical_element = canonical_element1 in
      (* We don't have to change anything: since [element1] and [element2] have
         the same canonical element, they must already be aliases. But what to
         return? According to the contract for [add],
         [alias_of_demoted_element] must not be canonical and must equal either
         [element1] or [element2]. Thus we must choose whichever of [element1]
         and [element2] is not canonical. (They cannot both be canonical: if
         [element1] is canonical then it's equal to [canonical_element], and
         the same goes for [element2], but they can't both be equal to
         [canonical_element] since we assume in [add] that they're different. *)
      (* CR lmaurer: These elaborate postconditions are there to avoid breaking
         [Typing_env.add_equations]. It would be better to decouple these
         functions. Per discussions with poechsel and vlaviron, the
         information that [Typing_env] is after (besides the updated [Aliases.t])
         is really:

         1. What aliases need to be updated?
         2. What do those aliases point to now?

         Currently #1 is always exactly one element, but it could be zero in
         this case since nothing needs to be updated.

         So the new [add_result] could be something like:
         {[
           type add_result = {
             t : t;
             updated_aliases : Name.t list;
             new_canonical_element : Simple.t;
           }
         ]}

         (It's not actually necessary that [new_canonical_element] is canonical
         as far as [Typing_env] is concerned, but I think this is easier
         to explain than "representative_of_new_alias_class" or some such.) *)
      let alias_of_demoted_element =
        if Simple.equal element1 canonical_element then element2 else element1
      in
      { t; canonical_element; alias_of_demoted_element; }
    else
      let canonical_element, to_be_demoted, alias_of_demoted_element =
        let which_element =
          choose_canonical_element_to_be_demoted t
            ~canonical_element1 ~canonical_element2
        in
        match which_element with
        | Demote_canonical_element1 ->
          canonical_element2, canonical_element1, element1
        | Demote_canonical_element2 ->
          canonical_element1, canonical_element2, element2
      in
      let t =
        add_alias_between_canonical_elements t ~canonical_element
          ~to_be_demoted
      in
      { t;
        canonical_element;
        alias_of_demoted_element;
      }

let add t element1 binding_time_and_mode1
      element2 binding_time_and_mode2 =
  if !Clflags.flambda_invariant_checks then begin
    if Simple.equal element1 element2 then begin
      Misc.fatal_errorf
        "Cannot alias an element to itself: %a" Simple.print element1
    end;
    Simple.pattern_match element1
      ~name:(fun _ -> ())
      ~const:(fun const1 ->
        Simple.pattern_match element2
          ~name:(fun _ -> ())
          ~const:(fun const2 ->
            Misc.fatal_errorf
              "Cannot add alias between two consts: %a, %a"
                Const.print const1
                Const.print const2
          ));
  end;
  let original_t = t in
  let element1 = Simple.without_coercion element1 in
  let element2 = Simple.without_coercion element2 in
  let add_if_name simple data map =
    Simple.pattern_match simple
      ~const:(fun _ -> map)
      ~name:(fun name -> Name.Map.add name data map)
  in
  let t =
    { t with binding_times_and_modes =
               add_if_name element1 binding_time_and_mode1
                 (add_if_name element2 binding_time_and_mode2
                    t.binding_times_and_modes);
    }
  in
  let add_result = add_alias t element1 element2 in
  if !Clflags.flambda_invariant_checks then begin
    invariant_add_result ~original_t add_result
  end;
  add_result

let mem t element =
  Simple.pattern_match element
    ~const:(fun const ->
      Const.Map.mem const t.aliases_of_consts)
    ~name:(fun name ->
      Name.Map.mem name t.binding_times_and_modes)

  (* CR mshinwell: This needs documenting.  For the moment we allow
     relations between canonical elements that are actually incomparable
     under the name mode ordering, and check in [get_canonical_element_exn]
     accordingly.  However maybe we should never allow these situations to
     arise. *)
  (*
  let canonical_mode =
    name_mode t add_result.canonical_element
  in
  let alias_of_mode = name_mode t add_result.alias_of in
  match
    Name_mode.compare_partial_order
      canonical_mode alias_of_mode
  with
  | Some _ -> add_result
  | None ->
    Misc.fatal_errorf "Canonical %a has mode incomparable with %a in:@ %a"
      Simple.print add_result.canonical_element
      Simple.print add_result.alias_of
      print t
  *)

let get_canonical_element_exn t element elt_name_mode ~min_name_mode
      ~min_binding_time =
  let canonical_element, name_mode =
    match canonical t element with
    | Is_canonical _ ->
      element, elt_name_mode
    | Alias_of_canonical { canonical_element; _ } ->
      let name_mode = name_mode t canonical_element ~min_binding_time in
      canonical_element, name_mode
  in
  (*
Format.eprintf "looking for canonical for %a, candidate canonical %a, min order %a\n%!"
  Simple.print element
  Simple.print canonical_element
  Name_mode.print min_name_mode;
*)
  let find_earliest () =
    (* There used to be a shortcut that avoided consulting the aliases in the
       common case that [element] is itself canonical and has no aliases, since
       then it does not appear in [canonical_elements]. However, this shortcut
       was broken: a canonical element *with* known aliases may still not appear
       in [canonical_elements]. See tests/flambda2-aliases for a test that gave
       incorrect output (saying x/39 had no aliases). It may be worth restoring
       the shortcut, perhaps by returning more information from [canonical]. *)
    let aliases = get_aliases_of_canonical_element t ~canonical_element in
    let filter_by_scope name_mode names =
      if Name_mode.equal name_mode Name_mode.in_types then names
      else
        Name.Map.filter (fun name _ ->
            let binding_time_and_mode =
              Name.Map.find name t.binding_times_and_modes
            in
            let scoped_name_mode =
              Binding_time.With_name_mode.scoped_name_mode
                binding_time_and_mode
                ~min_binding_time
            in
            Name_mode.equal name_mode scoped_name_mode)
          names
    in
    match
      Aliases_of_canonical_element.find_earliest_candidates aliases
        ~filter_by_scope ~min_name_mode
    with
    | Some at_earliest_mode ->
      (* Aliases_of_canonical_element.find_earliest_candidates only returns
         non-empty sets *)
      assert (not (Name.Map.is_empty at_earliest_mode));
      let earliest, _ =
        Name.Map.fold (fun elt coercion ((min_elt, _min_coercion) as min_binding) ->
            if name_defined_earlier t elt ~than:min_elt
            then elt, coercion
            else min_binding)
          at_earliest_mode
          (Name.Map.min_binding at_earliest_mode)
      in
      Simple.name earliest
    | None -> raise Not_found
  in
  match
    Name_mode.compare_partial_order name_mode min_name_mode
  with
  | None -> find_earliest ()
  | Some c ->
    if c >= 0 then canonical_element
    else find_earliest ()

let get_aliases t element =
  match canonical t element with
  | Is_canonical canonical_element ->
    let alias_names =
      Aliases_of_canonical_element.all
        (get_aliases_of_canonical_element t ~canonical_element)
    in
    Alias_set.create ~canonical_element ~alias_names
  | Alias_of_canonical { element; canonical_element; } ->
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Simple.equal (Simple.name element) canonical_element))
    end;
    let alias_names =
      Aliases_of_canonical_element.all
        (get_aliases_of_canonical_element t ~canonical_element)
    in
    if !Clflags.flambda_invariant_checks then begin
      assert (Name.Map.mem element alias_names)
    end;
    Alias_set.create ~canonical_element ~alias_names

let all_ids_for_export { canonical_elements = _;
                         aliases_of_canonical_names = _;
                         aliases_of_consts = _;
                         binding_times_and_modes; } =
  Name.Map.fold (fun name _binding_time_and_mode ids ->
      Ids_for_export.add_name ids name)
    binding_times_and_modes
    Ids_for_export.empty

let apply_renaming
      { canonical_elements;
        aliases_of_canonical_names;
        aliases_of_consts;
        binding_times_and_modes; }
      renaming =
  let rename_name = Renaming.apply_name renaming in
  let rename_simple = Renaming.apply_simple renaming in
  let canonical_elements =
    Name.Map.fold (fun elt canonical acc ->
        Name.Map.add (rename_name elt) (rename_simple canonical) acc)
      canonical_elements
      Name.Map.empty
  in
  let aliases_of_canonical_names =
    Name.Map.fold (fun canonical aliases acc ->
        Name.Map.add (rename_name canonical)
        (Aliases_of_canonical_element.rename rename_name aliases)
          acc)
      aliases_of_canonical_names
      Name.Map.empty
  in
  let aliases_of_consts =
    Const.Map.map (Aliases_of_canonical_element.rename rename_name)
      aliases_of_consts
  in
  let binding_times_and_modes =
    Name.Map.fold (fun name binding_time_and_mode acc ->
        Name.Map.add (rename_name name) binding_time_and_mode acc)
      binding_times_and_modes
      Name.Map.empty
  in
  { canonical_elements;
    aliases_of_canonical_names;
    aliases_of_consts;
    binding_times_and_modes;
  }

let merge t1 t2 =
  let canonical_elements =
    Name.Map.disjoint_union
      t1.canonical_elements
      t2.canonical_elements
  in
  (* Warning: we assume that the aliases in the two alias trackers are disjoint,
     but nothing stops them from sharing a canonical element. For instance, if
     multiple compilation units define aliases to the same canonical symbol,
     that symbol will be a canonical element in both of the units' alias
     trackers, and thus their [aliases_of_canonical_names] will have a key in
     common. *)
  let merge_aliases _canonical aliases1 aliases2 =
    Some (Aliases_of_canonical_element.merge aliases1 aliases2)
  in
  let aliases_of_canonical_names =
    Name.Map.union merge_aliases
      t1.aliases_of_canonical_names
      t2.aliases_of_canonical_names
  in
  let aliases_of_consts =
    Const.Map.union merge_aliases
      t1.aliases_of_consts
      t2.aliases_of_consts
  in

  let symbol_data =
    Binding_time.With_name_mode.create
      Binding_time.symbols
      Name_mode.normal
  in
  let binding_times_and_modes =
    Name.Map.union (fun name data1 data2 ->
        Name.pattern_match name
          ~var:(fun var ->
            (* TODO: filter variables on export and restore fatal_error *)
            if Binding_time.(equal (With_name_mode.binding_time data1)
                               imported_variables)
            then Some data2
            else if Binding_time.(equal (With_name_mode.binding_time data2)
                               imported_variables)
            then Some data1
            else
              Misc.fatal_errorf
                "Variable %a is present in multiple environments"
                Variable.print var)
          ~symbol:(fun _sym ->
            assert (Binding_time.With_name_mode.equal data1 symbol_data);
            assert (Binding_time.With_name_mode.equal data2 symbol_data);
            Some data1))
      t1.binding_times_and_modes
      t2.binding_times_and_modes
  in
  { canonical_elements;
    aliases_of_canonical_names;
    aliases_of_consts;
    binding_times_and_modes;
  }

let get_canonical_ignoring_name_mode t name =
  let simple = Simple.name name in
  match canonical t simple with
  | Is_canonical _ -> simple
  | Alias_of_canonical { canonical_element; _ } -> canonical_element

let clean_for_export
      { canonical_elements;
        aliases_of_canonical_names;
        aliases_of_consts;
        binding_times_and_modes; } =
  (* CR vlaviron: This function is kept as a reminder that we'd like
     to remove unreachable entries at some point. *)
  { canonical_elements;
    aliases_of_canonical_names;
    aliases_of_consts;
    binding_times_and_modes;
  }
