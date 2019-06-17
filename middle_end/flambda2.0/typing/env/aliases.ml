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

module Make (E : sig
  type t
  include Identifiable.S with type t := t

  val defined_earlier : t -> than:t -> bool
  val implicitly_bound_and_canonical : t -> bool
end) = struct
  type t = {
    canonical_elements : E.t E.Map.t;
    aliases_of_canonical_elements : E.Set.t E.Map.t;
  }

  let print ppf { canonical_elements; aliases_of_canonical_elements; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(canonical_elements@ %a)@]@ \
        @[<hov 1>(aliases_of_canonical_elements@ %a)@]\
        )@]"
      (E.Map.print E.print) canonical_elements
      (E.Map.print E.Set.print) aliases_of_canonical_elements

  let invariant t =
    if !Clflags.flambda_invariant_checks then begin
      let canonical_elements1 = E.Map.keys t.canonical_elements in
      let canonical_elements2 = E.Map.keys t.aliases_of_canonical_elements in
      if not (E.Set.equal canonical_elements1 canonical_elements2) then begin
        Misc.fatal_errorf "Keys of [canonical_elements] and \
            [aliases_of_canonical_elements] differ:@ %a"
          print t
      end;
      let _all_aliases : E.Set.t =
        E.Map.fold (fun canonical_element aliases all_aliases ->
            if E.Set.mem canonical_element aliases then begin
              Misc.fatal_errorf "Canonical element %a occurs in alias set:@ %a"
                E.print canonical_element
                E.Set.print aliases
            end;
            if not (E.Set.is_empty (E.Set.inter aliases all_aliases)) then
            begin
              Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
            end;
            E.Set.union aliases all_aliases)
          t.aliases_of_canonical_elements
          E.Set.empty
      in
      ()
    end

  let empty = {
    canonical_elements = E.Map.empty;
    aliases_of_canonical_elements = E.Map.empty;
  }

  let canonical_elements t = E.Map.keys t.canonical_elements

  type canonical =
    | Is_canonical of E.t
    | Alias_of_canonical of { element : E.t; canonical_element : E.t; }

  let canonical t element : canonical =
    match E.Map.find element t.canonical_elements with
    | exception Not_found ->
      Misc.fatal_errorf "Element %a hasn't been seen by alias tracker (ensure \
          that [add_canonical_element] was called)"
        E.print element
    | canonical_element ->
      if E.equal element canonical_element then Is_canonical element
      else Alias_of_canonical { element; canonical_element; }

  let get_aliases_of_canonical_element t ~canonical_element =
    match E.Map.find canonical_element t.aliases_of_canonical_elements with
    | exception Not_found ->
      Misc.fatal_errorf "No alias map entry for canonical element %a"
        E.print canonical_element
    | aliases -> aliases

  let add_alias_between_canonical_elements t ~canonical_element ~to_be_demoted =
    assert (E.Map.mem canonical_element t.aliases_of_canonical_elements);
    assert (E.Map.mem to_be_demoted t.aliases_of_canonical_elements);
    if E.equal canonical_element to_be_demoted then
      t
    else
      let canonical_elements =
        t.canonical_elements
        |> E.Map.map (fun element ->
             if E.equal element to_be_demoted then canonical_element
             else element)
        |> E.Map.add to_be_demoted canonical_element
      in
      let aliases_of_canonical_element =
        get_aliases_of_canonical_element t ~canonical_element
      in
      assert (not (E.Set.mem to_be_demoted aliases_of_canonical_element));
      let aliases_of_to_be_demoted =
        get_aliases_of_canonical_element t ~canonical_element:to_be_demoted
      in
      assert (not (E.Set.mem canonical_element aliases_of_to_be_demoted));
      assert (E.Set.is_empty (E.Set.inter
        aliases_of_canonical_element aliases_of_to_be_demoted));
      let aliases =
        aliases_of_canonical_element
        |> E.Set.union aliases_of_to_be_demoted
        |> E.Set.add to_be_demoted
      in
      let aliases_of_canonical_elements =
        t.aliases_of_canonical_elements
        |> E.Map.remove to_be_demoted
        |> E.Map.add (* replace *) canonical_element aliases
      in
      { canonical_elements;
        aliases_of_canonical_elements;
      }

  type to_be_demoted = {
    canonical_element : E.t;
    to_be_demoted : E.t;
  }

  let choose_canonical_element_to_be_demoted ~canonical_element1
        ~canonical_element2 =
    if E.defined_earlier canonical_element1 ~than:canonical_element2
    then
      { canonical_element = canonical_element1;
        to_be_demoted = canonical_element2;
      }
    else
      { canonical_element = canonical_element2;
        to_be_demoted = canonical_element1;
      }

  (* CR mshinwell: add submodule *)
  type add_result = {
    canonical_element : E.t;
    alias_of : E.t;
  }

  let invariant_add_result t ~original_t { canonical_element; alias_of; } =
    if not (E.equal canonical_element alias_of) then begin
      if not (E.defined_earlier canonical_element ~than:alias_of) then begin
        Misc.fatal_errorf "Canonical element %a should be defined earlier than \
            %a after alias addition.@ Original alias tracker:@ %a@ \
            Resulting alias tracker:@ %a"
          E.print canonical_element
          E.print alias_of
          print original_t
          print t
      end
    end

  let add_alias t element1 element2 =
    match canonical t element1, canonical t element2 with
    | Is_canonical canonical_element1, Is_canonical canonical_element2 ->
      let { canonical_element; to_be_demoted; } =
        choose_canonical_element_to_be_demoted ~canonical_element1
          ~canonical_element2
      in
      let add_result =
        { canonical_element;
          alias_of = to_be_demoted;
        }
      in
      let t =
        add_alias_between_canonical_elements t ~canonical_element
          ~to_be_demoted
      in
      Some add_result, t
    | Alias_of_canonical
          { element = _; canonical_element = canonical_element1; },
        Is_canonical canonical_element2
    | Is_canonical canonical_element1,
        Alias_of_canonical
          { element = _; canonical_element = canonical_element2; } ->
      let { canonical_element; to_be_demoted; } =
        choose_canonical_element_to_be_demoted ~canonical_element1
          ~canonical_element2
      in
      let add_result =
        { canonical_element;
          alias_of = to_be_demoted;
        }
      in
      let t =
        add_alias_between_canonical_elements t ~canonical_element
          ~to_be_demoted
      in
      Some add_result, t
    | Alias_of_canonical
          { element = _; canonical_element = canonical_element1; },
        Alias_of_canonical
          { element = _; canonical_element = canonical_element2; }
        ->
      let { canonical_element; to_be_demoted; } =
        choose_canonical_element_to_be_demoted ~canonical_element1
          ~canonical_element2
      in
      let add_result =
        { canonical_element;
          alias_of = to_be_demoted;
        }
      in
      let t =
        add_alias_between_canonical_elements t ~canonical_element
          ~to_be_demoted
      in
      Some add_result, t

  let add_canonical_element t element =
    if E.Map.mem element t.canonical_elements then begin
      Misc.fatal_errorf "Element %a already in alias tracker:@ %a"
        E.print element
        print t
    end;
    let canonical_elements = E.Map.add element element t.canonical_elements in
    let aliases_of_canonical_elements =
      E.Map.add element E.Set.empty t.aliases_of_canonical_elements
    in
    { canonical_elements;
      aliases_of_canonical_elements;
    }

  let add_implicitly_bound_canonical_element t element =
    if E.implicitly_bound_and_canonical element then
      add_canonical_element t element
    else
      t

  let add t element1 element2 =
    let original_t = t in
    let t = add_implicitly_bound_canonical_element t element1 in
    let t = add_implicitly_bound_canonical_element t element2 in
    let add_result, t = add_alias t element1 element2 in
    invariant t;
    Option.iter (fun add_result ->
        invariant_add_result t ~original_t add_result)
      add_result;
    add_result, t

  let get_canonical_element t element =
    match E.Map.find element t.canonical_elements with
    | exception Not_found -> None
    | canonical_element -> Some canonical_element

  let get_aliases t element =
    match canonical t element with
    | Is_canonical canonical_element ->
      get_aliases_of_canonical_element t ~canonical_element
    | Alias_of_canonical { element = _; canonical_element; } ->
      assert (not (E.equal element canonical_element));
      let aliases = get_aliases_of_canonical_element t ~canonical_element in
      assert (E.Set.mem element aliases);
      E.Set.add canonical_element aliases
end
