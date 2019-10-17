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

  module Order_within_equiv_class : sig
    type t
    include Identifiable.S with type t := t

    val compare_partial_order : t -> t -> int option

    val compare : t -> t -> [ `Be_explicit_about_total_or_partial_ordering ]
  end

  val order_within_equiv_class : t -> Order_within_equiv_class.t
end) = struct
  let _ = E.Order_within_equiv_class.compare

  module Aliases_of_canonical_element : sig
    type t

    val print : Format.formatter -> t -> unit

    val invariant : t -> unit

    val empty : t
    val is_empty : t -> bool

    val add : t -> E.t -> t

    val find_earliest
       : t
      -> min_order_within_equiv_class:E.Order_within_equiv_class.t
      -> E.t option

    val all : t -> E.Set.t

    val mem : t -> E.t -> bool

    val union : t -> t -> t
    val inter : t -> t -> t
  end = struct
    type t = {
      aliases : E.Set.t E.Order_within_equiv_class.Map.t;
      all : E.Set.t;
    }

    let invariant _t = ()

    let print ppf { aliases; all = _; } =
      E.Order_within_equiv_class.Map.print E.Set.print ppf aliases

    let empty = {
      aliases = E.Order_within_equiv_class.Map.empty;
      all = E.Set.empty;
    }

    let is_empty t = E.Set.is_empty t.all

    let add t elt =
      if E.Set.mem elt t.all then begin
        Misc.fatal_errorf "%a already added to [Aliases_of_canonical_element]: \
            %a"
          E.print elt
          print t
      end;
      let aliases =
        E.Order_within_equiv_class.Map.update (E.order_within_equiv_class elt)
          (function
            | None -> Some (E.Set.singleton elt)
            | Some elts ->
              assert (not (E.Set.mem elt elts));
              Some (E.Set.add elt elts))
          t.aliases
      in
      let all = E.Set.add elt t.all in
      { aliases;
        all;
      }

    let find_earliest t ~min_order_within_equiv_class =
      match
        E.Order_within_equiv_class.Map.find_first (fun order ->
            match
              E.Order_within_equiv_class.compare_partial_order
                order min_order_within_equiv_class
            with
            | None -> false
            | Some result -> result >= 0)
          t.aliases
      with
      | exception Not_found -> None
      | (_order, elts) -> E.Set.min_elt_opt elts

    let mem t elt =
      E.Set.mem elt t.all

    let all t = t.all

    let union t1 t2 =
      let aliases =
        E.Order_within_equiv_class.Map.merge (fun _order elts1 elts2 ->
            match elts1, elts2 with
            | None, None -> None
            | Some elts, None | None, Some elts -> Some elts
            | Some elts1, Some elts2 -> Some (E.Set.union elts1 elts2))
          t1.aliases t2.aliases
      in
      let t =
        { aliases;
          all = E.Set.union t1.all t2.all;
        }
      in
      invariant t;
      t

    let inter t1 t2 =
      let aliases =
        E.Order_within_equiv_class.Map.merge (fun _order elts1 elts2 ->
            match elts1, elts2 with
            | None, None | Some _, None | None, Some _ -> None
            | Some elts1, Some elts2 -> Some (E.Set.inter elts1 elts2))
          t1.aliases t2.aliases
      in
      let t =
        { aliases;
          all = E.Set.inter t1.all t2.all;
        }
      in
      invariant t;
      t
  end

  type t = {
    canonical_elements : E.t E.Map.t;
    aliases_of_canonical_elements : Aliases_of_canonical_element.t E.Map.t;
    (* For [elt |-> aliases] in [aliases_of_canonical_elements], then
       [aliases] never includes [elt]. *)
    (* CR mshinwell: check this always holds *)
  }

  let print ppf { canonical_elements; aliases_of_canonical_elements; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(canonical_elements@ %a)@]@ \
        @[<hov 1>(aliases_of_canonical_elements@ %a)@]\
        )@]"
      (E.Map.print E.print) canonical_elements
      (E.Map.print Aliases_of_canonical_element.print)
      aliases_of_canonical_elements

  let invariant t =
    if !Clflags.flambda_invariant_checks then begin
      let canonical_elements1 =
        E.Set.of_list (E.Map.data t.canonical_elements)
      in
      let canonical_elements2 = E.Map.keys t.aliases_of_canonical_elements in
      if not (E.Set.subset canonical_elements1 canonical_elements2) then begin
        Misc.fatal_errorf "Range of [canonical_elements] is not a subset of \
            the domain of [aliases_of_canonical_elements]:@ %a"
          print t
      end;
      let _all_aliases : E.Set.t =
        E.Map.fold (fun canonical_element aliases all_aliases ->
            Aliases_of_canonical_element.invariant aliases;
            let aliases = Aliases_of_canonical_element.all aliases in
            if not (E.Set.for_all (fun elt ->
              E.defined_earlier canonical_element ~than:elt) aliases)
            then begin
              Misc.fatal_errorf "Canonical element %a is not earlier than \
                  all of its aliases:@ %a"
                E.print canonical_element
                print t
            end;
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
      assert (not (Aliases_of_canonical_element.mem
        aliases_of_canonical_element to_be_demoted));
      let aliases_of_to_be_demoted =
        get_aliases_of_canonical_element t ~canonical_element:to_be_demoted
      in
      assert (not (Aliases_of_canonical_element.mem
        aliases_of_to_be_demoted canonical_element));
      assert (Aliases_of_canonical_element.is_empty (
        Aliases_of_canonical_element.inter
          aliases_of_canonical_element aliases_of_to_be_demoted));
      let aliases =
        Aliases_of_canonical_element.add
          (Aliases_of_canonical_element.union aliases_of_to_be_demoted
            aliases_of_canonical_element)
          to_be_demoted
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
          (* CR mshinwell: [alias_of] is not a good name. *)
          alias_of = to_be_demoted;
        }
      in
      let t =
        add_alias_between_canonical_elements t ~canonical_element
          ~to_be_demoted
      in
      Some add_result, t
    | Alias_of_canonical
          { element = element1; canonical_element = canonical_element1; },
        Is_canonical canonical_element2
    | Is_canonical canonical_element2,
        Alias_of_canonical
          { element = element1; canonical_element = canonical_element1; } ->
      let { canonical_element; to_be_demoted; } =
        choose_canonical_element_to_be_demoted ~canonical_element1
          ~canonical_element2
      in
      let alias_of =
        if E.equal to_be_demoted canonical_element1 then element1
        else canonical_element2
      in
      let add_result =
        { canonical_element;
          alias_of;
        }
      in
      let t =
        add_alias_between_canonical_elements t ~canonical_element
          ~to_be_demoted
      in
      Some add_result, t
    | Alias_of_canonical
          { element = element1; canonical_element = canonical_element1; },
        Alias_of_canonical
          { element = element2; canonical_element = canonical_element2; }
        ->
      let { canonical_element; to_be_demoted; } =
        choose_canonical_element_to_be_demoted ~canonical_element1
          ~canonical_element2
      in
      let alias_of =
        if E.equal to_be_demoted canonical_element1 then element1
        else element2
      in
      let add_result =
        { canonical_element;
          alias_of;
        }
      in
      let t =
        add_alias_between_canonical_elements t ~canonical_element
          ~to_be_demoted
      in
      Some add_result, t

  let add_canonical_element t element =
    if E.Map.mem element t.canonical_elements then begin
      if E.implicitly_bound_and_canonical element then
        t
      else
        Misc.fatal_errorf "Element %a already in alias tracker:@ %a"
          E.print element
          print t
    end else begin
      let canonical_elements = E.Map.add element element t.canonical_elements in
      let aliases_of_canonical_elements =
        E.Map.add element Aliases_of_canonical_element.empty
          t.aliases_of_canonical_elements
      in
      { canonical_elements;
        aliases_of_canonical_elements;
      }
    end

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

  let get_canonical_element t element ~min_order_within_equiv_class =
    let t = add_implicitly_bound_canonical_element t element in
    match E.Map.find element t.canonical_elements with
    | exception Not_found ->
      Misc.fatal_errorf "Element %a not in [canonical_elements]:@ %a"
        E.print element
        print t
    | canonical_element ->
(*
Format.eprintf "looking for canonical for %a, candidate canonical %a, min order %a\n%!"
  E.print element
  E.print canonical_element
  E.Order_within_equiv_class.print min_order_within_equiv_class;
*)
      let find_earliest () =
        let aliases = get_aliases_of_canonical_element t ~canonical_element in
        Aliases_of_canonical_element.find_earliest aliases
          ~min_order_within_equiv_class
      in
      match
        E.Order_within_equiv_class.compare_partial_order
          (E.order_within_equiv_class canonical_element)
          min_order_within_equiv_class
      with
      | None -> find_earliest ()
      | Some c ->
        if c >= 0 then Some canonical_element
        else find_earliest ()

  let get_aliases t element =
    let t = add_implicitly_bound_canonical_element t element in
    match canonical t element with
    | Is_canonical canonical_element ->
      let aliases =
        Aliases_of_canonical_element.all
          (get_aliases_of_canonical_element t ~canonical_element)
      in
      E.Set.add element aliases
    | Alias_of_canonical { element = _; canonical_element; } ->
      assert (not (E.equal element canonical_element));
      let aliases =
        Aliases_of_canonical_element.all
          (get_aliases_of_canonical_element t ~canonical_element)
      in
      assert (E.Set.mem element aliases);
      E.Set.add canonical_element aliases
end
