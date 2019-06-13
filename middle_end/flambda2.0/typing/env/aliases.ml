(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  canonical_names : Name.t Name.Map.t;
  aliases_of_canonical_names : Name.Set.t Name.Map.t;
}

let print ppf { canonical_names; aliases_of_canonical_names; } =
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(canonical_names@ %a)@]@ \
      @[<hov 1>(aliases_of_canonical_names@ %a)@]\
      )@]"
    (Name.Map.print Name.print) canonical_names
    (Name.Map.print Name.Set.print) aliases_of_canonical_names

let invariant t =
  if !Clflags.flambda_invariant_checks then begin
    let canonical_names1 = Name.Map.keys t.canonical_names in
    let canonical_names2 = Name.Map.keys t.aliases_of_canonical_names in
    if not (Name.Set.equal canonical_names1 canonical_names2) then begin
      Misc.fatal_errorf "Keys of [canonical_names] and \
          [aliases_of_canonical_names] differ:@ %a"
        print t
    end;
    let _all_aliases : Name.Set.t =
      Name.Map.fold (fun canonical_name aliases all_aliases ->
          if Name.Set.mem canonical_name aliases then begin
            Misc.fatal_errorf "Canonical name %a occurs in alias set:@ %a"
              Name.print canonical_name
              Name.Set.print aliases
          end;
          if not (Name.Set.is_empty (Name.Set.inter aliases all_aliases)) then
          begin
            Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
          end;
          Name.Set.union aliases all_aliases)
        t.aliases_of_canonical_names
        Name.Set.empty
    in
    ()
  end

let empty = {
  canonical_names = Name.Map.empty;
  aliases_of_canonical_names = Name.Map.empty;
}

let canonical_names t = Name.Map.keys t.canonical_names

type canonical =
  | Is_canonical of Name.t
  | Alias_of_canonical of { name : Name.t; canonical_name : Name.t; }

let canonical t name : canonical =
  match Name.Map.find name t.canonical_names with
  | exception Not_found ->
    Misc.fatal_errorf "Name %a hasn't been seen by alias tracker (ensure \
        that [add_canonical_name] was called)"
      Name.print name
  | canonical_name ->
    if Name.equal name canonical_name then Is_canonical name
    else Alias_of_canonical { name; canonical_name; }

let get_aliases_of_canonical_name t ~canonical_name =
  match Name.Map.find canonical_name t.aliases_of_canonical_names with
  | exception Not_found ->
    Misc.fatal_errorf "No alias map entry for canonical name %a"
      Name.print canonical_name
  | aliases -> aliases

let add_alias_between_canonical_names t ~canonical_name ~to_be_demoted =
  assert (Name.Map.mem canonical_name t.aliases_of_canonical_names);
  assert (Name.Map.mem to_be_demoted t.aliases_of_canonical_names);
  if Name.equal canonical_name to_be_demoted then
    t
  else
    let canonical_names =
      t.canonical_names
      |> Name.Map.map (fun name ->
           if Name.equal name to_be_demoted then canonical_name
           else name)
      |> Name.Map.add to_be_demoted canonical_name
    in
    let aliases_of_canonical_name =
      get_aliases_of_canonical_name t ~canonical_name
    in
    assert (not (Name.Set.mem to_be_demoted aliases_of_canonical_name));
    let aliases_of_to_be_demoted =
      get_aliases_of_canonical_name t ~canonical_name:to_be_demoted
    in
    assert (not (Name.Set.mem canonical_name aliases_of_to_be_demoted));
    assert (Name.Set.is_empty (Name.Set.inter
      aliases_of_canonical_name aliases_of_to_be_demoted));
    let aliases =
      aliases_of_canonical_name
      |> Name.Set.union aliases_of_to_be_demoted
      |> Name.Set.add to_be_demoted
    in
    let aliases_of_canonical_names =
      t.aliases_of_canonical_names
      |> Name.Map.remove to_be_demoted
      |> Name.Map.add (* replace *) canonical_name aliases
    in
    { canonical_names;
      aliases_of_canonical_names;
    }

type to_be_demoted = {
  canonical_name : Name.t;
  to_be_demoted : Name.t;
}

let choose_canonical_name_to_be_demoted ~canonical_name1 ~canonical_name2
      ~defined_earlier =
  if defined_earlier (Simple.name canonical_name1)
       ~than:(Simple.name canonical_name2)
  then
    { canonical_name = canonical_name1;
      to_be_demoted = canonical_name2;
    }
  else
    { canonical_name = canonical_name2;
      to_be_demoted = canonical_name1;
    }

(* CR mshinwell: add submodule *)
type add_result = {
  canonical_name : Name.t;
  alias_of : Name.t;
}

let invariant_add_result t ~original_t { canonical_name; alias_of; }
      ~defined_earlier =
  if not (Name.equal canonical_name alias_of) then begin
    let canonical_name' = Simple.name canonical_name in
    let alias_of' = Simple.name alias_of in
    if not (defined_earlier canonical_name' ~than:alias_of') then begin
      Misc.fatal_errorf "Canonical name %a should be defined earlier than \
          %a after alias addition.@ Original alias tracker:@ %a@ \
          Resulting alias tracker:@ %a"
        Name.print canonical_name
        Name.print alias_of
        print original_t
        print t
    end
  end

let add_alias t (simple1 : Simple.t) (simple2 : Simple.t) ~defined_earlier =
  match Simple.descr simple1, Simple.descr simple2 with
  | Name name1, Name name2 ->
    begin match canonical t name1, canonical t name2 with
    | Is_canonical canonical_name1, Is_canonical canonical_name2 ->
      let { canonical_name; to_be_demoted; } =
        choose_canonical_name_to_be_demoted ~canonical_name1 ~canonical_name2
          ~defined_earlier
      in
      let add_result =
        { canonical_name;
          alias_of = to_be_demoted;
        }
      in
      let t =
        add_alias_between_canonical_names t ~canonical_name ~to_be_demoted
      in
      Some add_result, t
    | Alias_of_canonical { name = _; canonical_name = canonical_name1; },
        Is_canonical canonical_name2
    | Is_canonical canonical_name1,
        Alias_of_canonical { name = _; canonical_name = canonical_name2; } ->
      let { canonical_name; to_be_demoted; } =
        choose_canonical_name_to_be_demoted ~canonical_name1 ~canonical_name2
          ~defined_earlier
      in
      let add_result =
        { canonical_name;
          alias_of = to_be_demoted;
        }
      in
      let t =
        add_alias_between_canonical_names t ~canonical_name ~to_be_demoted
      in
      Some add_result, t
    | Alias_of_canonical { name = _; canonical_name = canonical_name1; },
        Alias_of_canonical { name = _; canonical_name = canonical_name2; }
        ->
      let { canonical_name; to_be_demoted; } =
        choose_canonical_name_to_be_demoted ~canonical_name1 ~canonical_name2
          ~defined_earlier
      in
      let add_result =
        { canonical_name;
          alias_of = to_be_demoted;
        }
      in
      let t =
        add_alias_between_canonical_names t ~canonical_name ~to_be_demoted
      in
      Some add_result, t
    end
  (* CR mshinwell: Think about recording of aliases when one side is a Simple;
     may not be needed for first version. *)
  | _, _ -> None, t

let add t simple1 simple2 ~defined_earlier =
  let original_t = t in
  let add_result, t = add_alias t simple1 simple2 ~defined_earlier in
  invariant t;
  Option.iter (fun add_result ->
      invariant_add_result t ~original_t add_result ~defined_earlier)
    add_result;
  add_result, t

let add_canonical_name t name =
  if Name.Map.mem name t.canonical_names then begin
    Misc.fatal_errorf "Name %a already in alias tracker:@ %a"
      Name.print name
      print t
  end;
  let canonical_names = Name.Map.add name name t.canonical_names in
  let aliases_of_canonical_names =
    Name.Map.add name Name.Set.empty t.aliases_of_canonical_names
  in
  { canonical_names;
    aliases_of_canonical_names;
  }

let get_canonical_name t name =
  match Name.Map.find name t.canonical_names with
  | exception Not_found -> None
  | canonical_name -> Some canonical_name

let aliases_of_simple t (simple : Simple.t) =
  match Simple.descr simple with
  | Const _ | Discriminant _ -> Name.Set.empty
  | Name name ->
    match canonical t name with
    | Is_canonical canonical_name ->
      get_aliases_of_canonical_name t ~canonical_name
    | Alias_of_canonical { name = _; canonical_name; } ->
      assert (not (Name.equal name canonical_name));
      let aliases = get_aliases_of_canonical_name t ~canonical_name in
      assert (Name.Set.mem name aliases);
      Name.Set.add canonical_name aliases
