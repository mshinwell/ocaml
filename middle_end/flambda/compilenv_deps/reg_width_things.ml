(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare

module Id = Table_by_int_id.Id

let var_flags = 0
let symbol_flags = 1
let const_flags = 2
let simple_flags = 3

let which_thing_mask = 3

let current_compilation_unit_flag = 4

let renamed_flag = 8

let is_renamed id =
  id land renamed_flag = renamed_flag

let has_var_flag flags =
  flags land which_thing_mask = var_flags

let has_symbol_flag flags =
  flags land which_thing_mask = symbol_flags

let has_const_flag flags =
  flags land which_thing_mask = const_flags

let has_simple_flag flags =
  flags land which_thing_mask = simple_flags

let has_current_compilation_unit_flags flags =
  flags land current_compilation_unit_flag = current_compilation_unit_flag

let mask_for_find id =
  id land (lnot (current_compilation_unit_flag lor renamed_flag))

module Const_data = struct
  type t =
    | Naked_immediate of Target_imm.t
    | Tagged_immediate of Target_imm.t
    | Naked_float of Numbers.Float_by_bit_pattern.t
    | Naked_int32 of Int32.t
    | Naked_int64 of Int64.t
    | Naked_nativeint of Targetint.t

  let flags = const_flags

  include Identifiable.Make (struct
    type nonrec t = t

    let print ppf (t : t) =
      match t with
      | Naked_immediate i ->
        Format.fprintf ppf "@<0>%s#%a@<0>%s"
          (Flambda_colours.naked_number ())
          Target_imm.print i
          (Flambda_colours.normal ())
      | Tagged_immediate i ->
        Format.fprintf ppf "@<0>%s%a@<0>%s"
          (Flambda_colours.tagged_immediate ())
          Target_imm.print i
          (Flambda_colours.normal ())
      | Naked_float f ->
        Format.fprintf ppf "@<0>%s#%a@<0>%s"
          (Flambda_colours.naked_number ())
          Numbers.Float_by_bit_pattern.print f
          (Flambda_colours.normal ())
      | Naked_int32 n ->
        Format.fprintf ppf "@<0>%s#%ldl@<0>%s"
          (Flambda_colours.naked_number ())
          n
          (Flambda_colours.normal ())
      | Naked_int64 n ->
        Format.fprintf ppf "@<0>%s#%LdL@<0>%s"
          (Flambda_colours.naked_number ())
          n
          (Flambda_colours.normal ())
      | Naked_nativeint n ->
        Format.fprintf ppf "@<0>%s#%an@<0>%s"
          (Flambda_colours.naked_number ())
          Targetint.print n
          (Flambda_colours.normal ())

    let output _ _ = Misc.fatal_error "[output] not yet implemented"

    let compare t1 t2 =
      match t1, t2 with
      | Naked_immediate i1, Naked_immediate i2 ->
        Target_imm.compare i1 i2
      | Tagged_immediate i1, Tagged_immediate i2 ->
        Target_imm.compare i1 i2
      | Naked_float f1, Naked_float f2 ->
        Numbers.Float_by_bit_pattern.compare f1 f2
      | Naked_int32 n1, Naked_int32 n2 ->
        Int32.compare n1 n2
      | Naked_int64 n1, Naked_int64 n2 ->
        Int64.compare n1 n2
      | Naked_nativeint n1, Naked_nativeint n2 ->
        Targetint.compare n1 n2
      | Naked_immediate _, _ -> -1
      | _, Naked_immediate _ -> 1
      | Tagged_immediate _, _ -> -1
      | _, Tagged_immediate _ -> 1
      | Naked_float _, _ -> -1
      | _, Naked_float _ -> 1
      | Naked_int32 _, _ -> -1
      | _, Naked_int32 _ -> 1
      | Naked_int64 _, _ -> -1
      | _, Naked_int64 _ -> 1

    let equal t1 t2 =
      if t1 == t2 then true
      else
        match t1, t2 with
        | Naked_immediate i1, Naked_immediate i2 ->
          Target_imm.equal i1 i2
        | Tagged_immediate i1, Tagged_immediate i2 ->
          Target_imm.equal i1 i2
        | Naked_float f1, Naked_float f2 ->
          Numbers.Float_by_bit_pattern.equal f1 f2
        | Naked_int32 n1, Naked_int32 n2 ->
          Int32.equal n1 n2
        | Naked_int64 n1, Naked_int64 n2 ->
          Int64.equal n1 n2
        | Naked_nativeint n1, Naked_nativeint n2 ->
          Targetint.equal n1 n2
        | (Naked_immediate _ | Tagged_immediate _ | Naked_float _
            | Naked_int32 _ | Naked_int64 _ | Naked_nativeint _), _ -> false

    let hash t =
      match t with
      | Naked_immediate n -> Target_imm.hash n
      | Tagged_immediate n -> Target_imm.hash n
      | Naked_float n -> Numbers.Float_by_bit_pattern.hash n
      | Naked_int32 n -> Hashtbl.hash n
      | Naked_int64 n -> Hashtbl.hash n
      | Naked_nativeint n -> Targetint.hash n
  end)
end

module Variable_data = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    previous_compilation_units : Compilation_unit.t list;
    name : string;
    name_stamp : int;
    user_visible : bool;
  }

  let flags = var_flags

  let print ppf { compilation_unit; previous_compilation_units = _;
                  name; name_stamp; user_visible; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(name@ %s)@]@ \
        @[<hov 1>(name_stamp@ %d)@]@ \
        @[<hov 1>(user_visible@ %b)@]\
        )@]"
      Compilation_unit.print compilation_unit
      name
      name_stamp
      user_visible

  let hash { compilation_unit; previous_compilation_units;
             name = _; name_stamp; user_visible = _; } =
    let compilation_unit_hashes =
      List.fold_left (fun hash compilation_unit ->
          Misc.hash2 hash (Compilation_unit.hash compilation_unit))
        (Compilation_unit.hash compilation_unit)
        previous_compilation_units
    in
    Misc.hash2 compilation_unit_hashes (Hashtbl.hash name_stamp)

  let equal t1 t2 =
    if t1 == t2 then true
    else
      let { compilation_unit = compilation_unit1;
            previous_compilation_units = previous_compilation_units1;
            name = _; name_stamp = name_stamp1; user_visible = _;
          } = t1
      in
      let { compilation_unit = compilation_unit2;
            previous_compilation_units = previous_compilation_units2;
            name = _; name_stamp = name_stamp2; user_visible = _;
          } = t2
      in
      let rec previous_compilation_units_match l1 l2 =
        match l1, l2 with
        | [], [] -> true
        | [], _ :: _ | _ :: _, [] -> false
        | unit1 :: tl1, unit2 :: tl2 ->
          Compilation_unit.equal unit1 unit2
          && previous_compilation_units_match tl1 tl2
      in
      Int.equal name_stamp1 name_stamp2
        && Compilation_unit.equal compilation_unit1 compilation_unit2
        && previous_compilation_units_match
             previous_compilation_units1
             previous_compilation_units2
end

module Symbol_data = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    linkage_name : Linkage_name.t;
  }

  let flags = symbol_flags

  let print ppf { compilation_unit; linkage_name; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(linkage_name@ %a)@]\
        )@]"
      Compilation_unit.print compilation_unit
      Linkage_name.print linkage_name

  let hash { compilation_unit = _; linkage_name; } =
    (* Linkage names are unique across a whole project, so there's no need
       to hash the compilation unit. *)
    Linkage_name.hash linkage_name

  let equal t1 t2 =
    if t1 == t2 then true
    else
      let { compilation_unit = _; linkage_name = linkage_name1; } = t1 in
      let { compilation_unit = _; linkage_name = linkage_name2; } = t2 in
      (* Linkage names are unique across a whole project, so there's no need
         to check the compilation units. *)
      Linkage_name.equal linkage_name1 linkage_name2
end

module Simple_data = struct
  type t = {
    simple : Id.t;  (* always without [Coercion] *)
    coercion : Coercion.t;
  }

  let flags = simple_flags

  let print ppf { simple = _; coercion; } =
    Format.fprintf ppf "@[<hov 1>\
        @[<hov 1>(coercion@ %a)@]\
        @]"
      Coercion.print coercion

  let hash { simple; coercion; } =
    Hashtbl.hash (Id.hash simple, Coercion.hash coercion)

  let equal t1 t2 =
    if t1 == t2 then true
    else
      let { simple = simple1; coercion = coercion1; } = t1 in
      let { simple = simple2; coercion = coercion2; } = t2 in
      Id.equal simple1 simple2
        && Coercion.equal coercion1 coercion2
end

module Const = struct
  type t = Id.t
  type exported = Const_data.t

  module Table = Table_by_int_id.Make (Const_data)
  let grand_table_of_constants = ref (Table.create ())

  let initialise () =
    grand_table_of_constants := Table.create ()

  let find_data t = Table.find !grand_table_of_constants t

  module Descr = Const_data

  let create (data : Const_data.t) =
    Table.add !grand_table_of_constants data

  let naked_immediate imm = create (Naked_immediate imm)
  let tagged_immediate imm = create (Tagged_immediate imm)
  let naked_float f = create (Naked_float f)
  let naked_int32 i = create (Naked_int32 i)
  let naked_int64 i = create (Naked_int64 i)
  let naked_nativeint i = create (Naked_nativeint i)

  let const_true = tagged_immediate Target_imm.bool_true
  let const_false = tagged_immediate Target_imm.bool_false

  let untagged_const_true = naked_immediate Target_imm.bool_true
  let untagged_const_false = naked_immediate Target_imm.bool_false

  let untagged_const_zero = naked_immediate Target_imm.zero

  let untagged_const_int i = naked_immediate (Target_imm.int i)

  let const_int i = tagged_immediate (Target_imm.int i)

  let const_zero = tagged_immediate Target_imm.zero
  let const_one = tagged_immediate Target_imm.one
  let const_unit = const_zero

  let descr t = find_data t

  module T0 = struct
    let compare = Id.compare
    let equal = Id.equal
    let hash = Id.hash

    let print ppf t = Const_data.print ppf (descr t)

    let output chan t = print (Format.formatter_of_out_channel chan) t
  end

  include T0
  module T = struct
    type nonrec t = t
    include T0
  end

  module Set = Patricia_tree.Make_set (struct let print = print end)
  module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
  (* CR mshinwell: The [Tbl]s will still print integers! *)
  module Tbl = Identifiable.Make_tbl (Numbers.Int) (Map)

  let export t = find_data t

  let import (data : exported) =
    create data

  let map_compilation_unit _f data =
    (* No compilation unit in the data *)
    data
end

module Variable = struct
  type t = Id.t
  type exported = Variable_data.t

  module Table = Table_by_int_id.Make (Variable_data)
  let grand_table_of_variables = ref (Table.create ())

  let initialise () =
    grand_table_of_variables := Table.create ()

  let find_data t =
    Table.find !grand_table_of_variables (mask_for_find t)

  let is_renamed = is_renamed

  let mark_renamed t =
    if is_renamed t then
      Misc.fatal_error "Variable is already renamed"
    else
      t lor (renamed_flag lor current_compilation_unit_flag)

  let compilation_unit t =
    if has_current_compilation_unit_flags (Id.flags t) then
      Compilation_unit.get_current_exn ()
    else begin
      assert (not (is_renamed t));
      (find_data t).compilation_unit
    end

  let name t = (find_data t).name
  let user_visible t = (find_data t).user_visible

  let previous_name_stamp = ref (-1)

  let create_name_stamp () =
    (* CR mshinwell: check for overflow on 32 bit *)
    incr previous_name_stamp;
    !previous_name_stamp

  let create ?user_visible name =
    let data : Variable_data.t =
      { compilation_unit = Compilation_unit.get_current_exn ();
        previous_compilation_units = [];
        name;
        name_stamp = create_name_stamp ();
        user_visible = Option.is_some user_visible;
      }
    in
    let t = Table.add !grand_table_of_variables data in
    t lor current_compilation_unit_flag

  let name_stamp0 t (var_data : Variable_data.t) =
    var_data.name_stamp * 2
      + if is_renamed t then 1 else 0

  let name_stamp t =
    name_stamp0 t (find_data t)

  module T0 = struct
    let compare = Id.compare
    let equal = Id.equal
    let hash = Id.hash

    (* CR mshinwell: colour? *)
    let print ppf t =
      let cu = compilation_unit t in
      if Compilation_unit.equal cu (Compilation_unit.get_current_exn ())
      then Format.fprintf ppf "%s/%d" (name t) (name_stamp t)
      else
        Format.fprintf ppf "%a.%s/%d"
          Compilation_unit.print cu
          (name t)
          (name_stamp t)

    let output chan t = print (Format.formatter_of_out_channel chan) t
  end

  include T0
  module T = struct
    type nonrec t = t
    include T0
  end

  module Set = Patricia_tree.Make_set (struct let print = print end)
  module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
  module Tbl = Identifiable.Make_tbl (Numbers.Int) (Map)

  let export t =
    let exported = find_data t in
    (* The renaming flag will be cleared upon import, so we must distinguish
       between the un-renamed and once-renamed versions upon export.
       We do this by creating a new name stamp (this is like delaying the
       slow path case of [Variable.rename]).
       Since setting the renaming flag implies moving into the current
       compilation unit, we must also reflect any such change.
       The code in [import] below ensures that we do not re-hash based on this
       new data (which would cause large import renaming maps). *)
    let compilation_unit =
      if is_renamed t then Compilation_unit.get_current_exn ()
      else exported.compilation_unit
    in
    let name_stamp = name_stamp0 t exported in
    { exported with
      compilation_unit;
      name_stamp;
    }

  let import t (data : exported) =
    Table.add_using_existing_id !grand_table_of_variables data
      ~existing_id:(mask_for_find t)

  (* To avoid large import maps, we special-case the operation of clearing
     the "current compilation unit" and "has been renamed" bits, upon
     import. *)
  let has_been_imported t =
    mask_for_find t

  let map_compilation_unit f (data : exported) : exported =
    let new_compilation_unit = f data.compilation_unit in
    if Compilation_unit.equal new_compilation_unit data.compilation_unit
    then data
    else
      { data with compilation_unit = new_compilation_unit;
                  previous_compilation_units =
                    data.compilation_unit :: data.previous_compilation_units;
      }
end

module Symbol = struct
  type t = Id.t
  type exported = Symbol_data.t

  module Table = Table_by_int_id.Make (Symbol_data)
  let grand_table_of_symbols = ref (Table.create ())

  let initialise () =
    grand_table_of_symbols := Table.create ()

  let find_data t = Table.find !grand_table_of_symbols (mask_for_find t)

  let unsafe_create compilation_unit linkage_name =
    let data : Symbol_data.t = { compilation_unit; linkage_name; } in
    let extra_flags =
      match Compilation_unit.get_current () with
      | Some current_unit ->
        if Compilation_unit.equal compilation_unit current_unit
        then current_compilation_unit_flag
        else 0
      | None ->
        if Compilation_unit.is_predefined_exception compilation_unit
             || Compilation_unit.is_external_symbols compilation_unit
        then 0
        else
          Misc.fatal_error "Cannot create non-predef or external (non-Flambda) \
            symbols until current compilation unit has been set"
    in
    let t = Table.add !grand_table_of_symbols data in
    t lor extra_flags

  let create compilation_unit linkage_name =
    let unit_linkage_name =
      Linkage_name.to_string
        (Compilation_unit.get_linkage_name compilation_unit)
    in
    let linkage_name =
      Linkage_name.create
        (unit_linkage_name ^ "__" ^ (Linkage_name.to_string linkage_name))
    in
    unsafe_create compilation_unit linkage_name

  let compilation_unit t =
    if has_current_compilation_unit_flags (Id.flags t) then
      Compilation_unit.get_current_exn ()
    else
      (find_data t).compilation_unit

  let linkage_name t = (find_data t).linkage_name

  module T0 = struct
    let compare = Id.compare
    let equal = Id.equal
    let hash = Id.hash

    let print ppf t =
      Format.fprintf ppf "@<0>%s" (Flambda_colours.symbol ());
      Compilation_unit.print ppf (compilation_unit t);
      Format.pp_print_string ppf ".";
      Linkage_name.print ppf (linkage_name t);
      Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end

  include T0
  module T = struct
    type nonrec t = t
    include T0
  end

  module Set = Patricia_tree.Make_set (struct let print = print end)
  module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
  module Tbl = Identifiable.Make_tbl (Numbers.Int) (Map)

  let export t = find_data t

  let import (data : exported) =
    Table.add !grand_table_of_symbols data

  let map_compilation_unit f (data : exported) : exported =
    { data with compilation_unit = f data.compilation_unit; }
end

module Name = struct
  type t = Id.t

  let var v = v
  let symbol s = s

  let [@inline always] pattern_match t ~var ~symbol =
    let flags = Id.flags t in
    if has_var_flag flags then var t
    else if has_symbol_flag flags then symbol t
    else assert false

  module T0 = struct
    let compare = Id.compare
    let equal = Id.equal
    let hash = Id.hash

    let print ppf t =
      Format.fprintf ppf "@<0>%s" (Flambda_colours.name ());
      pattern_match t
        ~var:(fun var -> Variable.print ppf var)
        ~symbol:(fun symbol -> Symbol.print ppf symbol);
      Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end

  include T0
  module T = struct
    type nonrec t = t
    include T0
  end

  module Set = Patricia_tree.Make_set (struct let print = print end)
  module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
  module Tbl = Identifiable.Make_tbl (Numbers.Int) (Map)
end

module Simple = struct
  type t = Id.t
  type exported = Simple_data.t

  module Table = Table_by_int_id.Make (Simple_data)
  (* This table only holds [Simple]s that have auxiliary data associated
     with them, as indicated by the setting of the [simple_flags]. *)
  let grand_table_of_simples = ref (Table.create ())

  let initialise () =
    grand_table_of_simples := Table.create ()

  let find_data t = Table.find !grand_table_of_simples t

  let has_coercion t =
    has_simple_flag (Id.flags t)

  let name n = n
  let var v = v
  let vars vars = vars
  let symbol s = s
  let const cst = cst

  let [@inline always] pattern_match t ~name ~const =
    let flags = Id.flags t in
    if has_var_flag flags then
      (name [@inlined hint]) (Name.var t) ~coercion:Coercion.id
    else if has_symbol_flag flags then
      (name [@inlined hint]) (Name.symbol t) ~coercion:Coercion.id
    else if has_const_flag flags then (const [@inlined hint]) t
    else if has_simple_flag flags then
      let { Simple_data.simple = t; coercion } = find_data t in
      let flags = Id.flags t in
      if has_var_flag flags then (name [@inlined hint]) (Name.var t) ~coercion
      else if has_symbol_flag flags then
        (name [@inlined hint]) (Name.symbol t) ~coercion
      else if has_const_flag flags then (const [@inlined hint]) t
      else assert false
    else assert false

  let same t1 t2 =
    let name n1 ~coercion:co1 =
      pattern_match t2
        ~name:(fun n2 ~coercion:co2 -> Name.equal n1 n2 && Coercion.equal co1 co2)
        ~const:(fun _ -> false)
    in
    let const c1 =
      pattern_match t2 ~name:(fun _ ~coercion:_ -> false)
        ~const:(fun c2 -> Const.equal c1 c2)
    in
    pattern_match t1 ~name ~const

  let [@inline always] coercion t =
    let flags = Id.flags t in
    if has_simple_flag flags then (find_data t).coercion
    else Coercion.id

  module T0 = struct
    let compare = Id.compare
    let equal = Id.equal
    let hash = Id.hash

    let print ppf t =
      let print ppf t =
        pattern_match t
          ~name:(fun name ~coercion:_ -> Name.print ppf name)
          ~const:(fun cst -> Const.print ppf cst)
      in
      match coercion t with
      | Id -> print ppf t
      | Non_id _ as coercion ->
        Format.fprintf ppf "@[<hov 1>(coerce@ %a@ %a)@]"
          print t
          Coercion.print coercion

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end

  include T0
  module T = struct
    type nonrec t = t
    include T0
  end

  let with_coercion t new_coercion =
    if Coercion.is_id new_coercion then t
    else
      match coercion t with
      | Id ->
        let data : Simple_data.t = { simple = t; coercion = new_coercion; } in
        Table.add !grand_table_of_simples data
      | Non_id _ ->
        Misc.fatal_errorf "Cannot add [Coercion] to [Simple] %a that already \
            has non-identity [Coercion]"
          print t

  module Set = Patricia_tree.Make_set (struct let print = print end)
  module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
  module Tbl = Identifiable.Make_tbl (Numbers.Int) (Map)

  let export t = find_data t

  let import (data : exported) =
    (* Note: We do not import the underlying name or const.
       This is done on purpose, to make the import process simpler
       and well-defined, but means that the real import functions
       (in Renaming) are responsible for importing the underlying
       name/const. *)
    Table.add !grand_table_of_simples data

  let map_compilation_unit _f data =
    (* The compilation unit is not associated with the simple directly,
       only with the underlying name, which has its own entry. *)
    data
end

let initialise () =
  Const.initialise ();
  Variable.initialise ();
  Symbol.initialise ();
  Simple.initialise ()
