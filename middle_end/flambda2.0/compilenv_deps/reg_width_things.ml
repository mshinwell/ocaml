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

module Const_data = struct
  type t =
    | Naked_immediate of Immediate.t
    | Tagged_immediate of Immediate.t
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
          Immediate.print i
          (Flambda_colours.normal ())
      | Tagged_immediate i ->
        Format.fprintf ppf "@<0>%s%a@<0>%s"
          (Flambda_colours.tagged_immediate ())
          Immediate.print i
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
        Immediate.compare i1 i2
      | Tagged_immediate i1, Tagged_immediate i2 ->
        Immediate.compare i1 i2
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
          Immediate.equal i1 i2
        | Tagged_immediate i1, Tagged_immediate i2 ->
          Immediate.equal i1 i2
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
      | Naked_immediate n -> Immediate.hash n
      | Tagged_immediate n -> Immediate.hash n
      | Naked_float n -> Numbers.Float_by_bit_pattern.hash n
      | Naked_int32 n -> Hashtbl.hash n
      | Naked_int64 n -> Hashtbl.hash n
      | Naked_nativeint n -> Targetint.hash n
  end)
end

module Variable_data = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    name : string;
    name_stamp : int;
    user_visible : bool;
  }

  let flags = var_flags

  let print ppf { compilation_unit; name; name_stamp; user_visible; } =
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

  let hash { compilation_unit; name = _; name_stamp; user_visible = _; } =
    (* The [name_stamp] uniquely determines [name] and [user_visible]. *)
    Hashtbl.hash (Compilation_unit.hash compilation_unit, name_stamp)

  let equal t1 t2 =
    if t1 == t2 then true
    else
      let { compilation_unit = compilation_unit1; name = _;
            name_stamp = name_stamp1; user_visible = _; 
          } = t1
      in
      let { compilation_unit = compilation_unit2; name = _;
            name_stamp = name_stamp2; user_visible = _; 
          } = t2
      in
      Int.equal name_stamp1 name_stamp2
        && Compilation_unit.equal compilation_unit1 compilation_unit2
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

  let hash { compilation_unit; linkage_name; } =
    Hashtbl.hash (Compilation_unit.hash compilation_unit,
      Linkage_name.hash linkage_name)

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
    simple : Id.t;  (* always without [Rec_info] *)
    rec_info : Rec_info.t;
  }

  let flags = simple_flags

  let print ppf { simple = _; rec_info; } =
    Format.fprintf ppf "@[<hov 1>\
        @[<hov 1>(rec_info@ %a)@]\
        @]"
      Rec_info.print rec_info

  let hash { simple; rec_info; } =
    Hashtbl.hash (Id.hash simple, Rec_info.hash rec_info)

  let equal t1 t2 =
    if t1 == t2 then true
    else
      let { simple = simple1; rec_info = rec_info1; } = t1 in
      let { simple = simple2; rec_info = rec_info2; } = t2 in
      Id.equal simple1 simple2
        && Rec_info.equal rec_info1 rec_info2
end

module Const = struct
  type t = Id.t

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

  let const_true = tagged_immediate Immediate.bool_true
  let const_false = tagged_immediate Immediate.bool_false

  let untagged_const_true = naked_immediate Immediate.bool_true
  let untagged_const_false = naked_immediate Immediate.bool_false

  let const_int i = tagged_immediate (Immediate.int i)

  let const_zero = tagged_immediate Immediate.zero
  let const_one = tagged_immediate Immediate.one
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
end

module Variable = struct
  type t = Id.t

  module Table = Table_by_int_id.Make (Variable_data)
  let grand_table_of_variables = ref (Table.create ())

  let initialise () =
    grand_table_of_variables := Table.create ()

  let find_data t = Table.find !grand_table_of_variables t

  let compilation_unit t = (find_data t).compilation_unit
  let name t = (find_data t).name
  let name_stamp t = (find_data t).name_stamp
  let user_visible t = (find_data t).user_visible

  let previous_name_stamp = ref (-1)

  let create ?user_visible name =
    let name_stamp =
      (* CR mshinwell: check for overflow on 32 bit *)
      incr previous_name_stamp;
      !previous_name_stamp
    in
    let data : Variable_data.t =
      { compilation_unit = Compilation_unit.get_current_exn ();
        name;
        name_stamp;
        user_visible = Option.is_some user_visible;
      }
    in
    Table.add !grand_table_of_variables data

  module T0 = struct
    let compare = Id.compare
    let equal = Id.equal
    let hash = Id.hash

    (* CR mshinwell: colour? *)
    let print ppf t = Format.fprintf ppf "%s/%d" (name t) (name_stamp t)

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
end

module Symbol = struct
  type t = Id.t

  module Table = Table_by_int_id.Make (Symbol_data)
  let grand_table_of_symbols = ref (Table.create ())

  let initialise () =
    grand_table_of_symbols := Table.create ()

  let find_data t = Table.find !grand_table_of_symbols t

  let unsafe_create compilation_unit linkage_name =
    let data : Symbol_data.t = { compilation_unit; linkage_name; } in
    Table.add !grand_table_of_symbols data

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

  let compilation_unit t = (find_data t).compilation_unit
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
end

module Name = struct
  type t = Id.t

  let var v = v
  let symbol s = s

  let [@inline always] pattern_match t ~var ~symbol =
    let flags = Id.flags t in
    if flags = var_flags then var t
    else if flags = symbol_flags then symbol t
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

  module Table = Table_by_int_id.Make (Simple_data)
  (* This table only holds [Simple]s that have auxiliary data associated
     with them, as indicated by the setting of the [simple_flags]. *)
  let grand_table_of_simples = ref (Table.create ())

  let initialise () =
    grand_table_of_simples := Table.create ()

  let find_data t = Table.find !grand_table_of_simples t

  let name n = n
  let var v = v
  let vars vars = vars
  let symbol s = s
  let const cst = cst

  let [@inline always] pattern_match t ~name ~const =
    let flags = Id.flags t in
    if flags = var_flags then name (Name.var t)
    else if flags = symbol_flags then name (Name.symbol t)
    else if flags = const_flags then const t
    else if flags = simple_flags then
      let t = (find_data t).simple in
      let flags = Id.flags t in
      if flags = var_flags then name (Name.var t)
      else if flags = symbol_flags then name (Name.symbol t)
      else if flags = const_flags then const t
      else assert false
    else assert false

  let [@inline always] rec_info t =
    let flags = Id.flags t in
    if flags = simple_flags then Some ((find_data t).rec_info)
    else None

  module T0 = struct
    let compare = Id.compare
    let equal = Id.equal
    let hash = Id.hash

    let print ppf t =
      let print ppf t =
        pattern_match t
          ~name:(fun name -> Name.print ppf name)
          ~const:(fun cst -> Const.print ppf cst)
      in
      match rec_info t with
      | None -> print ppf t
      | Some rec_info ->
       Format.fprintf ppf "@[<hov 1>\
            @[<hov 1>(simple@ %a)@] \
            @[<hov 1>(rec_info@ %a)@]\
            @]"
          print t
          Rec_info.print rec_info

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end

  include T0
  module T = struct
    type nonrec t = t
    include T0
  end

  let with_rec_info t new_rec_info =
    if Rec_info.is_initial new_rec_info then t
    else
      match rec_info t with
      | None ->
        let data : Simple_data.t = { simple = t; rec_info = new_rec_info; } in
        Table.add !grand_table_of_simples data
      | Some _ ->
        Misc.fatal_errorf "Cannot add [Rec_info] to [Simple] %a that already \
            has [Rec_info]"
          print t

  module Set = Patricia_tree.Make_set (struct let print = print end)
  module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
  module Tbl = Identifiable.Make_tbl (Numbers.Int) (Map)
end

let initialise () =
  Const.initialise ();
  Variable.initialise ();
  Symbol.initialise ();
  Simple.initialise ()
