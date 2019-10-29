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
  name : Name.t;
  name_mode : Name_mode.t;
}

let create name name_mode =
  { name;
    name_mode;
  }

let name t = t.name
let name_mode t = t.name_mode

let var v =
  { name = Name.var (Var_in_binding_pos.var v);
    name_mode = Var_in_binding_pos.name_mode v;
  }

let symbol sym =
  { name = Name.symbol sym;
    name_mode = Name_mode.normal;
  }

let to_var t =
  match t.name with
  | Var var -> Some (Var_in_binding_pos.create var t.name_mode)
  | Symbol _ -> None

let to_name t = t.name
let to_simple t = Simple.name t.name

include Identifiable.Make (struct
  type nonrec t = t

  let print ppf { name; name_mode; } =
    Format.fprintf ppf "@[<hov 1>)\
        @[<hov 1>(name@ %a)@]@ \
        @[<hov 1>(name_mode@ %a)@]\
        )@]"
      Name.print name
      Name_mode.print name_mode

  let compare
        { name = name1; name_mode = name_mode1; }
        { name = name2; name_mode = name_mode2; } =
    let c = Name.compare name1 name2 in
    if c <> 0 then c
    else
      Name_mode.compare_total_order name_mode1 name_mode2

  let equal t1 t2 =
    compare t1 t2 = 0

  let hash _ = Misc.fatal_error "Not yet implemented"

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let is_symbol t =
  match t.name with
  | Symbol _ -> true
  | Var _ -> false

let must_be_symbol t =
  match t.name with
  | Symbol sym -> sym
  | Var _ -> Misc.fatal_errorf "Must be a symbol:@ %a" print t

let rename t =
  match t.name with
  | Symbol _ -> t
  | Var var ->
    { t with
      name = Name.var (Variable.rename var);
    }
