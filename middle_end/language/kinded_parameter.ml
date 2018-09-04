(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

type t = {
  param : Parameter.t;
  kind : Flambda_kind.t;
}

include Hashtbl.Make_with_map (struct
  type nonrec t = t

  let compare 
        { param = param1; kind = kind1; }
        { param = param2; kind = kind2; } =
    let c = Parameter.compare param1 param2 in
    if c <> 0 then c
    else Flambda_kind.compare kind1 kind2

  let hash { param; kind; } =
    Hashtbl.hash (Parameter.hash param, Flambda_kind.hash kind)

  let print ppf { param; kind; } =
    Format.fprintf ppf "(%a :@ %a)"
      Parameter.print param
      Flambda_kind.print kind
end)

let create param kind =
  { param;
    kind;
  }

let param t = t.param
let var t = Parameter.var t.param
let name t = Name.var (var t)
let simple t = Simple.var (var t)
let kind t = t.kind

let with_kind t kind = { t with kind; }

let rename t = { t with param = Parameter.rename t.param; }

let map_var t ~f = { t with param = Parameter.map_var f t.param; }

let map_kind t ~f = { t with kind = f t.kind; }

let equal_kinds t1 t2 =
  Flambda_kind.equal t1.kind t2.kind

let free_names ({ param = _; kind = _; } as t) =
  Name_occurrences.singleton_in_terms (Name (name t))

let apply_name_permutation ({ param = _; kind; } as t) perm =
  match Name_permutation.apply_name perm (name t) with
  | Var var -> create (Parameter.wrap var) kind
  | _ ->
    Misc.fatal_errorf "Illegal name permutation on [Kinded_parameter]: %a"
      Name_permutation.print perm

module List = struct
  type nonrec t = t list

  let create params_and_kinds =
    List.map (fun (param, kind) -> create param kind) params_and_kinds

  let vars t = List.map var t

  let simples t = List.map simple t

  let equal_vars t1 t2 =
    List.length t1 = List.length t2
      && List.for_all2 (fun param1 var2 -> Variable.equal (var param1) var2)
           t1 t2

  let var_set t = Variable.Set.of_list (vars t)

  let name_set t = Name.Set.of_list (List.map Name.var (vars t))

  let param_set t = Parameter.Set.of_list (List.map param t)

  let rename t = List.map (fun t -> rename t) t

  let arity t = List.map (fun t -> kind t) t

  let equal t1 t2 =
    List.compare_lengths t1 t2 = 0
      && List.for_all2 equal t1 t2

  let print ppf t =
    Format.pp_print_list ~pp_sep:Format.pp_print_space print ppf t

  let free_names t =
    List.fold_left (fun result param ->
        Name_occurrences.union result (free_names param))
      (Name_occurrences.create ())
      t

  let apply_name_permutation t perm =
    List.map (fun param -> apply_name_permutation param perm) t
end
