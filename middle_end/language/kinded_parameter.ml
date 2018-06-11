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

let equal 
      { param = param1; kind = kind1; }
      { param = param2; kind = kind2; } =
  Parameter.equal param1 param2
    && Flambda_kind.equal kind1 kind2

let equal_kinds t1 t2 =
  Flambda_kind.equal t1.kind t2.kind

let print ppf { param; kind; } =
  Format.fprintf ppf "(%a :@ %a)"
    Parameter.print param
    Flambda_kind.print kind

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
end
