(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type parameter = {
  var : Variable.t;
}

let create (name : Name.t) =
  match name with
  | Var var -> { var; }
  | Symbol _ ->
    Misc.fatal_errorf "Can only create [Parameter]s from [Variable]s: %a"
      Name.print name

let wrap var = { var; }

let var t = t.var

let name t = Name.var (var t)

module M =
  Identifiable.Make (struct
    type t = parameter

    let compare { var = var1 } { var = var2 } =
      Variable.compare var1 var2

    let equal t1 t2 = (compare t1 t2 = 0)

    let hash { var } =
      Variable.hash var

    let print ppf { var } =
      Variable.print ppf var

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

module T = M.T
include T

let print_list ppf ts =
  let pp_sep ppf () = Format.fprintf ppf "@ " in
  Format.pp_print_list ~pp_sep print ppf ts

let equal = M.equal

module Map = M.Map
module Tbl = M.Tbl
module Set = struct
  include M.Set
  let vars l = Variable.Set.of_list (List.map var l)

  let wrap vars =
    of_list (List.map (fun var -> wrap var) (Variable.Set.elements vars))
end

let rename ?current_compilation_unit ?append p =
  { var = Variable.rename ?current_compilation_unit ?append p.var }

let map_var f { var } = { var = f var }

module List = struct
  let vars params = List.map (fun { var } -> var) params

  let wrap vars = List.map (fun var -> wrap var) vars

  let equal_vars t vars =
    List.length t = List.length vars
      && List.for_all2 (fun param variable ->
          Variable.equal (var param) variable)
        t vars

  let rename ?current_compilation_unit ?append ts =
    List.map (fun t -> rename ?current_compilation_unit ?append t) ts

  let print ppf ts =
    let pp_sep ppf () = Format.fprintf ppf "@ " in
    Format.pp_print_list ~pp_sep print ppf ts

  let compare ts1 ts2 =
    Misc.Stdlib.List.compare compare ts1 ts2
end
