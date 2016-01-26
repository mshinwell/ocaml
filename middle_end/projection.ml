(**************************************************************************)
(*                                                                        *)
(*                                OCaml                                   *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file ../LICENSE.       *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Project_var of Flambda.project_var
  | Project_closure of Flambda.project_closure
  | Move_within_set_of_closures of Flambda.move_within_set_of_closures
  | Field of int * Variable.t

module Projectee = struct
  type t =
    | Project_var of Var_within_closure.t
    | Closure of Closure_id.t
    | Field of int

  include Identifiable.Make (struct
    type nonrec t = t

    let compare t1 t2 =
      match t1, t2 with
      | Project_var v1, Project_var v2 -> Var_within_closure.compare v1 v2
      | Closure c1, Closure c2 -> Closure_id.compare c1 c2
      | Field i1, Field i2 -> Pervasives.compare i1 i2
      | Project_var _, _ -> -1
      | _, Project_var _ -> 1
      | Closure _, _ -> -1
      | _, Closure _ -> 1

    let equal t1 t2 =
      match t1, t2 with
      | Project_var v1, Project_var v2 -> Var_within_closure.equal v1 v2
      | Closure c1, Closure c2 -> Closure_id.equal c1 c2
      | Field i1, Field i2 -> i1 = i2
      | _, _ -> false

    let hash = Hashtbl.hash

    let print _ _ = failwith "Projection.print: not yet implemented"
    let output _ _ = failwith "Projection.output: not yet implemented"
  end)
end

type var_and_projectee = Variable.t * Projectee.t

module Var_and_projectee = struct
  type t = Variable.t * Projectee.t

  include Identifiable.Make (struct
    type nonrec t = t

    let compare (var1, proj1) (var2, proj2) =
      let c = Variable.compare var1 var2 in
      if c <> 0 then c
      else Projectee.compare proj1 proj2

    let equal (var1, proj1) (var2, proj2) =
      Variable.equal var1 var2 && Projectee.equal proj1 proj2

    let hash = Hashtbl.hash

    let print _ _ = failwith "Projectee.print: not yet implemented"
    let output _ _ = failwith "Projectee.output: not yet implemented"
  end)
end
