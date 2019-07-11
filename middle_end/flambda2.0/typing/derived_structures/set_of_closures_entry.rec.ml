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

type t = Set_of_closures_entry.t

let create_bottom () : t =
  { by_closure_id = Types_by_closure_id.bottom;
  }

let print_with_cache ~cache ppf ({ by_closure_id; } : t) =
  Format.fprintf ppf
    "@[<hov 1>(\
       @[<hov 1>(by_closure_id@ %a)@]\
       )@]"
    (Types_by_closure_id.print_with_cache ~cache) by_closure_id

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let equal _ _ = Misc.fatal_error "Not yet implemented"

let widen t ~to_match:_ = t  (* XXX Think about this *)

module Meet_fabricated = Meet_and_join_fabricated.Make (Lattice_ops.For_meet)
module Join_fabricated = Meet_and_join_fabricated.Make (Lattice_ops.For_join)

let meet env t1 t2 =
  Meet_fabricated.meet_or_join_set_of_closures_entry env t1 t2

let join env t1 t2 =
  let env = Meet_env.create env in
  match Join_fabricated.meet_or_join_set_of_closures_entry env t1 t2 with
  | Ok (t, _env_extension) -> t
  | Bottom -> create_bottom ()

let erase_aliases ({ by_closure_id; } : t) env ~already_seen ~allowed : t =
  { by_closure_id =
      Types_by_closure_id.erase_aliases by_closure_id env
        ~already_seen ~allowed;
  }

let apply_name_permutation ({ by_closure_id; } : t) perm : t =
  (* CR mshinwell: phys-equal checks *)
  let by_closure_id =
    Types_by_closure_id.apply_name_permutation by_closure_id perm
  in
  { by_closure_id; }

let free_names ({ by_closure_id; } : t) =
  Types_by_closure_id.free_names by_closure_id

let map_closure_types ({ by_closure_id; } : t) ~f : _ Or_bottom.t =
  Or_bottom.map (Types_by_closure_id.map_closure_types by_closure_id ~f)
    ~f:(fun by_closure_id : t -> { by_closure_id; })
