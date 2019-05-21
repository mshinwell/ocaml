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

module JE = Typing_env
module TEE = Typing_env_extension

type meet_or_join = Meet | Join

module For_meet = struct
  let name () = "meet"
  let op () : meet_or_join = Meet
  let unknown_is_identity () = true
  let unknown_is_absorbing () = false

  module Make (I : Identifiable.S) = struct
    module Set = struct
      type t = I.Set.t
      let union_or_inter = I.Set.inter
    end

    module Map = struct
      type 'a t = 'a I.Map.t
      let union_or_inter = I.Map.inter
      let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both t1 t2 =
        I.Map.inter in_both t1 t2
    end
  end

  module String_info = Make (String_info)
  module Immediate = Make (Immediate)
  module Float = Make (Float)
  module Int32 = Make (Int32)
  module Int64 = Make (Int64)
  module Targetint = struct
    include Make (Targetint)
    module OCaml = Make (Targetint.OCaml)
  end
  module Closure_id = Make (Closure_id)
  module Var_within_closure = Make (Var_within_closure)
  module Tag = Make (Tag)
  module Discriminant = Make (Discriminant)

  let switch_no_bottom meet _join join_env thing1 thing2 =
    meet (JE.central_environment join_env) thing1 thing2

  let switch meet _join join_env thing1 thing2 =
    meet (JE.central_environment join_env) thing1 thing2

  let switch' meet _join join_env thing1 thing2 : _ Or_bottom.t =
    Or_bottom.map (meet (JE.central_environment join_env) thing1 thing2)
      ~f:(fun (thing, _) -> thing)
end

module For_join = struct
  let name () = "join"
  let op () : meet_or_join = Join
  let unknown_is_identity () = false
  let unknown_is_absorbing () = true

  module Make (I : Identifiable.S) = struct
    module Set = struct
      type t = I.Set.t
      let union_or_inter = I.Set.union
    end

    module Map = struct
      type 'a t = 'a I.Map.t
      let union_or_inter = I.Map.union
      let union_or_inter_both = I.Map.union_both
      let union_or_inter_and_left = I.Map.union  (* CR mshinwell: check *)
    end
  end

  module String_info = Make (String_info)
  module Immediate = Make (Immediate)
  module Float = Make (Float)
  module Int32 = Make (Int32)
  module Int64 = Make (Int64)
  module Targetint = struct
    include Make (Targetint)
    module OCaml = Make (Targetint.OCaml)
  end
  module Closure_id = Make (Closure_id)
  module Var_within_closure = Make (Var_within_closure)
  module Tag = Make (Tag)
  module Discriminant = Make (Discriminant)

  let switch_no_bottom _meet join join_env thing1 thing2 =
    join join_env thing1 thing2, TEE.empty

  let switch _meet join join_env thing1 thing2 : _ Or_bottom.t =
    Ok (join join_env thing1 thing2, TEE.empty)

  let switch' _meet join join_env thing1 thing2 : _ Or_bottom.t =
    Ok (join join_env thing1 thing2)
end
