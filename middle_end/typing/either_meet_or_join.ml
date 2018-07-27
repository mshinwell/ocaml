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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module For_meet (T : Typing_world.S) = struct
  let name = "meet"

  type meet_or_join = Meet | Join

  let op : meet_or_join = Meet
  let (_ : meet_or_join) = Join

  let unknown_is_identity = true
  let unknown_is_absorbing = false

  (* CR mshinwell: Write functors to generate these patterns *)
  module Immediate = struct
    module Set = struct
      type t = Immediate.Set.t
      let union_or_inter = Immediate.Set.inter
    end

    module Map = struct
      type 'a t = 'a Immediate.Map.t

      let union_or_inter = Immediate.Map.inter
    end
  end

  module Float_by_bit_pattern = struct
    module Set = struct
      type t = Float_by_bit_pattern.Set.t
      let union_or_inter = Float_by_bit_pattern.Set.inter
    end
  end

  module Int32 = struct
    module Set = struct
      type t = Int32.Set.t
      let union_or_inter = Int32.Set.inter
    end
  end

  module Int64 = struct
    module Set = struct
      type t = Int64.Set.t
      let union_or_inter = Int64.Set.inter
    end
  end

  module Targetint = struct
    module Set = struct
      type t = Targetint.Set.t
      let union_or_inter = Targetint.Set.inter
    end

    module OCaml = struct
      module Map = struct
        type 'a t = 'a Targetint.OCaml.Map.t

        let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
              t1 t2 =
          Targetint.OCaml.Map.inter in_both t1 t2
      end
    end
  end

  module Closure_id = struct
    module Map = struct
      type 'a t = 'a Closure_id.Map.t

      let union_or_inter = Closure_id.Map.inter

      (* CR mshinwell: implement these *)
      let union_or_inter_and_left _f _t1 _t2 = assert false
    end
  end

  module Var_within_closure = struct
    module Map = struct
      type 'a t = 'a Var_within_closure.Map.t

      let union_or_inter = Var_within_closure.Map.inter

      let union_or_inter_and_left _f _t1 _t2 = assert false
    end
  end

  module Tag = struct
    module Map = struct
      type 'a t = 'a Tag.Map.t

      let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
            t1 t2 =
        Tag.Map.inter in_both t1 t2
    end
  end

  module Discriminant = struct
    module Map = struct
      type 'a t = 'a Discriminant.Map.t

      let union_or_inter_both ~in_left_only:_ ~in_right_only:_ ~in_both
            t1 t2 =
        Discriminant.Map.inter in_both t1 t2
    end
  end

  let switch meet _join join_env thing1 thing2 =
    meet (Join_env.central_environment join_env) thing1 thing2

  let switch' meet _join join_env thing1 thing2 =
    meet (Join_env.central_environment join_env) thing1 thing2
end

module For_join (T : Typing_world.S) = struct
  let name = "join"

  type meet_or_join = Meet | Join

  let op : meet_or_join = Join
  let (_ : meet_or_join) = Meet

  let unknown_is_identity = false
  let unknown_is_absorbing = true

  module Immediate = struct
    module Set = struct
      type t = Immediate.Set.t
      let union_or_inter = Immediate.Set.union
    end

    module Map = struct
      type 'a t = 'a Immediate.Map.t

      let union_or_inter = Immediate.Map.union
    end
  end

  module Float_by_bit_pattern = struct
    module Set = struct
      type t = Float_by_bit_pattern.Set.t
      let union_or_inter = Float_by_bit_pattern.Set.union
    end
  end

  module Int32 = struct
    module Set = struct
      type t = Int32.Set.t
      let union_or_inter = Int32.Set.union
    end
  end

  module Int64 = struct
    module Set = struct
      type t = Int64.Set.t
      let union_or_inter = Int64.Set.union
    end
  end

  module Targetint = struct
    module Set = struct
      type t = Targetint.Set.t
      let union_or_inter = Targetint.Set.union
    end

    module OCaml = struct
      module Map = struct
        type 'a t = 'a Targetint.OCaml.Map.t

        let union_or_inter_both = Targetint.OCaml.Map.union_both
      end
    end
  end

  module Closure_id = struct
    module Map = struct
      type 'a t = 'a Closure_id.Map.t

      let union_or_inter = Closure_id.Map.union

      let union_or_inter_and_left f t1 t2 =
        Closure_id.Map.union f t1 t2
    end
  end

  module Var_within_closure = struct
    module Map = struct
      type 'a t = 'a Var_within_closure.Map.t

      let union_or_inter = Var_within_closure.Map.union

      let union_or_inter_and_left f t1 t2 =
        Var_within_closure.Map.union f t1 t2
    end
  end

  module Tag = struct
    module Map = struct
      type 'a t = 'a Tag.Map.t

      let union_or_inter_both = Tag.Map.union_both
    end
  end

  module Discriminant = struct
    module Map = struct
      type 'a t = 'a Discriminant.Map.t

      let union_or_inter_both = Discriminant.Map.union_both
    end
  end

  let switch _meet join join_env thing1 thing2 =
    join join_env thing1 thing2, Typing_env_extension.empty

  let switch' _meet join join_env thing1 thing2 =
    join join_env thing1 thing2
end
