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

(** Functions over which meet and join code is parameterised. *)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module type S = sig
  val name : string

  module Immediate : sig
    module Set : sig
      type t = Immediate.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Float_by_bit_pattern : sig
    module Set : sig
      type t = Numbers.Float_by_bit_pattern.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Int32 : sig
    module Set : sig
      type t = Numbers.Int32.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Int64 : sig
    module Set : sig
      type t = Numbers.Int64.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Targetint : sig
    module Set : sig
      type t = Targetint.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Closure_id : sig
    module Map : sig
      type 'a t = 'a Closure_id.Map.t

      val union_or_inter
         : (Closure_id.t -> 'a -> 'a -> 'a)
        -> 'a t
        -> 'a t
        -> 'a t

      (** When this operation is "intersection" it also copies through from
          the inputs [t1] and [t2] those bindings in [t1] that do not occur
          in [t2]. *)
      val union_or_inter_and_left
         : (Closure_id.t -> 'a -> 'a -> 'a option)
        -> 'a t
        -> 'a t
        -> 'a t
    end
  end

  module Var_within_closure : sig
    module Map : sig
      type 'a t = 'a Var_within_closure.Map.t

      val union_or_inter
         : (Var_within_closure.t -> 'a -> 'a -> 'a)
        -> 'a t
        -> 'a t
        -> 'a t

      (** As for [Closure_id.union_or_inter_and_left], above. *)
      val union_or_inter_and_left
         : (Var_within_closure.t -> 'a -> 'a -> 'a option)
        -> 'a t
        -> 'a t
        -> 'a t
    end
  end

  module T : sig
    type join_env
    type typing_environment
    type env_extension
  end

  val switch
     : (T.typing_environment -> 'a -> 'a -> 'a * T.env_extension)
    -> (T.join_env -> 'a -> 'a -> 'a)
    -> T.join_env
    -> 'a
    -> 'a
    -> 'a * T.env_extension
end
