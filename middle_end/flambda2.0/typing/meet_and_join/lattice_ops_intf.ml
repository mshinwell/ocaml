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

(** The signature of a module that is used for specialising generic
    meet-and-join operations to either meet or join. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  type typing_env
  type meet_env
  type typing_env_extension
  type meet_or_join

  val name : unit -> string

  val op : unit -> meet_or_join

  val unknown_is_identity : unit -> bool
  val unknown_is_absorbing : unit -> bool

  module String_info : sig
    module Set : sig
      type t = String_info.Set.t

      val union_or_inter : t -> t -> t
    end
  end

  module Immediate : sig
    module Set : sig
      type t = Immediate.Set.t

      val union_or_inter : t -> t -> t
    end

    module Map : sig
      type 'a t = 'a Immediate.Map.t

      val union_or_inter
         : (Immediate.t -> 'a -> 'a -> 'a option)
        -> 'a t
        -> 'a t
        -> 'a t
    end
  end

  module Float : sig
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

    module OCaml : sig
      module Map : sig
        type 'a t = 'a Targetint.OCaml.Map.t

        val union_or_inter_both
          : in_left_only:('a -> 'a)
         -> in_right_only:('a -> 'a)
         -> in_both:(Targetint.OCaml.t -> 'a -> 'a -> 'a option)
         -> 'a t
         -> 'a t
         -> 'a t
      end
    end
  end

  module Closure_id : sig
    module Map : sig
      type 'a t = 'a Closure_id.Map.t

      val union_or_inter
         : (Closure_id.t -> 'a -> 'a -> 'a option)
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
         : (Var_within_closure.t -> 'a -> 'a -> 'a option)
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

  module Tag : sig
    module Map : sig
      type 'a t = 'a Tag.Map.t

      val union_or_inter_both
        : in_left_only:('a -> 'a)
       -> in_right_only:('a -> 'a)
       -> in_both:(Tag.t -> 'a -> 'a -> 'a option)
       -> 'a t
       -> 'a t
       -> 'a t
    end
  end

  module Discriminant : sig
    module Map : sig
      type 'a t = 'a Discriminant.Map.t

      val union_or_inter_both
        : in_left_only:('a -> 'a)
       -> in_right_only:('a -> 'a)
       -> in_both:(Discriminant.t -> 'a -> 'a -> 'a option)
       -> 'a t
       -> 'a t
       -> 'a t
    end
  end

  val switch_no_bottom
     : (meet_env -> 'a -> 'a -> 'a * typing_env_extension)
    -> (typing_env -> 'a -> 'a -> 'a)
    -> typing_env
    -> 'a
    -> 'a
    -> 'a * typing_env_extension

  val switch
     : (meet_env -> 'a -> 'a -> ('a * typing_env_extension) Or_bottom.t)
    -> (typing_env -> 'a -> 'a -> 'a)
    -> typing_env
    -> 'a
    -> 'a
    -> ('a * typing_env_extension) Or_bottom.t

  val switch'
     : (meet_env -> 'a -> 'a -> ('a * typing_env_extension) Or_bottom.t)
    -> (typing_env -> 'a -> 'a -> 'a)
    -> typing_env
    -> 'a
    -> 'a
    -> 'a Or_bottom.t
end
