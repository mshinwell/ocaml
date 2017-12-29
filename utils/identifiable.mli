(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Uniform interface for common data structures over various things. *)

module type Thing = sig
  type t

  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t

  val print : Format.formatter -> t -> unit
end

module type Thing_no_hash = sig
  type t

  include Map.OrderedType with type t := t

  val print : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module Pair : functor (A : Thing) (B : Thing) -> Thing with type t = A.t * B.t

module type Set = sig
  module T : Set.OrderedType
  include Set.S
    with type elt = T.t

  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
  val union_list : t list -> t
  val get_singleton : t -> elt option
end

module type Map = sig
  module T : Map.OrderedType
  module Our_set : Set.S

  include Map.S
    with type key = T.t
     and type 'a t = 'a Map.Make (T).t

  val filter_map : 'a t -> f:(key -> 'a -> 'b option) -> 'b t
  val of_list : (key * 'a) list -> 'a t

  (** [disjoint_union m1 m2] contains all bindings from [m1] and
      [m2]. If some binding is present in both and the associated
      value is not equal, a Fatal_error is raised *)
  val disjoint_union : ?eq:('a -> 'a -> bool) -> ?print:(Format.formatter -> 'a -> unit) -> 'a t -> 'a t -> 'a t

  (** [union_right m1 m2] contains all bindings from [m1] and [m2]. If
      some binding is present in both, the one from [m2] is taken *)
  val union_right : 'a t -> 'a t -> 'a t

  (** [union_left m1 m2 = union_right m2 m1] *)
  val union_left : 'a t -> 'a t -> 'a t

  (** [union_merge f m1 m2] contains all bindings from [m1] and [m2].  Bindings
      present in both [m1] and [m2] are sent through [f]. *)
  val union_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  (** [union_both f g m1 m2] contains all bindings from [m1] and [m2].
      Bindings present in only one of [m1] and [m2] are sent through [f];
      bindings present in both [m1] and [m2] are sent through [g]. *)
  val union_both
     : ('a -> 'a)
    -> ('a -> 'a -> 'a)
    -> 'a t
    -> 'a t
    -> 'a t

  val inter : ('a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t

  val inter_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val rename : key t -> key -> key
  val map_keys : (key -> key) -> 'a t -> 'a t
  val keys : 'a t -> Our_set.t
  val data : 'a t -> 'a list
  val of_set : (key -> 'a) -> Our_set.t -> 'a t
  val transpose_keys_and_data : key t -> key t
  val transpose_keys_and_data_set : key t -> Our_set.t t
  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val get_singleton : 'a t -> (key * 'a) option
end

module type Tbl = sig
  module Our_map : Map.S

  module T : sig
    type t
    include Map.OrderedType with type t := t
    include Hashtbl.HashedType with type t := t
  end
  include Hashtbl.S
    with type key = T.t

  val to_list : 'a t -> (T.t * 'a) list
  val of_list : (T.t * 'a) list -> 'a t

  val to_map : 'a t -> 'a Our_map.t
  val of_map : 'a Our_map.t -> 'a t
  val memoize : 'a t -> (key -> 'a) -> key -> 'a
  val map : 'a t -> ('a -> 'b) -> 'b t
end

module type S = sig
  type t

  module T : Thing with type t = t
  include Thing with type t := T.t

  module Set : Set with module T := T
  module Map : Map with module T := T with module Our_set := Set
  module Tbl : Tbl with module T := T with module Our_map := Map
end

module Make (T : Thing) : S with type t := T.t

module Make_pair (T0 : S) (T1 : S) : sig
  include S with type t := T0.t * T1.t

  val create_from_cross_product : T0.Set.t -> T1.Set.t -> Set.t
end

module type S_no_hash = sig
  type t

  module T : Thing_no_hash with type t = t
  include Thing_no_hash with type t := T.t

  module Set : Set with module T := T
  module Map : Map with module T := T with module Our_set := Set

  val equal : t -> t -> bool
end

module Make_no_hash (T : Thing_no_hash) : S_no_hash with type t := T.t
