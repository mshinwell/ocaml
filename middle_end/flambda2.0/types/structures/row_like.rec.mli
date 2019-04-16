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

module Make
  (Tag : Identifiable.S)
  (Index : sig
     include Identifiable.S
     val subset : t -> t -> bool
  end)
  (Tag_and_index : sig
    (** These values will not contain any names. *)
    type t = Tag.t * Index.t
    include Identifiable.S with type t := t
  end)
  (Tag_or_unknown_and_index : sig
    (** Likewise. *)
    type t = Tag.t Or_unknown.t * Index.t
    include Identifiable.S with type t := t
  end)
  (Maps_to : Row_like_maps_to_intf.S
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type typing_env_extension := Typing_env_extension.t) :
sig
  (* CR mshinwell: Add _intf.ml *)

  type t

  val create_bottom : unit -> t

(*
  val create_exactly : Tag.t -> Index.t -> Maps_to.t -> t
*)
  val create_exactly_multiple : Maps_to.t Tag_and_index.Map.t -> t
(*
  val create_at_least : Tag_or_unknown_and_index.t -> Maps_to.t -> t
  val create_at_least_multiple : Maps_to.t Tag_or_unknown_and_index.Map.t -> t
*)

  val known : t -> Maps_to.t Tag_and_index.Map.t

  val at_least : t -> Maps_to.t Tag_or_unknown_and_index.Map.t

  val get_singleton : t -> (Tag_and_index.t * Maps_to.t) option

(*
  val all_tags_and_indexes : t -> Tag_and_index.Set.t Or_unknown.t
*)

  val is_bottom : t -> bool
(*
  val map_maps_to
     : t
    -> f:(Maps_to.t -> Maps_to.t Or_bottom.t)
    -> t Or_bottom.t
*)

  (** The [Maps_to] value which [meet] returns contains the join of all
      [Maps_to] values in the range of the row-like structure after the meet
      operation has been completed. *)
  include Type_structure_intf.S
    with type t := t
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type typing_env_extension := Typing_env_extension.t
end

(* It would be nicer to have the following elsewhere other than with the
   generic definition of the row-like structure, but unfortunately
   restrictions on the evaluation of recursive modules prevent this, as the
   functor applications produce "undefined recursive module" errors. *)

(* CR mshinwell: tidy up! *)
module For_blocks : sig
  type t

  val create_bottom : unit -> t

  type open_or_closed = Open | Closed of Tag.t

  val create : field_tys:Type_grammar.t list -> open_or_closed -> t

  val all_tags_and_sizes : t -> Targetint.OCaml.t Tag.Map.t Or_unknown.t

  val get_singleton : t -> (Tag_and_size.t * Product.Int_indexed.t) option

  val is_bottom : t -> bool

  (** The [Maps_to] value which [meet] returns contains the join of all
      [Maps_to] values in the range of the row-like structure after the meet
      operation has been completed. *)
  include Type_structure_intf.S
    with type t := t
    with type flambda_type := Type_grammar.t
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type typing_env_extension := Typing_env_extension.t
end

module For_closures_entry_by_set_of_closures_contents : sig
  type t

  val create_exactly_multiple
     : Closures_entry.t Set_of_closures_contents.With_closure_id.Map.t
    -> t

  val create_at_least_multiple
     : Closures_entry.t Set_of_closures_contents.With_closure_id_or_unknown.Map.t
    -> t

  val get_singleton
     : t
    -> ((Closure_id.t * Set_of_closures_contents.t) * Closures_entry.t) option

  val map_function_decl_types
     : t
    -> f:(Function_declaration_type.t -> Function_declaration_type.t Or_bottom.t)
    -> t Or_bottom.t

  include Type_structure_intf.S
    with type t := t
    with type flambda_type := Type_grammar.t
    with type meet_env := Meet_env.t
    with type typing_env := Typing_env.t
    with type typing_env_extension := Typing_env_extension.t
end

module For_discriminants : sig
  include Trivial_row_like_intf.S
    with module Thing_without_names := Discriminant
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type typing_env_extension := Typing_env_extension.t
    with type flambda_type := Type_grammar.t
end

module For_immediates : sig
  include Trivial_row_like_intf.S
    with module Thing_without_names := Immediate
    with type typing_env := Typing_env.t
    with type meet_env := Meet_env.t
    with type typing_env_extension := Typing_env_extension.t
    with type flambda_type := Type_grammar.t
end
