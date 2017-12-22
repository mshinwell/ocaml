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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* CR mshinwell: Add invariant checks, including e.g. on the bodies of
   functions in types. *)

(* CR-someday mshinwell: When disambiguation on GADT constructors works we
   can probably use an existential to combine the "Naked_" kind constructors
   into just one. *)

module type S = sig
  type expr

  type inline_attribute =
    | Always_inline
    | Never_inline
    | Unroll of int
    | Default_inline

  type specialise_attribute =
    | Always_specialise
    | Never_specialise
    | Default_specialise

  type string_contents = private
    (* Known strings are constrained to [Sys.max_string_length] on the machine
       running the compiler. *)
    | Contents of string
    | Unknown_or_mutable

  module String_info : sig
    type t = private {
      contents : string_contents;
      (* CR mshinwell: Enforce the invariant that the [size] really does not
         exceed [Targetint.OCaml.max_string_length] when this structure is
         created. *)
      size : Targetint.OCaml.t;
    }

    include Identifiable.S with type t := t
  end

  type 'a or_alias = private
    | No_alias of 'a
    | Type of Export_id.t
    | Type_of of Name.t

  type 'a or_unknown_length = private
    | Exactly of 'a
    | Unknown_length

  type typing_environment

  (** Values of type [t] are known as "Flambda types".  Each Flambda type
      has a unique kind. *)
  type t = private
    | Value of ty_value
    | Naked_number :
        'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> t
    | Fabricated of ty_fabricated
    | Phantom of ty_phantom

  and flambda_type = t

  (** Types of kind [Value] are equipped with an extra piece of information
      such that when we are at the top element, [Unknown], we still know
      whether a root has to be registered. *)
  and ty_value = (of_kind_value, Flambda_kind.Value_kind.t) ty
  and 'a ty_naked_number = ('a of_kind_naked_number, unit) ty
  and ty_fabricated = (of_kind_fabricated, Flambda_kind.Value_kind.t) ty
  and ty_phantom = (of_kind_phantom, Flambda_kind.Phantom_kind.t) ty

  and ('a, 'u) ty = ('a, 'u) or_unknown_or_bottom or_alias

 (** For each kind there is a lattice of types. *)
  and ('a, 'u) or_unknown_or_bottom = private
    | Unknown of 'u
    (** "Any value can flow to this point": the top element. *)
    | Ok of 'a or_join
    | Bottom
    (** "No value can flow to this point": the bottom element. *)

  and 'a or_join = private
    | Normal of 'a
    | Join of 'a or_join * 'a or_join
      (** A join between two incompatible types which has been remembered
          in case it is refined by a subsequent meet.  Joins between compatible
          types are immediately pushed down through the top level structure
          of the type. *)

  and of_kind_value = private
    | Blocks_and_tagged_immediates of blocks_and_tagged_immediates
    | Boxed_number : _ of_kind_value_boxed_number -> of_kind_value
    | Closure of closures
    | String of String_info.Set.t

  and immediate_case = private {
    env_extension : typing_environment;
  }
 
  and singleton_block = private {
    env_extension : typing_environment;
    first_fields : t array or_unknown_length;
  }

  (* CR mshinwell: Maybe these should also be stored as a binary tree. *)
  and block_case = private
    | Join of singleton_block list

  and blocks_and_tagged_immediates = private {
    immediates : immediate_case Immediate.Or_unknown.Map.t;
    blocks : block_case Tag.Scannable.Map.t;
  }

  and 'a of_kind_value_boxed_number = private
    | Boxed_float
         : Numbers.Float_by_bit_pattern.Set.t ty_naked_number
        -> Numbers.Float_by_bit_pattern.Set.t ty_naked_number
             of_kind_value_boxed_number
    | Boxed_int32
         : Numbers.Int32.Set.t ty_naked_number
        -> Numbers.Int32.Set.t ty_naked_number of_kind_value_boxed_number
    | Boxed_int64
         : Numbers.Int64.Set.t ty_naked_number
        -> Numbers.Int64.Set.t ty_naked_number of_kind_value_boxed_number
    | Boxed_nativeint
         : Targetint.Set.t ty_naked_number
        -> Targetint.Set.t ty_naked_number of_kind_value_boxed_number

  and closures = private {
    (* CR pchambart: should Unknown or Bottom really be allowed here ? *)
    (* XXX what exactly is needed here?  It needs to represent the join *)
    set_of_closures : ty_fabricated;
    closure_id : Closure_id.t;
  }

  and inlinable_function_declaration = private {
    closure_origin : Closure_origin.t;
    continuation_param : Continuation.t;
    (* CR-someday mshinwell: [is_classic_mode] should be changed to use a
       new type which records the combination of inlining (etc) options
       applied to the originating source file. *)
    is_classic_mode : bool;
    (** Whether the file from which this function declaration originated was
        compiled in classic mode. *)
    params : (Parameter.t * t) list;
    body : expr;
    free_names_in_body : Name.Set.t;
    result : t list;
    stub : bool;
    dbg : Debuginfo.t;
    inline : inline_attribute;
    specialise : specialise_attribute;
    is_a_functor : bool;
    (* CR mshinwell: try to change these to [Misc.Stdlib.Set_once.t]?
       (ask xclerc) *)
    invariant_params : Variable.Set.t lazy_t;
    size : int option lazy_t;
    (** For functions that are very likely to be inlined, the size of the
        function's body. *)
    direct_call_surrogate : Closure_id.t option;
  }

  and non_inlinable_function_declaration = private {
    result : t list;
    direct_call_surrogate : Closure_id.t option;
  }

  and function_declaration = private
    | Non_inlinable of non_inlinable_function_declaration
    | Inlinable of inlinable_function_declaration

  and set_of_closures = private {
    set_of_closures_id : Set_of_closures_id.t;
    set_of_closures_origin : Set_of_closures_origin.t;
    function_decls : function_declaration Closure_id.Map.t;
    closure_elements : ty_value Var_within_closure.Map.t;
  }

  and 'a of_kind_naked_number = private
    | Immediate : Immediate.Set.t -> Immediate.Set.t of_kind_naked_number
    | Float : Numbers.Float_by_bit_pattern.Set.t
        -> Numbers.Float_by_bit_pattern.Set.t of_kind_naked_number
    | Int32 : Numbers.Int32.Set.t -> Numbers.Int32.Set.t of_kind_naked_number
    | Int64 : Numbers.Int64.Set.t -> Numbers.Int64.Set.t of_kind_naked_number
    | Nativeint : Targetint.Set.t -> Targetint.Set.t of_kind_naked_number

  and tag_case = private {
    env_extension : typing_environment;
  }

  and of_kind_fabricated = private
    (* CR mshinwell: Note that these should be represented as naked
       immediates *)
    | Tag of tag_case Tag.Map.t
    | Set_of_closures of set_of_closures

  and of_kind_phantom = private
    | Value of ty_value
    | Naked_number
         : 'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t
        -> of_kind_phantom
    | Fabricated of ty_fabricated

  (** If the given type has kind [Phantom], return it; otherwise form the
      correct type of kind [Phantom] describing the given type. *)
  val phantomize : t -> t

(*
  val block_case_known_size
     : env_extension:typing_environment
    -> fields:ty_value array
    -> block_case

  val block_case_size_possibly_longer
     : env_extension:typing_environment
    -> first_fields:ty_value array
    -> block_case

  val block
     : tag:Simple.t
    -> block_case
    -> t

  val blocks
     : tag:Simple.t
    -> tags_to_block_cases:block_case Tag.Scannable.Map.t
    -> t

  val float_array_size_possibly_longer
     : first_fields:ty_naked_float array
    -> t

  val possible_tags : (t -> Tag.Set.t) type_accessor
*)

  module Typing_environment : sig
    type t = typing_environment

    val print : Format.formatter -> t -> unit

(*
    val create : unit -> t

    val add : t -> Name.t -> Scope_level.t -> flambda_type -> t

    type binding_type = Normal | Existential

    val find : t -> Name.t -> t * binding_type

    val cut
       : t
      -> minimum_scope_level_to_be_existential:Scope_level.t
      -> t

    val join : (t -> t -> t) type_accessor

    val meet : (t -> t -> t) type_accessor
*)
  end

  val print : Format.formatter -> t -> unit

  val print_ty_value : Format.formatter -> ty_value -> unit

  val print_ty_value_array : Format.formatter -> ty_value array -> unit

  val print_inlinable_function_declaration
     : Format.formatter
    -> inlinable_function_declaration
    -> unit


  (** Construction of top types. *)
  val unknown : Flambda_kind.t -> t

  val any_value : Flambda_kind.Value_kind.t -> t

  val any_value_as_ty_value
     : Flambda_kind.Value_kind.t
    -> ty_value

(*
  val any_tagged_immediate : unit -> t
*)

  val any_boxed_float : unit -> t
  val any_boxed_int32 : unit -> t
  val any_boxed_int64 : unit -> t
  val any_boxed_nativeint : unit -> t

  val any_naked_immediate : unit -> t
  val any_naked_float : unit -> t
  val any_naked_int32 : unit -> t
  val any_naked_int64 : unit -> t
  val any_naked_nativeint : unit -> t

  val any_fabricated : unit -> t
  val any_phantom : unit -> t

(*
  val any_naked_float_as_ty_naked_float : unit -> ty_naked_float
*)

  (** Building of types representing tagged / boxed values from specified
      constants. *)
  val this_tagged_immediate : Immediate.t -> t
  val these_tagged_immediates : Immediate.Set.t -> t
  val this_boxed_float : Numbers.Float_by_bit_pattern.t -> t
  val these_boxed_floats : Numbers.Float_by_bit_pattern.Set.t -> t
  val this_boxed_int32 : Int32.t -> t
  val these_boxed_int32s : Numbers.Int32.Set.t -> t
  val this_boxed_int64 : Int64.t -> t
  val these_boxed_int64s : Numbers.Int64.Set.t -> t
  val this_boxed_nativeint : Targetint.t -> t
  val these_boxed_nativeints : Targetint.Set.t -> t
  val this_immutable_string : string -> t

(*
  val this_immutable_float_array : Numbers.Float_by_bit_pattern.t array -> t
*)

  (** Building of types representing untagged / unboxed values from
      specified constants. *)
  val this_naked_immediate : Immediate.t -> t
  val this_naked_float : Numbers.Float_by_bit_pattern.t -> t
  val these_naked_floats : Numbers.Float_by_bit_pattern.Set.t -> t
  val this_naked_int32 : Int32.t -> t
  val these_naked_int32s : Numbers.Int32.Set.t -> t
  val this_naked_int64 : Int64.t -> t
  val these_naked_int64s : Numbers.Int64.Set.t -> t
  val this_naked_nativeint : Targetint.t -> t
  val these_naked_nativeints : Targetint.Set.t -> t

  (** Building of types corresponding to immutable values given only the
      size of such values. *)
  val immutable_string : size:Targetint.OCaml.t -> t

  (** Building of types corresponding to mutable values. *)
  val mutable_string : size:Targetint.OCaml.t -> t

(*
  val mutable_float_array : size:Targetint.OCaml.t -> t
  val mutable_float_arrays_of_various_sizes : sizes:Targetint.OCaml.Set.t -> t
*)

  (** Building of types corresponding to values that did not exist at
      source level. *)
  val these_tags : typing_environment Tag.Map.t -> t

(*

  (** Building of types from other types.  These functions will fail with
      a fatal error if the supplied type is not of the correct kind. *)
  (* XXX maybe we should change all of these to the "ty_..." variants, so
     we can avoid the exception case *)
  val tag_immediate : t -> t
*)
  val box_float : t -> t
  val box_int32 : t -> t
  val box_int64 : t -> t
  val box_nativeint : t -> t
(*
  val immutable_float_array : ty_naked_float array -> t

  val block
     : typing_environment
    -> tag:ty_fabricated
    -> fields:ty_value array
    -> t
*)

  (** The bottom type for the given kind ("no value can flow to this point"). *)
  val bottom : Flambda_kind.t -> t

  val create_inlinable_function_declaration
     : is_classic_mode:bool
    -> closure_origin:Closure_origin.t
    -> continuation_param:Continuation.t
    -> params:(Parameter.t * t) list
    -> body:expr
    -> result:t list
    -> stub:bool
    -> dbg:Debuginfo.t
    -> inline:inline_attribute
    -> specialise:specialise_attribute
    -> is_a_functor:bool
    -> invariant_params:Variable.Set.t lazy_t
    -> size:int option lazy_t
    -> direct_call_surrogate:Closure_id.t option
    -> inlinable_function_declaration

  val create_non_inlinable_function_declaration
     : result:t list
    -> direct_call_surrogate:Closure_id.t option
    -> non_inlinable_function_declaration

  val closure : set_of_closures:t -> Closure_id.t -> t

  val create_set_of_closures
     : set_of_closures_id:Set_of_closures_id.t
    -> set_of_closures_origin:Set_of_closures_origin.t
    -> function_decls:function_declaration Closure_id.Map.t
    -> closure_elements:ty_value Var_within_closure.Map.t
    -> set_of_closures

  val set_of_closures
     : set_of_closures_id:Set_of_closures_id.t
    -> set_of_closures_origin:Set_of_closures_origin.t
    -> function_decls:function_declaration Closure_id.Map.t
    -> closure_elements:ty_value Var_within_closure.Map.t
    -> t

  (** Construct a type equal to the type of the given name.  (The name
      must be present in the given environment when calling e.g. [join].) *)
  val alias_type_of : Flambda_kind.t -> Name.t -> t

  val alias_type : Flambda_kind.t -> Export_id.t -> t

  (** Free names in a type. *)
  val free_names : t -> Name.Set.t

  module Name_or_export_id : sig
    type t = private
      | Name of Name.t
      | Export_id of Export_id.t

    include Identifiable.S with type t := t
  end

  (** Annotation for functions that may require examination of the current
      simplification environment. *)
  type 'a type_accessor = type_of_name:(Name_or_export_id.t -> t option) -> 'a

  (** Determine the (unique) kind of a type. *)
  val kind : (t -> Flambda_kind.t) type_accessor

  (** Given a type known to be of kind [Value], determine the corresponding
      value kind. *)
  val value_kind_ty_value
     : (ty_value -> Flambda_kind.Value_kind.t) type_accessor

(*
  (** Least upper bound of two types. *)
  val join : (t -> t -> t) type_accessor

  (** Least upper bound of an arbitrary number of types. *)
  val join_list : (Flambda_kind.t -> t list -> t) type_accessor

  (** Least upper bound of two types known to be of kind [Value]. *)
  val join_ty_value : (ty_value -> ty_value -> ty_value) type_accessor

  (** Least upper bound of two types known to be of kind [Naked_float]. *)
  val join_ty_naked_float
     : (ty_naked_float -> ty_naked_float -> ty_naked_float) type_accessor

  (** Least upper bound of two types known to be of kind [Naked_int32]. *)
  val join_ty_naked_int32
     : (ty_naked_int32 -> ty_naked_int32 -> ty_naked_int32) type_accessor

  (** Least upper bound of two types known to be of kind [Naked_int64]. *)
  val join_ty_naked_int64
     : (ty_naked_int64 -> ty_naked_int64 -> ty_naked_int64) type_accessor

  (** Least upper bound of two types known to be of kind [Naked_nativeint]. *)
  val join_ty_naked_nativeint
     : (ty_naked_nativeint -> ty_naked_nativeint -> ty_naked_nativeint)
         type_accessor

  (** Greatest lower bound of two types.
      When meeting types of kind [Value] this can introduce new judgements
      into the typing context. *)
  val meet : (typing_environment -> t -> t -> typing_environment * t) type_accessor

  (** Greatest lower bound of an arbitrary number of types. *)
  val meet_list
     : (typing_environment
     -> Flambda_kind.t
     -> t list
     -> typing_environment * t) type_accessor

  (** Greatest lower bound of two types known to be of kind [Value]. *)
  val meet_ty_value
     : (typing_environment * ty_value
    -> ty_value
    -> typing_environment * ty_value) type_accessor

  (** Greatest lower bound of two types known to be of kind [Naked_float]. *)
  val meet_ty_naked_float
     : (ty_naked_float -> ty_naked_float -> ty_naked_float) type_accessor

  (** Greatest lower bound of two types known to be of kind [Naked_int32]. *)
  val meet_ty_naked_int32
     : (ty_naked_int32 -> ty_naked_int32 -> ty_naked_int32) type_accessor

  (** Greatest lower bound of two types known to be of kind [Naked_int64]. *)
  val meet_ty_naked_int64
     : (ty_naked_int64 -> ty_naked_int64 -> ty_naked_int64) type_accessor

  (** Greatest lower bound of two types known to be of kind
      [Naked_nativeint]. *)
  val meet_ty_naked_nativeint
     : (ty_naked_nativeint -> ty_naked_nativeint -> ty_naked_nativeint)
         type_accessor
*)

  (* CR mshinwell: We may not need to expose all of the following functions *)

  (** Follow chains of [Alias]es until either a [No_alias] type is reached
      or a name cannot be resolved.

      This function also returns the "canonical name" for the given type:
      the furthest-away [Name.t] in any chain of aliases leading from the given
      type.  (The chain may also involve [Export_id.t] links either before or
      after any returned canonical name.) *)
  val resolve_aliases : (t -> t * (Name.t option)) type_accessor

  (** Like [resolve_aliases], but unresolved names at the top level are
      changed into [Unknown]s. *)
  val resolve_aliases_and_squash_unresolved_names
     : (t -> t * (Name.t option)) type_accessor

  (** Like [resolve_aliases], but for use when you have a [ty], not a [t]. *)
  val resolve_aliases_on_ty
     : (force_to_kind:(t -> ('a, 'b) ty)
    -> ('a, 'b) ty
    -> ('a, 'b) ty * (Name.t option)) type_accessor

  (** Like [resolve_aliases_on_ty], but unresolved names at the top level are
      changed into [Unknown]s (with payloads given by [unknown_payload]). *)
  val resolve_aliases_and_squash_unresolved_names_on_ty
     : (force_to_kind:(t -> ('a, 'b) ty)
    -> unknown_payload:'b
    -> ('a, 'b) ty
    -> ('a, 'b) or_unknown_or_bottom * (Name.t option)) type_accessor

  val force_to_kind_value : t -> ty_value

  val force_to_kind_naked_number
     : 'kind Flambda_kind.Naked_number.t
    -> t
    -> 'kind ty_naked_number

  val force_to_kind_fabricated : t -> ty_fabricated

(*

  val t_of_ty_value : ty_value -> t

  val t_of_ty_naked_float : ty_naked_float -> t

(*
  type cleaning_spec =
    | Available
    | Available_different_name of Variable.t
    | Unavailable

  (** Adjust a type so that all of the free variables it references are in
      scope in some context. The context is expressed by a function that says
      whether the variable is available under its existing name, available
      under another name, or unavailable. *)
  val clean : (t -> (Variable.t -> cleaning_spec) -> t) type_accessor
*)

  val combination_component_to_ty
     : 'a singleton_or_combination or_alias
    -> ('a, _) ty

  module Closure : sig
    type t = closure

    val meet_lists : (t list -> t list -> t list) type_accessor

    val print : Format.formatter -> t -> unit
  end

  module Set_of_closures : sig
    type t = set_of_closures

    val meet_lists : (t list -> t list -> t list) type_accessor

    val print : Format.formatter -> t -> unit
  end
*)

end
