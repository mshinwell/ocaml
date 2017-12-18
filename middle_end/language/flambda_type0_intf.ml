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

  type unresolved_value =
    | Set_of_closures_id of Set_of_closures_id.t
    | Export_id of Export_id.t
    | Name of Name.t

  type unknown_because_of =
    | Unresolved_value of unresolved_value
    | Other

  type load_lazily =
    | Export_id of Export_id.t
    | Symbol of Symbol.t

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
    | Normal of 'a * typing_environment
    | Alias of Name.t

  (* CR-someday mshinwell / lwhite: Types in ANF form? *)

  type combining_op = Union | Intersection

  type 'a or_unknown_length =
    | Exactly of 'a
    | Unknown_length

  type type_environment

  (** Values of type [t] are known as "Flambda types".  Each Flambda type
      has a unique kind.

      Flambda types may be loaded lazily from .cmx files.  In some cases they
      may be formed into union types. *)
  type t = private
    | Value of ty_value
    | Naked_immediate of ty_naked_immediate
    | Naked_float of ty_naked_float
    | Naked_int32 of ty_naked_int32
    | Naked_int64 of ty_naked_int64
    | Naked_nativeint of ty_naked_nativeint
    | Fabricated of ty_fabricated
    | Phantom of ty_phantom

  and flambda_type = t

  (** Types of kind [Value] are equipped with an extra piece of information
      such that when we are at the top element, [Unknown], we still know
      whether a root has to be registered. *)
  and ty_value = (of_kind_value, Flambda_kind.Value_kind.t) ty
  and ty_naked_immediate = (of_kind_naked_immediate, unit) ty
  and ty_naked_float = (of_kind_naked_float, unit) ty
  and ty_naked_int32 = (of_kind_naked_int32, unit) ty
  and ty_naked_int64 = (of_kind_naked_int64, unit) ty
  and ty_naked_nativeint = (of_kind_naked_nativeint, unit) ty
  and ty_fabricated = (of_kind_fabricated, unit) ty
  and ty_phantom = (of_kind_phantom, unit) ty

  and ('a, 'u) ty = ('a, 'u) maybe_unresolved or_alias

  (* CR mshinwell: It's not quite clear to me that the extra complexity
     introduced by having this static "resolved" distinction is worth it. *)
  and resolved_t = private
    | Value of resolved_ty_value
    | Naked_immediate of resolved_ty_naked_immediate
    | Naked_float of resolved_ty_naked_float
    | Naked_int32 of resolved_ty_naked_int32
    | Naked_int64 of resolved_ty_naked_int64
    | Naked_nativeint of resolved_ty_naked_nativeint
    | Fabricated of resolved_ty_fabricated
    | Phantom of resolved_ty_phantom

  and resolved_ty_value = (of_kind_value, Flambda_kind.Value_kind.t) resolved_ty
  and resolved_ty_naked_immediate = (of_kind_naked_immediate, unit) resolved_ty
  and resolved_ty_naked_float = (of_kind_naked_float, unit) resolved_ty
  and resolved_ty_naked_int32 = (of_kind_naked_int32, unit) resolved_ty
  and resolved_ty_naked_int64 = (of_kind_naked_int64, unit) resolved_ty
  and resolved_ty_naked_nativeint = (of_kind_naked_nativeint, unit) resolved_ty
  and resolved_ty_naked_fabricated =
    (of_kind_naked_fabricated, unit) resolved_ty
  and resolved_ty_phantom = (of_kind_phantom, unit) resolved_ty

  and ('a, 'u) resolved_ty = ('a, 'u) or_unknown_or_bottom or_alias

  and ('a, 'u) maybe_unresolved = private
    | Resolved of ('a, 'u) or_unknown_or_bottom
    (** The head constructor is available in memory. *)
    | Load_lazily of load_lazily
    (** The head constructor requires loading from a .cmx file. *)

  (** For each kind (cf. [Flambda_kind], although with the "Value" cases
      merged into one) there is a lattice of types. *)
  and ('a, 'u) or_unknown_or_bottom = private
    | Unknown of unknown_because_of * 'u
    (** "Any value can flow to this point": the top element. *)
    | Ok of 'a singleton_or_union
    | Bottom
    (** "No value can flow to this point": the bottom element. *)

  (** Note: [Singleton] refers to the structure of the type.  A [Singleton]
      type may still describe more than one particular runtime value (for
      example, it may describe a boxed float whose contents is unknown). *)
  and 'a singleton_or_union = private
    | Singleton of 'a
    | Join of 'a singleton_or_union or_alias * 'a singleton_or_union or_alias

  and of_kind_value = private
    | Tagged_immediate of ty_naked_immediate
    | Boxed_float of ty_naked_float
    | Boxed_int32 of ty_naked_int32
    | Boxed_int64 of ty_naked_int64
    | Boxed_nativeint of ty_naked_nativeint
    | Blocks of {
        tag : Simple.t or_unknown;
        cases : block_case Tag.Scannable.Map.t;
      }
    | Set_of_closures of set_of_closures
    | Closure of closure
    | String of String_info.t
    | Float_array of ty_naked_float array or_unknown_length

  and block_case = private
    { env_extension : typing_environment;
      fields : ty_value array or_unknown_length;
    }

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

let meet ... =
  ...
  | Block { env = env1; tag = tag1; fields = fields1; },
      Block { env = env2; tag = tag2; fields = fields2; } ->
    if Array.length fields1 <> Array.length fields2 then
      Combine
    else
      let env = Typing_environment.meet ~importer ~type_of_name env1 env2 in
      let tag = meet_ty_fabricated ~importer ~type_of_name tag1 tag2 in
      let fields =
        Array.map2 (fun field1 field2 ->
            meet_ty_value ~importer ~type_of_name field1 field2)
          fields1 fields2
      in
      singleton (Block { env; tag; fields; })

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

  and function_declaration =
    | Non_inlinable of non_inlinable_function_declaration
    | Inlinable of inlinable_function_declaration

  and set_of_closures = private {
    set_of_closures_id : Set_of_closures_id.t;
    set_of_closures_origin : Set_of_closures_origin.t;
    function_decls : function_declaration Closure_id.Map.t;
    closure_elements : ty_value Var_within_closure.Map.t;
  }

  and closure = private {
    (* CR pchambart: should Unknown or Bottom really be allowed here ? *)
    set_of_closures : ty_value;
    closure_id : Closure_id.t;
  }

  and of_kind_naked_immediate = private
    | Naked_immediate of Immediate.t

  and of_kind_naked_float = private
    | Naked_float of Numbers.Float_by_bit_pattern.t

  and of_kind_naked_int32 = private
    | Naked_int32 of Int32.t

  and of_kind_naked_int64 = private
    | Naked_int64 of Int64.t

  and of_kind_naked_nativeint = private
    | Naked_nativeint of Targetint.t

  and of_kind_fabricated = private
    | Tag of Tag.Scannable.t
    | Dependent_tag of Name.t
    | Set_of_closures of set_of_closures

  and of_kind_phantom = private
    | Value of ty_kind_value
    | Naked_immediate of ty_kind_naked_immediate
    | Naked_float of ty_kind_naked_float
    | Naked_int32 of ty_kind_naked_int32
    | Naked_int64 of ty_kind_naked_int64
    | Naked_nativeint of ty_kind_naked_nativeint
    | Fabricated_pointer of ty_kind_fabricated_pointer

  module Type_environment : sig
    type t = type_environment

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
  end

  val print : Format.formatter -> t -> unit

  val print_ty_value : Format.formatter -> ty_value -> unit

  val print_ty_value_array : Format.formatter -> ty_value array -> unit

  val print_inlinable_function_declaration
     : Format.formatter
    -> inlinable_function_declaration
    -> unit

  (** Construction of top types. *)
  val unknown : Flambda_kind.t -> unknown_because_of -> t
  val any_value : Flambda_kind.Value_kind.t -> unknown_because_of -> t
  val any_value_as_ty_value
     : Flambda_kind.Value_kind.t
    -> unknown_because_of
    -> ty_value
  val any_tagged_immediate : unit -> t
  val any_boxed_float : unit -> t
  val any_boxed_int32 : unit -> t
  val any_boxed_int64 : unit -> t
  val any_boxed_nativeint : unit -> t
  val any_naked_immediate : unit -> t
  val any_naked_float : unit -> t
  val any_naked_float_as_ty_naked_float : unit -> ty_naked_float
  val any_naked_int32 : unit -> t
  val any_naked_int64 : unit -> t
  val any_naked_nativeint : unit -> t

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
  val this_immutable_float_array : Numbers.Float_by_bit_pattern.t array -> t

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
  val mutable_float_array : size:Targetint.OCaml.t -> t
  val mutable_float_arrays_of_various_sizes : sizes:Targetint.OCaml.Set.t -> t

  (** Building of types corresponding to values that did not exist at
      source level. *)
  val these_tags : Tag.Set.t -> t

  (** Building of types from other types.  These functions will fail with
      a fatal error if the supplied type is not of the correct kind. *)
  (* XXX maybe we should change all of these to the "ty_..." variants, so
     we can avoid the exception case *)
  val tag_immediate : t -> t
  val box_float : t -> t
  val box_int32 : t -> t
  val box_int64 : t -> t
  val box_nativeint : t -> t
  val immutable_float_array : ty_naked_float array -> t

  val block
     : type_environment
    -> tag:ty_fabricated
    -> fields:ty_value array
    -> t

  (** The bottom type for the given kind ("no value can flow to this point"). *)
  val bottom : Flambda_kind.t -> t

  (** Construction of types that link to other types which have not yet
      been loaded into memory (from a .cmx file). *)
  val export_id_loaded_lazily : Flambda_kind.t -> Export_id.t -> t
  val symbol_loaded_lazily : Symbol.t -> t

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
  val alias : Flambda_kind.t -> Name.t -> t

  (** Free names in a type. *)
  val free_names : t -> Name.Set.t

  (** A module type comprising operations for importing types from .cmx files.
      These operations are derived from the functions supplied to the
      [Make_backend] functor, below.  A first class module of this type has
      to be passed to various operations that destruct types. *)
  module type Importer = sig
    val import_value_type_as_resolved_ty_value
       : ty_value
      -> resolved_ty_value

    val import_naked_immediate_type_as_resolved_ty_naked_immediate
       : ty_naked_immediate
      -> resolved_ty_naked_immediate

    val import_naked_float_type_as_resolved_ty_naked_float
       : ty_naked_float
      -> resolved_ty_naked_float

    val import_naked_int32_type_as_resolved_ty_naked_int32
       : ty_naked_int32
      -> resolved_ty_naked_int32

    val import_naked_int64_type_as_resolved_ty_naked_int64
       : ty_naked_int64
      -> resolved_ty_naked_int64

    val import_naked_nativeint_type_as_resolved_ty_naked_nativeint
       : ty_naked_nativeint
      -> resolved_ty_naked_nativeint

    (* CR mshinwell: Are these next ones needed? *)
    val import_value_type : ty_value -> resolved_t
    val import_naked_immediate_type : ty_naked_immediate -> resolved_t
    val import_naked_float_type : ty_naked_float -> resolved_t
    val import_naked_int32_type : ty_naked_int32 -> resolved_t
    val import_naked_int64_type : ty_naked_int64 -> resolved_t
    val import_naked_nativeint_type : ty_naked_nativeint -> resolved_t
  end

  module type Importer_intf = sig
    (** Return the type stored on disk under the given export identifier, or
        [None] if no such type can be loaded.  This function should not attempt
        to resolve export IDs or symbols recursively in the event that the
        type on disk is another [Load_lazily].  (This will be performed
        automatically by the implementation of this functor.) *)
    val import_export_id : Export_id.t -> t option

    (** As for [import_export_id], except that the desired type is specified by
        symbol, rather than by export identifier. *)
    val import_symbol : Symbol.t -> t option
  end

  (** A functor used to construct the various type-importing operations from
      straightforward backend-provided ones. *)
  module Make_importer (S : Importer_intf) : Importer

  (** An [Importer] that does nothing. *)
  val null_importer : (module Importer)

  (** Annotation for functions that may require the importing of types from
      .cmx files or the examination of the current simplification
      environment. *)
  type 'a type_accessor =
       importer:(module Importer)
    -> type_of_name:(Name.t -> t option)
    -> 'a

  (** Annotation for functions that may require the importing of types from
      .cmx files (but not the examination of the current simplification
      environment). *)
  type 'a with_importer =
       importer:(module Importer)
    -> 'a

  (** Determine the (unique) kind of a type. *)
  val kind : (t -> Flambda_kind.t) type_accessor

  (** Given a type known to be of kind [Value], determine the corresponding
      value kind. *)
  val value_kind : (ty_value -> Flambda_kind.Value_kind.t) type_accessor

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
  val meet : (type_environment -> t -> t -> type_environment * t) type_accessor

  (** Greatest lower bound of an arbitrary number of types. *)
  val meet_list
     : (type_environment
     -> Flambda_kind.t
     -> t list
     -> type_environment * t) type_accessor

  (** Greatest lower bound of two types known to be of kind [Value]. *)
  val meet_ty_value
     : (type_environment * ty_value
    -> ty_value
    -> type_environment * ty_value) type_accessor

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

  (** Follow chains of [Alias]es, loading .cmx files as necessary, until
      either a [Normal] type is reached or a name cannot be resolved.

      This function also returns the "canonical name" for the given type.
      Canonical names are stated with reference to the input type [t] given
      to this function.  There are three cases:

      1. The returned type is [Normal]; following aliases from [t] it is
         pointed at by an [Alias].  The canonical name is the name given in
         that [Alias].

      2. The returned type is [Normal]; following aliases from [t] it is not
         pointed at by an [Alias].  There is no canonical name.

      3. The returned type is [Alias] due to an unresolved name.  That name is
         the canonical name. *)
  val resolve_aliases : (t -> t * (Name.t option)) type_accessor

  (** Like [resolve_aliases], but for use when you have a [ty], not a [t]. *)
  val resolve_aliases_on_ty
     : importer_this_kind:(('a, 'b) ty -> ('a, 'b) resolved_ty)
    -> force_to_kind:(t -> ('a, 'b) ty)
    -> type_of_name:(Name.t -> t option)
    -> ('a, 'b) ty
    -> ('a, 'b) resolved_ty * (Name.t option)

  (** Like [resolve_aliases_on_ty], but unresolved names are changed into
      an [Unknown] (with payload given by [unknown_payload]). *)
  val resolve_aliases_and_squash_unresolved_names_on_ty
     : importer_this_kind:(('a, 'b) ty -> ('a, 'b) resolved_ty)
    -> force_to_kind:(t -> ('a, 'b) ty)
    -> type_of_name:(Name.t -> t option)
    -> unknown_payload:'b
    -> ('a, 'b) ty
    -> ('a, 'b) or_unknown_or_bottom * (Name.t option)

  val force_to_kind_value : t -> ty_value

  val force_to_kind_naked_immediate : t -> ty_naked_immediate

  val force_to_kind_naked_float : t -> ty_naked_float

  val force_to_kind_naked_int32 : t -> ty_naked_int32

  val force_to_kind_naked_int64 : t -> ty_naked_int64

  val force_to_kind_naked_nativeint : t -> ty_naked_nativeint

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


end
