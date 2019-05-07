type 'a or_alias =
  | No_alias of 'a
  | Type of Export_id.t
  | Equals of Simple.t

type t =
  | Value of ty_value
  | Naked_number :
      'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> t
  | Fabricated of ty_fabricated

and flambda_type = t

and ty_value = of_kind_value ty
and 'a ty_naked_number = 'a of_kind_naked_number ty
and ty_fabricated = of_kind_fabricated ty

and 'a ty = 'a unknown_or_join or_alias

(** For each kind there is a lattice of types. *)
and 'a unknown_or_join =
  | Unknown
  (** "Any value can flow to this point": the top element. *)
  | Join of ('a * Name_permutation.t) list
  (** - The list being empty means bottom, the least element: "no value can
        flow to this point".
      - The list containing a single element is the usual case where there
        is no join between incompatible types.
      - If the list contains more than one element:
        A join, between incompatible types, which has been remembered
        in case it is refined by a subsequent meet.  Joins between
        compatible types are immediately pushed down through the top level
        structure of the type.

      The [Name_permutation.t] is a delayed permutation which must be
      pushed down through the structure of the type as it is examined.

      Invariant: every member of a [Join] is incompatible with the other
      members. *)

and of_kind_value =
  | Blocks_and_tagged_immediates of blocks_and_tagged_immediates
  | Boxed_number : _ of_kind_value_boxed_number -> of_kind_value
  | Closures of closures
  | String of String_info.Set.t

and blocks_and_tagged_immediates = {
  immediates : Immediates.t;
  (** Cases for constant constructors (in the case of variants) and
      arbitrary tagged immediates. *)
  blocks : Blocks.t;
  (** Cases for non-constant constructors (in the case of variants) and
      normal blocks. *)
}

(** Boxed integer and floating-point numbers together with the types
    of their contents. *)
and 'a of_kind_value_boxed_number =
  | Boxed_float
      : Float.Set.t ty_naked_number
      -> Float.Set.t ty_naked_number of_kind_value_boxed_number
  | Boxed_int32
      : Int32.Set.t ty_naked_number
      -> Int32.Set.t ty_naked_number of_kind_value_boxed_number
  | Boxed_int64
      : Int64.Set.t ty_naked_number
      -> Int64.Set.t ty_naked_number of_kind_value_boxed_number
  | Boxed_nativeint
      : Targetint.Set.t ty_naked_number
      -> Targetint.Set.t ty_naked_number of_kind_value_boxed_number

(** A function declaration which is inlinable (which in particular implies
    that the code of the function's body is known).  Such declarations are
    completely closed entities in terms of names. *)
and inlinable_function_declaration = {
  function_decl : Term_language_function_declaration.t;
  invariant_params : Variable.Set.t lazy_t;
  size : int option lazy_t;
  (** For functions that are very likely to be inlined, the size of the
      function's body. *)
  direct_call_surrogate : Closure_id.t option;
}

and function_declaration =
  | Non_inlinable
  | Inlinable of inlinable_function_declaration

and closures_entry = {
  function_decl : function_declaration;
  (** Information from the term language about the function declaration
      associated with the closure (call it [C]) described by a
      [closures_entry]. *)
  ty : Function_type.t;
  (** The type of the function associated with [C].
      Note: function parameter types are covariant! *)
  closure_elements : Closure_elements.t;
  (** Relational product describing the variables within a closure and
      equations between them. *)
  set_of_closures : ty_fabricated;
  (** Link back to the type of the set of closures containing [C]. *)
}

and closures = {
  by_closure_id : Closures_entry_by_closure_id.t;
  (** Row-like structure that selects [closures_entry] structures based
      on closure ID and the set of variables in the closure. *)
}

(** Unboxed ("naked") integer and floating-point numbers together with
    any information known about which particular numbers they might be. *)
and 'a of_kind_naked_number =
  | Immediate : Immediate.Set.t -> Immediate.Set.t of_kind_naked_number
  | Float : Float.Set.t -> Float.Set.t of_kind_naked_number
  | Int32 : Int32.Set.t -> Int32.Set.t of_kind_naked_number
  | Int64 : Int64.Set.t -> Int64.Set.t of_kind_naked_number
  | Nativeint : Targetint.Set.t -> Targetint.Set.t of_kind_naked_number

and of_kind_naked_immediate = Immediate.Set.t of_kind_naked_number
and of_kind_naked_float = Float.Set.t of_kind_naked_number
and of_kind_naked_int32 = Int32.Set.t of_kind_naked_number
and of_kind_naked_int64 = Int64.Set.t of_kind_naked_number
and of_kind_naked_nativeint = Targetint.Set.t of_kind_naked_number

and of_kind_fabricated =
  | Discriminants of Discriminants.t
    (** A discriminant is either:
        - a block tag, as returned by the [Get_tag] primitive; or
        - a constant constructor which has undergone a kind-cast to kind
          [Fabricated] using the [Discriminant_of_int] primitive. *)
  | Set_of_closures of set_of_closures
    (** A possibly mutually-recursive collection of closure values, which
        at runtime will be represented by a single block. *)

and set_of_closures_entry = {
  by_closure_id : Types_by_closure_id.t;
  (** Relational product, indexed by individual closure IDs, that (via
      logical variables) describes the makeup of a set of closures. *)
}

and set_of_closures = {
  closures : Closure_ids.t;
  (** Row-like structure that maps _sets_ of [Closure_id.t]s to
      [set_of_closures_entry] structures. *)
}
