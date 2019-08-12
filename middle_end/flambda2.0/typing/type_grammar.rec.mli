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

type 'a or_alias =
  | No_alias of 'a
  | Type of Export_id.t
  | Equals of Simple.t

type t =
  | Value of ty_value
  | Naked_number : 'k ty_naked_number * 'k Flambda_kind.Naked_number.t -> t
  | Fabricated of ty_fabricated

and flambda_type = t

and ty_value = of_kind_value ty
and 'a ty_naked_number = 'a of_kind_naked_number ty
and ty_fabricated = of_kind_fabricated ty

and 'a ty = 'a unknown_or_join or_alias

(** For each kind there is a lattice of types.
    Unknown = "Any value can flow to this point": the top element.
 *)
and 'a unknown_or_join = 'a Or_unknown_or_bottom.t

and resolved =
  | Const of Reg_width_const.t
  | Discriminant of Discriminant.t
  | Resolved of resolved_t

(* CR mshinwell: Put this in its own submodule *)
and resolved_t =
  | Resolved_value of resolved_ty_value
  | Resolved_naked_number
       : 'k resolved_ty_naked_number * 'k Flambda_kind.Naked_number.t
      -> resolved_t
  | Resolved_fabricated of resolved_ty_fabricated

and resolved_ty_value = of_kind_value unknown_or_join
and 'a resolved_ty_naked_number = 'a of_kind_naked_number unknown_or_join
and resolved_ty_fabricated = of_kind_fabricated unknown_or_join

and of_kind_value =
  | Blocks_and_tagged_immediates of blocks_and_tagged_immediates
  | Boxed_number : _ of_kind_value_boxed_number -> of_kind_value
  | Closures of closures
  | String of String_info.Set.t  (* CR mshinwell: String -> Strings *)
  | Array of { length : ty_value; }

(* CR mshinwell: Document why e.g. Blocks_and_immediates has Or_unknown yet
   the Discriminants constructor does not.  Also how Unknown and Bottom are
   always bubbled up to the top (although this isn't required for
   correctness).
   Are we losing useful information by not having e.g. Discriminants Unknown?
*)

(* CR mshinwell: Add invariant checks that these are not both bottom and not
   both Unknown. *)
and blocks_and_tagged_immediates = {
  immediates : Immediates.t Or_unknown.t;
  (** Cases for constant constructors (in the case of variants) and
      arbitrary tagged immediates. *)
  blocks : Blocks.t Or_unknown.t;
  (** Cases for non-constant constructors (in the case of variants) and
      normal blocks. *)
}

(** Boxed integer and floating-point numbers. *)
and 'a of_kind_value_boxed_number =
  | Boxed_float
       : Flambda_kind.naked_float ty_naked_number
      -> Flambda_kind.naked_float of_kind_value_boxed_number
  | Boxed_int32
       : Flambda_kind.naked_int32 ty_naked_number
      -> Flambda_kind.naked_int32 of_kind_value_boxed_number
  | Boxed_int64
       : Flambda_kind.naked_int64 ty_naked_number
      -> Flambda_kind.naked_int64 of_kind_value_boxed_number
  | Boxed_nativeint
       : Flambda_kind.naked_nativeint ty_naked_number
      -> Flambda_kind.naked_nativeint of_kind_value_boxed_number

(** A function declaration which is inlinable (which in particular implies that
    the code of the function's body is known). Such declarations are completely
    closed entities in terms of names. *)
and inlinable_function_declaration = {
  function_decl : Term_language_function_declaration.t;
}

and function_declaration =
  | Non_inlinable of {
      param_arity : Flambda_arity.t;
      result_arity : Flambda_arity.t;
      recursive : Recursive.t;
    }
  | Inlinable of inlinable_function_declaration

and closures_entry = {
  irrelevant_closure_vars : Variable.t Closure_id.Map.t;
  (** Irrelevant variables bound to the types of the individual closures. *)
  function_decls : function_declaration Or_unknown.t Closure_id.Map.t;
  (** Information from the term language, if available, about the function
      declaration(s) associated with the closures. *)
  closure_types : Types_by_closure_id.t;
  (** Product, indexed by individual closure IDs, that describes the closures
      within a set of closures. *)
  closure_var_types : Types_by_var_within_closure.t;
  (** Product describing the variables within a set of closures. *)
}

and closures = {
  by_closure_id : Closures_entry_by_set_of_closures_contents.t;
  (** Row-like structure that selects [closures_entry] structures based
      on closure ID and the set of variables and closures in the corresponding
      set of closures. *)
}

(** Unboxed ("naked") integer and floating-point numbers. *)
and 'a of_kind_naked_number =
  | Immediate
       : Immediate.Set.t
      -> Flambda_kind.naked_immediate of_kind_naked_number
  | Float
       : Float.Set.t
      -> Flambda_kind.naked_float of_kind_naked_number
  | Int32
       : Int32.Set.t
      -> Flambda_kind.naked_int32 of_kind_naked_number
  | Int64
       : Int64.Set.t
      -> Flambda_kind.naked_int64 of_kind_naked_number
  | Nativeint
       : Targetint.Set.t
      -> Flambda_kind.naked_nativeint of_kind_naked_number

(** Convenience aliases. *)
and of_kind_naked_immediate = Flambda_kind.naked_immediate of_kind_naked_number
and of_kind_naked_float = Flambda_kind.naked_float of_kind_naked_number
and of_kind_naked_int32 = Flambda_kind.naked_int32 of_kind_naked_number
and of_kind_naked_int64 = Flambda_kind.naked_int64 of_kind_naked_number
and of_kind_naked_nativeint = Flambda_kind.naked_nativeint of_kind_naked_number

(** Types of kind [Fabricated].  No source-level values are of this kind. *)
and of_kind_fabricated =
  (* CR mshinwell: Work out how to do the dependent-discriminant thing
     properly. *)
  | Discriminants of Discriminants.t
    (* CR mshinwell: update comment.  It's now either a tag, or the
       result of Pisint *)
    (** A discriminant is either:
        - a block tag, as returned by the [Get_tag] primitive; or
        - a constant constructor which has undergone a kind-cast to kind
          [Fabricated] using the [Discriminant_of_int] primitive. *)
  | Rec_info of Rec_info.t
