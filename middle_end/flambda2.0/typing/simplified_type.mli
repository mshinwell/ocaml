
(* Simplified types omit the following at top level:
   - alias information;
   - joins between incompatible types (these turn into "Unknown").
*)
type t = private
  | Value of ty_value
  | Naked_number :
      'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> t
  | Fabricated of ty_fabricated

and ty_value = of_kind_value ty
and 'a ty_naked_number = 'a of_kind_naked_number ty
and ty_fabricated = of_kind_fabricated ty

and 'a ty = private
  | Unknown
  | Ok of 'a * Name_permutation.t
  | Bottom

(* Create a simple type from a type.  If the type has an alias at its
   top level stating that it is the type of some named value, that alias
   is (recursively) expanded, and the final ("canonical") simple value
   returned. *)
val create : (flambda_type -> t * (Simple.t option)) type_accessor

val is_unknown : t -> bool
val is_bottom : t -> bool
