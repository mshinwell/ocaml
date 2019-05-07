
type 'a normal_ty = 'a ty

type t =
  | Value of ty_value
  | Naked_number :
      'kind ty_naked_number * 'kind Flambda_kind.Naked_number.t -> t
  | Fabricated of ty_fabricated

and ty_value = of_kind_value ty
and 'a ty_naked_number = 'a of_kind_naked_number ty
and ty_fabricated = of_kind_fabricated ty

and 'a ty =
  | Unknown
  | Ok of 'a * Name_permutation.t
  | Bottom

let is_unknown t =
  match t with
  | Value Unknown -> true
  | Naked_number (Unknown, _) -> true
  | Fabricated Unknown -> true
  | _ -> false

let is_bottom t =
  match t with
  | Value Bottom -> true
  | Naked_number (Bottom, _) -> true
  | Fabricated Bottom -> true
  | _ -> false

let ty_from_ty (ty : _ normal_ty) : _ ty =
  match ty with
  | Type _ | Equals _ -> Unknown
  | No_alias unknown_or_join ->
    match unknown_or_join with
    | Unknown -> Unknown
    | Join [] -> Bottom
    | Join [of_kind_foo, perm] -> Ok (of_kind_foo, perm)
    | Join _ -> Unknown

let create env (t : flambda_type) : t * (Simple.t option) =
  let t, canonical_simple = Typing_env.resolve_aliases env t in
  let t : t =
    match t with
    | Value ty_value ->
      let ty_value : ty_value = ty_from_ty ty_value in
      Value ty_value
    | Naked_number (ty_naked_number, kind) ->
      let ty_naked_number : _ ty_naked_number =
        ty_from_ty ty_naked_number
      in
      Naked_number (ty_naked_number, kind)
    | Fabricated ty_fabricated ->
      let ty_fabricated : ty_fabricated = ty_from_ty ty_fabricated in
      Fabricated ty_fabricated
  in
  t, canonical_simple
