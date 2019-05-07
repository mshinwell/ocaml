module Make
  (E : Either_meet_or_join_intf
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension)
  (S : Meet_and_join_spec_intf
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Typing_env_extension := Typing_env_extension) :
sig
  val meet_or_join_ty
     : ?bound_name:Name.t
    -> Join_env.t
    -> S.of_kind_foo Flambda_types.ty
    -> S.of_kind_foo Flambda_types.ty
    -> S.of_kind_foo Flambda_types.ty * Typing_env_extension.t
end
