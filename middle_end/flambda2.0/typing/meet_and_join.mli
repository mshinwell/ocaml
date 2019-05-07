module Make
  (E : Either_meet_or_join_intf
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension) :
sig
  val meet_or_join
     : ?bound_name:Name.t
    -> Join_env.t
    -> Flambda_types.t
    -> Flambda_types.t
    -> Flambda_types.t * Typing_env_extension.t
end
