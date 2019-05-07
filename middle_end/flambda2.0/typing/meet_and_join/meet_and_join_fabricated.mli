module Make
  (E : Either_meet_or_join_intf
    with module Join_env := Join_env
    with module Meet_env := Meet_env
    with module Typing_env_extension := Typing_env_extension) :
sig
  include Meet_and_join_spec_intf
    with module Flambda_types := Flambda_types
    with module Join_env := Join_env
    with module Typing_env_extension := Typing_env_extension
    with type of_kind_foo = Flambda_types.of_kind_fabricated

  val meet_or_join_set_of_closures_entry
     : Join_env.t
    -> Flambda_types.set_of_closures_entry
    -> Flambda_types.set_of_closures_entry
    -> (Flambda_types.set_of_closures_entry * Typing_env_extension.t)
         Or_absorbing.t
end
