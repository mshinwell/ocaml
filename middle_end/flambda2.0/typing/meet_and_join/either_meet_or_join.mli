module For_meet : Either_meet_or_join_intf.S
  with module Join_env := Join_env
  with module Meet_env := Meet_env
  with module Typing_env_extension := Typing_env_extension

module For_join : Either_meet_or_join_intf.S
  with module Join_env := Join_env
  with module Meet_env := Meet_env
  with module Typing_env_extension := Typing_env_extension
