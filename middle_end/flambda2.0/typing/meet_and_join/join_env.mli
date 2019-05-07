type t

(** Perform various invariant checks upon the given join environment. *)
val invariant : t -> unit

val print : Format.formatter -> t -> unit

val create : Meet_env.t -> t

val add_definition_central_environment
   : t
  -> Name.t
  -> Flambda_types.t
  -> t

(* CR mshinwell: Rename to [add_or_meet_extensions] *)
val add_extensions
   : t
  -> holds_on_left:Typing_env_extension.t
  -> holds_on_right:Typing_env_extension.t
  -> t

val add_opened_extensions
   : t
  -> holds_on_left:Typing_env_level.t
  -> holds_on_right:Typing_env_level.t
  -> t

(*
val add_extensions_and_extend_central_environment
   : t
  -> holds_on_left:Typing_env_extension.t
  -> holds_on_right:Typing_env_extension.t
  -> central_extension:Typing_env_extension.t
  -> t
*)

val central_environment : t -> Meet_env.t

val central_typing_environment : t -> Typing_env.t

val environment_on_left : t -> Typing_env.t

val environment_on_right : t -> Typing_env.t

val holds_on_left : t -> Typing_env_extension.t

val holds_on_right : t -> Typing_env_extension.t

val shortcut_precondition : t -> bool

val perm_left : t -> Name_permutation.t

val perm_right : t -> Name_permutation.t

val clear_name_permutations : t -> t

val compose_name_permutations
   : t
  -> perm_left:Name_permutation.t
  -> perm_right:Name_permutation.t
  -> t
