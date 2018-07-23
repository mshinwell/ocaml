(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(* CR-someday mshinwell: Try to find a way of factorising out the common
   parts of the module types below. *)

module Function_parameters : sig
  functor
     (T : Flambda_type0_internal_intf.S)
     (Typing_env : Typing_env_intf.S with module T := T)
     (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
     (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
     (Join_env : Join_env_intf.S with module T := T)
  ->
    Parameters_intf.S
      with module T := T
      with type EVS.t = Kinded_parameter.t list

module Function_results : sig
  functor
     (T : Flambda_type0_internal_intf.S)
     (Typing_env : Typing_env_intf.S with module T := T)
     (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
     (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
     (Join_env : Join_env_intf.S with module T := T)
  ->
    Parameters_intf.S
      with module T := T
      with type EVS.t = Kinded_parameter.t list

module Continuation_parameters : sig
  functor
     (T : Flambda_type0_internal_intf.S)
     (Typing_env : Typing_env_intf.S with module T := T)
     (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
     (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
     (Join_env : Join_env_intf.S with module T := T)
  ->
    Parameters_intf.S
      with module T := T
      with type EVS.t = Kinded_parameter.t list

module Block_fields : sig
  functor
     (T : Flambda_type0_internal_intf.S)
     (Typing_env : Typing_env_intf.S with module T := T)
     (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
     (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
     (Join_env : Join_env_intf.S with module T := T)
  ->
    Parameters_intf.S
      with module T := T
      with type EVS.t = Targetint.OCaml.Set.t

module Closure_elements : sig
  functor
     (T : Flambda_type0_internal_intf.S)
     (Typing_env : Typing_env_intf.S with module T := T)
     (Typing_env_extension : Typing_env_extension_intf.S with module T := T)
     (Meet_and_join : Meet_and_join_intf.S_both with module T := T)
     (Join_env : Join_env_intf.S with module T := T)
  ->
    Parameters_intf.S
      with module T := T
      with type EVS.t = Var_within_closure.Set.t
