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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  module Flambda_type0_core : sig type t end
  module Join_env : sig type t end
  module Typing_env : sig type t end

(* CR mshinwell: See whether we want Row_like_intf or a custom interface
   in the end.
  module RP : Relational_product_intf.S_applied
    with module Join_env := Join_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
    with module Index := Int_index
    with module Component := Logical_variable_component

  include Row_like_intf.S_applied
    with module Join_env := Join_env
    with module Typing_env := Typing_env
    with module Typing_env_extension := Typing_env_extension
    with module Tag := Tag_index
    with module Index := Int_index
    with module Maps_to := RP
*)

  type t

  type open_or_closed = Open | Closed of Tag.t

  val create : field_tys:Flambda_type0_core.t list -> open_or_closed -> t

  val invariant : t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val meet
     : Typing_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> t
    -> t
    -> t Or_bottom.t

  val join
     : Join_env.t
    -> Name_permutation.t
    -> Name_permutation.t
    -> t
    -> t
    -> t

  include Contains_names.S with type t := t
end
