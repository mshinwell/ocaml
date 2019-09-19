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

[@@@ocaml.warning "+a-30-40-41-42"]

(** The definition of types together with their constructors and operations
    upon them. *)

module Type_of_kind_value : Type_descr_intf.S
module Type_of_kind_naked_immediate : Type_descr_intf.S
module Type_of_kind_naked_float : Type_descr_intf.S
module Type_of_kind_naked_int32 : Type_descr_intf.S
module Type_of_kind_naked_int64 : Type_descr_intf.S
module Type_of_kind_naked_nativeint : Type_descr_intf.S
module Type_of_kind_fabricated : Type_descr_intf.S

type t = private
  | Value of Type_of_kind_value.t
  | Naked_immediate of Type_of_kind_naked_immediate.t
  | Naked_float of Type_of_kind_naked_float.t
  | Naked_int32 of Type_of_kind_naked_int32.t
  | Naked_int64 of Type_of_kind_naked_int64.t
  | Naked_nativeint of Type_of_kind_naked_nativeint.t
  | Fabricated of Type_of_kind_fabricated.t

include Type_grammar_intf.S with type t := t
