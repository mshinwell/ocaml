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

module Entry : sig
  type t = private
    | Const of Rec_info.t
    | Name of Name.t

  include Identifiable.S with type t := t

  val const : Rec_info.t -> t
  val name : Name.t -> t
end

type t

val empty : t

val is_empty : t -> bool

val add_newer_rec_info : t -> Entry.t -> t

val to_list_newest_first : t -> Entry.t list

val to_list_oldest_first : t -> Entry.t list

include Identifiable.S with type t := t
include Contains_names.S with type t := t

val free_names_in_types : t -> Name_occurrences.t
