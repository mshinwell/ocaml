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

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Typing_env_level

(* In the future, typing environment extensions might contain existential
   binders -- but these are not needed at present.  Any introduction of
   existentials in the present version is immediately succeeded by opening
   of such existentials to construct a joined typing environment (for
   simplifying a continuation handler).  As such, there is no need to do the
   packaging up under a binder, then subsequent opening.  We elide both. *)

let create level =
  if not (has_no_defined_vars level) then begin
    Misc.fatal_errorf "Typing environment extensions cannot define \
        variables:@ %a"
      print level
  end;
  level
