(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Contents of middle-end-specific portion of .cmx files when using
    Flambda. *)

type t = {
  final_typing_env : Flambda_type.Typing_env.Serializable.t;
  all_code : Flambda.Function_params_and_body.t Code_id.Map.t;
  exported_offsets : Exported_offsets.t;
}

let create ~final_typing_env ~all_code ~exported_offsets =
  { final_typing_env;
    all_code;
    exported_offsets;
  }

let final_typing_env t = t.final_typing_env
let all_code t = t.all_code
let exported_offsets t = t.exported_offsets
let with_exported_offsets t exported_offsets = { t with exported_offsets; }
