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

type t

val create
   : final_typing_env:Flambda_type.Typing_env.Serializable.t
  -> all_code:Exported_code.t
  -> exported_offsets:Exported_offsets.t
  -> used_closure_vars:Var_within_closure.Set.t
  -> t

val import_typing_env_and_code
   : t
  -> Flambda_type.Typing_env.Serializable.t *
     Exported_code.t

val exported_offsets : t -> Exported_offsets.t

val functions_info : t -> Exported_code.t

val with_exported_offsets : t -> Exported_offsets.t -> t

(** Rename the compilation units for packed modules to the pack unit,
    so that file lookups search for the right cmx *)
val update_for_pack
   : pack_units:Compilation_unit.Set.t
  -> pack:Compilation_unit.t
  -> t option
  -> t option

(** Aggregate several cmx into one for packs *)
val merge : t option -> t option -> t option

(** For ocamlobjinfo *)
val print : Format.formatter -> t -> unit

(** The following is for writing values of type [t] into different sections
    of a .cmx file.  This in particular allows code to be demarshalled
    lazily. *)

type subsidiary_sections_map

(** For each non-header section, the provided [f] must give the assigned index
    (as needed for [Compilenv.read_section_from_cmx_file]) of the section
    in the .cmx file being prepared. *)
val create_subsidiary_sections_map
   : t
  -> f:(Obj.t -> int)
  -> subsidiary_sections_map

val header_contents : t -> subsidiary_sections_map -> t

val associate_with_loaded_cmx_file
   : t
  -> read_flambda_section_from_cmx_file:(index:int -> Obj.t)
  -> t

val load_all_sections_from_cmx_file : t -> unit
