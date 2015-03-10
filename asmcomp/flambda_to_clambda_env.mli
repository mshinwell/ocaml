(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Pierre Chambart and Mark Shinwell                   *)
(*                                                                     *)
(*  Copyright 2014--2015, OCamlPro                                     *)
(*  Copyright 2015, Jane Street Group                                  *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

(* Environments used during Flambda -> Clambda conversion.  These keep
   track of:
   - variables whose approximations we know;
   - variables whose value we know will always be the value referenced
     by a given symbol.  We call these "variable-symbol equalities".
*)

type t

val create : unit -> t

(* [clean t] erases all information in the environment [t] that is local
   to a given function.  For example, how to access some free variable of
   a particular function is a local piece of information, whereas how to
   access some globally-defined constant attached to a symbol is not. *)
val clean : t -> t

val add_unique_ident : t -> Variable.t -> Ident.t * t

val add_substitution_for_variable
   : t
  -> Variable.t
  -> _ Flambda.t
  -> Clambda.ulambda
  -> t

(* [find_substitution_for_variable] retrieves a substitution previously
   added with [add_substitution_for_variable]. *)
val find_substitution_for_variable : t -> Variable.t -> _ Flambda.t option

(* [add_approx t var approx] adds a mapping to [t] that identifies
   the variable [var] as having approximation [approx]. *)
val add_approx : t -> Variable.t -> Flambdaexport.approx -> t

(* [add_approxs t approx_map] adds mappings to [t] that identify
   known approximations for given variables. *)
val add_approxs : t -> Flambdaexport.approx Variable.Map.t -> t

(* [get_approx] retrieves an approximation previously added with [add_approx]
   or [add_approxs]. *)
val get_approx : t -> Variable.t -> Flambdaexport.approx option

val add_variable_symbol_equality : t -> Variable.t -> t

(* [add_variable_symbol_equalities t var_mapping] examines each mapping
   v |-> v' in [var_mapping] and determines whether [t] tells us that
   the value of v' is always the value associated with some symbol S.
   If this is the case, then [t] is updated to also note down that v
   has the same relationship with S. *)
(* CR mshinwell: check whether these symbols only reference constants, and
   consider changing the name of these functions if so *)
val add_variable_symbol_equalities : t -> Variable.t Variable.Map.t -> t
(* CR mshinwell: should these be Ident.t? *)
(* [find_variable_symbol_equality t var] identifiers whether the value
   of [var] is known to always be that associated with some symbol. *)
val find_variable_symbol_equality : t -> Variable.t -> Symbol.t option

(* [variable_has_symbol_equality t var] returns [true] iff [t] has a
   variable-symbol equality for [var]. *)
val variable_has_symbol_equality : t -> Variable.t -> bool
