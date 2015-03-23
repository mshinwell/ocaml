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

val create :
     fclambda_and_approx_for_symbol:
       (Symbol.t -> unit Flambda.t * Clambda.ulambda * Flambdaexport.approx)
  -> t

(* [clean t] erases all information in the environment [t] that is local
   to a given function.  For example, how to access some free variable of
   a particular function is a local piece of information, whereas how to
   access some globally-defined constant attached to a symbol is not. *)
val clean : t -> t


(* [add_substitution_via_known_symbol] is like [add_substitution
val add_substitution_via_known_symbol
   : t
  -> Variable.t
  -> Symbol.t
  -> _ Flambda.t option
  -> Clambda.ulambda
  -> Flambdaexport.approx
  -> Ident.t * Symbol.t * t

val identifier_assignment : t -> Ident.t Variable.Map.t

val symbol_assignment
   : t
  -> (unit Flambda.t option * Clambda.ulambda * Flambdaexport.approx)
      Symbol.Map.t

(*
(* [add_approx t var approx] adds a mapping to [t] that identifies
   the variable [var] as having approximation [approx]. *)
val add_approx : t -> Variable.t -> Flambdaexport.approx -> t

(* [add_approxs t approx_map] adds mappings to [t] that identify
   known approximations for given variables. *)
val add_approxs : t -> Flambdaexport.approx Variable.Map.t -> t

(* [get_approx] retrieves an approximation previously added with [add_approx]
   or [add_approxs]. *)
val get_approx : t -> Variable.t -> Flambdaexport.approx option
*)
