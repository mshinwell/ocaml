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

(* This pass forms the last stage in the Flambda middle-end.  Rather than
   optimizations, it consists of transformations that are necessitated by
   the structure and subsequent intermediate languages of the compiler.

   Structured constants are assigned to symbols, yielding new Flambda
   terms that reference the constants via the symbols.  Value approximations
   are built that encapsulate these rewritten terms; these are recorded
   within [Compilenv], ready for subsequent writing into .cmx files.  At the
   same time, Clambda terms are produced, ready for the next phase of the
   compilation pipeline.

   The most significant transformations when producing Clambda relate to
   closures.  These are as follows:
   - determination of the runtime layout of closure blocks;
   - replacement of accesses to variables bound by a closure by field
     extractions from closure blocks;
   - replacement of accesses to closures by name by an "offset" Clambda
     construction indexing by an integer;
   - addition of the extra "environment" parameter to a function that
     requires to access its closure from within its body.  *)
val translate_and_update_compilenv
   : compilation_unit:Compilation_unit.t
  -> expr:_ Flambda.flambda
  -> Clambda.ulambda
