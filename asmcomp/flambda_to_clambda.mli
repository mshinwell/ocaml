(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*                     Pierre Chambart, OCamlPro                       *)
(*                                                                     *)
(*  Copyright 2014 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(*
Transform an expression to prepare conversion to clambda
- attributes symbols to structured constants
- replace access to constants from the current compilation unit by Fsymbol nodes,
  including access to fields from a constant closure inside the body of the function.
- Find used closure fields and remove unused ones.
- build value approximations for export

During symbol assignment, some alias can be created (when building let rec for instance).
They are replaced by their canonical representent in the Prepare functor application.

Then the tables needed to build the Flambdaexport.exported type are build.
*)

(* Converts a flambda expression to clambda.
   During the conversion it:
    * substitute variables bound by a closure by a field access
      inside the closure
    * replace symbolic closure offset by the real integer offset.
    * build the switch tables
    * add closure parameter for direct calls
    * detect constants values and transform them to Uconst
   For everything else, it is basically the identity.
*)
val convert
   : 'a Flambda.flambda * 'a Flambda.flambda Symbol.Map.t
       * Flambdaexport.exported
  -> Clambda.ulambda
