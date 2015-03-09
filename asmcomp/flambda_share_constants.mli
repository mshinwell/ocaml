(* Symbols needs to be sorted before transforming their definition to allow as
   much sharing as possible.a  For instance if the existing symbols are:
     sym_1 -> (sym_3,sym_3)
     sym_2 -> (sym_4,sym_4)
     sym_3 -> (1,2)
     sym_4 -> (1,2)

   In that case, we expect sym_3 and sym_4 to be shared and also sym_1 and
   sym_2. If it is converted in that order, when sym_1 and sym_2 are converted,
   sym_3 and sym_4 havent and are not known to be equal yet. Hence sym_1 and
   sym_2 won't be shared. To avoid that problem, we need to convert the symbol
   from leaf to roots, the dependency topological order:
      sym_3 -> (1,2)
      sym_4 -> (1,2)
      sym_1 -> (sym_3,sym_3)
      sym_2 -> (sym_4,sym_4)

    Notice that for cyclic values there is no good order that allows to find
    this sharing. We would need some kind of automata minimization procedure.
*)

val compute_sharing : constants:_ Flambda.t Symbol.Map.t
  -> fclambda_for_expr:(_ Flambda.t -> unit Flambda.t * Clambda.ulambda)
  -> Clambda.ustructured_constant Symbol.Map.t
