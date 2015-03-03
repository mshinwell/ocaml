open Abstract_identifiers

(* Determine in-memory layout of closures by assigning offsets to the
   variables they bind. *)
val assign_offsets
   : expr:'a Flambda.t
  -> constants:'a Flambda.t Symbol.Map.t
  -> (int Closure_id.Map.t) * (int Var_within_closure.Map.t)

(* Determine offsets within closures imported from other units that need to
   be exported from the current unit.  This information is returned as
   functions that augment existing offset mapping tables.

   To understand why closure offsets need to be re-exported, consider the
   following situation.
   1. foo.ml
     let f ... = ... fv ...   (* [fv] free in [f] *)
   2. bar.ml
     let g ... =
       ... f 42 ...  (* inlined copy of [f] *)
   The value of [fv] in [f]'s closure may be computed at runtime.  As such,
   the access to [fv] in the inlined copy of [f] within [g] must go
   through [f]'s closure.  The offsets in [f]'s closure had to have been
   assigned when compiling foo.ml, since code in foo.o references them.
   3. baz.ml
     let h ... =
       ... g 0 ...  (* inlined copy of [g] *)
   compiled in a context where foo.cmx is not accessible.  When [g] is
   inlined, there will be a [Fvariable_in_closure] reference to [fv]
   inside [f]'s closure.  However, we do not appear to know the offset
   since foo.cmx is not available.  As such, the compilation of any unit
   that may have inlined functions from another must identify the offsets
   assigned for that latter unit.
*)
val reexported_offsets
   : extern_fun_offset_table:int Closure_id.Map.t
  -> extern_fv_offset_table:int Var_within_closure.Map.t
  -> expr:'a Flambda.t
  -> (int Closure_id.Map.t -> int Closure_id.Map.t)
       * (int Var_within_closure.Map.t -> int Var_within_closure.Map.t)
