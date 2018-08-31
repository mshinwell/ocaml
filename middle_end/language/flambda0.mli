(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** The terms of the intermediate language used for tree-based analysis
    and optimization.

    Flambda expressions augment Ilambda expressions by adding constructs for:
    - the construction and manipulation of closures; and
    - accessing constants that have been lifted to static data.
*)

(** Whether the callee in a function application is known at compile time. *)
module Call_kind : sig
  type function_call =
    | Direct of {
        closure_id : Closure_id.t;
        (** The [closure_id] isn't actually sufficient to perform a direct
            call.  The set of closures involved has to be determined first
            using reachability analysis.  If such determination fails, then
            we have to fall back to an indirect call. *)
        (* CR mshinwell: Should this arity really permit "bottom"? *)
        return_arity : Flambda_arity.t;
        (** [return_arity] describes what the callee returns.  It matches up
            with the arity of [continuation] in the enclosing [Apply.t]
            record. *)
      }
    | Indirect_unknown_arity
    | Indirect_known_arity of {
        param_arity : Flambda_arity.t;
        return_arity : Flambda_arity.t;
      }

  type method_kind = Self | Public | Cached

  type t =
    | Function of function_call
    | Method of { kind : method_kind; obj : Name.t; }
    | C_call of {
        alloc : bool;
        param_arity : Flambda_arity.t;
        return_arity : Flambda_arity.t;
      }

  include Contains_names.S with type t := t

  val return_arity : t -> Flambda_arity.t
end

(** The application of a function (or method on a given object) to a list of
    arguments. *)
module Apply : sig
  type t = {
    callee : Name.t;
    continuation : Continuation.t;
    exn_continuation : Continuation.t;
    (** Where to send the result of the application. *)
    args : Simple.t list;
    call_kind : Call_kind.t;
    dbg : Debuginfo.t;
    inline : Inline_attribute.t;
    (** Instructions from the source code as to whether the callee should
        be inlined. *)
    specialise : Specialise_attribute.t;
    (** Instructions from the source code as to whether the callee should
        be specialised. *)
  }

  val print : Format.formatter -> t -> unit
end

(** Actions affecting exception traps on the stack.  These are always
    associated with an [Apply_cont] node; the trap action is executed before
    the application of the continuation.

    The [Trap_id] values tie up corresponding pairs of pushes and pops
    irrespective of the handler (which might be shared).  [Pop] may not appear
    to need the [exn_handler] value during Flambda passes---but in fact it
    does, since it compiles to a reference to such continuation, and must
    not be moved out of its scope.

    Beware: continuations cannot be used both as an exception handler and as
    a normal continuation (since continuations used as exception handlers
    use a calling convention that may differ from normal).
*)
module Trap_action : sig
  type t =
    | Push of { id : Trap_id.t; exn_handler : Continuation.t; }
    (* CR mshinwell: Think about whether we really need the trap IDs now *)
    | Pop of {
        id : Trap_id.t;
        exn_handler : Continuation.t;
        take_backtrace : bool;
      }
end

module Switch : sig
  (** Conditional control flow (the only such form).  Scrutinees of [Switch]es
      are [Discriminant]s of kind [Fabricated]---not regular integers or
      similar.  There are no default cases.  Switches always have at least
      two cases. *)

  type t

  include Map.With_set with type t := t

  val invariant : t -> bool

  (** Call the given function [f] on each (discriminant, destination) pair
      in the switch. *)
  val iter : t -> f:(Discriminant.t -> Continuation.t -> unit) -> unit

  (** Where the switch will jump to for each possible value of the
      discriminant. *)
  val arms : t -> Continuation.t Discriminant.Map.t

  (** How many cases the switch has.  (Note that this is not the number of
      destinations reached by the switch, which may be a smaller number.) *)
  val num_arms : t -> int
end

type recursive =
  | Non_recursive
  | Recursive

type mutable_or_immutable =
  | Mutable
  | Immutable

module rec Expr : sig
  (** With the exception of applications of primitives ([Prim]), Flambda terms
      are in CPS.

      Primitives being in direct style combined with care during CPS conversion
      should keep administrative redexes to a minimum.

      The CPS representation ensures that:
      - every intermediate value (and in particular every potential constant
        that we may want to lift) has a name;
      - there are no nested "let"s or complicated expressions as the defining
        expression of a "let";
      - every point to which we might wish to jump has a name;
      - no re-normalisation of terms is required when substituting an
        application for an inlined body (unlike in ANF form).  This is important
        for compilation speed.

      See comments on the [Let_cont.t] type below for information about the
      form of continuations used.

      Exception flow is currently handled (for simplicity) using explicit push
      and pop trap operations (see above) rather than double-barrelled CPS.
      This will likely change in the future.

      Note: All bound variables in Flambda terms must be distinct.
  *)
  type t =
    | Let of Let.t
    (** Bind a variable.  There can be no effect on control flow (save for
        asynchronous operations such as the invocation of finalisers or
        signal handlers as a result of reaching a safe point). *)
    | Let_cont of Let_cont.t
    (** Define one or more continuations. *)
    | Apply of Apply.t
    (** Call an OCaml function, external function or method. *)
    | Apply_cont of Continuation.t * Trap_action.t option * Simple.t list
    (** Call a continuation, optionally adding or removing exception trap
        frames from the stack, which thus allows for the raising of
        exceptions. *)
    | Switch of Name.t * Switch.t
    (** Conditional control flow. *)
    | Invalid of Invalid_term_semantics.t
    (** Code proved type-incorrect and therefore unreachable. *)

  include Contains_names.S with type t := t

  val print : Format.formatter -> t -> unit

  val invariant : Invariant_env.t -> t -> unit

  (** Creates a [Let] expression. *)
  val create_let : Variable.t -> Flambda_kind.t -> Named.t -> t -> t

  type switch_creation_result = private
    | Have_not_deleted_branch
    | Have_deleted_branch

  (** Create a [Switch] expression, save that zero-arm switches are converted
      to [Invalid], and one-arm switches to [Apply_cont]. *)
  val create_switch
     : scrutinee:Name.t
    -> arms:Continuation.t Discriminant.Map.t
    -> Expr.t * switch_creation_result

(*
  (** Compute the free names of a term.  (This is O(1) for [Let]s).
      If [ignore_uses_as_callee], all free names inside [Apply] expressions
      are ignored.  Likewise [ignore_uses_in_project_var] for [Project_var]
      expressions.
  *)
  val free_names_advanced
     : ?ignore_uses_as_callee:unit
    -> ?ignore_uses_as_argument:unit
    -> ?ignore_uses_as_continuation_argument:unit
    -> ?ignore_uses_in_project_var:unit
    -> ?ignore_uses_in_apply_cont:unit
    -> t
    -> Name_occurrences.t

  (** Compute _all_ names occurring inside an expression. *)
  val used_names
     : ?ignore_uses_as_callee:unit
    -> ?ignore_uses_as_argument:unit
    -> ?ignore_uses_as_continuation_argument:unit
    -> ?ignore_uses_in_project_var:unit
    -> t
    -> Name_occurrences.t
*)

  (* CR mshinwell: Consider if we want to cache these. *)
  val free_continuations : t -> Continuation.Set.t

  val invalid : unit -> t

  val iter_lets
     : t
    -> for_defining_expr:(Variable.t -> Flambda_kind.t -> Named.t -> unit)
    -> for_last_body:(t -> unit)
    -> for_each_let:(t -> unit)
    -> unit

  (* CR mshinwell: consider enhancing this in the same way as for
     [fold_lets_option] in the [defining_expr] type.  This would be useful eg
     for Ref_to_variables.  Maybe in fact there should be a new iterator that
     uses this function for such situations? *)
  val map_lets
     : t
    -> for_defining_expr:(Variable.t -> Flambda_kind.t -> Named.t -> Named.t)
    -> for_last_body:(t -> t)
    -> after_rebuild:(t -> t)
    -> t

  type maybe_named =
    | Is_expr of t
    | Is_named of Named.t

  (** This function is designed for the internal use of [Flambda_iterators].
      See that module for iterators to be used over Flambda terms. *)
  val iter_general
     : toplevel:bool
    -> (t -> unit)
    -> (Named.t -> unit)
    -> maybe_named
    -> unit

  val print : Format.formatter -> t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  (** Build a [Switch] corresponding to a traditional if-then-else. *)
  val if_then_else
     : scrutinee:Name.t
    -> if_true:Continuation.t
    -> if_false:Continuation.t
    -> t

  (** Returns [true] if the given expression has neither effects nor
      coeffects (see [Flambda_primitive] for documentation on such). *)
  val no_effects_or_coeffects : t -> bool

  (* CR mshinwell: Check that apply_cont is well-formed when there is a
     trap installation or removal. *)
  (* CR-someday pchambart: for sum types, we should probably add an exhaustive
     pattern in ignores functions to be reminded if a type change *)
  (* CR-someday mshinwell: We should make "direct applications should not have
     overapplication" be an invariant throughout.  At the moment I think this is
     only true after [Simplify] has split overapplications. *)

  val description_of_toplevel_node : t -> string

  (** [bind [var1, expr1; ...; varN, exprN] body] binds using
      [Immutable] [Let] expressions the given [(var, expr)] pairs around the
      body. *)
  val bind
     : bindings:(Variable.t * Flambda_kind.t * Named.t) list
    -> body:t
    -> t

  (** All continuations defined at toplevel within the given expression. *)
  val all_defined_continuations_toplevel : t -> Continuation.Set.t

  val count_continuation_uses_toplevel : t -> int Continuation.Map.t

  type with_wrapper =
    | Unchanged of { handler : Continuation_handler.t; }
    | With_wrapper of {
        new_cont : Continuation.t;
        new_handler : Continuation_handler.t;
        wrapper_handler : Continuation_handler.t;
      }

  val build_let_cont_with_wrappers
     : body:t
    -> recursive:F0.recursive
    -> with_wrappers:with_wrapper Continuation.Map.t
    -> t

  (* CR-soon mshinwell: we need to document whether these iterators follow any
     particular order. *)
  module Iterators : sig
    val iter : (t -> unit) -> (Named.t -> unit) -> t -> unit

    val iter_lets
       : t
      -> for_defining_expr:(Variable.t -> Flambda_kind.t -> Named.t -> unit)
      -> for_last_body:(t -> unit)
      -> for_each_let:(t -> unit)
      -> unit
        
    val iter_expr : (t -> unit) -> t -> unit

    val iter_named : (Named.t -> unit) -> t -> unit
    
    val iter_all_immutable_let_and_let_rec_bindings
       : t
      -> f:(Variable.t -> Named.t -> unit)
      -> unit

    val iter_sets_of_closures : (Set_of_closures.t -> unit) -> t -> unit

    (** Apply the given [f] to every function body within the given
        expression. *)
    val iter_function_bodies
       : t
      -> f:(continuation_arity:Flambda_arity.t
        -> Continuation.t
        -> Expr.t
        -> unit)
      -> unit

    (** Iterators, mappers and folders in [Toplevel_only] modules never
        recurse into the bodies of functions. *) 
    module Toplevel_only : sig 
      val iter
        : (t -> unit)
       -> (Named.t -> unit)
       -> t
       -> unit

      val iter_all_immutable_let_and_let_rec_bindings
         : t
        -> f:(Variable.t -> Named.t -> unit)
        -> unit
    end
  end
    
  module Mappers : sig
    val map : (t -> t) -> (Named.t -> Named.t) -> t -> t

    val map_lets
       : t
      -> for_defining_expr:(Variable.t -> Flambda_kind.t -> Named.t -> Named.t)
      -> for_last_body:(t -> t)
      -> after_rebuild:(t -> t)
      -> t

    val map_expr : (t -> t) -> t -> t

    val map_named : (Named.t -> Named.t) -> t -> t

    val map_named_with_id : (Variable.t -> Named.t -> Named.t) -> t -> t

    val map_symbols : t -> f:(Symbol.t -> Symbol.t) -> t

    val map_sets_of_closures
       : t
      -> f:(Set_of_closures.t -> Set_of_closures.t)
      -> t
  
    val map_apply : t -> f:(Apply.t -> Apply.t) -> t

    val map_all_immutable_let_and_let_rec_bindings
       : t
      -> f:(Variable.t -> Named.t -> Named.t)
      -> t

    val map_function_bodies
       : ?ignore_stubs:unit
      -> t
      -> f:(continuation_arity:Flambda_arity.t
        -> Continuation.t
        -> Expr.t
        -> Expr.t)
      -> t
         
    module Toplevel_only : sig 
      val map : (t -> t) -> (Named.t -> Named.t) -> t -> t

      val map_expr : (t -> t) -> t -> t

      val map_named : (Named.t -> Named.t) -> t -> t
  
      val map_sets_of_closures
         : t
        -> f:(Set_of_closures.t -> Set_of_closures.t)
        -> t
    end
  end

  module Folders : sig
    (** Used to avoid exceeding the stack limit when handling expressions with
        multiple consecutive nested [Let]-expressions. This saves rewriting
        large simplification functions in CPS. This function provides for the
        rewriting or elimination of expressions during the fold. *)
    val fold_lets_option
        : t
      -> init:'a
      -> for_defining_expr:(
          'a
        -> Variable.t
        -> Flambda_kind.t
        -> Named.t
        -> 'a
          * (Variable.t * Flambda_kind.t * Named.t) list
          * Variable.t
          * Flambda_kind.t
          * Reachable.t)
      -> for_last_body:('a -> t -> t * 'b)
      (* CR-someday mshinwell: consider making [filter_defining_expr]
        optional *)
      -> filter_defining_expr:(
          'b
        -> Variable.t
        -> Flambda_kind.t
        -> Named.t
        -> Name_occurrences.t
        -> 'b * Variable.t * Flambda_kind.t * (Named.t option))
      -> t * 'b
  end
end and Named : sig
  (** Values of type [t] will always be [Let]-bound to a [Variable.t].
      (Note that [Simple.t] values do not need to be [Let]-bound; but they are
      allowed here for convenience.) *)
  type t =
    | Simple of Simple.t
    | Prim of Flambda_primitive.t * Debuginfo.t
    | Set_of_closures of Set_of_closures.t

  include Contains_names.S with type t := t

  (** Build an expression boxing the name.  The returned kind is the
      one of the unboxed version. *)
  val box_value
      : Name.t
     -> Flambda_kind.t
     -> Debuginfo.t
     -> Named.t * Flambda_kind.t

  (** Build an expression unboxing the name.  The returned kind is the
      one of the unboxed version. *)
  val unbox_value
      : Name.t
     -> Flambda_kind.t
     -> Debuginfo.t
     -> Named.t * Flambda_kind.t

  val at_most_generative_effects : t -> bool

  (** Return a value which is kind-correct, but not necessarily
      type-correct, at the given kind. *)
  val dummy_value : Flambda_kind.t -> t

  val toplevel_substitution : Name.t Name.Map.t -> t -> t

  module Iterators : sig
    val iter : (Expr.t -> unit) -> (t -> unit) -> t -> unit
    
    val iter_named : (t -> unit) -> t -> unit

    module Toplevel_only : sig
      val iter : (Expr.t -> unit) -> (t -> unit) -> t -> unit
    end
  end
end and Let0 : sig
  include Contains_names.S

  (** The kind of the defining expression of the let-expression. *)
  val kind : t -> Flambda_kind.t

  (** The defining expression of the let-expression. *)
  val defining_expr : t -> Named.t

  (** The body of the let-expression. *)
  val body : t -> Expr.t

  (** The free names in the body of the [Let].  After the first query the
      result will be returned in O(1) time. *)
  val free_names_of_body : t -> Name_occurrences.t
end and Let : sig
  include Contains_names.S

  (** Create a let-expression. *)
  val create
     : bound_var:Variable.t
    -> kind:Flambda_kind.t
    -> defining_expr:Named.t
    -> body:Expr.t
    -> t

  (** Deconstruct a let-expression. *)
  val pattern_match : t -> f:(Variable.t -> Let0.t -> 'a) -> 'a

(*
  (** Apply the specified function [f] to the defining expression of the
      given let-expression. *)
  val map_defining_expr : t -> f:(Named.t -> Named.t) -> Expr.t
*)
end and Let_cont : sig
  (** Values of type [t] represent the definitions of continuations:
        let_cont [name] [args] = [handler] in [body]
      or in other words:
        [body]
        where [name] [args] = [handler]

      - Continuations are second-class.
      - Continuations do not capture variables.
      - Continuations may be (mutually-)recursive.

      It is an error to mark a continuation that might be recursive as
      non-recursive.  The converse is safe.
  *)
  (* CR mshinwell: We need to add the following invariant checks:
     1. Usual checks on [let_cont.specialised_args].
     2. Also on that specialised_args map, that only [Field] projections are
         used.  (The other projections are all things to do with closures.)  We
         might consider changing the type somehow to make this statically
         checked.
     3. Specialised args are only allowed to have [var = None] in the
         [specialised_to] record iff they are non-specialised parameters of a
         continuation.
     4. Exception handlers should be "Handlers" with a single non-recursive
         continuation.
     mshinwell: comment out of date now, but equivalent things still need
     doing.
  *)

  (** Note: any continuation used as an exception handler must be non-recursive
      by the point it reaches [Flambda_to_clambda].  (This means that it is
      permissible to introduce mutual recursion through stubs associated with
      such continuations, so long as [Simplify] is run afterwards
      to inline them out and turn the resulting single [Recursive] handler into
      a [Non_recursive] one. *)
  type t = private
    | Non_recursive of Non_recursive_let_cont_handler.t
    | Recursive of Recursive_let_cont_handlers.t

  (** Create a definition of a non-recursive continuation. *)
  val create_non_recursive
     : Continuation.t
    -> handler:Continuation_handler.t
    -> body:Expr.t
    -> t

  (** Create a definition of a continuation that will serve as an exception
      handler. *)
  val create_exception_handler
     : Continuation.t
    -> handler:Continuation_handler.t
    -> body:Expr.t
    -> t

  (** Create a definition of a set of possibly-recursive continuations. *)
  val create_recursive
     : handlers:Continuation_handlers.t
    -> body:Expr.t
    -> t

  val free_names : t -> Name_occurrences.t

  (** Return all continuations bound in the given handlers (traversing all
      the way down through the handlers, not just the immediately outermost
      bindings). *)
  val bound_continuations : t -> Continuation.Set.t

  (** Return all continuations free in the given handlers. *)
  val free_continuations : t -> Continuation.Set.t

  type free_and_bound = private {
    free : Continuation.Set.t;
    bound : Continuation.Set.t;
  }

  (** As for [free_continuations] and [bound_continuations], but returning
      the results together. *)
  val free_and_bound_continuations : t -> free_and_bound

  (** Form a map from continuations to their definitions.  This is useful
      for analyses that don't care about the (non-)recursiveness of the
      definition(s). *)
  val to_continuation_map : t -> Continuation_handlers.t

  (** [map t ~f] is equivalent to calling [f] on [to_continuation_map t],
      then repacking the result in the same constructor ([Recursive] or
      [Non_recursive]) as [t]. *)
  val map : t -> f:(Continuation_handlers.t -> Continuation_handlers.t) -> t

  val print : Format.formatter -> t -> unit
end and Non_recursive_let_cont_handler0 : sig
  include Contains_names.S

  val handler : t -> Continuation_handler.t

  val body : t -> Expr.t
end and Non_recursive_let_cont_handler : sig
  include Contains_names.S

  (** Deconstruct a continuation binding. *)
  val pattern_match
     : t
    -> f:(Continuation.t -> Non_recursive_let_cont_handler0.t -> 'a)
    -> 'a
end and Recursive_let_cont_handlers0 : sig
  include Contains_names.S

  val handlers : t -> Continuation_handlers.t

  val body : t -> Expr.t
end and Recursive_let_cont_handlers : sig
  include Contains_names.S

  (** Deconstruct a continuation binding. *)
  val pattern_match
     : t
    -> f:(Continuation.t -> Recursive_let_cont_handler0.t -> 'a)
    -> 'a
end and Continuation_handlers : sig
  type t = Continuation_handler.t Continuation.Map.t
end and Continuation_handler0 : sig
  include Contains_names.S

  val print : Format.formatter -> t -> unit

  (** The parameters of the continuation. *)
  val params : t -> Flambda_type.Parameters.t

  val param_arity : t -> Flambda_arity.t

  (** Whether the continuation is a compiler-generated wrapper that should
      always be inlined. *)
  val stub : t -> bool

  (** Whether the continuation is an exception handler.

      Continuations used as exception handlers are always [Non_recursive]
      and have exactly one argument.  To enable identification of them
      in passes not invoked from [Simplify] (where they could be
      identified by looking at the [Apply_cont]s that reference them) they
      are marked explicitly.

      (Relevant piece of background info: the backend cannot compile
      simultaneously-defined continuations when one or more of them is an
      exception handler.) *)
  val is_exn_handler : t -> bool

  (** The code of the continuation itself. *)
  val handler : t -> Expr.t
end and Continuation_handler : sig
  include Contains_names.S

  val pattern_match : t -> f:(Continuation_handler0.t -> 'a) -> 'a
end and Set_of_closures : sig
  type t

  (** Create a set of closures given the code for its functions and the
      closure variables. *)
  val create
     : function_decls:Function_declarations.t
    -> closure_elements:Simple.t Var_within_closure.Map.t
    -> direct_call_surrogates:Closure_id.t Closure_id.Map.t
    -> t

  val invariant : Invariant_env.t -> t -> unit

  val print : Format.formatter -> t -> unit

  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

  val function_decls : t -> Function_declarations.t

  val closure_elements : t -> Simple.t Var_within_closure.Map.t

  (** If [direct_call_surrogates t] maps [closure_id1] to [closure_id2] then
      direct calls to [closure_id1] should be redirected to [closure_id2].
      This is used to reduce the overhead of transformations that introduce
      wrapper functions (which will be inlined at direct call sites, but will
      penalise indirect call sites).
      N.B. [direct_call_surrogates t] might not be transitively closed. *)
  val direct_call_surrogates : Closure_id.t Closure_id.Map.t

  (** Returns true iff the given set of closures has an empty environment. *)
  val has_empty_environment : t -> bool

  (** All names free in the given set of closures. *)
  val free_names : t -> Name_occurrences.t

  val variables_bound_by_the_closure : t -> Var_within_closure.Set.t

  (** [find_free_variable v clos] raises [Not_found] if [c] is not in [clos]. *)
  val find_free_variable
     : Var_within_closure.t
    -> t
    -> Variable.t

  module Iterators : sig
    val iter_function_bodies
       : t
      -> f:(continuation_arity:Flambda_arity.t
        -> Continuation.t
        -> Expr.t
        -> unit)
      -> unit
  end

  module Mappers : sig
    val map_symbols
       : t
      -> f:(Symbol.t -> Symbol.t)
      -> t

    val map_function_bodies
       : ?ignore_stubs:unit
      -> t
      -> f:(continuation_arity:Flambda_arity.t
        -> Continuation.t
        -> Expr.t
        -> Expr.t)
      -> t
  end

  module Folders : sig
    val fold_function_decls_ignoring_stubs
       : t
      -> init:'a
      -> f:(closure_id:Closure_id.t
        -> function_decl:Function_declaration.t
        -> 'a
        -> 'a)
      -> 'a
  end
end and Function_declarations : sig
  (** The representation of a set of function declarations (possibly mutually
      recursive).  Such a set encapsulates the declarations themselves,
      information about their defining environment, and information used
      specifically for optimization.
      Before a function can be applied it must be "projected" from a set of
      closures to yield a "closure".  This is done using [Project_closure]
      (see above).  Given a closure, not only can it be applied, but information
      about its defining environment can be retrieved (using [Project_var],
      see above).
      At runtime, a [set_of_closures] corresponds to an OCaml value with tag
      [Closure_tag] (possibly with inline [Infix_tag](s)).  As an optimization,
      an operation ([Move_within_set_of_closures]) is provided (see above)
      which enables one closure within a set to be located given another
      closure in the same set.  This avoids keeping a pointer to the whole set
      of closures alive when compiling, for example, mutually-recursive
      functions.
  *)
  type t

  val print : Format.formatter -> t -> unit

  (** Create a set of function declarations given the individual
      declarations. *)
  val create : Function_declaration.t Closure_id.Map.t -> t

  (** An identifier of the original set of closures on which this set of
      function declarations is based.  Used to prevent different
      specialisations of the same functions from being inlined/specialised
      within each other. *)
  val set_of_closures_origin : Set_of_closures_origin.t

  (** The function(s) defined by the set of function declarations, indexed
      by closure ID. *)
  val funs : t -> Function_declaration.t Closure_id.Map.t

  (** [find f t] raises [Not_found] if [f] is not in [t]. *)
  val find : t -> Closure_id.t -> t -> Function_declaration.t

  (** Create a set of function declarations based on another set of function
      declarations. *)
  val update : t -> funs:Function_declaration.t Closure_id.Map.t -> t

  val import_for_pack
     : t
    -> (Set_of_closures_origin.t -> Set_of_closures_origin.t)
    -> t

  val find_declaration_variable : Closure_id.t -> t -> Variable.t

  (* CR pchambart: Update this comment *)
  (** Within a set of function declarations there is a set of function bodies,
      each of which may (or may not) reference one of the other functions in
      the same set.  Initially such intra-set references are by [Var]s (known
      as "fun_var"s) but if the function is lifted by [Lift_constants] then the
      references will be translated to [Symbol]s.  This means that optimization
      passes that need to identify whether a given "fun_var" (i.e. a key in the
      [funs] map in a value of type [function_declarations]) is used in one of
      the function bodies need to examine the [free_symbols] as well as the
      [free_variables] members of [function_declarations].  This function makes
      that process easier by computing all used "fun_var"s in the bodies of
      the given set of function declarations, including the cases where the
      references are [Symbol]s.  The returned value is a map from "fun_var"s
      to the "fun_var"s (if any) used in the body of the function associated
      with that "fun_var".
  *)
(*
  val fun_vars_referenced_in_decls
     : t
    -> backend:(module Backend_intf.S)
    -> Closure_id.Set.t Closure_id.Map.t

  (** Computes the set of closure_id in the set of closures that are
      used (transitively) by the [entry_point]. *)
  val closures_required_by_entry_point
     : entry_point:Closure_id.t
    -> backend:(module Backend_intf.S)
    -> t
    -> Closure_id.Set.t
*)

  val contains_stub : t -> bool
end and Params_and_body : sig
  (** A name abstraction that comprises a function's parameters (together with
      any relations between them, expressed using a relational product), the
      code of the function, and the [my_closure] variable.

      From the body of the function, accesses to variables within the closure
      need to go via a [Project_var] (from [my_closure]); accesses to any other
      simultaneously-defined functions need to go likewise via a
      [Move_within_set_of_closures]. *)
  type t

  include Contains_names.S with type t := t

  val create
     : Flambda_type.Parameters.t
    -> body:Expr.t
    -> my_closure:Variable.t
    -> t

  val pattern_match
     : t
    -> f:(Flambda_type.Parameters.t -> Expr.t -> my_closure:Variable.t -> 'a)
    -> 'a
end and Function_declaration : sig
  type t

  include Contains_names.S with type t := t

  (** Create a function declaration.  This calculates the free variables and
      symbols occurring in the specified [body].

      To just change the parameters or body of a function the "update" functions
      below should be used, if possible; otherwise care must be taken to
      preserve the [closure_origin].

      When adding a stub to a function the stub should receive a new
      [closure_origin] and the renamed original function should retain its
      existing [closure_origin]. *)
  val create
     : closure_origin:Closure_origin.t
    -> continuation_param:Continuation.t
    -> exn_continuation_param:Continuation.t
    -> params_and_body:Params_and_body.t
    -> result_arity:Flambda_arity.t
    -> stub:bool
    -> dbg:Debuginfo.t
    -> inline:Inline_attribute.t
    -> specialise:Specialise_attribute.t
    -> is_a_functor:bool
    -> t

  val print : Closure_id.t -> Format.formatter -> t -> unit

  (** The closure from which this function declaration originally came.
      Used as a backstop against unbounded recursion during inlining. *)
  val closure_origin : t -> Closure_origin.t

  (** The continuation parameter of the function, i.e. to where we must jump
      once the result of the function has been computed.  If the continuation
      takes more than one argument then the backend will compile the function
      so that it returns multiple values. *)
  val continuation_param : t -> Continuation.t

  (** To where we must jump if application of the function raises an
      exception. *)
  val exn_continuation_param : t -> Continuation.t

  (** Abstraction value containing the parameters and the body. *)
  val params_and_body : t -> Params_and_body.t

  (* CR mshinwell: bad name *)
  val function_arity : t -> Flambda_arity.t

  (** An identifier to provide fast (conservative) equality checking for
      function bodies. *)
  val code_id : t -> Code_id.t

  (** The arity of the return continuation of the function.  This provides the
      number of results that the function produces and their kinds. *)
  val result_arity : t -> Flambda_arity.t

  (** A stub function is a generated function used to prepare arguments or
      return values to allow indirect calls to functions with a special
      calling convention.  For instance indirect calls to tuplified functions
      must go through a stub.  Stubs will be unconditionally inlined. *)
  val stub : t -> bool

  (** Debug info for the function declaration. *)
  val dbg : t -> Debuginfo.t

  (** Inlining requirements from the source code. *)
  val inline : t -> Inline_attribute.t

  (** Specialising requirements from the source code. *)
  val specialise : t -> Specialise_attribute.t

  (** Whether the function is known definitively to be a functor. *)
  val is_a_functor : t -> bool

  (** Change the parameters and code of a function declaration. *)
  val update_params_and_body : t -> Params_and_body.t -> t
end and Flambda_type : Flambda_type0_intf.S with module Expr := Expr

(** A module for the manipulation of terms where the recomputation of free
    name sets is to be kept to a minimum. *)
module With_free_names : sig
  type 'a t

  val print : Format.formatter -> _ t -> unit

  (** O(1) time. *)
  val of_defining_expr_of_let : Let.t -> Named.t t

  (** O(1) time. *)
  val of_body_of_let : Let.t -> Expr.t t

  (** Takes the time required to calculate the free names of the given
      term (proportional to the size of the term, except that the calculation
      for [Let] is O(1)). *)
  val of_expr : Expr.t -> Expr.t t

  val of_named : Flambda_kind.t -> Named.t -> Named.t t

  (** This function shouldn't be used to build a let from the [named t];
      use the [create_let_...] functions below.  It is intended to be used
      for situations such as when you want to use the contents of a [named t]
      for [Effect_analysis]. *)
  val to_named : Named.t t -> Named.t

  (** Takes the time required to calculate the free names of the given
      [expr]. *)
  val create_let_reusing_defining_expr
     : Variable.t
    -> Named.t t
    -> Expr.t
    -> Expr.t

  (** Takes the time required to calculate the free names of the given
      [named].  The specified Flambda type must be fully resolved (i.e. no
      occurrences of [Load_lazily]) or a fatal error will result. *)
  val create_let_reusing_body
     : Variable.t
    -> Flambda_kind.t
    -> Named.t
    -> Expr.t t
    -> Expr.t

  (** O(1) time. *)
  val create_let_reusing_both
     : Variable.t
    -> Named.t t
    -> Expr.t t
    -> Expr.t

  val contents : 'a t -> 'a

  (** O(1) time. *)
  val free_names : _ t -> Name_occurrences.t
end
