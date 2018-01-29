(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Intermediate language used for tree-based analysis and optimization.

    Flambda expressions augment Ilambda expressions by adding constructs for:
    - the construction and manipulation of closures; and
    - accessing constants that have been lifted to static data.
*)

(** Whether the callee in a function application is known at compile time. *)
module Call_kind : sig
  type function_call =
    | Direct of {
        closure_id : Closure_id.t;
        (* CR mshinwell: Should this arity really permit "bottom"? *)
        return_arity : Flambda_arity.t;
        (** [return_arity] describes what the callee returns.  It matches up
            with the arity of [continuation] in the enclosing [apply]
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

  val return_arity : t -> Flambda_arity.t

  val equal : t -> t -> bool
end

type inline_attribute =
  | Always_inline
  | Never_inline
  | Unroll of int
  | Default_inline

type specialise_attribute =
  | Always_specialise
  | Never_specialise
  | Default_specialise

(** The application of a function (or method on a given object) to a list of
    arguments. *)
module Apply : sig
  type t = {
    (* CR-soon mshinwell: rename func -> callee, and
       lhs_of_application -> callee *)
    func : Name.t;
    continuation : Continuation.t;
    exn_continuation : Continuation.t;
    (** Where to send the result of the application. *)
    args : Simple.t list;
    call_kind : Call_kind.t;
    dbg : Debuginfo.t;
    inline : inline_attribute;
    (** Instructions from the source code as to whether the callee should
        be inlined. *)
    specialise : specialise_attribute;
    (** Instructions from the source code as to whether the callee should
        be specialised. *)
  }

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit
end

(** The update of a mutable variable.  Mutable variables are distinct from
    immutable variables in Flambda. *)
type assign = {
  being_assigned : Mutable_variable.t;
  new_value : Simple.t;
}

module Free_var : sig
  type t = {
    var : Variable.t;
    (* XXX [equalities] isn't right now.  Think about what should replace it
       (maybe nothing?) *)
    equalities : Flambda_primitive.With_fixed_value.Set.t;
  }

  val create : Variable.t -> t

  val print : Format.formatter -> t -> unit
end

module Free_vars : sig
  (** For closures: map from variables in a closure to "outer" variables.

      The [Var_within_closure.t] values should not be used as keys to keep
      track of information associated with closure variables, since the
      _same_ such values may be bound by multiple different sets of closures
      binding some particular closure ID.  Instead use the corresponding
      [Variable.t] in the [Free_var.t]. *)
  (* CR mshinwell: This makes me think we should rename [Var_within_closure].
     Maybe just to [In_closure]? *)

  type t = Free_var.t Var_within_closure.Map.t

  val find_by_variable : t -> Variable.t -> Var_within_closure.t option

  val all_outer_variables : t -> Variable.Set.t

  val map_vars : t -> f:(Variable.t -> Variable.t) -> t
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

  val equal : t -> t -> bool
end

module Switch : sig
  (** Conditional control flow (the only such form). *)

  (** Values of type [t] are indexed by the kind of the scrutinee.  They
      contain branches for all possible values of the scrutinee.
      Invariant: the maps are always non-empty. *)
  (* CR mshinwell: add invariant function *)
  type t = private
    | Value of Continuation.t Targetint.OCaml.Map.t
    | Fabricated of Continuation.t Tag.Map.t

  include Identifiable.S_no_hash with type t := t
end

(** What the optimizer should do when it reaches a term that is known to be
    invalid (for example because it is not type correct).  In all cases, code
    _after_ invalid code will be deleted. *)
type invalid_term_semantics =
  | Treat_as_unreachable
  (** Invalid code should be treated as unreachable and thus deleted.  The
      unreachability property may be propagated backwards through the term
      possibly causing other parts to be deleted. *)
  | Halt_and_catch_fire
  (** Invalid code should be replaced by an abort trap.  No back-propagation
      is performed. *)

val print_invalid_term_semantics
   : Format.formatter
  -> invalid_term_semantics
  -> unit

type recursive =
  | Non_recursive
  | Recursive

type mutable_or_immutable =
  | Mutable
  | Immutable

type coercion =
  | Kind of Simple.t * Flambda_kind.t

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

      See comments on the [let_cont] type below for information about the form
      of continuations used.

      Exception flow is currently handled (for simplicity) using explicit push
      and pop trap operations (see above) rather than double-barrelled CPS.
      This will likely change in the future.

      Note: All bound variables in Flambda terms must be distinct.
  *)
  type t =
    | Let of Let.t
    | Let_mutable of Let_mutable.t
    | Let_cont of Let_cont.t
    | Apply of Apply.t
    | Apply_cont of Continuation.t * Trap_action.t option * Simple.t list
    | Switch of Name.t * Switch.t
    | Invalid of invalid_term_semantics

  (** Creates a [Let] expression.  (This computes the free variables of the
      defining expression and the body.) *)
  val create_let : Variable.t -> Flambda_kind.t -> Named.t -> t -> t

  (** Create a suitable [expr] to represent the given switch.  (The result may
      not actually be a [Switch].) *)
  val create_int_switch
     : scrutinee:Name.t
    -> arms:Continuation.t Targetint.OCaml.Map.t
    -> Expr.t
  val create_int_switch'
     : scrutinee:Name.t
    -> arms:Continuation.t Targetint.OCaml.Map.t
    -> Expr.t * bool  (* CR mshinwell: improve result type *)
  val create_tag_switch
     : scrutinee:Name.t
    -> arms:Continuation.t Tag.Map.t
    -> Expr.t
  val create_tag_switch'
     : scrutinee:Name.t
    -> arms:Continuation.t Tag.Map.t
    -> Expr.t * bool  (* CR mshinwell: improve result type *)

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

  val free_names : t -> Name_occurrences.t

  (** Compute _all_ names occurring inside an expression. *)
  val used_names
     : ?ignore_uses_as_callee:unit
    -> ?ignore_uses_as_argument:unit
    -> ?ignore_uses_as_continuation_argument:unit
    -> ?ignore_uses_in_project_var:unit
    -> t
    -> Name_occurrences.t

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

  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool

  val print : Format.formatter -> t -> unit
end and Named : sig
  (** Values of type [t] will always be [Let]-bound to a [Variable.t].
      (Note that [Simple.t] values do not need to be [Let]-bound; but they are
      allowed here for convenience.) *)
  type t =
    | Simple of Simple.t
    | Prim of Flambda_primitive.t * Debuginfo.t
    | Set_of_closures of Set_of_closures.t
    | Assign of assign
    | Read_mutable of Mutable_variable.t
    | Coerce of coercion

  (** Compute the free names of the given term. *)
  val free_names
     : ?ignore_uses_in_project_var:unit
    -> t
    -> Name_occurrences.t

  (** Compute _all_ names occurring inside the given term. *)
  val used_names
     : ?ignore_uses_in_project_var:unit
    -> t
    -> Name_occurrences.t

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

  val print : Format.formatter -> t -> unit
end and Let : sig
  (* CR-someday mshinwell: Since we lack expression identifiers on every term,
     we should probably introduce [Mutable_var] into [named] if we introduce
     more complicated analyses on these in the future.  Alternatively, maybe
     consider removing mutable variables altogether. *)

  type t = private {
    var : Variable.t;
    kind : Flambda_kind.t;
    defining_expr : Named.t;
    body : Expr.t;
    (* CR-someday mshinwell: we could consider having these be keys into some
       kind of global cache, to reduce memory usage. *)
    free_names_of_defining_expr : Name_occurrences.t;
    (** A cache of the free names in the defining expression of the
        [Let]. *)
    free_names_of_body : Name_occurrences.t;
    (** A cache of the free variables of the body of the [let].  This is an
        important optimization. *)
  }

  (** Apply the specified function [f] to the given defining expression of
      a [Let]. *)
  val map_defining_expr : Let.t -> f:(Named.t -> Named.t) -> Expr.t
end and Let_mutable : sig
  type t = {
    var : Mutable_variable.t;
    initial_value : Simple.t;
    contents_type : Flambda_type.t;
    body : Expr.t;
  }
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
  type t = {
    body : Expr.t;
    handlers : Let_cont_handlers.t;
  }
end and Let_cont_handlers : sig
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
  type t =
    (* CR mshinwell: Change Non_recursive -> Non_recursive to be consistent *)
    | Non_recursive of {
        name : Continuation.t;
        handler : Continuation_handler.t;
      }
    | Recursive of Continuation_handlers.t

  val free_names : t -> Name_occurrences.t

  (** Return all continuations bound in the given handlers (traversing all
      the way down through the handlers, not just the immediately outermost
      bindings). *)
  val bound_continuations : t -> Continuation.Set.t

  (** Return all continuations free in the given handlers. *)
  val free_continuations : t -> Continuation.Set.t

  type free_and_bound = {
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
end and Continuation_handlers : sig
  type t = Continuation_handler.t Continuation.Map.t
end and Continuation_handler : sig
  type t = {
    params : Typed_parameter.t list;
    stub : bool;
    is_exn_handler : bool;
    (** Continuations used as exception handlers must always be [Non_recursive]
        and must have exactly one argument.  To enable identification of them
        in passes not invoked from [Simplify] (where they could be
        identified by looking at the [Apply_cont]s that reference them) they
        are marked explicitly.
        (Relevant piece of background info: the backend cannot compile
        simultaneously-defined continuations when one or more of them is an
        exception handler.)
    *)
    handler : Expr.t;
  }
end and Set_of_closures : sig
  type t = private {
    function_decls : Function_declarations.t;
    (* CR-soon mshinwell: consider renaming [free_vars].  Also, it's still
       really confusing which side of this map to use when.  "Vars bound by the
       closure" is the domain.
       Another example of when this is confusing:
         let bound_vars_approx =
           Variable.Map.map (Env.find_approx env) set.free_vars
         in
       in [Build_export_info]. *)
    (* CR mshinwell: Now that we've changed the meaning, this should definitely
       be renamed. *)
    free_vars : Free_vars.t;
    (** Mapping from all variables free in the body of the [function_decls] to
        variables in scope at the definition point of the [set_of_closures].
        The domain of this map is sometimes known as the "variables bound by
        the closure". *)
    direct_call_surrogates : Closure_id.t Closure_id.Map.t;
    (** If [direct_call_surrogates] maps [closure_id1] to [closure_id2] then
        direct calls to [closure_id1] should be redirected to [closure_id2].
        This is used to reduce the overhead of transformations that introduce
        wrapper functions (which will be inlined at direct call sites, but will
        penalise indirect call sites).
        N.B. [direct_call_surrogates] might not be transitively closed. *)
  }

  (** Create a set of closures.  Checks are made to ensure that [free_vars]
      are reasonable. *)
  val create
     : function_decls:Function_declarations.t
    -> in_closure:Free_vars.t
    -> direct_call_surrogates:Closure_id.t Closure_id.Map.t
    -> t

  (** Returns true iff the given set of closures has an empty environment. *)
  val has_empty_environment : t -> bool

  val print : Format.formatter -> t -> unit

  (** All names free in the given set of closures. *)
  val free_names : t -> Name_occurrences.t
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

  type t = private {
    set_of_closures_id : Set_of_closures_id.t;
    (** An identifier (unique across all Flambda trees currently in memory)
        of the set of closures associated with this set of function
        declarations. *)
    set_of_closures_origin : Set_of_closures_origin.t;
    (** An identifier of the original set of closures on which this set of
        function declarations is based.  Used to prevent different
        specialisations of the same functions from being inlined/specialised
        within each other. *)
    funs : Function_declaration.t Closure_id.Map.t;
    (** The function(s) defined by the set of function declarations.  The
        keys of this map are often referred to in the code as "fun_var"s. *)
  }

  (** Create a set of function declarations given the individual
      declarations. *)
  val create : funs:Function_declaration.t Closure_id.Map.t -> t

  (** [find f t] raises [Not_found] if [f] is not in [t]. *)
  val find : Closure_id.t -> t -> Function_declaration.t

  (** Create a set of function declarations based on another set of function
      declarations. *)
  val update : t -> funs:Function_declaration.t Closure_id.Map.t -> t

  val import_for_pack
     : t
    -> (Set_of_closures_id.t -> Set_of_closures_id.t)
    -> (Set_of_closures_origin.t -> Set_of_closures_origin.t)
    -> t

  val print : Format.formatter -> t -> unit

  (** All names free in the given function declarations. *)
  val free_names : t -> Name_occurrences.t
end and Function_declaration : sig
  type t = private {
    closure_origin : Closure_origin.t;
    (** The closure from which this function declaration originally came.
        Used as a backstop against unbounded recursion during inlining. *)
    continuation_param : Continuation.t;
    (** The continuation parameter of the function, i.e. to where we must jump
        once the result of the function has been computed.  If the continuation
        takes more than one argument then the backend will compile the function
        so that it returns multiple values. *)
    exn_continuation_param : Continuation.t;
    (** To where we must jump if application of the function raises an
        exception. *)
    return_arity : Flambda_arity.t;
    (** The kinds of the parameters of the [continuation_param] continuation.
        (This encodes whether the function returns multiple and/or unboxed
        values, for example.) *)
    params : Typed_parameter.t list;
    (** The normal (variable) parameters of the function together with their
        types.  Some of the parameters may have non-trivial types that
        indicate previous specialisation of the function. *)
    body : Expr.t;
    (** The code of the function's body. *)
    free_names_in_body : Name_occurrences.t;
    (** All names that occur free in the function's body.  (See note on the
        [free_names] function, below.) *)
    stub : bool;
    (** A stub function is a generated function used to prepare arguments or
        return values to allow indirect calls to functions with a special
        calling convention.  For instance indirect calls to tuplified functions
        must go through a stub.  Stubs will be unconditionally inlined. *)
    dbg : Debuginfo.t;
    (** Debug info for the function declaration. *)
    inline : inline_attribute;
    (** Inlining requirements from the source code. *)
    specialise : specialise_attribute;
    (** Specialising requirements from the source code. *)
    is_a_functor : bool;
    (** Whether the function is known definitively to be a functor. *)
    my_closure : Variable.t;
    (** Binding name of the closure inside the function body.  The only free
        variables allowed in such a body are this variable and the parameters
        of the function.  Accesses to variables within the closure need to go
        via a [Project_var]; accesses to any other simultaneously-defined
        functions need to go via a [Move_within_set_of_closures]. *)
  }

  (** Create a function declaration.  This calculates the free variables and
      symbols occurring in the specified [body].

      To just change the parameters or body of a function the "update" functions
      below should be used, if possible; otherwise care must be taken to
      preserve the [closure_origin].

      When adding a stub to a function the stub should receive a new
      [closure_origin] and the renamed original function should retain its
      existing [closure_origin].
  *)
  val create
     : params:Typed_parameter.t list
    -> continuation_param:Continuation.t
    -> exn_continuation_param:Continuation.t
    -> return_arity:Flambda_arity.t
    -> my_closure:Variable.t
    -> body:Expr.t
    -> stub:bool
    -> dbg:Debuginfo.t
    -> inline:inline_attribute
    -> specialise:specialise_attribute
    -> is_a_functor:bool
    (* CR mshinwell for pchambart: check Closure_origin stuff *)
    -> closure_origin:Closure_origin.t
    -> t

  (** Change only the code of a function declaration. *)
  val update_body : t -> body:Expr.t -> t

  (** Change only the parameters of a function declaration. *)
  val update_params : t -> params:Typed_parameter.t list -> t

  (** Change only the code and parameters of a function declaration. *)
  val update_params_and_body
     : t
    -> params:Typed_parameter.t list
    -> body:Expr.t
    -> t

  (** All names free in the function declaration.  (Note that this may be
      different from the names free in the function _body_, as per [free_names]
      in the type [t], above.) *)
  val free_names : t -> Name_occurrences.t

  val print : Closure_id.t -> Format.formatter -> t -> unit
end and Typed_parameter : sig
  (** A parameter (to a function, continuation, etc.) together with its
      type. *)
  type t

  (** Create a typed parameter. *)
  val create : (Parameter.t -> Flambda_type.t -> t) Flambda_type.type_accessor

  (** As for [create] except that the parameter is given an unknown type of the
      specified kind.  This means that the [type_accessor] functions do not
      need to be provided. *)
  val create_from_kind : Parameter.t -> Flambda_kind.t -> t

  (** The underlying parameter. *)
  val param : t -> Parameter.t

  (** The underlying variable (cf. [Parameter.var]). *)
  val var : t -> Variable.t

  (** As for [var], but returns a [Simple.t] describing the variable. *)
  val simple : t -> Simple.t

  (** The type of the given parameter. *)
  val ty : t -> Flambda_type.t

  (** The kind of the given parameter. *)
  val kind : t -> Flambda_kind.t

  (** Replace the type of the given parameter. *)
  val with_type : t -> Flambda_type.t -> t

  (** Map the underlying variable of the given parameter. *)
  val map_var : t -> f:(Variable.t -> Variable.t) -> t

  (** Map the type of the given parameter. *)
  val map_type : t -> f:(Flambda_type.t -> Flambda_type.t) -> t

  (** Free names in the given parameter's type.  (The variable corresponding
      to the parameter is assumed to be always a binding occurrence.) *)
  val free_names : t -> Name_occurrences.t

  (** Equality on typed parameters. *)
  val equal
     : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
    -> t
    -> t
    -> bool

  val rename : t -> t

  module List : sig
    type nonrec t = t list

    val free_names : t -> Name_occurrences.t

    (** As for [Parameter.List.vars]. *)
    val vars : t -> Variable.t list

    (** As for [vars] but returns a list of [Simple.t] values describing the
        variables. *)
    val simples : t -> Simple.t list

    (** As for [vars] but returns a set. *)
    val var_set : t -> Variable.Set.t

    (** As for [var_set] but returns a set of [Name]s. *)
    val name_set : t -> Name.Set.t

    val equal_vars : t -> Variable.t list -> bool

    val rename : t -> t

    val arity : t -> Flambda_arity.t

    val print : Format.formatter -> t -> unit

    val equal
       : equal_type:(Flambda_type.t -> Flambda_type.t -> bool)
      -> t
      -> t
      -> bool
  end

(* XXX try to remove this
  (** N.B. Sets, maps and hash tables keyed on values of type [t] do not
      take into account the parameter's type in the comparison relation. *)
  include Identifiable.S with type t := t
*)
  val print : Format.formatter -> t -> unit
end and Flambda_type : sig
  include Flambda_type0_intf.S with type expr := Expr.t
end

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
