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

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module K = Flambda_kind

module Make (Expr : sig
  type t
  val print : Format.formatter -> t -> unit
  val free_names : t -> Name.Set.t
end) = struct
  type expr = Expr.t

  type inline_attribute =
    | Always_inline
    | Never_inline
    | Unroll of int
    | Default_inline

  let print_inline_attribute ppf attr =
    let fprintf = Format.fprintf in
    match attr with
    | Always_inline -> fprintf ppf "Always_inline"
    | Never_inline -> fprintf ppf "Never_inline"
    | Unroll n -> fprintf ppf "@[(Unroll %d)@]" n
    | Default_inline -> fprintf ppf "Default_inline"

  type specialise_attribute =
    | Always_specialise
    | Never_specialise
    | Default_specialise

  let print_specialise_attribute ppf attr =
    let fprintf = Format.fprintf in
    match attr with
    | Always_specialise -> fprintf ppf "Always_specialise"
    | Never_specialise -> fprintf ppf "Never_specialise"
    | Default_specialise -> fprintf ppf "Default_specialise"

  type unresolved_value =
    | Set_of_closures_id of Set_of_closures_id.t
    | Export_id of Export_id.t
    | Name of Name.t

  type unknown_because_of =
    | Unresolved_value of unresolved_value
    | Other

  let combine_unknown_because_of u1 u2 =
    (* This logic is valid for both joins and meets. *)
    match u1, u2 with
    | Unresolved_value (Set_of_closures_id s1),
        Unresolved_value (Set_of_closures_id s2) ->
      if Set_of_closures_id.equal s1 s2 then u1 else Other
    | Unresolved_value (Export_id s1), Unresolved_value (Export_id s2) ->
      if Export_id.equal s1 s2 then u1 else Other
    | Unresolved_value (Name s1), Unresolved_value (Name s2) ->
      if Name.equal s1 s2 then u1 else Other
    | _, _ -> Other

  (** Types from other compilation units are loaded lazily.  There are two
      kinds of cross-compilation unit reference to be resolved: via
      [Export_id.t] values and via [Symbol.t] values. *)
  type load_lazily =
    | Export_id of Export_id.t
    | Symbol of Symbol.t

  let print_load_lazily ppf (ll : load_lazily) =
    match ll with
    | Export_id id -> Format.fprintf ppf "(eid %a)" Export_id.print id
    | Symbol sym ->
      Format.fprintf ppf "(sym %a)" Symbol.print sym

  (* CR mshinwell: update comment *)
  (* A value of type [T.t] corresponds to an "approximation" of the result of
     a computation in the program being compiled.  That is to say, it
     represents what knowledge we have about such a result at compile time.
     The simplification pass exploits this information to partially evaluate
     computations.

     At a high level, an approximation for a value [v] has three parts:
     - the "description" (for example, "the constant integer 42");
     - an optional variable;
     - an optional symbol or symbol field.
     If the variable (resp. symbol) is present then that variable (resp.
     symbol) may be used to obtain the value [v].

     The exact semantics of the variable and symbol fields follows.

     Approximations are deduced at particular points in an expression tree,
     but may subsequently be propagated to other locations.

     At the point at which an approximation is built for some value [v], we can
     construct a set of variables (call the set [S]) that are known to alias the
     same value [v].  Each member of [S] will have the same or a more precise
     [descr] field in its approximation relative to the approximation for [v].
     (An increase in precision may currently be introduced for pattern
     matches.)  If [S] is non-empty then it is guaranteed that there is a
     unique member of [S] that was declared in a scope further out ("earlier")
     than all other members of [S].  If such a member exists then it is
     recorded in the [var] field.  Otherwise [var] is [None].

     Analogous to the construction of the set [S], we can construct a set [T]
     consisting of all symbols that are known to alias the value whose
     approximation is being constructed.  If [T] is non-empty then the
     [symbol] field is set to some member of [T]; it does not matter which
     one.  (There is no notion of scope for symbols.)

     Note about mutable blocks:

     Mutable blocks are always represented by [Unknown] or
     [Bottom].  Any other approximation could leave the door open to
     a miscompilation.   Such bad scenarios are most likely a user using
     [Obj.magic] or [Obj.set_field] in an inappropriate situation.
     Such a situation might be:
     [let x = (1, 1) in
     Obj.set_field (Obj.repr x) 0 (Obj.repr 2);
     assert(fst x = 2)]
     The user would probably expect the assertion to be true, but the
     compiler could in fact propagate the value of [x] across the
     [Obj.set_field].

     Insisting that mutable blocks have [Unknown] or [bottom]
     approximations certainly won't always prevent this kind of error, but
     should help catch many of them.

     It is possible that there may be some false positives, with correct
     but unreachable code causing this check to fail.  However the likelihood
     of this seems sufficiently low, especially compared to the advantages
     gained by performing the check, that we include it.

     An example of a pattern that might trigger a false positive is:
     [type a = { a : int }
     type b = { mutable b : int }
     type t =
       | A : a t
       | B : b t
     let f (type x) (v:x t) (r:x) =
       match v with
       | A -> r.a
       | B -> r.b <- 2; 3

     let v =
     let r =
       ref A in
       r := A; (* Some pattern that the compiler can't understand *)
       f !r { a = 1 }]
     When inlining [f], the B branch is unreachable, yet the compiler
     cannot prove it and must therefore keep it.
  *)

  type string_contents =
    | Contents of string
    | Unknown_or_mutable

  module String_info = struct
    type t = {
      contents : string_contents;
      size : Targetint.OCaml.t;
    }

    include Identifiable.Make (struct
      type nonrec t = t

      let compare t1 t2 =
        let c =
          match t1.contents, t2.contents with
          | Contents s1, Contents s2 -> String.compare s1 s2
          | Unknown_or_mutable, Unknown_or_mutable -> 0
          | Contents _, Unknown_or_mutable -> -1
          | Unknown_or_mutable, Contents _ -> 1
        in
        if c <> 0 then c
        else Pervasives.compare t1.size t2.size

      let equal t1 t2 =
        compare t1 t2 = 0

      let hash t = Hashtbl.hash t

      let print _ppf _t = Misc.fatal_error "Not yet implemented"
    end)
  end

  type 'a or_alias =
    | Normal of 'a
    | Alias of Name.t

  type combining_op = Union | Intersection

  type t =
    | Value of ty_value
    | Naked_immediate of ty_naked_immediate
    | Naked_float of ty_naked_float
    | Naked_int32 of ty_naked_int32
    | Naked_int64 of ty_naked_int64
    | Naked_nativeint of ty_naked_nativeint

  and flambda_type = t

  and ty_value = (of_kind_value, Flambda_kind.Value_kind.t) ty
  and ty_naked_immediate = (of_kind_naked_immediate, unit) ty
  and ty_naked_float = (of_kind_naked_float, unit) ty
  and ty_naked_int32 = (of_kind_naked_int32, unit) ty
  and ty_naked_int64 = (of_kind_naked_int64, unit) ty
  and ty_naked_nativeint = (of_kind_naked_nativeint, unit) ty

  and ('a, 'u) ty = ('a, 'u) maybe_unresolved or_alias

  and resolved_t =
    | Value of resolved_ty_value
    | Naked_immediate of resolved_ty_naked_immediate
    | Naked_float of resolved_ty_naked_float
    | Naked_int32 of resolved_ty_naked_int32
    | Naked_int64 of resolved_ty_naked_int64
    | Naked_nativeint of resolved_ty_naked_nativeint

  and resolved_ty_value = (of_kind_value, Flambda_kind.Value_kind.t) resolved_ty
  and resolved_ty_naked_immediate = (of_kind_naked_immediate, unit) resolved_ty
  and resolved_ty_naked_float = (of_kind_naked_float, unit) resolved_ty
  and resolved_ty_naked_int32 = (of_kind_naked_int32, unit) resolved_ty
  and resolved_ty_naked_int64 = (of_kind_naked_int64, unit) resolved_ty
  and resolved_ty_naked_nativeint = (of_kind_naked_nativeint, unit) resolved_ty

  and ('a, 'u) resolved_ty = ('a, 'u) or_unknown_or_bottom or_alias

  and ('a, 'u) maybe_unresolved =
    | Resolved of ('a, 'u) or_unknown_or_bottom
    | Load_lazily of load_lazily

  and ('a, 'u) or_unknown_or_bottom =
    | Unknown of unknown_because_of * 'u
    | Ok of 'a singleton_or_combination
    | Bottom

  and 'a singleton_or_combination =
    | Singleton of 'a
    | Combination of combining_op
        * 'a singleton_or_combination or_alias
        * 'a singleton_or_combination or_alias

  and of_kind_value =
    | Tagged_immediate of ty_naked_immediate
    | Boxed_float of ty_naked_float
    | Boxed_int32 of ty_naked_int32
    | Boxed_int64 of ty_naked_int64
    | Boxed_nativeint of ty_naked_nativeint
    (* CR mshinwell: Add an [Immutable_array] module *)
    | Block of Tag.Scannable.t * (ty_value array)
    | Set_of_closures of set_of_closures
    | Closure of closure
    | String of String_info.t
    | Float_array of ty_naked_float array

  and inlinable_function_declaration = {
    closure_origin : Closure_origin.t;
    continuation_param : Continuation.t;
    is_classic_mode : bool;
    params : (Parameter.t * t) list;
    body : expr;
    free_names_in_body : Name.Set.t;
    result : t list;
    stub : bool;
    dbg : Debuginfo.t;
    inline : inline_attribute;
    specialise : specialise_attribute;
    is_a_functor : bool;
    invariant_params : Variable.Set.t lazy_t;
    size : int option lazy_t;
    direct_call_surrogate : Closure_id.t option;
  }

  and non_inlinable_function_declaration = {
    result : t list;
    direct_call_surrogate : Closure_id.t option;
  }

  and function_declaration =
    | Non_inlinable of non_inlinable_function_declaration
    | Inlinable of inlinable_function_declaration

  and closure = {
    set_of_closures : ty_value;
    closure_id : Closure_id.t;
  }

  and set_of_closures = {
    set_of_closures_id : Set_of_closures_id.t;
    set_of_closures_origin : Set_of_closures_origin.t;
    function_decls : function_declaration Closure_id.Map.t;
    closure_elements : ty_value Var_within_closure.Map.t;
  }

  and of_kind_naked_immediate =
    | Naked_immediate of Immediate.t

  and of_kind_naked_float =
    | Naked_float of Numbers.Float_by_bit_pattern.t

  and of_kind_naked_int32 =
    | Naked_int32 of Int32.t

  and of_kind_naked_int64 =
    | Naked_int64 of Int64.t

  and of_kind_naked_nativeint =
    | Naked_nativeint of Targetint.t

  and typing_context = {
    names_to_types : t Name.Map.t;
    levels_to_names : Name.Set.t Scope_level.Map.t;
    existentials : Name.Set.t;
    existential_freshening : Freshening.t;
  }

  let print_unresolved_value ppf (unresolved : unresolved_value) =
    match unresolved with
    | Set_of_closures_id set ->
      Format.fprintf ppf "Set_of_closures_id %a" Set_of_closures_id.print set
    | Name name ->
      Format.fprintf ppf "Name %a" Name.print name
    | Export_id id ->
      Format.fprintf ppf "Export_id %a" Export_id.print id

  let print_or_alias print_descr ppf var_or_symbol =
    match var_or_symbol with
    | Normal descr -> print_descr ppf descr
    | Alias name -> Name.print ppf name

  let print_maybe_unresolved print_contents ppf (m : _ maybe_unresolved) =
    match m with
    | Resolved contents -> print_contents ppf contents
    | Load_lazily ll -> Format.fprintf ppf "lazy(%a)" print_load_lazily ll

  let print_of_kind_naked_immediate ppf (o : of_kind_naked_immediate) =
    match o with
    | Naked_immediate i -> Format.fprintf ppf "%a" Immediate.print i

  let print_of_kind_naked_float ppf (o : of_kind_naked_float) =
    match o with
    | Naked_float f ->
      Format.fprintf ppf "%a" Numbers.Float_by_bit_pattern.print f

  let print_of_kind_naked_int32 ppf (o : of_kind_naked_int32) =
    match o with
    | Naked_int32 i -> Format.fprintf ppf "%a" Int32.print i

  let print_of_kind_naked_int64 ppf (o : of_kind_naked_int64) =
    match o with
    | Naked_int64 i -> Format.fprintf ppf "%a" Int64.print i

  let print_of_kind_naked_nativeint ppf (o : of_kind_naked_nativeint) =
    match o with
    | Naked_nativeint i -> Format.fprintf ppf "%a" Targetint.print i

  let print_or_unknown_or_bottom print_contents print_unknown_payload ppf
        (o : _ or_unknown_or_bottom) =
    match o with
    | Unknown (reason, payload) ->
      begin match reason with
      | Unresolved_value value ->
        Format.fprintf ppf "?%a(due to unresolved %a)"
          print_unknown_payload payload
          print_unresolved_value value
      | Other -> Format.fprintf ppf "?%a" print_unknown_payload payload
      end;
    | Ok contents -> print_contents ppf contents
    | Bottom -> Format.fprintf ppf "bottom"

  let rec print_singleton_or_combination print_contents ppf soc =
    match soc with
    | Singleton contents -> print_contents ppf contents
    | Combination (op, or_alias1, or_alias2) ->
      let print_part ppf w =
        print_or_alias (print_singleton_or_combination print_contents)
          ppf w
      in
      Format.fprintf ppf "@[(%s@ @[(%a)@]@ @[(%a)@])@]"
        (match op with Union -> "Union" | Intersection -> "Intersection")
        print_part or_alias1
        print_part or_alias2

  let print_ty_generic print_contents print_unknown_payload ppf ty =
    (print_or_alias
      (print_maybe_unresolved
        (print_or_unknown_or_bottom
          (print_singleton_or_combination print_contents)
          print_unknown_payload)))
      ppf ty

(*
  let print_resolved_ty_generic print_contents print_unknown_payload ppf ty =
    (print_or_alias
      (print_or_unknown_or_bottom
        (print_singleton_or_combination print_contents)
        print_unknown_payload))
      ppf ty
*)

  let rec print_of_kind_value ppf (of_kind_value : of_kind_value) =
    match of_kind_value with
    | Tagged_immediate t ->
      Format.fprintf ppf "(tagged_imm %a)" print_ty_naked_immediate t
    | Boxed_float f ->
      Format.fprintf ppf "(boxed_float %a)" print_ty_naked_float f
    | Boxed_int32 n ->
      Format.fprintf ppf "(boxed_int32 %a)" print_ty_naked_int32 n
    | Boxed_int64 n ->
      Format.fprintf ppf "(boxed_int64 %a)" print_ty_naked_int64 n
    | Boxed_nativeint n ->
      Format.fprintf ppf "(boxed_nativeint %a)" print_ty_naked_nativeint n
    | Block (tag, fields) ->
      Format.fprintf ppf "@[[%a: %a]@]"
        Tag.Scannable.print tag
        (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf "; ")
          print_ty_value) (Array.to_list fields)
    | Set_of_closures set_of_closures ->
      print_set_of_closures ppf set_of_closures
    | Closure closure -> print_closure ppf closure
    | String { contents; size; } ->
      begin match contents with
      | Unknown_or_mutable ->
        Format.fprintf ppf "string %a" Targetint.OCaml.print size
      | Contents s ->
        let s =
          let max_size = Targetint.OCaml.ten in
          let long = Targetint.OCaml.compare size max_size > 0 in
          if long then String.sub s 0 8 ^ "..."
          else s
        in
        Format.fprintf ppf "string %a %S" Targetint.OCaml.print size s
      end
    | Float_array fields ->
      Format.fprintf ppf "@[(float_array %a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          print_ty_naked_float)
        (Array.to_list fields)

  and print_ty_value ppf (ty : ty_value) =
    print_ty_generic print_of_kind_value K.Value_kind.print ppf ty

  and _unused = Expr.print

  and print_closure ppf ({ closure_id; set_of_closures; } : closure) =
    Format.fprintf ppf "(closure:@ @[<2>[@ %a @[<2>from@ %a@];@ ]@])"
      Closure_id.print closure_id
      print_ty_value set_of_closures

  and print_inlinable_function_declaration ppf
        (decl : inlinable_function_declaration) =
    Format.fprintf ppf
      "@[(inlinable@ \
        @[(closure_origin %a)@]@,\
        @[(continuation_param %a)@]@,\
        @[(is_classic_mode %b)@]@,\
        @[(params (%a))@]@,\
        @[(body <elided>)@]@,\
        @[(free_names_in_body %a)@]@,\
        @[(result (%a))@]@,\
        @[(stub %b)@]@,\
        @[(dbg %a)@]@,\
        @[(inline %a)@]@,\
        @[(specialise %a)@]@,\
        @[(is_a_functor %b)@]@,\
        @[(invariant_params %a)@]@,\
        @[(size %a)@]@,\
        @[(direct_call_surrogate %a)@])@]"
      Closure_origin.print decl.closure_origin
      Continuation.print decl.continuation_param
      decl.is_classic_mode
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        (fun ppf (param, ty) ->
          Format.fprintf ppf "@[(%a@ :@ %a)@]"
            Parameter.print param
            print ty)) decl.params
      Name.Set.print decl.free_names_in_body
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        (fun ppf ty ->
          Format.fprintf ppf "%a"
            print ty)) decl.result
      decl.stub
      Debuginfo.print_compact decl.dbg
      print_inline_attribute decl.inline
      print_specialise_attribute decl.specialise
      decl.is_a_functor
      Variable.Set.print (Lazy.force decl.invariant_params)
      (Misc.Stdlib.Option.print Format.pp_print_int) (Lazy.force decl.size)
      (Misc.Stdlib.Option.print Closure_id.print) decl.direct_call_surrogate

  and print_non_inlinable_function_declaration ppf
        (decl : non_inlinable_function_declaration) =
    Format.fprintf ppf
      "@[(non_inlinable@ \
        @[(result (%a))@]@,\
        @[(direct_call_surrogate %a)@])@]"
      (Format.pp_print_list ~pp_sep:(fun ppf () -> Format.fprintf ppf ", ")
        (fun ppf ty ->
          Format.fprintf ppf "%a"
            print ty)) decl.result
      (Misc.Stdlib.Option.print Closure_id.print) decl.direct_call_surrogate

  and print_function_declaration ppf (decl : function_declaration) =
    match decl with
    | Inlinable decl -> print_inlinable_function_declaration ppf decl
    | Non_inlinable decl -> print_non_inlinable_function_declaration ppf decl

  and print_function_declarations ppf function_decls =
    Format.fprintf ppf "%a"
      (Closure_id.Map.print print_function_declaration)
      function_decls

  and print_set_of_closures ppf set =
    Format.fprintf ppf
      "@[(@[(set_of_closures_id@ %a)@]@,\
          @[(set_of_closures_origin@ %a)@]@,\
          @[(function_decls@ %a)@]@,\
          @[(closure_elements@ %a)@])@]"
      Set_of_closures_id.print set.set_of_closures_id
      Set_of_closures_origin.print set.set_of_closures_origin
      print_function_declarations set.function_decls
      (Var_within_closure.Map.print print_ty_value) set.closure_elements

  and print_ty_naked_immediate ppf (ty : ty_naked_immediate) =
    print_ty_generic print_of_kind_naked_immediate (fun _ () -> ()) ppf ty

  and print_ty_naked_float ppf (ty : ty_naked_float) =
    print_ty_generic print_of_kind_naked_float (fun _ () -> ()) ppf ty

  and print_ty_naked_int32 ppf (ty : ty_naked_int32) =
    print_ty_generic print_of_kind_naked_int32 (fun _ () -> ()) ppf ty

  and print_ty_naked_int64 ppf (ty : ty_naked_int64) =
    print_ty_generic print_of_kind_naked_int64 (fun _ () -> ()) ppf ty

  and print_ty_naked_nativeint ppf (ty : ty_naked_nativeint) =
    print_ty_generic print_of_kind_naked_nativeint (fun _ () -> ()) ppf ty

  and print ppf (t : t) =
    match t with
    | Value ty ->
      Format.fprintf ppf "(Value (%a))" print_ty_value ty
    | Naked_immediate ty ->
      Format.fprintf ppf "(Naked_immediate (%a))" print_ty_naked_immediate ty
    | Naked_float ty ->
      Format.fprintf ppf "(Naked_float (%a))" print_ty_naked_float ty
    | Naked_int32 ty ->
      Format.fprintf ppf "(Naked_int32 (%a))" print_ty_naked_int32 ty
    | Naked_int64 ty ->
      Format.fprintf ppf "(Naked_int64 (%a))" print_ty_naked_int64 ty
    | Naked_nativeint ty ->
      Format.fprintf ppf "(Naked_nativeint (%a))" print_ty_naked_nativeint ty

  let print_ty_value_array ppf ty_values =
    Format.fprintf ppf "@[[| %a |]@]"
      (Format.pp_print_list
        ~pp_sep:(fun ppf () -> Format.pp_print_string ppf "; ")
        print_ty_value)
      (Array.to_list ty_values)

  let alias (kind : Flambda_kind.t) name : t =
    match kind with
    | Value _ -> Value (Alias name)
    | Naked_immediate -> Naked_immediate (Alias name)
    | Naked_float -> Naked_float (Alias name)
    | Naked_int32 -> Naked_int32 (Alias name)
    | Naked_int64 -> Naked_int64 (Alias name)
    | Naked_nativeint -> Naked_nativeint (Alias name)

(*
  let unknown_as_ty_value reason value_kind : ty_value =
    Normal (Resolved (Unknown (reason, value_kind)))
*)

  let unknown_as_resolved_ty_value reason value_kind : resolved_ty_value =
    Normal (Unknown (reason, value_kind))

  let unknown_as_resolved_ty_naked_immediate reason value_kind
        : resolved_ty_naked_immediate =
    Normal (Unknown (reason, value_kind))

  let unknown_as_resolved_ty_naked_float reason value_kind
        : resolved_ty_naked_float =
    Normal (Unknown (reason, value_kind))

  let unknown_as_resolved_ty_naked_int32 reason value_kind
        : resolved_ty_naked_int32 =
    Normal (Unknown (reason, value_kind))

  let unknown_as_resolved_ty_naked_int64 reason value_kind
        : resolved_ty_naked_int64 =
    Normal (Unknown (reason, value_kind))

  let unknown_as_resolved_ty_naked_nativeint reason value_kind
        : resolved_ty_naked_nativeint =
    Normal (Unknown (reason, value_kind))

  let bottom (kind : K.t) : t =
    match kind with
    | Value _ -> Value (Normal (Resolved Bottom))
    | Naked_immediate -> Naked_immediate (Normal (Resolved Bottom))
    | Naked_float -> Naked_float (Normal (Resolved Bottom))
    | Naked_int32 -> Naked_int32 (Normal (Resolved Bottom))
    | Naked_int64 -> Naked_int64 (Normal (Resolved Bottom))
    | Naked_nativeint -> Naked_nativeint (Normal (Resolved Bottom))

  let this_naked_immediate (i : Immediate.t) : t =
    let i : of_kind_naked_immediate = Naked_immediate i in
    Naked_immediate (Normal (Resolved (Ok (Singleton i))))

  let this_naked_float f : t =
    let f : of_kind_naked_float = Naked_float f in
    Naked_float (Normal (Resolved (Ok (Singleton f))))

  let this_naked_int32 n : t =
    let n : of_kind_naked_int32 = Naked_int32 n in
    Naked_int32 (Normal (Resolved (Ok (Singleton n))))

  let this_naked_int64 n : t =
    let n : of_kind_naked_int64 = Naked_int64 n in
    Naked_int64 (Normal (Resolved (Ok (Singleton n))))

  let this_naked_nativeint n : t =
    let n : of_kind_naked_nativeint = Naked_nativeint n in
    Naked_nativeint (Normal (Resolved (Ok (Singleton n))))

  let tag_immediate (t : t) : t =
    match t with
    | Naked_immediate ty_naked_immediate ->
      Value (Normal (Resolved (Ok (Singleton (
        Tagged_immediate ty_naked_immediate)))))
    | Value _
    | Naked_float _
    | Naked_int32 _
    | Naked_int64 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Expected type of kind [Naked_immediate] but got %a"
        print t

  let box_float (t : t) : t =
    match t with
    | Naked_float ty_naked_float ->
      Value (Normal (Resolved (Ok (Singleton (
        Boxed_float ty_naked_float)))))
    | Value _
    | Naked_immediate _
    | Naked_int32 _
    | Naked_int64 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Expected type of kind [Naked_float] but got %a"
        print t

  let box_int32 (t : t) : t =
    match t with
    | Naked_int32 ty_naked_int32 ->
      Value (Normal (Resolved (Ok (Singleton (
        Boxed_int32 ty_naked_int32)))))
    | Value _
    | Naked_immediate _
    | Naked_float _
    | Naked_int64 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Expected type of kind [Naked_int32] but got %a"
        print t

  let box_int64 (t : t) : t =
    match t with
    | Naked_int64 ty_naked_int64 ->
      Value (Normal (Resolved (Ok (Singleton (
        Boxed_int64 ty_naked_int64)))))
    | Value _
    | Naked_immediate _
    | Naked_float _
    | Naked_int32 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Expected type of kind [Naked_int64] but got %a"
        print t

  let box_nativeint (t : t) : t =
    match t with
    | Naked_nativeint ty_naked_nativeint ->
      Value (Normal (Resolved (Ok (Singleton (
        Boxed_nativeint ty_naked_nativeint)))))
    | Value _
    | Naked_immediate _
    | Naked_float _
    | Naked_int32 _
    | Naked_int64 _ ->
      Misc.fatal_errorf "Expected type of kind [Naked_nativeint] but got %a"
        print t

  let this_tagged_immediate i : t =
    let i : ty_naked_immediate =
      let i : of_kind_naked_immediate = Naked_immediate i in
      Normal (Resolved (Ok (Singleton i)))
    in
    Value (Normal (Resolved (Ok (Singleton (Tagged_immediate i)))))

  let this_boxed_float f =
    let f : ty_naked_float =
      let f : of_kind_naked_float = Naked_float f in
      Normal (Resolved (Ok (Singleton f)))
    in
    Value (Normal (Resolved (Ok (Singleton (Boxed_float f)))))

  let this_boxed_int32 n =
    let n : ty_naked_int32 =
      let n : of_kind_naked_int32 = Naked_int32 n in
      Normal (Resolved (Ok (Singleton n)))
    in
    Value (Normal (Resolved (Ok (Singleton (Boxed_int32 n)))))

  let this_boxed_int64 n =
    let n : ty_naked_int64 =
      let n : of_kind_naked_int64 = Naked_int64 n in
      Normal (Resolved (Ok (Singleton n)))
    in
    Value (Normal (Resolved (Ok (Singleton (Boxed_int64 n)))))

  let this_boxed_nativeint n =
    let n : ty_naked_nativeint =
      let n : of_kind_naked_nativeint = Naked_nativeint n in
      Normal (Resolved (Ok (Singleton n)))
    in
    Value (Normal (Resolved (Ok (Singleton (Boxed_nativeint n)))))

  let this_immutable_string_as_ty_value str : ty_value =
    let str : String_info.t =
      { contents = Contents str;
        (* CR mshinwell: Possibility for exception? *)
        size = Targetint.OCaml.of_int (String.length str);
      }
    in
    Normal (Resolved (Ok (Singleton (String str))))

  let this_immutable_string str : t =
    Value (this_immutable_string_as_ty_value str)

  let immutable_string_as_ty_value ~size : ty_value =
    let str : String_info.t =
      { contents = Unknown_or_mutable;
        size;
      }
    in
    Normal (Resolved (Ok (Singleton (String str))))

  let immutable_string ~size : t =
    Value (immutable_string_as_ty_value ~size)

  let mutable_string ~size : t =
    let str : String_info.t =
      { contents = Unknown_or_mutable;
        size;
      }
    in
    Value (Normal (Resolved (Ok (Singleton (String str)))))

  (* CR mshinwell: We need to think about these float array functions in
     conjunction with the 4.06 feature for disabling the float array
     optimisation *)

  let this_immutable_float_array fields : t =
    let make_field f : ty_naked_float =
      let f : of_kind_naked_float = Naked_float f in
      Normal (Resolved (Ok (Singleton f)))
    in
    let fields = Array.map make_field fields in
    Value (Normal (Resolved (Ok (Singleton (Float_array fields)))))

  let immutable_float_array fields : t =
(*
    let fields =
      Array.map (fun (field : t) ->
          match field with
          | Naked_float ty_naked_float -> ty_naked_float
          | Value _ | Naked_immediate _ | Naked_int32 _ | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Can only form [Float_array] types with fields \
                of kind [Naked_float].  Wrong field type: %a"
              print field)
        fields
    in
*)
    Value (Normal (Resolved (Ok (Singleton (Float_array fields)))))

  let mutable_float_array0 ~size : _ singleton_or_combination =
    let make_field () : ty_naked_float =
      Normal (Resolved (Unknown (Other, ())))
    in
    (* CR mshinwell: dubious for cross compilation *)
    let size = Targetint.OCaml.to_int size in
    let fields = Array.init size (fun _ -> make_field ()) in
    Singleton (Float_array fields)

  let mutable_float_array ~size : t =
    let ty = mutable_float_array0 ~size in
    Value (Normal (Resolved (Ok ty)))

  let block tag fields : t =
(*
    let fields =
      Array.map (fun (field : t) ->
          match field with
          | Value ty_value -> ty_value
          | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Can only form [Block] types with fields of \
                kind [Value].  Wrong field type: %a"
              print field)
        fields
    in
*)
    Value (Normal (Resolved (Ok (Singleton (Block (tag, fields))))))

  let export_id_loaded_lazily (kind : K.t) export_id : t =
    match kind with
    | Value _ ->
      Value (Normal (Load_lazily (Export_id export_id)))
    | Naked_immediate ->
      Naked_immediate (Normal (Load_lazily (Export_id export_id)))
    | Naked_float ->
      Naked_float (Normal (Load_lazily (Export_id export_id)))
    | Naked_int32 ->
      Naked_int32 (Normal (Load_lazily (Export_id export_id)))
    | Naked_int64 ->
      Naked_int64 (Normal (Load_lazily (Export_id export_id)))
    | Naked_nativeint ->
      Naked_nativeint (Normal (Load_lazily (Export_id export_id)))

  let symbol_loaded_lazily sym : t =
    Value (Normal (Load_lazily (Symbol sym)))

  let any_naked_immediate () : t =
    Naked_immediate (Normal (Resolved (Unknown (Other, ()))))

  let any_naked_float () : t =
    Naked_float (Normal (Resolved (Unknown (Other, ()))))

  let any_naked_float_as_ty_naked_float () : ty_naked_float =
    Normal (Resolved (Unknown (Other, ())))

  let any_naked_int32 () : t =
    Naked_int32 (Normal (Resolved (Unknown (Other, ()))))

  let any_naked_int64 () : t =
    Naked_int64 (Normal (Resolved (Unknown (Other, ()))))

  let any_naked_nativeint () : t =
    Naked_nativeint (Normal (Resolved (Unknown (Other, ()))))

  let any_value_as_ty_value value_kind unknown_because_of : ty_value =
    Normal (Resolved (Unknown (unknown_because_of, value_kind)))

  let any_value value_kind unknown_because_of : t =
    Value (any_value_as_ty_value value_kind unknown_because_of)

  let unknown (kind : K.t) unknown_because_of =
    match kind with
    | Value value_kind -> any_value value_kind unknown_because_of
    | Naked_immediate -> any_naked_immediate ()
    | Naked_float -> any_naked_float ()
    | Naked_int32 -> any_naked_int32 ()
    | Naked_int64 -> any_naked_int64 ()
    | Naked_nativeint -> any_naked_nativeint ()

  let any_tagged_immediate () : t =
    let i : ty_naked_immediate = Normal (Resolved (Unknown (Other, ()))) in
    Value (Normal (Resolved (Ok (Singleton (Tagged_immediate i)))))

  let any_boxed_float () =
    let f : ty_naked_float = Normal (Resolved (Unknown (Other, ()))) in
    Value (Normal (Resolved (Ok (Singleton (Boxed_float f)))))

  let any_boxed_int32 () =
    let n : ty_naked_int32 = Normal (Resolved (Unknown (Other, ()))) in
    Value (Normal (Resolved (Ok (Singleton (Boxed_int32 n)))))

  let any_boxed_int64 () =
    let n : ty_naked_int64 = Normal (Resolved (Unknown (Other, ()))) in
    Value (Normal (Resolved (Ok (Singleton (Boxed_int64 n)))))

  let any_boxed_nativeint () =
    let n : ty_naked_nativeint = Normal (Resolved (Unknown (Other, ()))) in
    Value (Normal (Resolved (Ok (Singleton (Boxed_nativeint n)))))

  (* CR mshinwell: Check this is being used correctly
  let resolved_ty_value_for_predefined_exception ~name : resolved_ty_value =
    let fields =
      [| this_immutable_string_as_ty_value name;
         unknown_as_ty_value Other Must_scan;
      |]
    in
    Normal (Ok (Singleton (Block (Tag.Scannable.object_tag, fields))))
*)

  module type Importer = sig
    val import_value_type_as_resolved_ty_value
       : ty_value
      -> resolved_ty_value

    val import_naked_immediate_type_as_resolved_ty_naked_immediate
       : ty_naked_immediate
      -> resolved_ty_naked_immediate

    val import_naked_float_type_as_resolved_ty_naked_float
       : ty_naked_float
      -> resolved_ty_naked_float

    val import_naked_int32_type_as_resolved_ty_naked_int32
       : ty_naked_int32
      -> resolved_ty_naked_int32

    val import_naked_int64_type_as_resolved_ty_naked_int64
       : ty_naked_int64
      -> resolved_ty_naked_int64

    val import_naked_nativeint_type_as_resolved_ty_naked_nativeint
       : ty_naked_nativeint
      -> resolved_ty_naked_nativeint

    val import_value_type : ty_value -> resolved_t
    val import_naked_immediate_type : ty_naked_immediate -> resolved_t
    val import_naked_float_type : ty_naked_float -> resolved_t
    val import_naked_int32_type : ty_naked_int32 -> resolved_t
    val import_naked_int64_type : ty_naked_int64 -> resolved_t
    val import_naked_nativeint_type : ty_naked_nativeint -> resolved_t
  end

  module type Importer_intf = sig
    val import_export_id : Export_id.t -> t option
    val import_symbol : Symbol.t -> t option
  end

  type 'a type_accessor =
       importer:(module Importer)
    -> type_of_name:(Name.t -> t option)
    -> 'a

  type 'a with_importer =
       importer:(module Importer)
    -> 'a

  type 'a create_resolved_t_result =
    | Have_resolved of 'a
    | Load_lazily_again of load_lazily

  module Make_importer (S : Importer_intf) : Importer = struct
    type 'a import_result =
      | Have_resolved of 'a
      | Treat_as_unknown_must_scan of unknown_because_of

    let import_type (type a) ll ~create_symbol
          ~(create_resolved_t : t -> a create_resolved_t_result) =
      let rec import_type (ll : load_lazily) ~export_ids_seen ~symbols_seen
            : a import_result =
        match ll with
        | Export_id id ->
          if Export_id.Set.mem id export_ids_seen then begin
            Misc.fatal_errorf "Circularity whilst resolving export ID %a"
              Export_id.print id
          end;
          begin match S.import_export_id id with
          | None -> Treat_as_unknown_must_scan (Unresolved_value (Export_id id))
          | Some t ->
            match create_resolved_t t with
            | Have_resolved resolved_t -> Have_resolved resolved_t
            | Load_lazily_again ll ->
              let export_ids_seen = Export_id.Set.add id export_ids_seen in
              import_type ll ~export_ids_seen ~symbols_seen
          end
        | Symbol sym ->
          let t =
            let current_unit = Compilation_unit.get_current_exn () in
            if Symbol.in_compilation_unit sym current_unit
              || Symbol.is_predefined_exception sym
            then begin
              Some (create_symbol sym)
            end else begin
              if Symbol.Set.mem sym symbols_seen then begin
                Misc.fatal_errorf "Circularity whilst resolving symbol %a"
                  Symbol.print sym
              end;
              S.import_symbol sym
            end
          in
          match t with
          | None ->
            Treat_as_unknown_must_scan (Unresolved_value (
              Name (Name.symbol sym)))
          | Some t ->
            match create_resolved_t t with
            (* CR mshinwell: We used to [augment_with_symbol] here but maybe
               we don't need it any more? *)
            | Have_resolved resolved_t -> Have_resolved resolved_t
            | Load_lazily_again ll ->
              let symbols_seen = Symbol.Set.add sym symbols_seen in
              import_type ll ~export_ids_seen ~symbols_seen
      in
      import_type ll ~export_ids_seen:Export_id.Set.empty
        ~symbols_seen:Symbol.Set.empty

    let import_value_type_as_resolved_ty_value (ty : ty_value)
          : resolved_ty_value =
      match ty with
      | Alias name -> Alias name
      | Normal (Resolved ty) -> Normal ty
      | Normal (Load_lazily load_lazily) ->
        let create_resolved_t t : resolved_ty_value create_resolved_t_result =
          match t with
          | Value ty ->
            begin match ty with
            | Alias name -> Have_resolved (Alias name)
            | Normal (Resolved descr) -> Have_resolved (Normal descr)
            | Normal (Load_lazily ll) -> Load_lazily_again ll
            end
          | Naked_immediate _
          | Naked_float _
          | Naked_int32 _
          | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Value]"
              print_load_lazily load_lazily
        in
        let create_symbol sym : t = Value (Alias (Name.symbol sym)) in
        let result =
          import_type load_lazily ~create_symbol ~create_resolved_t
        in
        match result with
        | Have_resolved result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_value reason Unknown

    let import_value_type ty : resolved_t =
      Value (import_value_type_as_resolved_ty_value ty)

    let import_naked_immediate_type_as_resolved_ty_naked_immediate
          (ty : ty_naked_immediate) : resolved_ty_naked_immediate =
      match ty with
      | Alias name -> Alias name
      | Normal (Resolved ty) -> Normal ty
      | Normal (Load_lazily load_lazily) ->
        let create_resolved_t t
              : resolved_ty_naked_immediate create_resolved_t_result =
          match t with
          | Naked_immediate ty ->
            begin match ty with
            | Alias name -> Have_resolved (Alias name)
            | Normal (Resolved descr) -> Have_resolved (Normal descr)
            | Normal (Load_lazily ll) -> Load_lazily_again ll
            end
          | Value _
          | Naked_float _
          | Naked_int32 _
          | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Naked_immediate]"
              print_load_lazily load_lazily
        in
        let create_symbol sym =
          Misc.fatal_errorf "Symbols cannot be imported at kinds other than \
              [Value]: %a"
            Symbol.print sym
        in
        let result =
          import_type load_lazily ~create_symbol ~create_resolved_t
        in
        match result with
        | Have_resolved result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_naked_immediate reason ()

    let import_naked_immediate_type ty : resolved_t =
      Naked_immediate (
        import_naked_immediate_type_as_resolved_ty_naked_immediate ty)

    let import_naked_float_type_as_resolved_ty_naked_float
          (ty : ty_naked_float) : resolved_ty_naked_float =
      match ty with
      | Alias name -> Alias name
      | Normal (Resolved ty) -> Normal ty
      | Normal (Load_lazily load_lazily) ->
        let create_resolved_t t
              : resolved_ty_naked_float create_resolved_t_result =
          match t with
          | Naked_float ty ->
            begin match ty with
            | Alias name -> Have_resolved (Alias name)
            | Normal (Resolved descr) -> Have_resolved (Normal descr)
            | Normal (Load_lazily ll) -> Load_lazily_again ll
            end
          | Value _
          | Naked_immediate _
          | Naked_int32 _
          | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Naked_float]"
              print_load_lazily load_lazily
        in
        let create_symbol sym =
          Misc.fatal_errorf "Symbols cannot be imported at kinds other than \
              [Value]: %a"
            Symbol.print sym
        in
        let result =
          import_type load_lazily ~create_symbol ~create_resolved_t
        in
        match result with
        | Have_resolved result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_naked_float reason ()

    let import_naked_float_type ty : resolved_t =
      Naked_float (import_naked_float_type_as_resolved_ty_naked_float ty)

    let import_naked_int32_type_as_resolved_ty_naked_int32
          (ty : ty_naked_int32) : resolved_ty_naked_int32 =
      match ty with
      | Alias name -> Alias name
      | Normal (Resolved ty) -> Normal ty
      | Normal (Load_lazily load_lazily) ->
        let create_resolved_t t
              : resolved_ty_naked_int32 create_resolved_t_result =
          match t with
          | Naked_int32 ty ->
            begin match ty with
            | Alias name -> Have_resolved (Alias name)
            | Normal (Resolved descr) -> Have_resolved (Normal descr)
            | Normal (Load_lazily ll) -> Load_lazily_again ll
            end
          | Value _
          | Naked_immediate _
          | Naked_float _
          | Naked_int64 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Naked_int32]"
              print_load_lazily load_lazily
        in
        let create_symbol sym =
          Misc.fatal_errorf "Symbols cannot be imported at kinds other than \
              [Value]: %a"
            Symbol.print sym
        in
        let result =
          import_type load_lazily ~create_symbol ~create_resolved_t
        in
        match result with
        | Have_resolved result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_naked_int32 reason ()

    let import_naked_int32_type ty : resolved_t =
      Naked_int32 (import_naked_int32_type_as_resolved_ty_naked_int32 ty)

    let import_naked_int64_type_as_resolved_ty_naked_int64
          (ty : ty_naked_int64) : resolved_ty_naked_int64 =
      match ty with
      | Alias name -> Alias name
      | Normal (Resolved ty) -> Normal ty
      | Normal (Load_lazily load_lazily) ->
        let create_resolved_t t
              : resolved_ty_naked_int64 create_resolved_t_result =
          match t with
          | Naked_int64 ty ->
            begin match ty with
            | Alias name -> Have_resolved (Alias name)
            | Normal (Resolved descr) -> Have_resolved (Normal descr)
            | Normal (Load_lazily ll) -> Load_lazily_again ll
            end
          | Value _
          | Naked_immediate _
          | Naked_float _
          | Naked_int32 _
          | Naked_nativeint _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Naked_int64]"
              print_load_lazily load_lazily
        in
        let create_symbol sym =
          Misc.fatal_errorf "Symbols cannot be imported at kinds other than \
              [Value]: %a"
            Symbol.print sym
        in
        let result =
          import_type load_lazily ~create_symbol ~create_resolved_t
        in
        match result with
        | Have_resolved result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_naked_int64 reason ()

    let import_naked_int64_type ty : resolved_t =
      Naked_int64 (import_naked_int64_type_as_resolved_ty_naked_int64 ty)

    let import_naked_nativeint_type_as_resolved_ty_naked_nativeint
          (ty : ty_naked_nativeint) : resolved_ty_naked_nativeint =
      match ty with
      | Alias name -> Alias name
      | Normal (Resolved ty) -> Normal ty
      | Normal (Load_lazily load_lazily) ->
        let create_resolved_t t
              : resolved_ty_naked_nativeint create_resolved_t_result =
          match t with
          | Naked_nativeint ty ->
            begin match ty with
            | Alias name -> Have_resolved (Alias name)
            | Normal (Resolved descr) -> Have_resolved (Normal descr)
            | Normal (Load_lazily ll) -> Load_lazily_again ll
            end
          | Value _
          | Naked_immediate _
          | Naked_float _
          | Naked_int32 _
          | Naked_int64 _ ->
            Misc.fatal_errorf "Kind mismatch when importing %a; expected kind \
                [Naked_nativeint]"
              print_load_lazily load_lazily
        in
        let create_symbol sym =
          Misc.fatal_errorf "Symbols cannot be imported at kinds other than \
              [Value]: %a"
            Symbol.print sym
        in
        let result =
          import_type load_lazily ~create_symbol ~create_resolved_t
        in
        match result with
        | Have_resolved result -> result
        | Treat_as_unknown_must_scan reason ->
          unknown_as_resolved_ty_naked_nativeint reason ()

    let import_naked_nativeint_type ty : resolved_t =
      Naked_nativeint (
        import_naked_nativeint_type_as_resolved_ty_naked_nativeint ty)
  end

  module No_importing = struct
    let import_export_id _ = None
    let import_symbol _ = None
  end

  module Null_importer = Make_importer (No_importing)

  let null_importer = (module Null_importer : Importer)

  let with_null_importer f =
    f ~importer:null_importer
      ~type_of_name:(fun name ->
        Misc.fatal_errorf "Unbound name %a in null importer" Name.print name)

  let force_to_kind_value t =
    match t with
    | Value ty_value -> ty_value
    | Naked_immediate _
    | Naked_float _
    | Naked_int32 _
    | Naked_int64 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Value]): %a"
        print t

  let force_to_kind_naked_immediate t =
    match t with
    | Naked_immediate ty_naked_immediate -> ty_naked_immediate
    | Value _
    | Naked_float _
    | Naked_int32 _
    | Naked_int64 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Naked_immediate]): %a"
        print t

  let force_to_kind_naked_float t =
    match t with
    | Naked_float ty_naked_float -> ty_naked_float
    | Value _
    | Naked_immediate _
    | Naked_int32 _
    | Naked_int64 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Naked_float]): %a"
        print t

  let force_to_kind_naked_int32 t =
    match t with
    | Naked_int32 ty_naked_int32 -> ty_naked_int32
    | Value _
    | Naked_immediate _
    | Naked_float _
    | Naked_int64 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Naked_int32]): %a"
        print t

  let force_to_kind_naked_int64 t =
    match t with
    | Naked_int64 ty_naked_int64 -> ty_naked_int64
    | Value _
    | Naked_immediate _
    | Naked_float _
    | Naked_int32 _
    | Naked_nativeint _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Naked_int64]): %a"
        print t

  let force_to_kind_naked_nativeint t =
    match t with
    | Naked_nativeint ty_naked_nativeint -> ty_naked_nativeint
    | Value _
    | Naked_immediate _
    | Naked_float _
    | Naked_int32 _
    | Naked_int64 _ ->
      Misc.fatal_errorf "Type has wrong kind (expected [Naked_nativeint]): %a"
        print t

  let t_of_ty_value (ty : ty_value) : t = Value ty

  let t_of_ty_naked_float (ty : ty_naked_float) : t = Naked_float ty

  let ty_of_resolved_ty (ty : _ resolved_ty) : _ ty =
    match ty with
    | Normal ty -> Normal ((Resolved ty) : _ maybe_unresolved)
    | Alias name -> Alias name

  let ty_of_resolved_ok_ty (ty : _ singleton_or_combination or_alias)
        : _ ty =
    match ty with
    | Normal ty -> Normal ((Resolved (Ok ty)) : _ maybe_unresolved)
    | Alias name -> Alias name

  let resolve_aliases_on_ty (type a) ~importer_this_kind
        ~(force_to_kind : t -> (a, _) ty)
        ~(type_of_name : Name.t -> t option)
        (ty : (a, _) ty)
        : (a, _) resolved_ty * (Name.t option) =
    let rec resolve_aliases names_seen ~canonical_name (ty : _ resolved_ty) =
      match ty with
      | Normal _ -> ty, canonical_name
      | Alias name ->
        if Name.Set.mem name names_seen then begin
          (* CR-soon mshinwell: Improve message -- but this means passing the
             printing functions to this function. *)
          Misc.fatal_errorf "Loop on %a whilst resolving aliases"
            Name.print name
        end;
        let canonical_name = Some name in
        begin match type_of_name name with
        | None ->
          (* The type could not be obtained but we still wish to keep the
             name (in case for example a .cmx file subsequently becomes
             available). *)
          ty, canonical_name
        | Some t ->
          let names_seen = Name.Set.add name names_seen in
          let ty = force_to_kind t in
          resolve_aliases names_seen ~canonical_name (importer_this_kind ty)
        end
    in
    resolve_aliases Name.Set.empty ~canonical_name:None
      (importer_this_kind ty)

  let resolve_aliases_and_squash_unresolved_names_on_ty ~importer_this_kind
        ~force_to_kind ~type_of_name ~unknown_payload ty =
    let ty, canonical_name =
      resolve_aliases_on_ty ~importer_this_kind ~force_to_kind ~type_of_name ty
    in
    let ty =
      match ty with
      | Normal ty -> ty
      | Alias name -> Unknown (Unresolved_value (Name name), unknown_payload)
    in
    ty, canonical_name

  let resolve_aliases ~importer ~type_of_name t : t * (Name.t option) =
    let module I = (val importer : Importer) in
    match t with
    | Value ty ->
      let importer_this_kind = I.import_value_type_as_resolved_ty_value in
      let force_to_kind = force_to_kind_value in
      let resolved_ty, canonical_name =
        resolve_aliases_on_ty ~importer_this_kind ~force_to_kind
          ~type_of_name ty
      in
      Value (ty_of_resolved_ty resolved_ty), canonical_name
    | Naked_immediate ty ->
      let importer_this_kind =
        I.import_naked_immediate_type_as_resolved_ty_naked_immediate
      in
      let force_to_kind = force_to_kind_naked_immediate in
      let resolved_ty, canonical_name =
        resolve_aliases_on_ty ~importer_this_kind ~force_to_kind
          ~type_of_name ty
      in
      Naked_immediate (ty_of_resolved_ty resolved_ty), canonical_name
    | Naked_float ty ->
      let importer_this_kind =
        I.import_naked_float_type_as_resolved_ty_naked_float
      in
      let force_to_kind = force_to_kind_naked_float in
      let resolved_ty, canonical_name =
        resolve_aliases_on_ty ~importer_this_kind ~force_to_kind
          ~type_of_name ty
      in
      Naked_float (ty_of_resolved_ty resolved_ty), canonical_name
    | Naked_int32 ty ->
      let importer_this_kind =
        I.import_naked_int32_type_as_resolved_ty_naked_int32
      in
      let force_to_kind = force_to_kind_naked_int32 in
      let resolved_ty, canonical_name =
        resolve_aliases_on_ty ~importer_this_kind ~force_to_kind
          ~type_of_name ty
      in
      Naked_int32 (ty_of_resolved_ty resolved_ty), canonical_name
    | Naked_int64 ty ->
      let importer_this_kind =
        I.import_naked_int64_type_as_resolved_ty_naked_int64
      in
      let force_to_kind = force_to_kind_naked_int64 in
      let resolved_ty, canonical_name =
        resolve_aliases_on_ty ~importer_this_kind ~force_to_kind
          ~type_of_name ty
      in
      Naked_int64 (ty_of_resolved_ty resolved_ty), canonical_name
    | Naked_nativeint ty ->
      let importer_this_kind =
        I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint
      in
      let force_to_kind = force_to_kind_naked_nativeint in
      let resolved_ty, canonical_name =
        resolve_aliases_on_ty ~importer_this_kind ~force_to_kind
          ~type_of_name ty
      in
      Naked_nativeint (ty_of_resolved_ty resolved_ty), canonical_name

  let value_kind_ty_value ~importer ~type_of_name ty =
    let rec value_kind_ty_value (ty : ty_value) : K.Value_kind.t =
      let module I = (val importer : Importer) in
      let importer_this_kind = I.import_value_type_as_resolved_ty_value in
      let (ty : _ or_unknown_or_bottom), _canonical_name =
        resolve_aliases_and_squash_unresolved_names_on_ty ~importer_this_kind
          ~force_to_kind:force_to_kind_value
          ~type_of_name
          ~unknown_payload:K.Value_kind.Unknown
          ty
      in
      match ty with
      | Unknown (_, value_kind) -> value_kind
      | Ok (Singleton (Tagged_immediate _)) -> Definitely_immediate
      | Ok (Singleton _) -> Unknown
      | Ok (Combination (Union, ty1, ty2)) ->
        let ty1 = ty_of_resolved_ok_ty ty1 in
        let ty2 = ty_of_resolved_ok_ty ty2 in
        K.Value_kind.join (value_kind_ty_value ty1)
          (value_kind_ty_value ty2)
      | Ok (Combination (Intersection, ty1, ty2)) ->
        let ty1 = ty_of_resolved_ok_ty ty1 in
        let ty2 = ty_of_resolved_ok_ty ty2 in
        (* CR mshinwell: Think more about the following two uses of
           [Definitely_immediate] *)
        let meet =
          K.Value_kind.meet (value_kind_ty_value ty1)
            (value_kind_ty_value ty2)
        in
        begin match meet with
        | Ok value_kind -> value_kind
        | Bottom -> Definitely_immediate
        end
      | Bottom -> Definitely_immediate
    in
    value_kind_ty_value ty

  let kind_ty_value ~importer ~type_of_name (ty : ty_value) =
    let value_kind =
      value_kind_ty_value ~importer ~type_of_name ty
    in
    K.value value_kind

  let kind ~importer ~type_of_name (t : t) =
    match t with
    | Naked_immediate _ -> K.naked_immediate ()
    | Naked_float _ -> K.naked_float ()
    | Naked_int32 _ -> K.naked_int32 ()
    | Naked_int64 _ -> K.naked_int64 ()
    | Naked_nativeint _ -> K.naked_nativeint ()
    | Value ty -> kind_ty_value ~importer ~type_of_name ty

  let value_kind = value_kind_ty_value

  let create_inlinable_function_declaration ~is_classic_mode ~closure_origin
        ~continuation_param ~params ~body ~result ~stub ~dbg ~inline
        ~specialise ~is_a_functor ~invariant_params ~size ~direct_call_surrogate
        : inlinable_function_declaration =
    { closure_origin;
      continuation_param;
      is_classic_mode;
      params;
      body;
      free_names_in_body = Expr.free_names body;
      result;
      stub;
      dbg;
      inline;
      specialise;
      is_a_functor;
      invariant_params;
      size;
      direct_call_surrogate;
    }

  let create_non_inlinable_function_declaration ~result ~direct_call_surrogate
        : non_inlinable_function_declaration =
    { result;
      direct_call_surrogate;
    }

  let closure ~set_of_closures closure_id : t =
    (* CR mshinwell: pass a description to the "force" functions *)
    let set_of_closures = force_to_kind_value set_of_closures in
    Value (Normal (Resolved (Ok (
      Singleton (Closure { set_of_closures; closure_id; })))))

  let create_set_of_closures ~set_of_closures_id ~set_of_closures_origin
        ~function_decls ~closure_elements : set_of_closures =
    { set_of_closures_id;
      set_of_closures_origin;
      function_decls;
      closure_elements;
    }

  let set_of_closures ~set_of_closures_id ~set_of_closures_origin
        ~function_decls ~closure_elements =
    let set_of_closures =
      create_set_of_closures ~set_of_closures_id ~set_of_closures_origin
        ~function_decls ~closure_elements
    in
    Value (Normal (Resolved (Ok (
      Singleton (Set_of_closures set_of_closures)))))

  let rec free_names t acc =
    match t with
    | Value ty -> free_names_ty_value ty acc
    | Naked_immediate ty -> free_names_ty_naked_immediate ty acc
    | Naked_float ty -> free_names_ty_naked_float ty acc
    | Naked_int32 ty -> free_names_ty_naked_int32 ty acc
    | Naked_int64 ty -> free_names_ty_naked_int64 ty acc
    | Naked_nativeint ty -> free_names_ty_naked_nativeint ty acc

  and free_names_ty_value (ty : ty_value) acc =
    match ty with
    | Alias name -> Name.Set.add name acc
    | Normal (Resolved ((Unknown _) | Bottom)) -> acc
    | Normal (Resolved (Ok of_kind_value)) ->
      free_names_of_kind_value of_kind_value acc
    | Normal (Load_lazily _load_lazily) ->
      (* Types saved in .cmx files cannot contain free names. *)
      acc

  and free_names_ty_naked_immediate (ty : ty_naked_immediate) acc =
    match ty with
    | Alias name -> Name.Set.add name acc
    | Normal _ -> acc

  and free_names_ty_naked_float (ty : ty_naked_float) acc =
    match ty with
    | Alias name -> Name.Set.add name acc
    | Normal _ -> acc

  and free_names_ty_naked_int32 (ty : ty_naked_int32) acc =
    match ty with
    | Alias name -> Name.Set.add name acc
    | Normal _ -> acc

  and free_names_ty_naked_int64 (ty : ty_naked_int64) acc =
    match ty with
    | Alias name -> Name.Set.add name acc
    | Normal _ -> acc

  and free_names_ty_naked_nativeint (ty : ty_naked_nativeint) acc =
    match ty with
    | Alias name -> Name.Set.add name acc
    | Normal _ -> acc

  and free_names_set_of_closures (set_of_closures : set_of_closures) acc =
    let acc =
      Var_within_closure.Map.fold (fun _var ty_value acc ->
          free_names_ty_value ty_value acc)
        set_of_closures.closure_elements acc
    in
    Closure_id.Map.fold
      (fun _closure_id (decl : function_declaration) acc ->
        match decl with
        | Inlinable decl ->
          let acc =
            List.fold_left (fun acc ty ->
              free_names ty acc)
              acc
              decl.result
          in
          List.fold_left (fun acc (_param, ty) ->
              free_names ty acc)
            acc
            decl.params
        | Non_inlinable decl ->
          List.fold_left (fun acc ty ->
            free_names ty acc)
            acc
            decl.result)
      set_of_closures.function_decls
      acc

  and free_names_of_kind_value
        (o : of_kind_value singleton_or_combination) acc =
    match o with
    | Singleton singleton ->
      begin match singleton with
      | Tagged_immediate i ->
        free_names_ty_naked_immediate i acc
      | Boxed_float f ->
        free_names_ty_naked_float f acc
      | Boxed_int32 n ->
        free_names_ty_naked_int32 n acc
      | Boxed_int64 n ->
        free_names_ty_naked_int64 n acc
      | Boxed_nativeint n ->
        free_names_ty_naked_nativeint n acc
      | Block (_tag, fields) ->
        Array.fold_left (fun acc t -> free_names_ty_value t acc)
          acc fields
      | Set_of_closures set_of_closures ->
        free_names_set_of_closures set_of_closures acc
      | Closure { set_of_closures; closure_id = _; } ->
        free_names_ty_value set_of_closures acc
      | String _ -> acc
      | Float_array fields ->
        Array.fold_left (fun acc field ->
            free_names_ty_naked_float field acc)
          acc fields
      end
    | Combination (_op, ty1, ty2) ->
      let ty1 = ty_of_resolved_ok_ty ty1 in
      let ty2 = ty_of_resolved_ok_ty ty2 in
      free_names_ty_value ty2 (free_names_ty_value ty1 acc)

  let free_names t = free_names t Name.Set.empty

  (* CR mshinwell: We need tests to check that [clean] matches up with
     [free_variables]. *)
(*
  type cleaning_spec =
    | Available
    | Available_different_name of Variable.t
    | Unavailable

  let rec clean ~importer t classify =
    let clean_var var =
      match classify var with
      | Available -> Some var
      | Available_different_name new_var -> Some new_var
      | Unavailable -> None
    in
    let clean_var_opt var_opt =
      match var_opt with
      | None -> None
      | Some var ->
        match clean_var var with
        | None -> None
        | (Some var') as var_opt' ->
          if var == var' then var_opt
          else var_opt'
    in
    clean_t ~importer t clean_var_opt

  and clean_t ~importer (t : t) clean_var_opt : t =
    match t with
    | Value ty ->
      Value (clean_ty_value ~importer ty clean_var_opt)
    | Naked_immediate ty ->
      Naked_immediate (clean_ty_naked_immediate ~importer ty clean_var_opt)
    | Naked_float ty ->
      Naked_float (clean_ty_naked_float ~importer ty clean_var_opt)
    | Naked_int32 ty ->
      Naked_int32 (clean_ty_naked_int32 ~importer ty clean_var_opt)
    | Naked_int64 ty ->
      Naked_int64 (clean_ty_naked_int64 ~importer ty clean_var_opt)
    | Naked_nativeint ty ->
      Naked_nativeint (clean_ty_naked_nativeint ~importer ty clean_var_opt)

  and clean_ty_value ~importer ty_value clean_var_opt : ty_value =
    let module I = (val importer : Importer) in
    let ty_value = I.import_value_type_as_resolved_ty_value ty_value in
    let var = clean_var_opt ty_value.var in
    let descr : (of_kind_value, _) or_unknown_or_bottom =
      match ty_value.descr with
      | (Unknown _) | Bottom -> ty_value.descr
      | Ok of_kind_value ->
        Ok (clean_of_kind_value ~importer of_kind_value clean_var_opt)
    in
    { var;
      symbol = ty_value.symbol;
      descr = Ok descr;
    }

  and clean_resolved_ty_set_of_closures ~importer
        (resolved_ty_set_of_closures : resolved_ty_set_of_closures)
        clean_var_opt
        : resolved_ty_set_of_closures =
    let var = clean_var_opt resolved_ty_set_of_closures.var in
    let descr : (set_of_closures, _) or_unknown_or_bottom =
      match resolved_ty_set_of_closures.descr with
      | (Unknown _) | Bottom -> resolved_ty_set_of_closures.descr
      | Ok set_of_closures ->
        Ok (clean_set_of_closures ~importer set_of_closures clean_var_opt)
    in
    { var;
      symbol = resolved_ty_set_of_closures.symbol;
      descr = descr;
    }

  and clean_ty_naked_immediate ~importer ty_naked_immediate clean_var_opt
        : ty_naked_immediate =
    let module I = (val importer : Importer) in
    let ty_naked_immediate =
      I.import_naked_immediate_type_as_resolved_ty_naked_immediate
        ty_naked_immediate
    in
    let var = clean_var_opt ty_naked_immediate.var in
    { var;
      symbol = ty_naked_immediate.symbol;
      descr = Ok ty_naked_immediate.descr;
    }

  and clean_ty_naked_float ~importer ty_naked_float clean_var_opt
        : ty_naked_float =
    let module I = (val importer : Importer) in
    let ty_naked_float =
      I.import_naked_float_type_as_resolved_ty_naked_float ty_naked_float
    in
    let var = clean_var_opt ty_naked_float.var in
    { var;
      symbol = ty_naked_float.symbol;
      descr = Ok ty_naked_float.descr;
    }

  and clean_ty_naked_int32 ~importer ty_naked_int32 clean_var_opt
        : ty_naked_int32 =
    let module I = (val importer : Importer) in
    let ty_naked_int32 =
      I.import_naked_int32_type_as_resolved_ty_naked_int32 ty_naked_int32
    in
    let var = clean_var_opt ty_naked_int32.var in
    { var;
      symbol = ty_naked_int32.symbol;
      descr = Ok ty_naked_int32.descr;
    }

  and clean_ty_naked_int64 ~importer ty_naked_int64 clean_var_opt
        : ty_naked_int64 =
    let module I = (val importer : Importer) in
    let ty_naked_int64 =
      I.import_naked_int64_type_as_resolved_ty_naked_int64 ty_naked_int64
    in
    let var = clean_var_opt ty_naked_int64.var in
    { var;
      symbol = ty_naked_int64.symbol;
      descr = Ok ty_naked_int64.descr;
    }

  and clean_ty_naked_nativeint ~importer ty_naked_nativeint clean_var_opt
        : ty_naked_nativeint =
    let module I = (val importer : Importer) in
    let ty_naked_nativeint =
      I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint
        ty_naked_nativeint
    in
    let var = clean_var_opt ty_naked_nativeint.var in
    { var;
      symbol = ty_naked_nativeint.symbol;
      descr = Ok ty_naked_nativeint.descr;
    }

  and clean_set_of_closures ~importer set_of_closures clean_var_opt =
    let closure_elements =
      Var_within_closure.Map.map (fun t ->
          clean_ty_value ~importer t clean_var_opt)
        set_of_closures.closure_elements
    in
    let function_decls =
      Closure_id.Map.map
        (fun (decl : function_declaration) : function_declaration ->
          match decl with
          | Inlinable decl ->
            let params =
              List.map (fun (param, t) ->
                  param, clean_t ~importer t clean_var_opt)
                decl.params
            in
            let result =
              List.map (fun ty ->
                clean_t ~importer ty clean_var_opt)
                decl.result
            in
            Inlinable { decl with params; result; }
          | Non_inlinable decl ->
            let result =
              List.map (fun ty ->
                clean_t ~importer ty clean_var_opt)
                decl.result
            in
            Non_inlinable { decl with result; })
        set_of_closures.function_decls
    in
    { set_of_closures with
      function_decls;
      closure_elements;
    }

  and clean_of_kind_value ~importer (o : of_kind_value) clean_var_opt
        : of_kind_value =
    match o with
    | Singleton singleton ->
      let singleton : of_kind_value_singleton =
        match singleton with
        | Tagged_immediate i ->
          Tagged_immediate (clean_ty_naked_immediate ~importer i clean_var_opt)
        | Boxed_float f ->
          Boxed_float (clean_ty_naked_float ~importer f clean_var_opt)
        | Boxed_int32 n ->
          Boxed_int32 (clean_ty_naked_int32 ~importer n clean_var_opt)
        | Boxed_int64 n ->
          Boxed_int64 (clean_ty_naked_int64 ~importer n clean_var_opt)
        | Boxed_nativeint n ->
          Boxed_nativeint (clean_ty_naked_nativeint ~importer n clean_var_opt)
        | Block (tag, fields) ->
          let fields =
            Array.map (fun t -> clean_ty_value ~importer t clean_var_opt)
              fields
          in
          Block (tag, fields)
        | Set_of_closures set_of_closures ->
          Set_of_closures
            (clean_set_of_closures ~importer set_of_closures clean_var_opt)
        | Closure { set_of_closures; closure_id; } ->
          let set_of_closures =
            clean_resolved_ty_set_of_closures ~importer set_of_closures
              clean_var_opt
          in
          Closure { set_of_closures; closure_id; }
        | String _ -> singleton
        | Float_array fields ->
          let fields =
            Array.map (fun field ->
                clean_ty_naked_float ~importer field clean_var_opt)
              fields
          in
          Float_array fields
      in
      Singleton singleton
    | Union (w1, w2) ->
      let w1 =
        { var = clean_var_opt w1.var;
          symbol = w1.symbol;
          descr = clean_of_kind_value ~importer w1.descr clean_var_opt;
        }
      in
      let w2 =
        { var = clean_var_opt w2.var;
          symbol = w2.symbol;
          descr = clean_of_kind_value ~importer w2.descr clean_var_opt;
        }
      in
      Union (w1, w2)
*)

  module Join_or_meet (P : sig
    val description : string
    val combining_op : combining_op
  end) = struct
    let combine_unknown_payload_for_value ~importer ~type_of_name
          _ty_value1 value_kind1 ty_value2 value_kind2_opt =
      let value_kind2 : K.Value_kind.t =
        match value_kind2_opt with
        | Some value_kind2 -> value_kind2
        | None ->
          value_kind_ty_value ~importer ~type_of_name
            (Normal ((Resolved ty_value2) : _ maybe_unresolved))
      in
      match P.combining_op with
      | Union -> K.Value_kind.join value_kind1 value_kind2
      | Intersection ->
        (* CR mshinwell: Same comment as above re. Definitely_immediate *)
        begin match K.Value_kind.meet value_kind1 value_kind2 with
        | Ok value_kind -> value_kind
        | Bottom -> K.Value_kind.Definitely_immediate
        end

    let combine_unknown_payload_for_non_value _ty1 () _ty2 (_ : unit option) =
      ()

    type 'a or_combine =
      | Exactly of 'a
      | Combine

    let combine_singleton_or_combination ty1 ty2
          ~combine_of_kind : _ or_unknown_or_bottom =
      let combine () : _ or_unknown_or_bottom =
        Ok (Combination (P.combining_op, Normal ty1, Normal ty2))
      in
      match ty1, ty2 with
      | Singleton s1, Singleton s2 ->
        begin match combine_of_kind s1 s2 with
        | Exactly result -> result
        | Combine -> combine ()
        end
      | Singleton _, Combination _
      | Combination _, Singleton _
      | Combination _, Combination _ -> combine ()

    let combine_ty (type a) (type u) ~importer:_ ~importer_this_kind
          ~(force_to_kind : t -> (a, u) ty)
          ~(type_of_name : Name.t -> t option)
          unknown_payload_top
          combine_contents combine_unknown_payload
          (ty1 : (a, u) ty) (ty2 : (a, u) ty) : (a, u) ty =
      (* CR mshinwell: Should something be happening here with the canonical
         names? *)
      let ty1, _canonical_name1 =
        resolve_aliases_on_ty ~importer_this_kind ~force_to_kind
          ~type_of_name ty1
      in
      let ty2, _canonical_name2 =
        resolve_aliases_on_ty ~importer_this_kind ~force_to_kind
          ~type_of_name ty2
      in
      match ty1, ty2 with
      | Alias name1, Alias name2 when Name.equal name1 name2 -> Alias name1
      | _, _ ->
        let unresolved_var_or_symbol_to_unknown (ty : _ resolved_ty)
              : _ or_unknown_or_bottom =
          match ty with
          | Normal ty -> ty
          | Alias _ -> Unknown (Other, unknown_payload_top)
        in
        let ty1 = unresolved_var_or_symbol_to_unknown ty1 in
        let ty2 = unresolved_var_or_symbol_to_unknown ty2 in
        let ty =
          (* Care: we need to handle the payloads of [Unknown]. *)
          match ty1, ty2 with
          | Unknown (reason1, payload1), Unknown (reason2, payload2) ->
            Unknown (combine_unknown_because_of reason1 reason2,
              combine_unknown_payload ty1 payload1
                ty2 (Some payload2))
          | Ok ty1, Ok ty2 ->
            combine_singleton_or_combination
              ~combine_of_kind:combine_contents
              ty1 ty2
          | Unknown (reason, payload), _ ->
            begin match P.combining_op with
            | Union ->
              Unknown (reason, combine_unknown_payload ty1 payload ty2 None)
            | Intersection -> ty2
            end
          | _, Unknown (reason, payload) ->
            begin match P.combining_op with
            | Union ->
              Unknown (reason, combine_unknown_payload ty2 payload ty1 None)
            | Intersection -> ty1
            end
          | Bottom, _ ->
            begin match P.combining_op with
            | Union -> ty2
            | Intersection -> Bottom
            end
          | _, Bottom ->
            begin match P.combining_op with
            | Union -> ty1
            | Intersection -> Bottom
            end
        in
        Normal ((Resolved ty) : _ maybe_unresolved)

    let rec combine_of_kind_value ~importer ~type_of_name
          (t1 : of_kind_value) t2
          : (of_kind_value, K.Value_kind.t) or_unknown_or_bottom or_combine =
      let singleton s : _ or_combine =
        Exactly ((Ok (Singleton s)) : _ or_unknown_or_bottom)
      in
      match t1, t2 with
      | Tagged_immediate ty1, Tagged_immediate ty2 ->
        singleton (Tagged_immediate (
          combine_ty_naked_immediate ~importer ~type_of_name
            ty1 ty2))
      | Boxed_float ty1, Boxed_float ty2 ->
        singleton (Boxed_float (
          combine_ty_naked_float ~importer ~type_of_name
            ty1 ty2))
      | Boxed_int32 ty1, Boxed_int32 ty2 ->
        singleton (Boxed_int32 (
          combine_ty_naked_int32 ~importer ~type_of_name
            ty1 ty2))
      | Boxed_int64 ty1, Boxed_int64 ty2 ->
        singleton (Boxed_int64 (
          combine_ty_naked_int64 ~importer ~type_of_name
            ty1 ty2))
      | Boxed_nativeint ty1, Boxed_nativeint ty2 ->
        singleton (Boxed_nativeint (
          combine_ty_naked_nativeint ~importer ~type_of_name
            ty1 ty2))
      | Block (tag1, fields1), Block (tag2, fields2)
          when Tag.Scannable.equal tag1 tag2
            && Array.length fields1 = Array.length fields2 ->
        let fields =
          Array.map2 (fun ty1 ty2 ->
              combine_ty_value ~importer ~type_of_name
                ty1 ty2)
            fields1 fields2
        in
        singleton (Block (tag1, fields))
      | String { contents = Contents str1; _ },
          String { contents = Contents str2; _ }
          when String.equal str1 str2 ->
        singleton t1
      | Float_array fields1, Float_array fields2
          when Array.length fields1 = Array.length fields2 ->
        let fields =
          Array.map2 (fun ty1 ty2 ->
              combine_ty_naked_float ~importer ~type_of_name
                ty1 ty2)
            fields1 fields2
        in
        singleton (Float_array fields)
      | _, _ -> Combine

    and combine_of_kind_naked_immediate
          (t1 : of_kind_naked_immediate)
          (t2 : of_kind_naked_immediate)
          : (of_kind_naked_immediate, _) or_unknown_or_bottom or_combine =
      match t1, t2 with
      | Naked_immediate i1, Naked_immediate i2 ->
        if not (Immediate.equal i1 i2) then
          Combine
        else
          Exactly (Ok (
            Singleton ((Naked_immediate i1) : of_kind_naked_immediate)))

    and combine_of_kind_naked_float
          (t1 : of_kind_naked_float) (t2 : of_kind_naked_float)
          : (of_kind_naked_float, _) or_unknown_or_bottom or_combine =
      match t1, t2 with
      | Naked_float i1, Naked_float i2 ->
        if not (Numbers.Float_by_bit_pattern.equal i1 i2) then
          Combine
        else
          Exactly (Ok (Singleton ((Naked_float i1) : of_kind_naked_float)))

    and combine_of_kind_naked_int32
          (t1 : of_kind_naked_int32) (t2 : of_kind_naked_int32)
          : (of_kind_naked_int32, _) or_unknown_or_bottom or_combine =
      match t1, t2 with
      | Naked_int32 i1, Naked_int32 i2 ->
        if not (Int32.equal i1 i2) then
          Combine
        else
          Exactly (Ok (Singleton ((Naked_int32 i1) : of_kind_naked_int32)))

    and combine_of_kind_naked_int64
          (t1 : of_kind_naked_int64) (t2 : of_kind_naked_int64)
          : (of_kind_naked_int64, _) or_unknown_or_bottom or_combine =
      match t1, t2 with
      | Naked_int64 i1, Naked_int64 i2 ->
        if not (Int64.equal i1 i2) then
          Combine
        else
          Exactly (Ok (Singleton ((Naked_int64 i1) : of_kind_naked_int64)))

    and combine_of_kind_naked_nativeint
          (t1 : of_kind_naked_nativeint) (t2 : of_kind_naked_nativeint)
          : (of_kind_naked_nativeint, _) or_unknown_or_bottom or_combine =
      match t1, t2 with
      | Naked_nativeint i1, Naked_nativeint i2 ->
        if not (Targetint.equal i1 i2) then
          Combine
        else
          Exactly (Ok (
            Singleton ((Naked_nativeint i1) : of_kind_naked_nativeint)))

    and combine_ty_value ~importer ~type_of_name
          (ty1 : ty_value) (ty2 : ty_value) : ty_value =
      let module I = (val importer : Importer) in
      combine_ty ~importer ~type_of_name
        ~importer_this_kind:I.import_value_type_as_resolved_ty_value
        ~force_to_kind:force_to_kind_value
        K.Value_kind.Unknown
        (combine_of_kind_value ~importer ~type_of_name)
        (combine_unknown_payload_for_value ~importer ~type_of_name)
        ty1 ty2

    and combine_ty_naked_immediate ~importer ~type_of_name ty1 ty2 =
      let module I = (val importer : Importer) in
      combine_ty ~importer ~type_of_name 
        ~importer_this_kind:
          I.import_naked_immediate_type_as_resolved_ty_naked_immediate
        ~force_to_kind:force_to_kind_naked_immediate
        ()
        combine_of_kind_naked_immediate
        combine_unknown_payload_for_non_value
        ty1 ty2

    and combine_ty_naked_float ~importer ~type_of_name ty1 ty2 =
      let module I = (val importer : Importer) in
      combine_ty ~importer ~type_of_name 
        ~importer_this_kind:I.import_naked_float_type_as_resolved_ty_naked_float
        ~force_to_kind:force_to_kind_naked_float
        ()
        combine_of_kind_naked_float
        combine_unknown_payload_for_non_value
        ty1 ty2

    and combine_ty_naked_int32 ~importer ~type_of_name ty1 ty2 =
      let module I = (val importer : Importer) in
      combine_ty ~importer ~type_of_name 
        ~importer_this_kind:I.import_naked_int32_type_as_resolved_ty_naked_int32
        ~force_to_kind:force_to_kind_naked_int32
        ()
        combine_of_kind_naked_int32
        combine_unknown_payload_for_non_value
        ty1 ty2

    and combine_ty_naked_int64 ~importer ~type_of_name ty1 ty2 =
      let module I = (val importer : Importer) in
      combine_ty ~importer ~type_of_name 
        ~importer_this_kind:I.import_naked_int64_type_as_resolved_ty_naked_int64
        ~force_to_kind:force_to_kind_naked_int64
        ()
        combine_of_kind_naked_int64
        combine_unknown_payload_for_non_value
        ty1 ty2

    and combine_ty_naked_nativeint ~importer ~type_of_name ty1 ty2 =
      let module I = (val importer : Importer) in
      combine_ty ~importer ~type_of_name 
        ~importer_this_kind:
          I.import_naked_nativeint_type_as_resolved_ty_naked_nativeint
        ~force_to_kind:force_to_kind_naked_nativeint
        ()
        combine_of_kind_naked_nativeint
        combine_unknown_payload_for_non_value
        ty1 ty2

    let combine ~importer ~type_of_name (t1 : t) (t2 : t) : t =
      if t1 == t2 then t1
      else
        match t1, t2 with
        | Value ty1, Value ty2 ->
          Value (combine_ty_value ~importer
            ~type_of_name ty1 ty2)
        | Naked_immediate ty1, Naked_immediate ty2 ->
          Naked_immediate (combine_ty_naked_immediate ~importer
            ~type_of_name ty1 ty2)
        | Naked_float ty1, Naked_float ty2 ->
          Naked_float (combine_ty_naked_float ~importer
            ~type_of_name ty1 ty2)
        | Naked_int32 ty1, Naked_int32 ty2 ->
          Naked_int32 (combine_ty_naked_int32 ~importer
            ~type_of_name ty1 ty2)
        | Naked_int64 ty1, Naked_int64 ty2 ->
          Naked_int64 (combine_ty_naked_int64 ~importer
            ~type_of_name ty1 ty2)
        | Naked_nativeint ty1, Naked_nativeint ty2 ->
          Naked_nativeint (combine_ty_naked_nativeint ~importer
            ~type_of_name ty1 ty2)
        | _, _ ->
          Misc.fatal_errorf "Cannot take the %s of two types with different \
              kinds: %a and %a"
            P.description
            print t1
            print t2
  end

  module Join = Join_or_meet (struct
    let description = "join"
    let combining_op = Union
  end)

  module Meet = Join_or_meet (struct
    let description = "meet"
    let combining_op = Intersection
  end)

  let join = Join.combine
  let join_ty_value = Join.combine_ty_value
  let join_ty_naked_float = Join.combine_ty_naked_float
  let join_ty_naked_int32 = Join.combine_ty_naked_int32
  let join_ty_naked_int64 = Join.combine_ty_naked_int64
  let join_ty_naked_nativeint = Join.combine_ty_naked_nativeint

  let join_list ~importer ~type_of_name kind ts =
    match ts with
    | [] -> bottom kind
    | t::ts ->
      List.fold_left (fun result t -> join ~importer ~type_of_name result t)
        t
        ts

  let meet = Meet.combine
  let meet_ty_value = Meet.combine_ty_value
  let meet_ty_naked_float = Meet.combine_ty_naked_float
  let meet_ty_naked_int32 = Meet.combine_ty_naked_int32
  let meet_ty_naked_int64 = Meet.combine_ty_naked_int64
  let meet_ty_naked_nativeint = Meet.combine_ty_naked_nativeint

  let meet_list ~importer ~type_of_name kind ts =
    match ts with
    | [] -> bottom kind
    | t::ts ->
      List.fold_left (fun result t -> meet ~importer ~type_of_name result t)
        t
        ts

  type 'a or_bottom =
    | Ok of 'a
    | Bottom

  let generic_meet_list ~meet ~importer ~type_of_name ts t =
    Misc.Stdlib.List.filter_map (fun t' ->
        match meet ~importer ~type_of_name t t' with
        | Ok meet -> Some meet
        | Bottom -> None)
      ts

  let generic_meet_lists ~meet ~importer ~type_of_name ts1 ts2 =
    List.fold_left (fun result t1 ->
        generic_meet_list ~importer ~type_of_name ~meet result t1)
      ts2
      ts1

  module Closure = struct
    type t = closure

    let meet ~importer ~type_of_name (t1 : t) (t2 : t) : t or_bottom =
      if not (Closure_id.equal t1.closure_id t2.closure_id) then
        Bottom
      else
        let set_of_closures =
          meet_ty_value ~importer ~type_of_name
            t1.set_of_closures t2.set_of_closures
        in
        Ok {
          set_of_closures;
          closure_id = t1.closure_id;
        }

    let meet_lists = generic_meet_lists ~meet

    let print = print_closure
  end

  module Set_of_closures = struct
    type t = set_of_closures

    let meet ~importer ~type_of_name (t1 : t) (t2 : t) : t or_bottom =
      let same_set =
        Set_of_closures_id.equal t1.set_of_closures_id t2.set_of_closures_id
          && Set_of_closures_origin.equal t1.set_of_closures_origin
            t2.set_of_closures_origin
      in
      if not same_set then Bottom
      else
        let closure_elements =
          Var_within_closure.Map.inter_merge (fun elt1 elt2 ->
              join_ty_value ~importer ~type_of_name elt1 elt2)
            t1.closure_elements
            t2.closure_elements
        in
        Ok {
          set_of_closures_id = t1.set_of_closures_id;
          set_of_closures_origin = t1.set_of_closures_origin;
          function_decls = t1.function_decls;
          closure_elements;
        }

    let meet_lists = generic_meet_lists ~meet

    let print = print_set_of_closures
  end

  (* CR mshinwell: Try to move these next ones to flambda_type.ml *)

  let these_naked_floats fs =
    let tys =
      List.map (fun f -> this_naked_float f)
        (Numbers.Float_by_bit_pattern.Set.elements fs)
    in
    (with_null_importer join_list) (K.naked_float ()) tys

  let these_naked_int32s ns =
    let tys =
      List.map (fun n -> this_naked_int32 n)
        (Int32.Set.elements ns)
    in
    (with_null_importer join_list) (K.naked_int32 ()) tys

  let these_naked_int64s ns =
    let tys =
      List.map (fun n -> this_naked_int64 n)
        (Int64.Set.elements ns)
    in
    (with_null_importer join_list) (K.naked_int64 ()) tys

  let these_naked_nativeints ns =
    let tys =
      List.map (fun n -> this_naked_nativeint n)
        (Targetint.Set.elements ns)
    in
    (with_null_importer join_list) (K.naked_nativeint ()) tys

  let these_tagged_immediates imms =
    let tys =
      List.map (fun imm -> this_tagged_immediate imm)
        (Immediate.Set.elements imms)
    in
    (with_null_importer join_list) (K.value Definitely_immediate) tys

  let these_boxed_floats fs =
    let tys =
      List.map (fun f -> this_boxed_float f)
        (Numbers.Float_by_bit_pattern.Set.elements fs)
    in
    (with_null_importer join_list) (K.value Definitely_pointer) tys

  let these_boxed_int32s ns =
    let tys =
      List.map (fun f -> this_boxed_int32 f)
        (Int32.Set.elements ns)
    in
    (with_null_importer join_list) (K.value Definitely_pointer) tys

  let these_boxed_int64s ns =
    let tys =
      List.map (fun f -> this_boxed_int64 f)
        (Int64.Set.elements ns)
    in
    (with_null_importer join_list) (K.value Definitely_pointer) tys

  let these_boxed_nativeints ns =
    let tys =
      List.map (fun f -> this_boxed_nativeint f)
        (Targetint.Set.elements ns)
    in
    (with_null_importer join_list) (K.value Definitely_pointer) tys

  let mutable_float_arrays_of_various_sizes ~sizes : t =
    let tys =
      List.map (fun size -> mutable_float_array ~size)
        (Targetint.OCaml.Set.elements sizes)
    in
    (with_null_importer join_list) (K.value Definitely_pointer) tys

  let combination_component_to_ty (type a)
        (ty : a singleton_or_combination or_alias)
        : (a, _) ty =
    match ty with
    | Alias alias -> Alias alias
    | Normal s_or_c -> Normal (Resolved (Ok s_or_c))

  module Typing_context = struct
    type t = typing_context

    let print ppf { names_to_types; levels_to_names;
          existentials; existential_freshening; } =
      Format.fprintf ppf
        "@[((names_to_types %a)@ \
            (levels_to_names %a)@ \
            (existentials %a)@ \
            (existential_freshening %a))@]"
        Name.Map.print print names_to_types
        Scope_level.Map.print Name.Set.print levels_to_names
        Name.Set.print existentials
        Freshening.print existential_freshening

    let create () =
      let existential_freshening = Freshening.activate Freshening.empty in
      { names_to_types = Name.Map.empty;
        levels_to_names : Scope_level.Map.empty;
        existentials : Name.Set.empty;
        existential_freshening;
      }

    let add t name scope_level ty =
      match Name.Map.find name t.names_to_types with
      | exception Not_found ->
        let names = Name.Map.add name ty t.names_to_types in
        let levels_to_names =
          Scope_level.Map.update scope_level
            (function
               | None -> Name.Set.singleton name
               | Some names -> Name.Set.add name names)
        in
        { t with
          names;
          levels_to_names;
        }
      | _ty ->
        Misc.fatal_errorf "Cannot rebind %a in environment: %a"
          Name.print name
          print t

    type binding_type = Normal | Existential

    let find t name =
      match Name.Map.find name t.names_to_types with
      | exception Not_found ->
        Misc.fatal_errorf "Cannot find %a in environment: %a"
          Name.print name
          print t
      | ty ->
        let binding_type =
          if Name.Map.mem name t.existentials then Existential
          else Normal
        in
        match binding_type with
        | Normal -> ty, Normal
        | Existential ->
          let ty = rename_variables t freshening in
          ty, Existential

    let cut t ~minimum_scope_level_to_be_existential =
      let existentials =
        Scope_level.Map.fold (fun scope_level names resulting_existentials ->
            let will_be_existential =
              Scope_level.(>=) scope_level minimum_scope_level_to_be_existential
            in
            if will_be_existential then
              Name.Set.union names resulting_existentials
            else
              resulting_existentials)
          t.levels_to_names
          Name.Set.empty
      in
      let existential_freshening =
        Name.Set.fold (fun (name : Name.t) freshening ->
            match name with
            | Symbol _ -> freshening
            | Var var ->
              let new_var = Variable.rename var in
              Freshening.add_variable freshening var new_var)
          t.existential_freshening
      in
      (* XXX we actually need to rename in the domain of [names_to_types] *)
      { names_to_types = t.names_to_types;
        levels_to_names = t.levels_to_names;
        existentials;
        existential_freshening;
      }

    let join ~importer ~type_of_name t1 t2 =
      let names_to_types =
        Name.Map.inter (fun ty1 ty2 ->
            join ~importer ~type_of_name t1 t2)
          t1.names_to_types
          t2.names_to_types
      in
      let all_levels_to_names =
        Scope_level.Map.union
          (fun names1 names2 -> Name.Set.union names1 names2)
          t1.levels_to_names
          t2.levels_to_names
      in
      let levels_to_names =
        Scope_level.Map.filter (fun _scope_level name ->
            Name.Map.mem name names_to_types)
          all_levels_to_names
      in
      let existentials =
        (* XXX care if a name is non-existential in one and existential
           in the other *)
        Name.Set.inter t1.existentials t2.existentials
      in
      let existential_freshening =
        ...
      in
      { names_to_types;
        types_to_levels;
        existentials;
        existential_freshening;
      }

    let meet ~importer ~type_of_name t1 t2 =
      let names_to_types =
        Name.Map.union (fun ty1 ty2 ->
            meet ~importer ~type_of_name t1 t2)
          t1.names_to_types
          t2.names_to_types
      in
      let all_levels_to_names =
        Scope_level.Map.union
          (fun names1 names2 -> Name.Set.union names1 names2)
          t1.levels_to_names
          t2.levels_to_names
      in
      let levels_to_names =
        Scope_level.Map.filter (fun _scope_level name ->
            Name.Map.mem name names_to_types)
          all_levels_to_names
      in
      let existentials =
        Name.Set.union t1.existentials t2.existentials
      in
      let existential_freshening =
        ...
      in
      { names_to_types;
        types_to_levels;
        existentials;
        existential_freshening;
      }
  end
end
