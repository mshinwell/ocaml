(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  params_and_handler : Continuation_params_and_handler.t;
  stub : bool;
  is_exn_handler : bool;
}

let invariant _env _t = ()

let print_parameter_with_type ppf param =
  (* CR mshinwell: Share with Flambda_type somehow *)
  let kind_with_subkind = Kinded_parameter.kind_with_subkind param in
  let kind = Flambda_kind.With_subkind.kind kind_with_subkind in
  let print_type ppf () =
    let colour = Flambda_colours.top_or_bottom_type () in
    match kind with
    | Value ->
      begin match Flambda_kind.With_subkind.subkind kind_with_subkind with
      | Anything ->
        Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s"
          colour
          (Flambda_colours.normal ())
      | Boxed_float ->
        let colour = Flambda_colours.top_or_bottom_type () in
        Format.fprintf ppf
          "@<0>%s(Boxed_float (Naked_float @<0>%s@<1>\u{22a4}@<0>%s))@<0>%s"
          (Flambda_colours.type_in_term ())
          colour
          (Flambda_colours.type_in_term ())
          (Flambda_colours.normal ())
      | Boxed_int32 ->
        Format.fprintf ppf
          "@<0>%s(Boxed_int32 (Naked_int32 @<0>%s@<1>\u{22a4}@<0>%s))@<0>%s"
          (Flambda_colours.type_in_term ())
          colour
          (Flambda_colours.type_in_term ())
          (Flambda_colours.normal ())
      | Boxed_int64 ->
        Format.fprintf ppf
          "@<0>%s(Boxed_int64 (Naked_int64 @<0>%s@<1>\u{22a4}@<0>%s))@<0>%s"
          (Flambda_colours.type_in_term ())
          colour
          (Flambda_colours.type_in_term ())
          (Flambda_colours.normal ())
      | Boxed_nativeint ->
        Format.fprintf ppf
          "@<0>%s\
           (Boxed_nativeint (Naked_nativeint @<0>%s@<1>\u{22a4}@<0>%s))@<0>%s"
          (Flambda_colours.type_in_term ())
          colour
          (Flambda_colours.type_in_term ())
          (Flambda_colours.normal ())
      | Immediate ->
        Format.fprintf ppf
          "@<0>%s(Variant@ ((blocks@ @<0>%s@<1>\u{22a5}@<0>%s)@ \
            (immediates@ @<0>%s@<1>\u{22a4}@<0>%s)))@<0>%s"
          (Flambda_colours.type_in_term ())
          colour
          (Flambda_colours.normal ())
          colour
          (Flambda_colours.type_in_term ())
          (Flambda_colours.normal ())
      end
    | Naked_number Naked_immediate ->
      Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s"
        colour
        (Flambda_colours.normal ())
    | Naked_number Naked_float ->
      Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s"
        colour
        (Flambda_colours.normal ())
    | Naked_number Naked_int32 ->
      Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s"
        colour
        (Flambda_colours.normal ())
    | Naked_number Naked_int64 ->
      Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s"
        colour
        (Flambda_colours.normal ())
    | Naked_number Naked_nativeint ->
      Format.fprintf ppf "@<0>%s@<1>\u{22a4}@<0>%s"
        colour
        (Flambda_colours.normal ())
    | Fabricated -> Misc.fatal_error "No longer used"
  in
  Format.fprintf ppf "@[(@<0>%s%a@<0>%s : %a @<1>\u{2237} %a)@]"
    (Flambda_colours.parameter ())
    Parameter.print (Kinded_parameter.param param)
    (Flambda_colours.normal ())
    print_type ()
    Flambda_kind.print kind

let print_using_where_with_cache (recursive : Recursive.t) ~cache ppf k
      ({ params_and_handler = _; stub; is_exn_handler; } as t) ~first =
  if not first then begin
    fprintf ppf "@ "
  end;
  Continuation_params_and_handler.pattern_match t.params_and_handler
    ~f:(fun params ~handler ->
      fprintf ppf "@[<hov 0>@<0>%s%a@<0>%s@<0>%s%s@<0>%s%s@<0>%s%s@<0>%s"
        (Flambda_colours.continuation_definition ())
        Continuation.print k
        (Flambda_colours.normal ())
        (Flambda_colours.expr_keyword ())
        (match recursive with Non_recursive -> "" | Recursive -> " (rec)")
        (Flambda_colours.normal ())
        (if stub then " *stub*" else "")
        (Flambda_colours.continuation_annotation ())
        (if is_exn_handler then "[eh]" else "")
        (Flambda_colours.normal ());
      if List.length params > 0 then begin
        match recursive with
        | Non_recursive ->
          fprintf ppf " @[<hov 0>%a@]" KP.List.print params
        | Recursive ->
          fprintf ppf " @[<hov 0>%a@]"
            (Format.pp_print_list ~pp_sep:Format.pp_print_space
              print_parameter_with_type)
            params
      end;
      fprintf ppf "@<0>%s:@<0>%s@ %a"
        (Flambda_colours.elide ())
        (Flambda_colours.normal ())
        (Expr.print_with_cache ~cache) handler;
      fprintf ppf "@]")

let print_with_cache ~cache ppf { params_and_handler; stub; is_exn_handler; } =
  Format.fprintf ppf "@[<hov 1>\
      @[<hov 1>(params_and_handler@ %a)@]@ \
      @[<hov 1>(stub@ %b)@]@ \
      @[<hov 1>(is_exn_handler@ %b)@]\
      @]"
    (Continuation_params_and_handler.print_with_cache ~cache) params_and_handler
    stub
    is_exn_handler

let print ppf t =
  print_with_cache ~cache:(Printing_cache.create ()) ppf t

(*
let print_with_cache ~cache ppf (t : t) =
  match t with
  | Non_recursive { name; handler = {
      params; stub; handler; is_exn_handler; }; } ->
    fprintf ppf "%a@ %s%s%a=@ %a"
      Continuation.print name
      (if stub then "*stub* " else "")
      (if is_exn_handler then "*exn* " else "")
      (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
      (Expr.print_with_cache ~cache) handler
  | Recursive handlers ->
    let first = ref true in
    Continuation.Map.iter (fun name
            { Continuation_handler.params; stub; is_exn_handler; handler; } ->
        if !first then begin
          fprintf ppf "@;rec "
        end else begin
          fprintf ppf "@;and "
        end;
        fprintf ppf "%a@ %s%s%a=@ %a"
          Continuation.print name
          (if stub then "*stub* " else "")
          (if is_exn_handler then "*exn* " else "")
          (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
          (Expr.print_with_cache ~cache) handler;
        first := false)
      handlers

let print_with_cache ~cache ppf { params_and_handler; stub; handler; } =
  Continuation_params_and_handler.pattern_match params_and_handler
    ~f:(fun params ~handler ->
      fprintf ppf "%s%s%a@ =@ %a"
        (if stub then "*stub* " else "")
        (if is_exn_handler then "*exn* " else "")
        (Flambda_type.Parameters.print_or_omit_with_cache ~cache) params
        Expr.print handler)

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t
*)

let create ~params_and_handler ~stub ~is_exn_handler =
  { params_and_handler;
    stub;
    is_exn_handler;
  }

let params_and_handler t = t.params_and_handler
let stub t = t.stub
let is_exn_handler t = t.is_exn_handler

let free_names
      { params_and_handler; stub = _; is_exn_handler = _; } =
  Continuation_params_and_handler.free_names params_and_handler

let apply_name_permutation
      ({ params_and_handler; stub;
         is_exn_handler; } as t) perm =
  let params_and_handler' =
    Continuation_params_and_handler.apply_name_permutation
      params_and_handler perm
  in
  if params_and_handler == params_and_handler' then t
  else
    { params_and_handler = params_and_handler';
      stub;
      is_exn_handler;
    }

type behaviour =
  | Unreachable of { arity : Flambda_arity.t; }
  | Alias_for of { arity : Flambda_arity.t; alias_for : Continuation.t; }
  | Apply_cont_with_constant_arg of {
      cont : Continuation.t;
      arg : Simple.Const.t;
      arity : Flambda_arity.t;
    }
  | Unknown of { arity : Flambda_arity.t; }

let behaviour t : behaviour =
  (* CR mshinwell: Maybe [behaviour] should be cached, to avoid re-opening
     the abstraction? *)
  (* This could be replaced by a more sophisticated analysis, but for the
     moment we just use a simple syntactic check. *)
  Continuation_params_and_handler.pattern_match t.params_and_handler
    ~f:(fun params ~handler ->
      let arity = Kinded_parameter.List.arity params in
      if t.is_exn_handler then
        Unknown { arity; }
      else
        match Expr.descr handler with
        | Apply_cont apply_cont ->
          begin match Apply_cont.trap_action apply_cont with
          | Some _ -> Unknown { arity; }
          | None ->
            let args = Apply_cont.args apply_cont in
            let params = List.map KP.simple params in
            if Misc.Stdlib.List.compare Simple.compare args params = 0 then
              Alias_for {
                arity;
                alias_for = Apply_cont.continuation apply_cont;
              }
            else
              match args with
              | [simple] ->
                begin match Simple.descr simple with
                | Const arg ->
                  Apply_cont_with_constant_arg {
                    cont = Apply_cont.continuation apply_cont;
                    arg;
                    arity;
                  }
                | Name _ -> Unknown { arity; }
                end
              | _ -> Unknown { arity; }
          end
        | Invalid Treat_as_unreachable -> Unreachable { arity; }
        | _ -> Unknown { arity; })

let arity t =
  Continuation_params_and_handler.pattern_match t.params_and_handler
    ~f:(fun params ~handler:_ -> Kinded_parameter.List.arity params)

let with_params_and_handler t params_and_handler =
  { t with params_and_handler; }
