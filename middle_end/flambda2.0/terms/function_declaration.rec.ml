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
  params_and_body : Function_params_and_body.t;
  (* CR mshinwell: Need to document that [code_id] is used for equality
     checking, so it must be updated.  Maybe it's a misnomer in fact. *)
  code_id : Code_id.t;
  result_arity : Flambda_arity.t;
  stub : bool;
  dbg : Debuginfo.t;
  inline : Inline_attribute.t;
  is_a_functor : bool;
  recursive : Recursive.t;
}

let invariant _env _t = ()

let create ~params_and_body ~result_arity ~stub ~dbg
      ~(inline : Inline_attribute.t)
      ~is_a_functor ~recursive : t =
  begin match stub, inline with
  | true, (Never_inline | Default_inline)
  | false, (Never_inline | Default_inline | Always_inline | Unroll _) -> ()
  | true, (Always_inline | Unroll _) ->
    Misc.fatal_errorf
      "Stubs may not be annotated as [Always_inline] or [Unroll]: %a"
      Function_params_and_body.print params_and_body
  end;
  { params_and_body;
    code_id = Code_id.create (Compilation_unit.get_current_exn ());
    result_arity;
    stub;
    dbg;
    inline;
    is_a_functor;
    recursive;
  }

let print_with_cache0 ~compact ~cache ppf
      { params_and_body;
        code_id = _;
        result_arity;
        stub;
        dbg;
        inline;
        is_a_functor;
        recursive;
      } =
  (* CR mshinwell: It's a bit strange that this doesn't use
     [Function_params_and_body.print_with_cache].  However a proper
     function to print in a more human-readable form will probably look more
     like this code. *)
  let module C = Flambda_colours in
  Function_params_and_body.pattern_match params_and_body
    ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure ->
      let my_closure =
        Kinded_parameter.create (Parameter.wrap my_closure) Flambda_kind.value
      in
      fprintf ppf "@[<hov 1>(\
          @[<hov 1>@<0>%s(stub@ %b)@<0>%s@]@ \
          @[<hov 1>@<0>%s(dbg@ %a)@<0>%s@]@ \
          @[<hov 1>@<0>%s(inline@ %a)@<0>%s@]@ \
          @[<hov 1>@<0>%s(is_a_functor@ %b)@<0>%s@]@ \
          @[<hov 1>@<0>%s(result_arity@ @<0>%s%a@<0>%s)@<0>%s@]@ \
          @[<hov 1>@<0>%s(recursive@ %a)@<0>%s@]@ "
        (if not stub then Flambda_colours.elide () else C.normal ())
        stub
        (Flambda_colours.normal ())
        (Flambda_colours.debuginfo ())
        Debuginfo.print_compact dbg
        (Flambda_colours.normal ())
        (if Inline_attribute.is_default inline
         then Flambda_colours.elide ()
         else C.normal ())
        Inline_attribute.print inline
        (Flambda_colours.normal ())
        (if not is_a_functor then Flambda_colours.elide () else C.normal ())
        is_a_functor
        (Flambda_colours.normal ())
        (if Flambda_arity.is_singleton_value result_arity
         then Flambda_colours.elide ()
         else Flambda_colours.normal ())
        (Flambda_colours.normal ())
        Flambda_arity.print result_arity
        (if Flambda_arity.is_singleton_value result_arity
         then Flambda_colours.elide ()
         else Flambda_colours.normal ())
        (Flambda_colours.normal ())
        (match recursive with
         | Non_recursive -> Flambda_colours.elide ()
         | Recursive -> Flambda_colours.normal ())
        Recursive.print recursive
        (Flambda_colours.normal ());
      if compact then begin
        fprintf ppf "@[<hov 1>(params_and_body@ <elided>)@])@]"
      end else begin
        fprintf ppf
          "@[<hov 1>(@<0>%s@<1>\u{03bb}@<0>%s@[<hov 1>\
           @<1>\u{3008}%a@<1>\u{3009}@<1>\u{300a}%a@<1>\u{300b}\
           %a %a @<0>%s.@<0>%s@]@ %a)@])@]"
          (Flambda_colours.lambda ())
          (Flambda_colours.normal ())
          Continuation.print return_continuation
          Exn_continuation.print exn_continuation
          Kinded_parameter.List.print params
          Kinded_parameter.print my_closure
          (Flambda_colours.elide ())
          (Flambda_colours.normal ())
          (Expr.print_with_cache ~cache) body
      end)

let print_with_cache ~cache ppf t =
  print_with_cache0 ~compact:false ~cache ppf t

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let print_compact ppf t =
  print_with_cache0 ~compact:true ~cache:(Printing_cache.create ()) ppf t

let params_and_body t = t.params_and_body
let code_id t = t.code_id
let result_arity t = t.result_arity
let stub t = t.stub
let dbg t = t.dbg
let inline t = t.inline
let is_a_functor t = t.is_a_functor
let recursive t = t.recursive

let update_params_and_body t params_and_body =
  { t with
    params_and_body;
    code_id = Code_id.create (Compilation_unit.get_current_exn ());
  }

let free_names
      { params_and_body;
        code_id = _;
        result_arity = _;
        stub = _;
        dbg = _;
        inline = _;
        is_a_functor = _;
        recursive = _;
      } =
  Function_params_and_body.free_names params_and_body

let apply_name_permutation
      ({ params_and_body;
         code_id;
         result_arity;
         stub;
         dbg;
         inline;
         is_a_functor;
         recursive;
       } as t) perm =
  let params_and_body' =
    Function_params_and_body.apply_name_permutation params_and_body perm
  in
  if params_and_body == params_and_body' then t
  else
    { params_and_body = params_and_body';
      code_id;
      result_arity;
      stub;
      dbg;
      inline;
      is_a_functor;
      recursive;
    }

let params_arity t =
  Function_params_and_body.pattern_match t.params_and_body
    ~f:(fun ~return_continuation:_ _exn_continuation params ~body:_
            ~my_closure:_ ->
      KP.List.arity params)
