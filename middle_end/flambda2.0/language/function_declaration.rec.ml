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
  closure_origin : Closure_origin.t;
  params_and_body : Function_params_and_body.t;
  (* CR mshinwell: Need to document that [code_id] is used for equality
     checking, so it must be updated.  Maybe it's a misnomer in fact. *)
  code_id : Code_id.t;
  result_arity : Flambda_arity.t;
  stub : bool;
  dbg : Debuginfo.t;
  inline : Inline_attribute.t;
  is_a_functor : bool;
}

let invariant _env _t = ()

let create ~closure_origin ~params_and_body ~result_arity ~stub ~dbg
      ~(inline : Inline_attribute.t)
      ~is_a_functor : t =
  begin match stub, inline with
  | true, (Never_inline | Default_inline)
  | false, (Never_inline | Default_inline | Always_inline | Unroll _) -> ()
  | true, (Always_inline | Unroll _) ->
    Misc.fatal_errorf
      "Stubs may not be annotated as [Always_inline] or [Unroll]: %a"
      Function_params_and_body.print params_and_body
  end;
  { closure_origin;
    params_and_body;
    code_id = Code_id.create (Compilation_unit.get_current_exn ());
    result_arity;
    stub;
    dbg;
    inline;
    is_a_functor;
  }

let print_with_cache0 ~compact ~cache ppf
      { closure_origin;
        params_and_body;
        code_id = _;
        result_arity;
        stub;
        dbg;
        inline;
        is_a_functor;
      } =
  (* CR mshinwell: It's a bit strange that this doesn't use
     [Function_params_and_body.print_with_cache].  However a proper
     function to print in a more human-readable form will probably look more
     like this code. *)
  Function_params_and_body.pattern_match params_and_body
    ~f:(fun ~return_continuation exn_continuation params ~body ~my_closure ->
      fprintf ppf "@[<hov 1>(\
          @[<hov 1>(closure_origin@ %a)@]@ \
          @[<hov 1>(return_continuation@ %a)@]@ \
          @[<hov 1>(exn_continuation@ %a)@]@ \
          @[<hov 1>(stub@ %b)@]@ \
          @[<hov 1>(dbg@ %a)@]@ \
          @[<hov 1>(inline@ %a)@]@ \
          @[<hov 1>(is_a_functor@ %b)@]@ \
          @[<hov 1>(params@ %a)@]@ \
          @[<hov 1>(my_closure@ %s%a%s)@]@ \
          @[<hov 1>(result_arity@ %a)@]@ "
        Closure_origin.print closure_origin
        Continuation.print return_continuation
        Exn_continuation.print exn_continuation
        stub
        Debuginfo.print_compact dbg
        Inline_attribute.print inline
        is_a_functor
        Kinded_parameter.List.print params
        (Misc.Color.bold_magenta ())
        Variable.print my_closure
        (Flambda_colours.normal ())
        Flambda_arity.print result_arity;
      if compact then begin
        fprintf ppf "@[<hov 1>(body@ <elided>)@])@]"
      end else begin
        fprintf ppf "@[<hov 1>(body@ %a)@])@]"
          (Expr.print_with_cache ~cache) body
      end)

let print_with_cache ~cache ppf t =
  print_with_cache0 ~compact:false ~cache ppf t

let print ppf t = print_with_cache ~cache:(Printing_cache.create ()) ppf t

let print_compact ppf t =
  print_with_cache0 ~compact:true ~cache:(Printing_cache.create ()) ppf t

let closure_origin t = t.closure_origin
let params_and_body t = t.params_and_body
let code_id t = t.code_id
let result_arity t = t.result_arity
let stub t = t.stub
let dbg t = t.dbg
let inline t = t.inline
let is_a_functor t = t.is_a_functor

let update_params_and_body t params_and_body =
  { t with
    params_and_body;
    code_id = Code_id.create (Compilation_unit.get_current_exn ());
  }

let free_names
      { closure_origin = _;
        params_and_body;
        code_id = _;
        result_arity = _;
        stub = _;
        dbg = _;
        inline = _;
        is_a_functor = _;
      } =
  Function_params_and_body.free_names params_and_body

let apply_name_permutation
      ({ closure_origin;
         params_and_body;
         code_id;
         result_arity;
         stub;
         dbg;
         inline;
         is_a_functor;
       } as t) perm =
  let params_and_body' =
    Function_params_and_body.apply_name_permutation params_and_body perm
  in
  if params_and_body == params_and_body' then t
  else
    { closure_origin;
      params_and_body = params_and_body';
      code_id;
      result_arity;
      stub;
      dbg;
      inline;
      is_a_functor;
    }

let params_arity t =
  Function_params_and_body.pattern_match t.params_and_body
    ~f:(fun ~return_continuation:_ _exn_continuation params ~body:_
            ~my_closure:_ ->
      KP.List.arity params)
