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

[@@@ocaml.warning "+a-30-40-41-42"]

module TE = Typing_env
module TEE = Typing_env_extension

type inlinable = {
  code_id : Code_id.t;
  param_arity : Flambda_arity.t;
  result_arity : Flambda_arity.t;
  stub : bool;
  dbg : Debuginfo.t;
  inline : Inline_attribute.t;
  is_a_functor : bool;
  recursive : Recursive.t;
  rec_info : Rec_info.t;
}

type t0 =
  | Non_inlinable of {
      param_arity : Flambda_arity.t;
      result_arity : Flambda_arity.t;
      recursive : Recursive.t;
    }
  | Inlinable of inlinable

type t = t0 Or_unknown.t

let print_inlinable_with_cache ~cache ppf
      ({ code; rec_info; } as decl) =
  Printing_cache.with_cache cache ppf "inlinable_fundecl" decl (fun ppf () ->
    Format.fprintf ppf
    "@[<hov 1>(Inlinable@ \
        @[<hov 1>(code@ %a)@]@ \
        @[<hov 1>(rec_info@ %a)@]\
        )@]"
    Type_grammar.print code
    Rec_info.print rec_info)

let print_t0_with_cache ~cache ppf t0 =
  match t0 with
  | Inlinable decl ->
    print_inlinable_with_cache ~cache ppf decl
  | Non_inlinable { param_arity; result_arity; recursive; } ->
    Format.fprintf ppf
      "@[<hov 1>(Non_inlinable@ \
       @[<hov 1>(param_arity@ %a)@]@ \
       @[<hov 1>(result_arity@ %a)@] \
       @[<hov 1>(recursive@ %a)@]\
       )@]"
      Flambda_arity.print param_arity
      Flambda_arity.print result_arity
      Recursive.print recursive

let print_with_cache ~cache ppf t =
  Or_unknown.print (print_t0_with_cache ~cache) ppf t

module Make_meet_or_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  let meet_or_join env t1 t2 : _ Or_bottom.t =
    match t1, t2 with
    (* CR mshinwell: Try to factor out "Or_unknown" handling from here
       and elsewhere *)
    | Unknown, Unknown -> Ok (Unknown, TEE.empty ())
    | Unknown, _ ->
      begin match E.op with
      | Meet -> Ok (t2, TEE.empty ())
      | Join -> Ok (Unknown, TEE.empty ())
      end
    | _, Unknown ->
      begin match E.op with
      | Meet -> Ok (t1, TEE.empty ())
      | Join -> Ok (Unknown, TEE.empty ())
      end
    | Known (Non_inlinable {
        param_arity = param_arity1; result_arity = result_arity1;
        recursive = recursive1;
      }), Known (Non_inlinable {
        param_arity = param_arity2; result_arity = result_arity2;
        recursive = recursive2;
      }) ->
      (* CR mshinwell: Are fatal errors right here?  Given the arbitrary
          choice below, it would seem so, but unsure.  Also, the error
          message is currently poor. *)
      if Flambda_arity.equal param_arity1 param_arity2
        && Flambda_arity.equal result_arity1 result_arity2
        && Recursive.equal recursive1 recursive2
      then Ok (t1, TEE.empty ())
      else Misc.fatal_error "Mismatched Non_inlinable arities"
    | Known (Non_inlinable _), Known (Inlinable _)
    | Known (Inlinable _), Known (Non_inlinable _) ->
      (* CR mshinwell: This should presumably return [Non_inlinable] if
         the arities match. *)
      Ok (Unknown, TEE.empty ())
    | Known (Inlinable {
        code_id = code_id1;
        param_arity = param_arity1;
        result_arity = result_arity1;
        stub = stub1;
        dbg = dbg1;
        inline = inline1;
        is_a_functor = is_a_functor1;
        recursive = recursive1;
        rec_info = _rec_info1;
      }),
      Known (Inlinable {
        code_id = code_id2;
        param_arity = param_arity2;
        result_arity = result_arity2;
        stub = stub2;
        dbg = dbg2;
        inline = inline2;
        is_a_functor = is_a_functor2;
        recursive = recursive2;
        rec_info = _rec_info2;
      }),
      let typing_env = Meet_env.env env in
      let code_age_rel = TE.code_age_relation typing_env in
      let check_other_things code_id : _ Or_bottom.t =
        assert (Flambda_arity.equal param_arity1 param_arity2);
        assert (Flambda_arity.equal result_arity1 result_arity2);
        assert (Bool.equal stub1 stub2);
        assert (Int.equal (Debuginfo.compare dbg1 dbg2) 0);
        assert (Inline_attribute.equal inline1 inline2);
        assert (Bool.equal is_a_functor1 is_a_functor2);
        assert (Recursive.equal recursive1 recursive2);
        Ok (Known (Inlinable {
            code_id;
            param_arity = param_arity1;
            result_arity = result_arity1;
            stub = stub1;
            dbg = dbg1;
            inline = inline1;
            is_a_functor = is_a_functor1;
            recursive = recursive1;
            rec_info = _rec_info1;
          }),
          TEE.empty)
      in
      (* CR mshinwell: What about [rec_info]? *)
      match E.op with
      | Meet ->
        begin match Code_age_relation.meet code_age_rel code_id1 code_id2 with
        | Ok code_id -> check_other_things_and_return code_id
        | Bottom -> Bottom
        end
      | Join ->
        begin match Code_age_relation.join code_age_rel code_id1 code_id2 with
        | Known code_id -> check_other_things_and_return code_id
        | Unknown -> Ok (Unknown, TEE.empty ())
        end
end