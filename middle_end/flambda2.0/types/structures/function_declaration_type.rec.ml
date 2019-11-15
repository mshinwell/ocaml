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

type inlinable = {
  code : Type_grammar.t;
  rec_info : Rec_info.t;
}

type t =
  | Non_inlinable of {
      param_arity : Flambda_arity.t;
      result_arity : Flambda_arity.t;
      recursive : Recursive.t;
    }
  | Inlinable of inlinable

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

let print_with_cache ~cache ppf t =
  match t with
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

module Make_meet_or_join
  (E : Lattice_ops_intf.S
   with type meet_env := Meet_env.t
   with type typing_env := Typing_env.t
   with type typing_env_extension := Typing_env_extension.t) =
struct
  let meet_or_join env t1 t2 : _ Or_bottom.t =
    match t1, t2 with
    | Non_inlinable {
        param_arity = param_arity1; result_arity = result_arity1;
        recursive = recursive1;
      }, Non_inlinable {
        param_arity = param_arity2; result_arity = result_arity2;
        recursive = recursive2;
      } ->
      (* CR mshinwell: Are fatal errors right here?  Given the arbitrary
          choice below, it would seem so, but unsure.  Also, the error
          message is currently poor. *)
      if Flambda_arity.equal param_arity1 param_arity2
        && Flambda_arity.equal result_arity1 result_arity2
        && Recursive.equal recursive1 recursive2
      then
        Ok decl1
      else
        Misc.fatal_error "Mismatched Non_inlinable arities"
    | Non_inlinable _ , Inlinable _
    | Inlinable _, Non_inlinable _ ->
      (* CR mshinwell: This should presumably return [Non_inlinable] if
         the arities match. *)
      Unknown
    | Inlinable { code = code1; rec_info = rec_info1; },
        Inlinable { code = code2; rec_info = _rec_info2; } ->
      (* CR mshinwell: What about [rec_info]? *)
      (* CR mshinwell: This function should be able to return bottom,
          presumably?  What does bottom mean here? *)
      Or_bottom.map (E.switch Type_grammar.meet Type_grammar.join code1 code2)
        ~f:(fun code -> Inlinable { code; rec_info = rec_info1; (* XXX *) })
end