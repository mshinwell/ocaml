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

module Inlinable = struct
  type t = {
    code_id : Code_id.t;
    dbg : Debuginfo.t;
    rec_info : Rec_info.t;
    is_tupled : bool;
    must_be_inlined : bool;
  }

  let print ppf { code_id; dbg; rec_info; is_tupled; must_be_inlined } =
    Format.fprintf ppf
      "@[<hov 1>(Inlinable@ \
        @[<hov 1>(code_id@ %a)@]@ \
        @[<hov 1>(dbg@ %a)@]@ \
        @[<hov 1>(rec_info@ %a)@]@ \
        @[<hov 1><is_tupled@ %b)@]@ \
        @[<hov 1><must_be_inlined@ %b)@]\
        )@]"
      Code_id.print code_id
      Debuginfo.print_compact dbg
      Rec_info.print rec_info
      is_tupled
      must_be_inlined

  let code_id t = t.code_id
  let dbg t = t.dbg
  let is_tupled t = t.is_tupled
  let must_be_inlined t = t.must_be_inlined

  let apply_renaming
        ({ code_id; dbg = _; rec_info = _; is_tupled = _;
           must_be_inlined = _ } as t) renaming =
    let code_id' = Renaming.apply_code_id renaming code_id in
    if code_id == code_id' then t
    else { t with code_id = code_id'; rec_info = Rec_info.unknown }

end

module Non_inlinable = struct
  type t = {
    code_id : Code_id.t;
    is_tupled : bool;
  }

  let print ppf { code_id; is_tupled; } =
    Format.fprintf ppf
      "@[<hov 1>(Non_inlinable@ \
        @[<hov 1>(code_id@ %a)@]@ \
        @[<hov 1>(is_tupled@ %b)@]\
        )@]"
      Code_id.print code_id
      is_tupled

  let code_id t = t.code_id
  let is_tupled t = t.is_tupled

  let apply_renaming ({ code_id; is_tupled = _; } as t) renaming =
    let code_id' = Renaming.apply_code_id renaming code_id in
    if code_id == code_id' then t
    else { t with code_id = code_id'; }
end

type t0 =
  | Inlinable of Inlinable.t
  | Non_inlinable of Non_inlinable.t

type t = t0 Or_unknown_or_bottom.t

let create ~code ~dbg ~rec_info ~is_tupled =
  let code_id = Flambda.Code.code_id code in
  let inlining_decision = Function_decl_inlining_decision.make_decision code in
  let t : t =
    match Function_decl_inlining_decision.behaviour inlining_decision with
    | Cannot_be_inlined ->
      Ok (Non_inlinable { code_id; is_tupled; })
    | Must_be_inlined ->
      Ok (Inlinable { code_id; dbg; is_tupled; rec_info;
                      must_be_inlined = true; })
    | Could_possibly_be_inlined ->
      Ok (Inlinable { code_id; dbg; is_tupled; rec_info;
                      must_be_inlined = false; })
  in
  t, inlining_decision

let create_non_inlinable ~code_id ~is_tupled : t =
  Ok (Non_inlinable { code_id; is_tupled; })

let print_t0 ppf t0 =
  match t0 with
  | Inlinable inlinable -> Inlinable.print ppf inlinable
  | Non_inlinable non_inlinable -> Non_inlinable.print ppf non_inlinable

let print_with_cache ~cache:_ ppf t =
  Or_unknown_or_bottom.print print_t0 ppf t

let print ppf t =
  Or_unknown_or_bottom.print print_t0 ppf t

let free_names (t : t) =
  match t with
  | Bottom | Unknown -> Name_occurrences.empty
  | Ok (Inlinable { code_id; dbg = _; rec_info = _; is_tupled = _;
                    must_be_inlined = _; })
  | Ok (Non_inlinable { code_id; is_tupled = _; }) ->
    Name_occurrences.add_code_id Name_occurrences.empty code_id
      Name_mode.in_types

let all_ids_for_export (t : t) =
  match t with
  | Bottom | Unknown -> Ids_for_export.empty
  | Ok (Inlinable { code_id; dbg = _; rec_info = _; is_tupled = _;
                    must_be_inlined = _; })
  | Ok (Non_inlinable { code_id; is_tupled = _; }) ->
    Ids_for_export.add_code_id Ids_for_export.empty code_id

let apply_renaming (t : t) renaming : t =
  match t with
  | Bottom | Unknown -> t
  | Ok (Inlinable inlinable) ->
    Ok (Inlinable (Inlinable.apply_renaming inlinable renaming))
  | Ok (Non_inlinable non_inlinable) ->
    Ok (Non_inlinable (Non_inlinable.apply_renaming non_inlinable renaming))

let meet (env : Meet_env.t) (t1 : t) (t2 : t)
      : (t * TEE.t) Or_bottom.t =
  match t1, t2 with
  (* CR mshinwell: Try to factor out "Or_unknown_or_bottom" handling from here
     and elsewhere *)
  | Bottom, _ | _, Bottom -> Ok (Bottom, TEE.empty ())
  | Unknown, t | t, Unknown -> Ok (t, TEE.empty ())
  | Ok (Non_inlinable {
      code_id = code_id1; is_tupled = is_tupled1;
    }), Ok (Non_inlinable {
      code_id = code_id2; is_tupled = is_tupled2;
    }) ->
    let typing_env = Meet_env.env env in
    let target_code_age_rel = TE.code_age_relation typing_env in
    let resolver = TE.code_age_relation_resolver typing_env in
    let check_other_things_and_return code_id : (t * TEE.t) Or_bottom.t =
      assert (Bool.equal is_tupled1 is_tupled2);
      Ok (Ok (Non_inlinable {
          code_id;
          is_tupled = is_tupled1;
        }),
        TEE.empty ())
    in
    begin match
      Code_age_relation.meet target_code_age_rel ~resolver code_id1 code_id2
    with
    | Ok code_id -> check_other_things_and_return code_id
    | Bottom -> Bottom
    end
  | Ok (Non_inlinable _), Ok (Inlinable _)
  | Ok (Inlinable _), Ok (Non_inlinable _) ->
    (* CR mshinwell: This should presumably return [Non_inlinable] if
       the arities match. *)
    (* CR vlaviron: The above comment was from before meet and join were split.
       Now that we know we're in meet, we can actually keep either of them
       (the inlinable one seems better) *)
    Ok (Unknown, TEE.empty ())
  | Ok (Inlinable {
      code_id = code_id1;
      dbg = dbg1;
      rec_info = _;
      is_tupled = is_tupled1;
      must_be_inlined = _;
    }),
    Ok (Inlinable {
      code_id = code_id2;
      dbg = dbg2;
      rec_info = _;
      is_tupled = is_tupled2;
      must_be_inlined = _;
    }) ->
    let typing_env = Meet_env.env env in
    let target_code_age_rel = TE.code_age_relation typing_env in
    let resolver = TE.code_age_relation_resolver typing_env in
    let check_other_things_and_return code_id : (t * TEE.t) Or_bottom.t =
      assert (Int.equal (Debuginfo.compare dbg1 dbg2) 0);
      assert (Bool.equal is_tupled1 is_tupled2);
      let code = TE.find_code typing_env code_id in
      let t, _inlining_decision =
        create ~code ~dbg:dbg1 ~is_tupled:is_tupled1 ~rec_info:Rec_info.unknown
      in
      Ok (t, TEE.empty ())
    in
    begin match
      Code_age_relation.meet target_code_age_rel ~resolver code_id1 code_id2
    with
    | Ok code_id -> check_other_things_and_return code_id
    | Bottom -> Bottom
    end

let join (env : Join_env.t) (t1 : t) (t2 : t) : t =
  match t1, t2 with
  (* CR mshinwell: Try to factor out "Or_unknown_or_bottom" handling from here
     and elsewhere *)
  | Bottom, t | t, Bottom -> t
  | Unknown, _ | _, Unknown -> Unknown
  | Ok (Non_inlinable {
      code_id = code_id1; is_tupled = is_tupled1;
    }), Ok (Non_inlinable {
      code_id = code_id2; is_tupled = is_tupled2;
    }) ->
    let typing_env = Join_env.target_join_env env in
    let target_code_age_rel = TE.code_age_relation typing_env in
    let resolver = TE.code_age_relation_resolver typing_env in
    let check_other_things_and_return code_id : t =
      assert (Bool.equal is_tupled1 is_tupled2);
      Ok (Non_inlinable {
        code_id;
        is_tupled = is_tupled1; })
    in
    let code_age_rel1 =
      TE.code_age_relation (Join_env.left_join_env env)
    in
    let code_age_rel2 =
      TE.code_age_relation (Join_env.right_join_env env)
    in
    begin match
      Code_age_relation.join ~target_t:target_code_age_rel ~resolver
        code_age_rel1 code_age_rel2 code_id1 code_id2
    with
    | Known code_id -> check_other_things_and_return code_id
    | Unknown -> Unknown
    end
  | Ok (Non_inlinable _), Ok (Inlinable _)
  | Ok (Inlinable _), Ok (Non_inlinable _) ->
    (* CR mshinwell: This should presumably return [Non_inlinable] if
       the arities match. *)
    Unknown
  | Ok (Inlinable {
      code_id = code_id1;
      dbg = dbg1;
      rec_info = _;
      is_tupled = is_tupled1;
      must_be_inlined = _;
    }),
    Ok (Inlinable {
      code_id = code_id2;
      dbg = dbg2;
      rec_info = _;
      is_tupled = is_tupled2;
      must_be_inlined = _;
    }) ->
    let typing_env = Join_env.target_join_env env in
    let target_code_age_rel = TE.code_age_relation typing_env in
    let resolver = TE.code_age_relation_resolver typing_env in
    let check_other_things_and_return code_id : t =
      assert (Int.equal (Debuginfo.compare dbg1 dbg2) 0);
      assert (Bool.equal is_tupled1 is_tupled2);
      let code = TE.find_code typing_env code_id in
      let t, _inlining_decision =
        create ~code ~dbg:dbg1 ~rec_info:Rec_info.unknown ~is_tupled:is_tupled1
      in
      t
    in
    let code_age_rel1 =
      TE.code_age_relation (Join_env.left_join_env env)
    in
    let code_age_rel2 =
      TE.code_age_relation (Join_env.right_join_env env)
    in
    begin match
      Code_age_relation.join ~target_t:target_code_age_rel ~resolver
        code_age_rel1 code_age_rel2 code_id1 code_id2
    with
    | Known code_id -> check_other_things_and_return code_id
    | Unknown -> Unknown
    end

let apply_coercion (t : t) _coercion : t Or_bottom.t =
  Ok t
