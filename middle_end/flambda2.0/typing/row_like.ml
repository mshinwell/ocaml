module Make
  (Tag : Hashtbl.With_map)
  (Index : Hashtbl.With_map)
  (Tag_and_index : sig
    type t = Tag.t * Index.t
    include Hashtbl.With_map with type t := t
  end)
  (Maps_to : sig
    type t

    val bottom : unit -> t

    val print_with_cache
       : cache:Printing_cache.t
      -> Format.formatter
      -> t
      -> unit

    val equal
       : Type_equality_env.t
      -> Type_equality_result.t
      -> t
      -> t
      -> Type_equality_result.t

    val add_or_meet_equations
       : t
      -> Meet_env.t
      -> Typing_env_extension.t
      -> t

    val widen : t -> to_match:t -> t

    val meet
       : Meet_env.t
      -> t
      -> t
      -> (t * Typing_env_extension.t) Or_bottom.t

    val join
       : Join_env.t
      -> t
      -> t
      -> t

    include Contains_names.S with type t := t
  end) =
struct
  module Tag_and_index = struct
    include Tag_and_index

    let create tag index = tag, index
    let index (_tag, index) = index
  end

  module T0 = struct
    type t = {
      known : Maps_to.t Tag_and_index.Map.t;
      at_least : Maps_to.t Index.Map.t;
    }

    let print ~cache ppf ({ known; at_least } : t) =
      Format.fprintf ppf 
        "@[<v 1>(\
           @[<hov 1>(known@ %a)@]@ \
           @[<hov 1>(at_least@ %a)@])@]"
        (Tag_and_index.Map.print (Maps_to.print_with_cache ~cache)) known
        (Index.Map.print (Maps_to.print_with_cache ~cache)) at_least

    let create_bottom () =
      { known = Tag_and_index.Map.empty;
        at_least = Index.Map.empty;
      }

    let create_exactly tag index maps_to =
      let tag_and_index = Tag_and_index.create tag index in
      { known = Tag_and_index.Map.singleton tag_and_index maps_to;
        at_least = Index.Map.empty;
      }

    let create_exactly_multiple known =
      { known;
        at_least = Index.Map.empty;
      }

    let create_at_least index maps_to =
      { known = Tag_and_index.Map.empty;
        at_least = Index.Map.singleton index maps_to;
      }

    let create_at_least_multiple at_least =
      { known = Tag_and_index.Map.empty;
        at_least;
      }

    let equal env result
          { known = known1; at_least = at_least1; }
          { known = known2; at_least = at_least2; } =
      let (>>=) = Type_equality_result.(>>=) in
      result
      >>= fun result ->
      let result =
        Tag_and_index.Map.fold2_stop_on_key_mismatch
          (fun _index maps_to1 maps_to2 result ->
            result
            >>= fun result ->
            Maps_to.equal env result maps_to1 maps_to2)
          known1 known2 result
      in
      (* CR mshinwell: factor out *)
      let result =
        match result with
        | None -> Type_equality_result.types_known_unequal ()
        | Some result -> result
      in
      result
      >>= fun result ->
      let result =
        Index.Map.fold2_stop_on_key_mismatch
          (fun _index maps_to1 maps_to2 result ->
            result
            >>= fun result ->
            Maps_to.equal env result maps_to1 maps_to2)
          at_least1 at_least2 result
      in
      match result with
      | None -> Type_equality_result.types_known_unequal ()
      | Some result -> result 

    let apply_name_permutation { known; at_least; } perm =
      let known =
        (* CR mshinwell: Can just use [Tag_and_index.Map.map] now. *)
        Tag_and_index.Map.fold (fun tag_and_index maps_to known ->
            let maps_to = Maps_to.apply_name_permutation maps_to perm in
            Tag_and_index.Map.add tag_and_index maps_to known)
          known
          Tag_and_index.Map.empty
      in
      let at_least =
        Index.Map.fold (fun index maps_to at_least ->
            let maps_to = Maps_to.apply_name_permutation maps_to perm in
            Index.Map.add index maps_to at_least)
          at_least
          Index.Map.empty
      in
      { known;
        at_least;
      }

    module Meet_or_join
      (E : Either_meet_or_join_intf
        with module Join_env := Join_env
        with module Meet_env := Meet_env
        with module Typing_env_extension := Typing_env_extension) =
    struct
      let meet_or_join env t1 t2 =
        let t1 = apply_name_permutation t1 (Join_env.perm_left env) in
        let t2 = apply_name_permutation t2 (Join_env.perm_right env) in
        let env = Join_env.clear_name_permutations env in
        let ({ known = known1; at_least = at_least1; } : t) = t1 in
        let ({ known = known2; at_least = at_least2; } : t) = t2 in
        let one_side_only index1 maps_to1 at_least2
              ~get_equations_to_deposit1 =
          let from_at_least2 =
            Index.Map.find_last_opt
              (fun index -> Index.compare index index1 <= 0)
              at_least2
          in
          (* XXX This should widen the products as required rather than
             having such code in RP0 *)
          begin match from_at_least2 with
          | None ->
            begin match E.op () with
            | Meet -> None
            | Join ->
              let maps_to1 =
                Maps_to.add_or_meet_equations
                  maps_to1
                  (Join_env.central_environment env)
                  (get_equations_to_deposit1 env)
              in
              Some maps_to1
            end
          | Some (index2, from_at_least2) ->
            assert (Index.compare index2 index1 <= 0);
            (* CR mshinwell: What happens to any generated equations in the
               [meet] case (same below)? *)
            let maps_to =
              E.switch' Maps_to.meet Maps_to.join env
                maps_to1
                (Maps_to.widen from_at_least2 ~to_match:maps_to1)
            in
            match maps_to with
            | Bottom -> None
            | Ok maps_to -> Some maps_to
          end
        in
        let merge index maps_to1 maps_to2 =
          match maps_to1, maps_to2 with
          | Some maps_to1, None ->
            one_side_only index maps_to1 at_least2
              ~get_equations_to_deposit1:Join_env.holds_on_left
          | None, Some maps_to2 ->
            one_side_only index maps_to2 at_least1
              ~get_equations_to_deposit1:Join_env.holds_on_right
          | Some maps_to1, Some maps_to2 ->
            let maps_to =
              E.switch' Maps_to.meet Maps_to.join env
                maps_to1 maps_to2
            in
            begin match maps_to with
            | Bottom -> None
            | Ok maps_to -> Some maps_to
            end
          | None, None -> None
        in
        (* CR mshinwell: Shouldn't we be applying name permutations to
           these two as well? *)
        let known =
          Tag_and_index.Map.merge (fun tag_and_index maps_to1 maps_to2 ->
              let index = Tag_and_index.index tag_and_index in
              merge index maps_to1 maps_to2)
            known1
            known2
        in
        let at_least =
          Index.Map.merge (fun index maps_to1 maps_to2 ->
              merge index maps_to1 maps_to2)
            at_least1
            at_least2
        in
        { known;
          at_least;
        }
    end

    let all_maps_to { known; at_least; } =
      (Tag_and_index.Map.data known) @ (Index.Map.data at_least)

    module Meet = Meet_or_join (Either_meet_or_join.For_meet)
    module Join = Meet_or_join (Either_meet_or_join.For_join)

    let meet = Meet.meet_or_join
    let join = Join.meet_or_join

    let is_bottom { known; at_least; } =
      Tag_and_index.Map.is_empty known && Index.Map.is_empty at_least

    let known t = t.known
    let at_least t = t.at_least

    let get_singleton { known; at_least; } =
      if not (Index.Map.is_empty at_least) then None
      else Tag_and_index.Map.get_singleton known

    let free_names t =
      let { known; at_least; } = t in
      let from_known =
        Tag_and_index.Map.fold (fun _tag_and_index maps_to free_names ->
            Name_occurrences.union free_names
              (Maps_to.free_names maps_to))
          known
          (Name_occurrences.create ())
      in
      let from_at_least =
        Index.Map.fold (fun _index maps_to free_names ->
            Name_occurrences.union free_names
              (Maps_to.free_names maps_to))
          at_least
          (Name_occurrences.create ())
      in
      Name_occurrences.union from_known from_at_least

    let join_of_all_maps_to env t =
      (* Any name permutations have already been applied during
         [Meet.meet_or_join], above. *)
let maps_to =
      let env = Join_env.clear_name_permutations (Join_env.create env) in
      List.fold_left (fun result maps_to ->
Format.eprintf "Joining one Maps_to:@ %a\n%!"
(Maps_to.print_with_cache ~cache:(Printing_cache.create ())) maps_to;
          Maps_to.join env maps_to result)
        (Maps_to.bottom ())
        (all_maps_to t)
in
Format.eprintf "Join of all Maps_to:@ %a\n%!"
(Maps_to.print_with_cache ~cache:(Printing_cache.create ())) maps_to;
maps_to
  end

  type t = T0.t Or_unknown.t

  let print ~cache ppf (t : t) =
    match t with
    | Known t0 -> T0.print ~cache ppf t0
    | Unknown ->
      Format.fprintf ppf "%sT%s"
        (Misc_color.bold_red ())
        (Misc_color.reset ())

  let create_bottom () : t = Known (T0.create_bottom ())

  let create_unknown () : t = Unknown

  let create_exactly tag index maps_to : t =
    Known (T0.create_exactly tag index maps_to)

  let create_exactly_multiple known : t =
    Known (T0.create_exactly_multiple known)

  let create_at_least index maps_to : t =
    Known (T0.create_at_least index maps_to)

  let create_at_least_multiple at_least : t =
    Known (T0.create_at_least_multiple at_least)

  let equal env result (t1 : t) (t2 : t) =
    match t1, t2 with
    | Known t0_1, Known t0_2 -> T0.equal env result t0_1 t0_2
    | Known _, Unknown | Unknown, Known _ ->
      Type_equality_result.types_known_unequal ()
    | Unknown, Unknown -> result

  let apply_name_permutation (t : t) perm : t =
    match t with
    | Known t0 ->
      let t0' = T0.apply_name_permutation t0 perm in
      if t0 == t0' then t
      else Known t0'
    | Unknown -> Unknown

  let meet env (t1 : t) (t2 : t) : (t * Maps_to.t) Or_bottom.t =
    (* CR mshinwell: Perhaps we should subtract the "join of all maps" from
       the returned extension *)
    match t1, t2 with
    | Known t0_1, Known t0_2 ->
      let t0 = T0.meet (Join_env.create env) t0_1 t0_2 in
      Ok (Known t0, T0.join_of_all_maps_to env t0)
    | Unknown, Known t0 ->
      Ok (t2, T0.join_of_all_maps_to env t0)
    | Known t0, Unknown ->
      Ok (t1, T0.join_of_all_maps_to env t0)
    | Unknown, Unknown ->
      Ok (Unknown, Maps_to.bottom ())

  let join env (t1 : t) (t2 : t) : t =
    match t1, t2 with
    | Known t0_1, Known t0_2 -> Known (T0.join env t0_1 t0_2)
    | Unknown, t2 -> t2
    | t1, Unknown -> t1

  let is_bottom (t : t) =
    match t with
    | Known t0 -> T0.is_bottom t0
    | Unknown -> false

  let known (t : t) : _ Or_unknown.t =
    match t with
    | Known t0 -> Known (T0.known t0)
    | Unknown -> Unknown

  let at_least (t : t) : _ Or_unknown.t =
    match t with
    | Known t0 -> Known (T0.at_least t0)
    | Unknown -> Unknown

  let get_singleton (t : t) =
    match t with
    | Known t0 -> T0.get_singleton t0
    | Unknown -> None

  let free_names (t : t) =
    match t with
    | Known t0 -> T0.free_names t0
    | Unknown -> Name_occurrences.create ()

  let classify (t : t) : unit Or_unknown_or_bottom.t =
    match t with
    | Known t0 -> if T0.is_bottom t0 then Bottom else Ok ()
    | Unknown -> Unknown
end
