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

module type Thing = sig
  type t

  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t

  val print : Format.formatter -> t -> unit
end

module type Thing_no_hash = sig
  type t

  include Map.OrderedType with type t := t

  val print : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module type Set = sig
  module T : Set.OrderedType
  include Set.S
    with type elt = T.t

  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
  val union_list : t list -> t
  val get_singleton : t -> elt option
end

module type Map = sig
  module T : Map.OrderedType
  module Our_set : Set.S

  include Map.S
    with type key = T.t

  val filter_map : 'a t -> f:(key -> 'a -> 'b option) -> 'b t
  val of_list : (key * 'a) list -> 'a t
  val diff : 'a t -> 'a t -> 'a t
  val disjoint_union : ?eq:('a -> 'a -> bool) -> ?print:(Format.formatter -> 'a -> unit) -> 'a t -> 'a t -> 'a t
  val union_right : 'a t -> 'a t -> 'a t
  val union_left : 'a t -> 'a t -> 'a t
  val union_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val union_both
     : ('a -> 'a)
    -> ('a -> 'a -> 'a)
    -> 'a t
    -> 'a t
    -> 'a t
  val intersection_fold_and_remainder
     : 'a t
    -> 'a t
    -> init:'b
    -> inter:('b -> key -> 'a -> 'a -> 'a * 'b)
    -> 'a t * 'b
  val for_all2_opt : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool option
  val inter : ('a -> 'a -> 'b option) -> 'a t -> 'a t -> 'b t
  val inter_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val rename : key t -> key -> key
  val map_keys : (key -> key) -> 'a t -> 'a t
  val keys : 'a t -> Our_set.t
  val data : 'a t -> 'a list
  val of_set : (key -> 'a) -> Our_set.t -> 'a t
  val transpose_keys_and_data : key t -> key t
  val transpose_keys_and_data_set : key t -> Our_set.t t
  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val get_singleton : 'a t -> (key * 'a) option
end

module type Tbl = sig
  module Our_map : Map.S

  module T : sig
    type t
    include Map.OrderedType with type t := t
    include Hashtbl.HashedType with type t := t
  end
  include Hashtbl.S
    with type key = T.t

  val to_list : 'a t -> (T.t * 'a) list
  val of_list : (T.t * 'a) list -> 'a t
  val to_map : 'a t -> 'a Our_map.t
  val of_map : 'a Our_map.t -> 'a t
  val memoize : 'a t -> (key -> 'a) -> key -> 'a
  val map : 'a t -> ('a -> 'b) -> 'b t
end

module Pair (A : Thing) (B : Thing) : Thing with type t = A.t * B.t = struct
  type t = A.t * B.t

  let compare (a1, b1) (a2, b2) =
    let c = A.compare a1 a2 in
    if c <> 0 then c
    else B.compare b1 b2

  let hash (a, b) = Hashtbl.hash (A.hash a, B.hash b)
  let equal (a1, b1) (a2, b2) = A.equal a1 a2 && B.equal b1 b2
  let print ppf (a, b) = Format.fprintf ppf " (%a, @ %a)" A.print a B.print b
end

module Make_map (T : Thing_no_hash) = struct
  include Map.Make (T)

  let diff t1 t2 =
    filter (fun key _ -> not (mem key t2)) t1

  let filter_map t ~f =
    fold (fun id v map ->
        match f id v with
        | None -> map
        | Some r -> add id r map) t empty

  let of_list l =
    List.fold_left (fun map (id, v) -> add id v map) empty l

  let disjoint_union ?eq ?print m1 m2 =
    union (fun id v1 v2 ->
        let ok = match eq with
          | None -> false
          | Some eq -> eq v1 v2
        in
        if not ok then
          let err =
            match print with
            | None ->
              Format.asprintf "Map.disjoint_union %a" T.print id
            | Some print ->
              Format.asprintf "Map.disjoint_union %a => %a <> %a"
                T.print id print v1 print v2
          in
          (* CR mshinwell: should be [Misc.fatal_error], but dependency
             cycle *)
          failwith err
        else Some v1)
      m1 m2

  let union_right m1 m2 =
    merge (fun _id x y -> match x, y with
        | None, None -> None
        | None, Some v
        | Some v, None
        | Some _, Some v -> Some v)
      m1 m2

  let union_left m1 m2 = union_right m2 m1

  let union_merge f m1 m2 =
    let aux _ m1 m2 =
      match m1, m2 with
      | None, m | m, None -> m
      | Some m1, Some m2 -> Some (f m1 m2)
    in
    merge aux m1 m2

  let union_both f g m1 m2 =
    let aux _ m1 m2 =
      match m1, m2 with
      | None, None -> None
      | None, Some m | Some m, None -> Some (f m)
      | Some m1, Some m2 -> Some (g m1 m2)
    in
    merge aux m1 m2

  let inter f t1 t2 =
    fold (fun key t1_elt inter ->
        match find key t2 with
        | exception Not_found -> inter
        | t2_elt ->
          match f t1_elt t2_elt with
          | None -> inter
          | Some elt -> add key elt inter)
      t1
      empty

  let inter_merge f t1 t2 =
    fold (fun key t1_elt inter ->
        match find key t2 with
        | exception Not_found -> inter
        | t2_elt -> add key (f t1_elt t2_elt) inter)
      t1
      empty

  let intersection_fold_and_remainder t1 t2 ~init ~inter =
    let in_both =
      inter (fun datum1 datum2 -> Some (datum1, datum2)) t1 t2
    in
    let in_both, acc =
      fold (fun name (datum1, datum2) (in_both, acc) ->
          let datum, acc = inter acc name datum1 datum2 in
          let in_both = add name datum in_both in
          in_both, acc)
        in_both
        init
    in
    let only_in_t1 =
      filter (fun name _ -> not (mem name in_both)) t1
    in
    let only_in_t2 =
      filter (fun name _ -> not (mem name in_both)) t2
    in
    let remainder = disjoint_union only_in_t1 only_in_t2 in
    let result = disjoint_union in_both remainder in
    result, acc

  let rename m v =
    try find v m
    with Not_found -> v

  let map_keys f m =
    of_list (List.map (fun (k, v) -> f k, v) (bindings m))

  let print f ppf t =
    let print_binding ppf (id, v) =
      Format.fprintf ppf "@[<hov 1>(%s%a%s@ %a)@]"
        (Misc_color.bold_green ())
        T.print id
        (Misc_color.reset ())
        f v
    in
    let bindings = bindings t in
    match bindings with
    | [] -> Format.fprintf ppf "()"
    | _ ->
      Format.fprintf ppf "@[<hov 1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          print_binding) bindings

  module T_set = Set.Make (T)

  let keys map = fold (fun k _ set -> T_set.add k set) map T_set.empty

  let data t = List.map snd (bindings t)

  let of_set f set = T_set.fold (fun e map -> add e (f e) map) set empty

  let transpose_keys_and_data map = fold (fun k v m -> add v k m) map empty
  let transpose_keys_and_data_set map =
    fold (fun k v m ->
        let set =
          match find v m with
          | exception Not_found ->
            T_set.singleton k
          | set ->
            T_set.add k set
        in
        add v set m)
      map empty

  let get_singleton t =
    match bindings t with
    | [key, value] -> Some (key, value)
    | _ -> None

  let for_all2_opt f t1 t2 =
    (* CR mshinwell: Provide a proper implementation *)
    if cardinal t1 <> cardinal t2 then None
    else
      let t1 = bindings t1 in
      let t2 = bindings t2 in
      let for_all2 =
        List.for_all2 (fun (key1, datum1) (key2, datum2) ->
            T.compare key1 key2 = 0 && f datum1 datum2)
          t1 t2
      in
      Some for_all2
end

module Make_set (T : Thing_no_hash) = struct
  include Set.Make (T)

  let of_list l = match l with
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q

  let map f s = of_list (List.map f (elements s))

  let union_list ts =
    List.fold_left (fun acc t -> union acc t) empty ts

  let get_singleton t =
    match elements t with
    | [elt] -> Some elt
    | _ -> None

  let print ppf t =
    match get_singleton t with
    | None ->
      Format.fprintf ppf "@[<hov 1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
        (elements t)
    | Some elt -> T.print ppf elt

  let to_string s = Format.asprintf "%a" print s
end

module Make_tbl (T : Thing) = struct
  include Hashtbl.Make (T)

  module T_map = Make_map (T)

  let to_list t =
    fold (fun key datum elts -> (key, datum)::elts) t []

  let of_list elts =
    let t = create 42 in
    List.iter (fun (key, datum) -> add t key datum) elts;
    t

  let to_map v = fold T_map.add v T_map.empty

  let of_map m =
    let t = create (T_map.cardinal m) in
    T_map.iter (fun k v -> add t k v) m;
    t

  let memoize t f = fun key ->
    try find t key with
    | Not_found ->
      let r = f key in
      add t key r;
      r

  let map t f =
    of_map (T_map.map f (to_map t))
end

module type S = sig
  type t

  module T : Thing with type t = t
  include Thing with type t := T.t

  module Set : Set with module T := T
  module Map : Map with module T := T with module Our_set := Set
  module Tbl : Tbl with module T := T with module Our_map := Map
end

module Make (T : Thing) = struct
  module T = T
  include T

  module Set = Make_set (T)
  module Map = Make_map (T)
  module Tbl = Make_tbl (T)
end

module Make_pair (T0 : S) (T1 : S) = struct
  module T = struct
    type t = T0.t * T1.t

    let compare (t0, t1) (t0', t1') =
      let c = T0.compare t0 t0' in
      if c <> 0 then c
      else T1.compare t1 t1'

    let equal (t0, t1) (t0', t1') =
      T0.equal t0 t0' && T1.equal t1 t1'

    let hash (t0, t1) = Hashtbl.hash (T0.hash t0, T1.hash t1)

    let print ppf (t0, t1) =
      Format.fprintf ppf "@[(%a, %a)@]" T0.print t0 T1.print t1
  end

  include T

  module Set = Make_set (T)
  module Map = Make_map (T)
  module Tbl = Make_tbl (T)

  let create_from_cross_product t0_set t1_set =
    T0.Set.fold (fun t0 result ->
        T1.Set.fold (fun t1 result ->
            Set.add (t0, t1) result)
          t1_set
          result)
      t0_set
      Set.empty
end

module type S_no_hash = sig
  type t

  module T : Thing_no_hash with type t = t
  include Thing_no_hash with type t := T.t

  module Set : Set with module T := T
  module Map : Map with module T := T with module Our_set := Set

  val equal : t -> t -> bool
end

module Make_no_hash (T : Thing_no_hash) = struct
  module T = T
  include T

  module Set = Make_set (T)
  module Map = Make_map (T)
end
