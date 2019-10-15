(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type Thing = sig
  type t

  include Hashtbl.HashedType with type t := t
  include Map.OrderedType with type t := t

  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
end

module type Set = sig
  module T : Set.OrderedType
  include Set.S with type elt = T.t

  val output : out_channel -> t -> unit
  val print : Format.formatter -> t -> unit
  val to_string : t -> string
  val of_list : elt list -> t
  val map : (elt -> elt) -> t -> t
  val get_singleton : t -> elt option
end

module type Map = sig
  module T : Map.OrderedType
  include Map.S with type key = T.t

  module Set : Set with module T := T

  val filter_map : 'a t -> f:(key -> 'a -> 'b option) -> 'b t
  val of_list : (key * 'a) list -> 'a t

  (** Like [map], but the returned map will be physically equal to the input
      map if every call [f a] made during the mapping returns a value
      physically equal to [a]. *)
  val map_sharing: ('a -> 'a) -> 'a t -> 'a t

  val get_singleton : 'a t -> (key * 'a) option

  val disjoint_union :
    ?eq:('a -> 'a -> bool) -> ?print:(Format.formatter -> 'a -> unit) -> 'a t ->
    'a t -> 'a t

  val union_right : 'a t -> 'a t -> 'a t

  val union_left : 'a t -> 'a t -> 'a t

  val union_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val rename : key t -> key -> key
  val map_keys : (key -> key) -> 'a t -> 'a t
  val keys : 'a t -> Set.t
  val data : 'a t -> 'a list
  val of_set : (key -> 'a) -> Set.t -> 'a t
  val transpose_keys_and_data : key t -> key t
  val transpose_keys_and_data_set : key t -> Set.t t
  val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val diff_domains : 'a t -> 'a t -> 'a t
  val fold2_stop_on_key_mismatch
      : (key -> 'a -> 'a -> 'b -> 'b)
        -> 'a t
        -> 'a t
        -> 'b
        -> 'b option
end

module type Tbl = sig
  module T : sig
    type t
    include Map.OrderedType with type t := t
    include Hashtbl.HashedType with type t := t
  end
  include Hashtbl.S with type key = T.t

  module Map : Map with module T := T

  val to_list : 'a t -> (T.t * 'a) list
  val of_list : (T.t * 'a) list -> 'a t

  val to_map : 'a t -> 'a Map.t
  val of_map : 'a Map.t -> 'a t
  val memoize : 'a t -> (key -> 'a) -> key -> 'a
  val map : 'a t -> ('a -> 'b) -> 'b t
end

module Pair (A : Thing) (B : Thing) : Thing with type t = A.t * B.t = struct
  type t = A.t * B.t

  let compare (a1, b1) (a2, b2) =
    let c = A.compare a1 a2 in
    if c <> 0 then c
    else B.compare b1 b2

  let output oc (a, b) = Printf.fprintf oc " (%a, %a)" A.output a B.output b
  let hash (a, b) = Hashtbl.hash (A.hash a, B.hash b)
  let equal (a1, b1) (a2, b2) = A.equal a1 a2 && B.equal b1 b2
  let print ppf (a, b) = Format.fprintf ppf " (%a, @ %a)" A.print a B.print b
end

module Make_map (T : Thing) (Set : Set with module T := T) = struct
  include Map.Make (T)

  module Set = Set

  let get_singleton t =
    match bindings t with
    | [key, datum] -> Some (key, datum)
    | _ -> None

  let filter_map t ~f =
    fold (fun id v map ->
        match f id v with
        | None -> map
        | Some r -> add id r map) t empty

  (* CR mshinwell: Implement this properly.  We should move things like
     [get_singleton] into the stdlib at the same time. *)
  let map_sharing = map

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
          Misc.fatal_error err
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

  let rename m v =
    try find v m
    with Not_found -> v

  let map_keys f m =
    of_list (List.map (fun (k, v) -> f k, v) (bindings m))

  let print print_datum ppf t =
    if is_empty t then
      Format.fprintf ppf "{}"
    else
      Format.fprintf ppf "@[<hov 1>{%a}@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          (fun ppf (key, datum) ->
            Format.fprintf ppf "@[<hov 1>(%a@ %a)@]"
              T.print key print_datum datum))
        (bindings t)

  let keys map = fold (fun k _ set -> Set.add k set) map Set.empty

  let data t = List.map snd (bindings t)

  let of_set f set = Set.fold (fun e map -> add e (f e) map) set empty

  let transpose_keys_and_data map = fold (fun k v m -> add v k m) map empty
  let transpose_keys_and_data_set map =
    fold (fun k v m ->
        let set =
          match find v m with
          | exception Not_found ->
            Set.singleton k
          | set ->
            Set.add k set
        in
        add v set m)
      map empty

  let diff_domains t1 t2 =
    merge (fun _key datum1 datum2 ->
        match datum1, datum2 with
        | None, None -> None
        | Some datum1, None -> Some datum1
        | None, Some _datum2 -> None
        | Some _datum1, Some _datum2 -> None)
      t1 t2

  let fold2_stop_on_key_mismatch f t1 t2 init =
    (* CR mshinwell: Provide a proper implementation *)
    if cardinal t1 <> cardinal t2 then None
    else
      let t1 = bindings t1 in
      let t2 = bindings t2 in
      List.fold_left2 (fun acc (key1, datum1) (key2, datum2) ->
          match acc with
          | None -> None
          | Some acc ->
             if T.compare key1 key2 <> 0 then None
             else Some (f key1 datum1 datum2 acc))
        (Some init) t1 t2
end

module Make_set (T : Thing) = struct
  include Set.Make (T)

  let output oc s =
    Printf.fprintf oc " ( ";
    iter (fun v -> Printf.fprintf oc "%a " T.output v) s;
    Printf.fprintf oc ")"

  let print ppf s =
    let elts ppf s = iter (fun e -> Format.fprintf ppf "@ %a" T.print e) s in
    Format.fprintf ppf "@[<1>{@[%a@ @]}@]" elts s

  let to_string s = Format.asprintf "%a" print s

  let of_list l = match l with
    | [] -> empty
    | [t] -> singleton t
    | t :: q -> List.fold_left (fun acc e -> add e acc) (singleton t) q

  let map f s = of_list (List.map f (elements s))

  let get_singleton t =
    match elements t with
    | [elt] -> Some elt
    | _ -> None
end

module Make_tbl (T : Thing) (Map : Map with module T := T) = struct
  include Hashtbl.Make (T)

  module Map = Map

  let to_list t =
    fold (fun key datum elts -> (key, datum)::elts) t []

  let of_list elts =
    let t = create 42 in
    List.iter (fun (key, datum) -> add t key datum) elts;
    t

  let to_map v = fold Map.add v Map.empty

  let of_map m =
    let t = create (Map.cardinal m) in
    Map.iter (fun k v -> add t k v) m;
    t

  let memoize t f = fun key ->
    try find t key with
    | Not_found ->
      let r = f key in
      add t key r;
      r

  let map t f =
    of_map (Map.map f (to_map t))
end

module type S = sig
  type t

  module T : Thing with type t = t
  include Thing with type t := T.t

  module Set : Set with module T := T
  module Map : Map with module T := T with module Set = Set
  module Tbl : Tbl with module T := T with module Map = Map
end

module Make (T : Thing) = struct
  module T = T
  include T

  module Set = Make_set (T)
  module Map = Make_map (T) (Set)
  module Tbl = Make_tbl (T) (Map)
end

module Make_pair (T1 : S) (T2 : S) = struct
  module Pair = Pair (T1.T) (T2.T)

  include Make (Pair)

  let create_from_cross_product t1_set t2_set =
    T1.Set.fold (fun t1 result ->
        T2.Set.fold (fun t2 result ->
            Set.add (t1, t2) result)
          t2_set
          result)
      t1_set
      Set.empty
end
