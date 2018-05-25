(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type key
    type +'a t
    val empty: 'a t
    val is_empty: 'a t -> bool
    val mem:  key -> 'a t -> bool
    val add: key -> 'a -> 'a t -> 'a t
    val update: key -> ('a option -> 'a option) -> 'a t -> 'a t
    val singleton: key -> 'a -> 'a t
    val remove: key -> 'a t -> 'a t
    val merge:
          (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val union: (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
    val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal: ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val for_all: (key -> 'a -> bool) -> 'a t -> bool
    val exists: (key -> 'a -> bool) -> 'a t -> bool
    val filter: (key -> 'a -> bool) -> 'a t -> 'a t
    val partition: (key -> 'a -> bool) -> 'a t -> 'a t * 'a t
    val cardinal: 'a t -> int
    val bindings: 'a t -> (key * 'a) list
    val min_binding: 'a t -> (key * 'a)
    val min_binding_opt: 'a t -> (key * 'a) option
    val max_binding: 'a t -> (key * 'a)
    val max_binding_opt: 'a t -> (key * 'a) option
    val choose: 'a t -> (key * 'a)
    val choose_opt: 'a t -> (key * 'a) option
    val split: key -> 'a t -> 'a t * 'a option * 'a t
    val find: key -> 'a t -> 'a
    val find_opt: key -> 'a t -> 'a option
    val find_first: (key -> bool) -> 'a t -> key * 'a
    val find_first_opt: (key -> bool) -> 'a t -> (key * 'a) option
    val find_last: (key -> bool) -> 'a t -> key * 'a
    val find_last_opt: (key -> bool) -> 'a t -> (key * 'a) option
    val map: ('a -> 'b) -> 'a t -> 'b t
    val map_sharing: ('a -> 'a) -> 'a t -> 'a t
    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t
    val filter_map : (key -> 'a -> 'b option) -> 'a t -> 'b t
    val of_list : (key * 'a) list -> 'a t
    val diff : 'a t -> 'a t -> 'a t
    val union_right : 'a t -> 'a t -> 'a t
    val union_left : 'a t -> 'a t -> 'a t
    val union_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val union_both
       : in_left_only:('a -> 'a)
      -> in_right_only:('a -> 'a)
      -> in_both:('a -> 'a -> 'a)
      -> 'a t
      -> 'a t
      -> 'a t
    val for_all2_opt : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool option
    val inter : ('a -> 'a -> 'b option) -> 'a t -> 'a t -> 'b t
    val inter_merge : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val rename : key t -> key -> key
    val map_keys : (key -> key) -> 'a t -> 'a t
    val data : 'a t -> 'a list
    val get_singleton : 'a t -> (key * 'a) option
    val transpose_keys_and_data : key t -> key t
    val to_seq : 'a t -> (key * 'a) Seq.t
    val to_seq_from : key -> 'a t -> (key * 'a) Seq.t
    val add_seq : (key * 'a) Seq.t -> 'a t -> 'a t
    val of_seq : (key * 'a) Seq.t -> 'a t
  end

module Make(Ord: OrderedType) = struct

    type key = Ord.t

    type 'a t =
        Empty
      | Node of {l:'a t; v:key; d:'a; r:'a t; h:int}

    let height = function
        Empty -> 0
      | Node {h} -> h

    let create l x d r =
      let hl = height l and hr = height r in
      Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    let singleton x d = Node{l=Empty; v=x; d; r=Empty; h=1}

    let bal l x d r =
      let hl = match l with Empty -> 0 | Node {h} -> h in
      let hr = match r with Empty -> 0 | Node {h} -> h in
      if hl > hr + 2 then begin
        match l with
          Empty -> invalid_arg "Map.bal"
        | Node{l=ll; v=lv; d=ld; r=lr} ->
            if height ll >= height lr then
              create ll lv ld (create lr x d r)
            else begin
              match lr with
                Empty -> invalid_arg "Map.bal"
              | Node{l=lrl; v=lrv; d=lrd; r=lrr}->
                  create (create ll lv ld lrl) lrv lrd (create lrr x d r)
            end
      end else if hr > hl + 2 then begin
        match r with
          Empty -> invalid_arg "Map.bal"
        | Node{l=rl; v=rv; d=rd; r=rr} ->
            if height rr >= height rl then
              create (create l x d rl) rv rd rr
            else begin
              match rl with
                Empty -> invalid_arg "Map.bal"
              | Node{l=rll; v=rlv; d=rld; r=rlr} ->
                  create (create l x d rll) rlv rld (create rlr rv rd rr)
            end
      end else
        Node{l; v=x; d; r; h=(if hl >= hr then hl + 1 else hr + 1)}

    let empty = Empty

    let is_empty = function Empty -> true | _ -> false

    let rec add x data = function
        Empty ->
          Node{l=Empty; v=x; d=data; r=Empty; h=1}
      | Node {l; v; d; r; h} as m ->
          let c = Ord.compare x v in
          if c = 0 then
            if d == data then m else Node{l; v=x; d=data; r; h}
          else if c < 0 then
            let ll = add x data l in
            if l == ll then m else bal ll v d r
          else
            let rr = add x data r in
            if r == rr then m else bal l v d rr

    let rec find x = function
        Empty ->
          raise Not_found
      | Node {l; v; d; r} ->
          let c = Ord.compare x v in
          if c = 0 then d
          else find x (if c < 0 then l else r)

    let rec find_first_aux v0 d0 f = function
        Empty ->
          (v0, d0)
      | Node {l; v; d; r} ->
          if f v then
            find_first_aux v d f l
          else
            find_first_aux v0 d0 f r

    let rec find_first f = function
        Empty ->
          raise Not_found
      | Node {l; v; d; r} ->
          if f v then
            find_first_aux v d f l
          else
            find_first f r

    let rec find_first_opt_aux v0 d0 f = function
        Empty ->
          Some (v0, d0)
      | Node {l; v; d; r} ->
          if f v then
            find_first_opt_aux v d f l
          else
            find_first_opt_aux v0 d0 f r

    let rec find_first_opt f = function
        Empty ->
          None
      | Node {l; v; d; r} ->
          if f v then
            find_first_opt_aux v d f l
          else
            find_first_opt f r

    let rec find_last_aux v0 d0 f = function
        Empty ->
          (v0, d0)
      | Node {l; v; d; r} ->
          if f v then
            find_last_aux v d f r
          else
            find_last_aux v0 d0 f l

    let rec find_last f = function
        Empty ->
          raise Not_found
      | Node {l; v; d; r} ->
          if f v then
            find_last_aux v d f r
          else
            find_last f l

    let rec find_last_opt_aux v0 d0 f = function
        Empty ->
          Some (v0, d0)
      | Node {l; v; d; r} ->
          if f v then
            find_last_opt_aux v d f r
          else
            find_last_opt_aux v0 d0 f l

    let rec find_last_opt f = function
        Empty ->
          None
      | Node {l; v; d; r} ->
          if f v then
            find_last_opt_aux v d f r
          else
            find_last_opt f l

    let rec find_opt x = function
        Empty ->
          None
      | Node {l; v; d; r} ->
          let c = Ord.compare x v in
          if c = 0 then Some d
          else find_opt x (if c < 0 then l else r)

    let rec mem x = function
        Empty ->
          false
      | Node {l; v; r} ->
          let c = Ord.compare x v in
          c = 0 || mem x (if c < 0 then l else r)

    let rec min_binding = function
        Empty -> raise Not_found
      | Node {l=Empty; v; d} -> (v, d)
      | Node {l} -> min_binding l

    let rec min_binding_opt = function
        Empty -> None
      | Node {l=Empty; v; d} -> Some (v, d)
      | Node {l}-> min_binding_opt l

    let rec max_binding = function
        Empty -> raise Not_found
      | Node {v; d; r=Empty} -> (v, d)
      | Node {r} -> max_binding r

    let rec max_binding_opt = function
        Empty -> None
      | Node {v; d; r=Empty} -> Some (v, d)
      | Node {r} -> max_binding_opt r

    let rec remove_min_binding = function
        Empty -> invalid_arg "Map.remove_min_elt"
      | Node {l=Empty; r} -> r
      | Node {l; v; d; r} -> bal (remove_min_binding l) v d r

    let merge t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          bal t1 x d (remove_min_binding t2)

    let rec remove x = function
        Empty ->
          Empty
      | (Node {l; v; d; r} as m) ->
          let c = Ord.compare x v in
          if c = 0 then merge l r
          else if c < 0 then
            let ll = remove x l in if l == ll then m else bal ll v d r
          else
            let rr = remove x r in if r == rr then m else bal l v d rr

    let rec update x f = function
        Empty ->
          begin match f None with
          | None -> Empty
          | Some data -> Node{l=Empty; v=x; d=data; r=Empty; h=1}
          end
      | Node {l; v; d; r; h} as m ->
          let c = Ord.compare x v in
          if c = 0 then begin
            match f (Some d) with
            | None -> merge l r
            | Some data ->
                if d == data then m else Node{l; v=x; d=data; r; h}
          end else if c < 0 then
            let ll = update x f l in
            if l == ll then m else bal ll v d r
          else
            let rr = update x f r in
            if r == rr then m else bal l v d rr

    let rec iter f = function
        Empty -> ()
      | Node {l; v; d; r} ->
          iter f l; f v d; iter f r

    let rec map f node =
      match node with
      | Empty ->
          Empty
      | Node {l; v; d; r; h} ->
          let l' = map f l in
          let d' = f d in
          let r' = map f r in
          Node{l=l'; v; d=d'; r=r'; h}

    let rec map_sharing f node =
      match node with
      | Empty ->
          Empty
      | Node {l; v; d; r; h} ->
          let l' = map_sharing f l in
          let d' = f d in
          let r' = map_sharing f r in
          if l == l' && d == d' && r == r' then node
          else Node{l=l'; v; d=d'; r=r'; h}

    let rec mapi f node =
      match node with
      | Empty ->
          Empty
      | Node {l; v; d; r; h} ->
          let l' = mapi f l in
          let d' = f v d in
          let r' = mapi f r in
          Node{l=l'; v; d=d'; r=r'; h}

    let rec fold f m accu =
      match m with
        Empty -> accu
      | Node {l; v; d; r} ->
          fold f r (f v d (fold f l accu))

    let rec for_all p = function
        Empty -> true
      | Node {l; v; d; r} -> p v d && for_all p l && for_all p r

    let rec exists p = function
        Empty -> false
      | Node {l; v; d; r} -> p v d || exists p l || exists p r

    (* Beware: those two functions assume that the added k is *strictly*
       smaller (or bigger) than all the present keys in the tree; it
       does not test for equality with the current min (or max) key.

       Indeed, they are only used during the "join" operation which
       respects this precondition.
    *)

    let rec add_min_binding k x = function
      | Empty -> singleton k x
      | Node {l; v; d; r} ->
        bal (add_min_binding k x l) v d r

    let rec add_max_binding k x = function
      | Empty -> singleton k x
      | Node {l; v; d; r} ->
        bal l v d (add_max_binding k x r)

    (* Same as create and bal, but no assumptions are made on the
       relative heights of l and r. *)

    let rec join l v d r =
      match (l, r) with
        (Empty, _) -> add_min_binding v d r
      | (_, Empty) -> add_max_binding v d l
      | (Node{l=ll; v=lv; d=ld; r=lr; h=lh}, Node{l=rl; v=rv; d=rd; r=rr; h=rh}) ->
          if lh > rh + 2 then bal ll lv ld (join lr v d r) else
          if rh > lh + 2 then bal (join l v d rl) rv rd rr else
          create l v d r

    (* Merge two trees l and r into one.
       All elements of l must precede the elements of r.
       No assumption on the heights of l and r. *)

    let concat t1 t2 =
      match (t1, t2) with
        (Empty, t) -> t
      | (t, Empty) -> t
      | (_, _) ->
          let (x, d) = min_binding t2 in
          join t1 x d (remove_min_binding t2)

    let concat_or_join t1 v d t2 =
      match d with
      | Some d -> join t1 v d t2
      | None -> concat t1 t2

    let rec split x = function
        Empty ->
          (Empty, None, Empty)
      | Node {l; v; d; r} ->
          let c = Ord.compare x v in
          if c = 0 then (l, Some d, r)
          else if c < 0 then
            let (ll, pres, rl) = split x l in (ll, pres, join rl v d r)
          else
            let (lr, pres, rr) = split x r in (join l v d lr, pres, rr)

    let rec merge f s1 s2 =
      match (s1, s2) with
        (Empty, Empty) -> Empty
      | (Node {l=l1; v=v1; d=d1; r=r1; h=h1}, _) when h1 >= height s2 ->
          let (l2, d2, r2) = split v1 s2 in
          concat_or_join (merge f l1 l2) v1 (f v1 (Some d1) d2) (merge f r1 r2)
      | (_, Node {l=l2; v=v2; d=d2; r=r2}) ->
          let (l1, d1, r1) = split v2 s1 in
          concat_or_join (merge f l1 l2) v2 (f v2 d1 (Some d2)) (merge f r1 r2)
      | _ ->
          assert false

    let rec union f s1 s2 =
      match (s1, s2) with
      | (Empty, s) | (s, Empty) -> s
      | (Node {l=l1; v=v1; d=d1; r=r1; h=h1}, Node {l=l2; v=v2; d=d2; r=r2; h=h2}) ->
          if h1 >= h2 then
            let (l2, d2, r2) = split v1 s2 in
            let l = union f l1 l2 and r = union f r1 r2 in
            match d2 with
            | None -> join l v1 d1 r
            | Some d2 -> concat_or_join l v1 (f v1 d1 d2) r
          else
            let (l1, d1, r1) = split v2 s1 in
            let l = union f l1 l2 and r = union f r1 r2 in
            match d1 with
            | None -> join l v2 d2 r
            | Some d1 -> concat_or_join l v2 (f v2 d1 d2) r

    let rec filter p = function
        Empty -> Empty
      | Node {l; v; d; r} as m ->
          (* call [p] in the expected left-to-right order *)
          let l' = filter p l in
          let pvd = p v d in
          let r' = filter p r in
          if pvd then if l==l' && r==r' then m else join l' v d r'
          else concat l' r'

    let rec partition p = function
        Empty -> (Empty, Empty)
      | Node {l; v; d; r} ->
          (* call [p] in the expected left-to-right order *)
          let (lt, lf) = partition p l in
          let pvd = p v d in
          let (rt, rf) = partition p r in
          if pvd
          then (join lt v d rt, concat lf rf)
          else (concat lt rt, join lf v d rf)

    type 'a enumeration = End | More of key * 'a * 'a t * 'a enumeration

    let rec cons_enum m e =
      match m with
        Empty -> e
      | Node {l; v; d; r} -> cons_enum l (More(v, d, r, e))

    let compare cmp m1 m2 =
      let rec compare_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> 0
        | (End, _)  -> -1
        | (_, End) -> 1
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            let c = Ord.compare v1 v2 in
            if c <> 0 then c else
            let c = cmp d1 d2 in
            if c <> 0 then c else
            compare_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in compare_aux (cons_enum m1 End) (cons_enum m2 End)

    let equal cmp m1 m2 =
      let rec equal_aux e1 e2 =
          match (e1, e2) with
          (End, End) -> true
        | (End, _)  -> false
        | (_, End) -> false
        | (More(v1, d1, r1, e1), More(v2, d2, r2, e2)) ->
            Ord.compare v1 v2 = 0 && cmp d1 d2 &&
            equal_aux (cons_enum r1 e1) (cons_enum r2 e2)
      in equal_aux (cons_enum m1 End) (cons_enum m2 End)

    let rec cardinal = function
        Empty -> 0
      | Node {l; r} -> cardinal l + 1 + cardinal r

    let rec bindings_aux accu = function
        Empty -> accu
      | Node {l; v; d; r} -> bindings_aux ((v, d) :: bindings_aux accu r) l

    let bindings s =
      bindings_aux [] s

    let choose = min_binding

    let choose_opt = min_binding_opt

    let diff t1 t2 =
      filter (fun key _ -> not (mem key t2)) t1

    let filter_map f t =
      fold (fun id v map ->
          match f id v with
          | None -> map
          | Some r -> add id r map) t empty

    let of_list l =
      List.fold_left (fun map (id, v) -> add id v map) empty l

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

    let union_both ~in_left_only ~in_right_only ~in_both m1 m2 =
      let aux _ m1 m2 =
        match m1, m2 with
        | None, None -> None
        | None, Some m -> Some (in_right_only m)
        | Some m, None -> Some (in_left_only m)
        | Some m1, Some m2 -> Some (in_both m1 m2)
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

    let rename m v =
      try find v m
      with Not_found -> v

    let map_keys f m =
      of_list (List.map (fun (k, v) -> f k, v) (bindings m))

    let data t = List.map snd (bindings t)

    let transpose_keys_and_data map = fold (fun k v m -> add v k m) map empty

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
              Ord.compare key1 key2 = 0 && f datum1 datum2)
            t1 t2
        in
        Some for_all2

    let add_seq i m =
      Seq.fold_left (fun m (k,v) -> add k v m) m i

    let of_seq i = add_seq i empty

    let rec seq_of_enum_ c () = match c with
      | End -> Seq.Nil
      | More (k,v,t,rest) -> Seq.Cons ((k,v), seq_of_enum_ (cons_enum t rest))

    let to_seq m =
      seq_of_enum_ (cons_enum m End)

    let to_seq_from low m =
      let rec aux low m c = match m with
        | Empty -> c
        | Node {l; v; d; r; _} ->
            begin match Ord.compare v low with
              | 0 -> More (v, d, r, c)
              | n when n<0 -> aux low r c
              | _ -> aux low l (More (v, d, r, c))
            end
      in
      seq_of_enum_ (aux low m End)
end

module type S_printable = sig
  include S

  val print
     : ?before_key:string
    -> ?after_key:string
    -> (Format.formatter -> 'a -> unit)
    -> Format.formatter
    -> 'a t
    -> unit

  (** [disjoint_union m1 m2] contains all bindings from [m1] and
      [m2]. If some binding is present in both and the associated
      value is not equal, an exception is raised. *)
  val disjoint_union
     : ?eq:('a -> 'a -> bool)
    -> ?print:(Format.formatter -> 'a -> unit)
    -> 'a t
    -> 'a t
    -> 'a t

  module Set_of_keys : Set.S_printable with type elt = key

  val keys : 'a t -> Set_of_keys.t
  val of_set : (key -> 'a) -> Set_of_keys.t -> 'a t
  val transpose_keys_and_data_set : key t -> Set_of_keys.t t
end

module Make_printable (T : sig
  include OrderedType
  val print : Format.formatter -> t -> unit
end) : S_printable with type key = T.t = struct
  include Make (T)

  let print ?(before_key = "") ?(after_key = "") f ppf t =
    let print_binding ppf (id, v) =
      Format.fprintf ppf "@[<hov 1>(%s%a%s@ %a)@]"
        before_key
        T.print id
        after_key
        f v
    in
    let bindings = bindings t in
    match bindings with
    | [] -> Format.fprintf ppf "()"
    | _ ->
      Format.fprintf ppf "@[<hov 1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          print_binding) bindings

  let disjoint_union ?eq ?print m1 m2 =
    union (fun id v1 v2 ->
        let ok =
          match eq with
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
          failwith err
        else Some v1)
      m1 m2

  module Set_of_keys = Set.Make_printable (T)

  let keys t =
    fold (fun k _ set -> Set_of_keys.add k set) t Set_of_keys.empty

  let of_set f set =
    Set_of_keys.fold (fun e map -> add e (f e) map) set empty

  let transpose_keys_and_data_set map =
    fold (fun k v m ->
        let set =
          match find v m with
          | exception Not_found -> Set_of_keys.singleton k
          | set -> Set_of_keys.add k set
        in
        add v set m)
      map empty
end

module type With_set_arg = sig
  include OrderedType
  val print : Format.formatter -> t -> unit
end

module type With_set = sig
  type t

  include OrderedType with type t := t

  val print : Format.formatter -> t -> unit

  module T : With_set_arg with type t = t

  module Set : Set.S_printable with type elt = t

  module Map : S_printable
    with type key = t
    with module Set_of_keys = Set
end

module Make_with_set (T : With_set_arg) = struct
  module T = T

  let compare = T.compare
  let print = T.print

(*  module Set = Set.Make_printable (T)*)
  module Map = Make_printable (T)
  module Set = Map.Set_of_keys
end
