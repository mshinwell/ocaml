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
external raise : exn -> 'a = "%raise"
external raise_notrace : exn -> 'a = "%raise_notrace"
external ( |> ) : 'a -> ('a -> 'b) -> 'b = "%revapply"
external ( @@ ) : ('a -> 'b) -> 'a -> 'b = "%apply"
external __LOC__ : string = "%loc_LOC"
external __FILE__ : string = "%loc_FILE"
external __LINE__ : int = "%loc_LINE"
external __MODULE__ : string = "%loc_MODULE"
external __POS__ : string * int * int * int = "%loc_POS"
external __LOC_OF__ : 'a -> string * 'a = "%loc_LOC"
external __LINE_OF__ : 'a -> int * 'a = "%loc_LINE"
external __POS_OF__ : 'a -> (string * int * int * int) * 'a = "%loc_POS"
external ( = ) : 'a -> 'a -> bool = "%equal"
external ( <> ) : 'a -> 'a -> bool = "%notequal"
external ( < ) : 'a -> 'a -> bool = "%lessthan"
external ( > ) : 'a -> 'a -> bool = "%greaterthan"
external ( <= ) : 'a -> 'a -> bool = "%lessequal"
external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
external compare : 'a -> 'a -> int = "%compare"
external ( == ) : 'a -> 'a -> bool = "%eq"
external ( != ) : 'a -> 'a -> bool = "%noteq"
external not : bool -> bool = "%boolnot"
external ( & ) : bool -> bool -> bool = "%sequand"
external ( && ) : bool -> bool -> bool = "%sequand"
external ( or ) : bool -> bool -> bool = "%sequor"
external ( || ) : bool -> bool -> bool = "%sequor"
external ( ~- ) : int -> int = "%negint"
external ( ~+ ) : int -> int = "%identity"
external succ : int -> int = "%succint"
external pred : int -> int = "%predint"
external ( + ) : int -> int -> int = "%addint"
external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"
external ( / ) : int -> int -> int = "%divint"
external ( mod ) : int -> int -> int = "%modint"
external ( land ) : int -> int -> int = "%andint"
external ( lor ) : int -> int -> int = "%orint"
external ( lxor ) : int -> int -> int = "%xorint"
external ( lsl ) : int -> int -> int = "%lslint"
external ( lsr ) : int -> int -> int = "%lsrint"
external ( asr ) : int -> int -> int = "%asrint"
external ( ~-. ) : float -> float = "%negfloat"
external ( ~+. ) : float -> float = "%identity"
external ( +. ) : float -> float -> float = "%addfloat"
external ( -. ) : float -> float -> float = "%subfloat"
external ( *. ) : float -> float -> float = "%mulfloat"
external ( /. ) : float -> float -> float = "%divfloat"
external ( ** ) : float -> float -> float = "caml_power_float" "pow"
external exp : float -> float = "caml_exp_float" "exp" [@@unboxed] [@@noalloc]
external expm1 : float -> float = "caml_expm1_float" "caml_expm1"
external acos : float -> float = "caml_acos_float" "acos"
external asin : float -> float = "caml_asin_float" "asin"
external atan : float -> float = "caml_atan_float" "atan"
external atan2 : float -> float -> float = "caml_atan2_float" "atan2"
external cos : float -> float = "caml_cos_float" "cos" [@@unboxed] [@@noalloc]
external cosh : float -> float = "caml_cosh_float" "cosh"
external log : float -> float = "caml_log_float" "log" [@@unboxed] [@@noalloc]
external log10 : float -> float = "caml_log10_float" "log10"
external log1p : float -> float = "caml_log1p_float" "caml_log1p"
external sin : float -> float = "caml_sin_float" "sin" [@@unboxed] [@@noalloc]
external sinh : float -> float = "caml_sinh_float" "sinh"
external sqrt : float -> float = "caml_sqrt_float" "sqrt"
external tan : float -> float = "caml_tan_float" "tan" [@@unboxed] [@@noalloc]
external tanh : float -> float = "caml_tanh_float" "tanh"
external ceil : float -> float = "caml_ceil_float" "ceil"
external floor : float -> float = "caml_floor_float" "floor"
external abs_float : float -> float = "%absfloat"
external mod_float : float -> float -> float = "caml_fmod_float" "fmod"
external frexp : float -> float * int = "caml_frexp_float"
external modf : float -> float * float = "caml_modf_float"
external float : int -> float = "%floatofint"
external float_of_int : int -> float = "%floatofint"
external truncate : float -> int = "%intoffloat"
external int_of_float : float -> int = "%intoffloat"
external string_length : string -> int = "%string_length"
external bytes_length : bytes -> int = "%bytes_length"
external bytes_create : int -> bytes = "caml_create_bytes"
external bytes_unsafe_to_string : bytes -> string = "%bytes_to_string"
external int_of_char : char -> int = "%identity"
external unsafe_char_of_int : int -> char = "%identity"
external ignore : 'a -> unit = "%ignore"
external fst : 'a * 'b -> 'a = "%field0"
external snd : 'a * 'b -> 'b = "%field1"
type 'a ref = { mutable contents : 'a; }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
external incr : int ref -> unit = "%incr"
external decr : int ref -> unit = "%decr"
external format_int : string -> int -> string = "caml_format_int"
external format_float : string -> float -> string = "caml_format_float"
external int_of_string : string -> int = "caml_int_of_string"
external string_get : string -> int -> char = "%string_safe_get"
external float_of_string : string -> float = "caml_float_of_string"
external sys_exit : int -> 'a = "caml_sys_exit"

let failwith s = raise(Failure s)
let invalid_arg s = raise(Invalid_argument s)

let rec ( @ ) l1 l2 =
  match l1 with
    [] -> l2
  | hd :: tl -> hd :: (tl @ l2)

(* An alias for the type of lists. *)
type 'a t = 'a list = [] | (::) of 'a * 'a list

(* List operations *)

let rec length_aux len = function
    [] -> len
  | _::l -> length_aux (len + 1) l

let length l = length_aux 0 l

let cons a l = a::l

let hd = function
    [] -> failwith "hd"
  | a::_ -> a

let tl = function
    [] -> failwith "tl"
  | _::l -> l

let nth l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> failwith "nth"
    | a::l -> if n = 0 then a else nth_aux l (n-1)
  in nth_aux l n

let nth_opt l n =
  if n < 0 then invalid_arg "List.nth" else
  let rec nth_aux l n =
    match l with
    | [] -> None
    | a::l -> if n = 0 then Some a else nth_aux l (n-1)
  in nth_aux l n

let append = (@)

let rec rev_append l1 l2 =
  match l1 with
    [] -> l2
  | a :: l -> rev_append l (a :: l2)

let rev l = rev_append l []

let rec init_tailrec_aux acc i n f =
  if i >= n then acc
  else init_tailrec_aux (f i :: acc) (i+1) n f

let rec init_aux i n f =
  if i >= n then []
  else
    let r = f i in
    r :: init_aux (i+1) n f

let rev_init_threshold = 10000

let init len f =
  if len < 0 then invalid_arg "List.init" else
  if len > rev_init_threshold then rev (init_tailrec_aux [] 0 len f)
  else init_aux 0 len f

let rec flatten = function
    [] -> []
  | l::r -> l @ flatten r

let concat = flatten

let rec map f = function
    [] -> []
  | a::l -> let r = f a in r :: map f l

let rec mapi i f = function
    [] -> []
  | a::l -> let r = f i a in r :: mapi (i + 1) f l

let mapi f l = mapi 0 f l

let rev_map f l =
  let rec rmap_f accu = function
    | [] -> accu
    | a::l -> rmap_f (f a :: accu) l
  in
  rmap_f [] l


let rec iter f = function
    [] -> ()
  | a::l -> f a; iter f l

let rec iteri i f = function
    [] -> ()
  | a::l -> f i a; iteri (i + 1) f l

let iteri f l = iteri 0 f l

let rec fold_left f accu l =
  match l with
    [] -> accu
  | a::l -> fold_left f (f accu a) l

let rec fold_right f l accu =
  match l with
    [] -> accu
  | a::l -> f a (fold_right f l accu)

let rec map2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> []
  | (a1::l1, a2::l2) -> let r = f a1 a2 in r :: map2 f l1 l2
  | (_, _) -> invalid_arg "List.map2"

let rev_map2 f l1 l2 =
  let rec rmap2_f accu l1 l2 =
    match (l1, l2) with
    | ([], []) -> accu
    | (a1::l1, a2::l2) -> rmap2_f (f a1 a2 :: accu) l1 l2
    | (_, _) -> invalid_arg "List.rev_map2"
  in
  rmap2_f [] l1 l2


let rec iter2 f l1 l2 =
  match (l1, l2) with
    ([], []) -> ()
  | (a1::l1, a2::l2) -> f a1 a2; iter2 f l1 l2
  | (_, _) -> invalid_arg "List.iter2"

let rec fold_left2 f accu l1 l2 =
  match (l1, l2) with
    ([], []) -> accu
  | (a1::l1, a2::l2) -> fold_left2 f (f accu a1 a2) l1 l2
  | (_, _) -> invalid_arg "List.fold_left2"

let rec fold_right2 f l1 l2 accu =
  match (l1, l2) with
    ([], []) -> accu
  | (a1::l1, a2::l2) -> f a1 a2 (fold_right2 f l1 l2 accu)
  | (_, _) -> invalid_arg "List.fold_right2"

let rec for_all p = function
    [] -> true
  | a::l -> p a && for_all p l

let rec exists p = function
    [] -> false
  | a::l -> p a || exists p l

let rec for_all2 p l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | (a1::l1, a2::l2) -> p a1 a2 && for_all2 p l1 l2
  | (_, _) -> invalid_arg "List.for_all2"

let rec exists2 p l1 l2 =
  match (l1, l2) with
    ([], []) -> false
  | (a1::l1, a2::l2) -> p a1 a2 || exists2 p l1 l2
  | (_, _) -> invalid_arg "List.exists2"

let rec mem x = function
    [] -> false
  | a::l -> compare a x = 0 || mem x l

let rec memq x = function
    [] -> false
  | a::l -> a == x || memq x l

let rec assoc x = function
    [] -> raise Not_found
  | (a,b)::l -> if compare a x = 0 then b else assoc x l

let rec assoc_opt x = function
    [] -> None
  | (a,b)::l -> if compare a x = 0 then Some b else assoc_opt x l

let rec assq x = function
    [] -> raise Not_found
  | (a,b)::l -> if a == x then b else assq x l

let rec assq_opt x = function
    [] -> None
  | (a,b)::l -> if a == x then Some b else assq_opt x l

let rec mem_assoc x = function
  | [] -> false
  | (a, _) :: l -> compare a x = 0 || mem_assoc x l

let rec mem_assq x = function
  | [] -> false
  | (a, _) :: l -> a == x || mem_assq x l

let rec remove_assoc x = function
  | [] -> []
  | (a, _ as pair) :: l ->
      if compare a x = 0 then l else pair :: remove_assoc x l

let rec remove_assq x = function
  | [] -> []
  | (a, _ as pair) :: l -> if a == x then l else pair :: remove_assq x l

let rec find p = function
  | [] -> raise Not_found
  | x :: l -> if p x then x else find p l

let rec find_opt p = function
  | [] -> None
  | x :: l -> if p x then Some x else find_opt p l

let find_all p =
  let rec find accu = function
  | [] -> rev accu
  | x :: l -> if p x then find (x :: accu) l else find accu l in
  find []

let filter = find_all
