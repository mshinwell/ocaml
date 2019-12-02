(* The MIT License

Copyright (c) 2016 Jane Street Group, LLC <opensource@janestreet.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. *)

module type Keys = sig
  type t

  type elt

  val empty : t

  val add : t -> elt -> t

  val mem : t -> elt -> bool
end

module type S = sig
  type key

  type 'a monad

  (** Returns [Error cycle] in case the graph is not a DAG *)
  val top_closure :
       key:('a -> key)
    -> deps:('a -> 'a list monad)
    -> 'a list
    -> ('a list, 'a list) result monad
end

module Make (Keys : Keys) (Monad : Monad.S) :
  S with type key := Keys.elt and type 'a monad := 'a Monad.t
[@@inlined always]
