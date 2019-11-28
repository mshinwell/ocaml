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

  val top_closure :
       key:('a -> key)
    -> deps:('a -> 'a list monad)
    -> 'a list
    -> ('a list, 'a list) result monad
end

module Make (Keys : Keys) (Monad : Monad.S) = struct
  open Monad

  let top_closure ~key ~deps elements =
    let visited = ref Keys.empty in
    let res = ref [] in
    let rec loop elt ~temporarily_marked =
      let key = key elt in
      if Keys.mem temporarily_marked key then
        return (Error [ elt ])
      else if not (Keys.mem !visited key) then (
        visited := Keys.add !visited key;
        let temporarily_marked = Keys.add temporarily_marked key in
        deps elt >>= iter_elts ~temporarily_marked >>= function
        | Ok () ->
          res := elt :: !res;
          return (Ok ())
        | Error l -> return (Error (elt :: l))
      ) else
        return (Ok ())
    and iter_elts elts ~temporarily_marked =
      return elts >>= function
      | [] -> return (Ok ())
      | elt :: elts -> (
        loop elt ~temporarily_marked >>= function
        | Error _ as result -> return result
        | Ok () -> iter_elts elts ~temporarily_marked )
    in
    iter_elts elements ~temporarily_marked:Keys.empty >>= function
    | Ok () -> return (Ok (List.rev !res))
    | Error elts -> return (Error elts)
end
[@@inlined always]
