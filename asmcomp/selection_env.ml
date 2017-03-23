(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                     Mark Shinwell, Jane Street Europe                  *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Int = Numbers.Int

type t = {
  vars : (Reg.t array * Asttypes.mutable_flag) Ident.Map.t;
  simple_expressions : Cmm.expression Ident.Map.t;
  static_exceptions : Reg.t array list Int.Map.t;
  (* Which registers must be populated when jumping to the given
     handler. *)
}

let empty = {
  vars = Ident.Map.empty;
  simple_expressions = Ident.Map.empty;
  static_exceptions = Int.Map.empty;
}

let add t id v mut =
  { t with
    vars = Ident.Map.add id (v, mut) t.vars;
  }

let add_static_exception t id v =
  { t with
    static_exceptions = Int.Map.add id v t.static_exceptions;
  }

let add_simple_expression t id expr =
  { t with
    simple_expressions = Ident.Map.add id expr t.simple_expressions;
  }

let find t id =
  let regs, _mut = Ident.Map.find id t.vars in
  regs

let find_with_mutability t id =
  Ident.Map.find id t.vars

let find_static_exception t id =
  Int.Map.find id t.static_exceptions

let find_simple_expression t id =
  Ident.Map.find id t.simple_expressions

let mutability t id =
  let _regs, mut = Ident.Map.find id t.vars in
  mut
