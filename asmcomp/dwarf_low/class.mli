(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2015, Jane Street Holding                                *)
(*                                                                     *)
(*  Licensed under the Apache License, Version 2.0 (the "License");    *)
(*  you may not use this file except in compliance with the License.   *)
(*  You may obtain a copy of the License at                            *)
(*                                                                     *)
(*      http://www.apache.org/licenses/LICENSE-2.0                     *)
(*                                                                     *)
(*  Unless required by applicable law or agreed to in writing,         *)
(*  software distributed under the License is distributed on an        *)
(*  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,       *)
(*  either express or implied.  See the License for the specific       *)
(*  language governing permissions and limitations under the License.  *)
(*                                                                     *)
(***********************************************************************)

(* DWARF-4 specification section 7.5.4, page 147 *)

type address = [ `address ]
type address_or_constant = [ `address | `constant ]
type address_or_flag_or_reference_or_string =
  [ `address | `flag | `reference | `string ]
type block = [ `block ]
type block_or_constant_or_string = [ `block | `constant | `string ]
type constant = [ `constant ]
type constant_or_exprloc_or_loclistptr =
  [ `constant | `exprloc | `loclistptr ]
type constant_or_exprloc_or_reference =
  [ `constant | `exprloc | `reference ]
type constant_or_rangelistptr = [ `constant | `rangelistptr ]
type exprloc = [ `exprloc ]
type exprloc_or_loclistptr = [ `exprloc | `loclistptr ]
type flag = [ `flag ]
type lineptr = [ `lineptr ]
type lineptr_or_loclistptr_or_macptr_or_rangelistptr =
  [ `lineptr | `loclistptr | `macptr | `rangelistptr ]
type macptr = [ `macptr ]
type rangelistptr = [ `rangelistptr ]
type reference = [ `reference ]
type string = [ `string ]
