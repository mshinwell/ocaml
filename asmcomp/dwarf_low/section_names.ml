(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Mark Shinwell, Jane Street Europe                   *)
(*                                                                     *)
(*  Copyright 2013--2014, Jane Street Holding                          *)
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

type t = {
  name : string;
  starting_label : Linearize.label;
}

let create ~name =
  { name;
    starting_label = Linearize.new_label ();
  }

let name t = t.name
let starting_label t = t.starting_label

let debug_abbrev = create ~name:"debug_abbrev"
let debug_line = create ~name:"debug_line"
let debug_loc = create ~name:"debug_loc"
let debug_info = create ~name:"debug_info"
let debug_pubnames = create ~name:"debug_pubnames"
let debug_aranges = create ~name:"debug_aranges"
let debug_str = create ~name:"debug_str"

let all = [
 debug_abbrev;
 debug_line;
 debug_loc;
 debug_info;
 debug_pubnames;
 debug_aranges;
 debug_str;
]
