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

module Dwarf_low = struct
  module Abbreviations_table = Abbreviations_table
  module Aranges_table = Aranges_table
  module Attribute_value = Attribute_value
  module Debug_info_section = Debug_info_section
  module Debug_loc_table = Debug_loc_table
  module Emitter = Emitter
  module Encoding_attribute = Encoding_attribute
  module Location_expression = Location_expression
  module Location_list = Location_list
  module Location_list_entry = Location_list_entry
  module Proto_DIE = Proto_die
  module Pubnames_table = Pubnames_table
  module Section_names = Section_names
  module Simple_location_description = Simple_location_expression
  module Single_location_description = Single_location_description
  module Tag = Tag
end
