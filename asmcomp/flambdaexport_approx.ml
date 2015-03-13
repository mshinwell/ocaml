(***********************************************************************)
(*                                                                     *)
(*                               OCaml                                 *)
(*                                                                     *)
(*                 Pierre Chambart and Mark Shinwell                   *)
(*                                                                     *)
(*  Copyright 2014--2015, OCamlPro                                     *)
(*  Copyright 2015, Jane Street Group                                  *)
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

module Export_id = UnitId (Innerid) (Compilation_unit)

type tag = int

type _ boxed_int =
  | Int32 : int32 boxed_int
  | Int64 : int64 boxed_int
  | Nativeint : nativeint boxed_int

type descr =
  | Value_block of tag * approx array
  | Value_int of int
  | Value_constptr of int
  | Value_float of float
  | Value_boxed_int : 'a boxed_int * 'a -> descr
  | Value_string
  | Value_closure of descr_closure
  | Value_set_of_closures of descr_set_of_closures

and descr_closure = {
  closure_id : Closure_id.t;
  set_of_closures : descr_set_of_closures;
}

and descr_set_of_closures = {
  set_of_closures_id : Set_of_closures_id.t;
  bound_var : approx Var_within_closure.Map.t;
  results : approx Closure_id.Map.t;
}

type approx =
  | Value_unknown
  | Value_id of Export_id.t
  | Value_symbol of Symbol.t

let print_approx ppf export =
  let values = export.ex_values in
  let open Format in
  let printed = ref Export_id.Set.empty in
  let printed_closure = ref Set_of_closures_id.Set.empty in
  let rec print_approx ppf = function
    | Value_unknown -> fprintf ppf "?"
    | Value_id id ->
      if Export_id.Set.mem id !printed
      then fprintf ppf "(%a: _)" Export_id.print id
      else
        (try
           let descr = find_ex_value id values in
           printed := Export_id.Set.add id !printed;
           fprintf ppf "(%a: %a)"
             Export_id.print id
             print_descr descr
         with Not_found ->
           fprintf ppf "(%a: Not available)"
             Export_id.print id)
    | Value_symbol sym -> Symbol.print ppf sym
  and print_descr ppf = function
    | Value_int i -> pp_print_int ppf i
    | Value_constptr i -> fprintf ppf "%ip" i
    | Value_block (tag, fields) ->
      fprintf ppf "[%i:%a]" tag print_fields fields
    | Value_closure {closure_id; set_of_closures} ->
      fprintf ppf "(function %a, %a)"
        Closure_id.print closure_id print_set_of_closures set_of_closures
    | Value_set_of_closures set_of_closures ->
      fprintf ppf "(ufunction %a)" print_set_of_closures set_of_closures
    | Value_string -> Format.pp_print_string ppf "string"
    | Value_float f -> Format.pp_print_float ppf f
    | Value_boxed_int (t, i) ->
      match t with
      | Int32 -> Format.fprintf ppf "%li" i
      | Int64 -> Format.fprintf ppf "%Li" i
      | Nativeint -> Format.fprintf ppf "%ni" i
  and print_fields ppf fields =
    Array.iter (fun approx -> fprintf ppf "%a@ " print_approx approx) fields
  and print_set_of_closures ppf { set_of_closures_id; bound_var } =
    if Set_of_closures_id.Set.mem set_of_closures_id !printed_closure
    then fprintf ppf "%a" Set_of_closures_id.print set_of_closures_id
    else begin
      printed_closure :=
        Set_of_closures_id.Set.add set_of_closures_id !printed_closure;
      fprintf ppf "{%a: %a}"
        Set_of_closures_id.print set_of_closures_id
        print_binding bound_var
    end
  and print_binding ppf bound_var =
    Var_within_closure.Map.iter (fun clos_id approx ->
        fprintf ppf "%a -> %a,@ "
          Var_within_closure.print clos_id
          print_approx approx) bound_var
  in
  let print_approxs id approx =
    fprintf ppf "%a -> %a;@ " Ident.print id print_approx approx
  in
  Ident.Map.iter print_approxs export.globals

(* CR mshinwell: this should be improved.  Fix the code above. *)
let print_descr ppf = function
  | Value_int _ -> Format.fprintf ppf "Value_int"
  | Value_constptr _ -> Format.fprintf ppf "Value_constptr"
  | Value_block _ -> Format.fprintf ppf "Value_block"
  | Value_closure _ -> Format.fprintf ppf "Value_closure"
  | Value_set_of_closures _ -> Format.fprintf ppf "Value_set_of_closures"
  | Value_string -> Format.fprintf ppf "Value_string"
  | Value_float _ -> Format.fprintf ppf "Value_float"
  | Value_boxed_int _ -> Format.fprintf ppf "Value_boxed_int"

let print_symbols ppf export =
  let open Format in
  let print_symbol eid sym =
    fprintf ppf "%a -> %a@." Symbol.print sym Export_id.print eid
  in
    Compilation_unit.Map.iter (fun _ -> Export_id.Map.iter print_symbol)
      export.id_symbol

let print_all ppf export =
  let open Format in
  fprintf ppf "approxs@ %a@.@."
    print_approx export;
  fprintf ppf "id_symbol@ %a@.@."
    (Compilation_unit.Map.print (Export_id.Map.print Symbol.print))
      export.id_symbol;
  fprintf ppf "symbol_id@ %a@.@."
    (Symbol.Map.print Export_id.print) export.symbol_id;
  fprintf ppf "constants@ %a@.@."
    Symbol.Set.print export.constants;
  fprintf ppf "functions@ %a@.@."
    (Set_of_closures_id.Map.print Printflambda.function_declarations)
      export.functions
