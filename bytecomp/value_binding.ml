(* For recording information about value bindings in structures that is
   required for native code debugging information emission. *)

(* Call [id] the key in the table that maps to a value of type [t]. *)
type t = {
  ty : Types.type_expr;  (* the type of [id] *)
  module_id : Ident.t;  (* the symbol for access to the module block *)
  pos : int;  (* which field in the module block contains the value of [id] *)
  path : Path.t option;  (* the module path to the binding, if appropriate *)
  location : Location.t;  (* the location of the structure around the binding *)
}

let rec print_path = function
  | Path.Pident ident -> Ident.name ident
  | Path.Pdot (path, s, _) -> (print_path path) ^ "." ^ s
  | Path.Papply (path1, path2) ->
    "(" ^ (print_path path1) ^ ")(" ^ (print_path path2) ^ ")"

let to_string t =
  Printf.sprintf "module_id='%s' pos=%d path=%s file=%s line=%d\n"
    (Ident.unique_name t.module_id)
    t.pos
    (match t.path with None -> "<none>" | Some path -> print_path path)
    t.location.Location.loc_start.Lexing.pos_fname
    t.location.Location.loc_start.Lexing.pos_lnum
