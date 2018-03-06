(* Color handling *)

(* use ANSI color codes, see https://en.wikipedia.org/wiki/ANSI_escape_code *)
type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
;;

type style =
  | FG of color (* foreground *)
  | BG of color (* background *)
  | Bold
  | Reset

let ansi_of_color = function
  | Black -> "0"
  | Red -> "1"
  | Green -> "2"
  | Yellow -> "3"
  | Blue -> "4"
  | Magenta -> "5"
  | Cyan -> "6"
  | White -> "7"

let code_of_style = function
  | FG c -> "3" ^ ansi_of_color c
  | BG c -> "4" ^ ansi_of_color c
  | Bold -> "1"
  | Reset -> "0"

let ansi_of_style_l l =
  let s = match l with
    | [] -> code_of_style Reset
    | [s] -> code_of_style s
    | _ -> String.concat ";" (List.map code_of_style l)
  in
  "\x1b[" ^ s ^ "m"

type styles = {
  error: style list;
  warning: style list;
  loc: style list;
}

let default_styles = {
  warning = [Bold; FG Magenta];
  error = [Bold; FG Red];
  loc = [Bold];
}

let cur_styles = ref default_styles
let get_styles () = !cur_styles
let set_styles s = cur_styles := s

(* map a tag to a style, if the tag is known.
   @raise Not_found otherwise *)
let style_of_tag s = match s with
  | "error" -> (!cur_styles).error
  | "warning" -> (!cur_styles).warning
  | "loc" -> (!cur_styles).loc
  | _ -> raise Not_found

let color_enabled = ref true

(* either prints the tag of [s] or delegates to [or_else] *)
let mark_open_tag ~or_else s =
  try
    let style = style_of_tag s in
    if !color_enabled then ansi_of_style_l style else ""
  with Not_found -> or_else s

let mark_close_tag ~or_else s =
  try
    let _ = style_of_tag s in
    if !color_enabled then ansi_of_style_l [Reset] else ""
  with Not_found -> or_else s

(* add color handling to formatter [ppf] *)
let set_color_tag_handling ppf =
  let open Format in
  let functions = pp_get_formatter_tag_functions ppf () in
  let functions' = {functions with
    mark_open_tag=(mark_open_tag ~or_else:functions.mark_open_tag);
    mark_close_tag=(mark_close_tag ~or_else:functions.mark_close_tag);
  } in
  pp_set_mark_tags ppf true; (* enable tags *)
  pp_set_formatter_tag_functions ppf functions';
  (* also setup margins *)
  pp_set_margin ppf (pp_get_margin std_formatter());
  ()

external isatty : out_channel -> bool = "caml_sys_isatty"

(* reasonable heuristic on whether colors should be enabled *)
let should_enable_color () =
  let term = try Sys.getenv "TERM" with Not_found -> "" in
  term <> "dumb"
  && term <> ""
  && isatty stderr

type setting = Auto | Always | Never

let setup =
  let first = ref true in (* initialize only once *)
  let formatter_l =
    [Format.std_formatter; Format.err_formatter; Format.str_formatter]
  in
  fun o ->
    if !first then (
      first := false;
      Format.set_mark_tags true;
      List.iter set_color_tag_handling formatter_l;
      color_enabled := (match o with
          | Some Always -> true
          | Some Auto -> should_enable_color ()
          | Some Never -> false
          | None -> should_enable_color ())
    );
    ()

let enable = ref true  (* CR mshinwell: fixme *)

let bold_red () =
  if !enable then ansi_of_style_l [FG Red; Bold] else ""

let bold_green () =
  if !enable then ansi_of_style_l [FG Green; Bold] else ""

let reset () =
  if !enable then ansi_of_style_l [Reset] else ""
