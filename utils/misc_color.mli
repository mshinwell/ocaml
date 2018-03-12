(* Color handling *)

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

val ansi_of_style_l : style list -> string
(* ANSI escape sequence for the given style *)

type styles = {
  error: style list;
  warning: style list;
  loc: style list;
}

val default_styles: styles
val get_styles: unit -> styles
val set_styles: styles -> unit

type setting = Auto | Always | Never

val setup : setting option -> unit
(* [setup opt] will enable or disable color handling on standard formatters
   according to the value of color setting [opt].
   Only the first call to this function has an effect. *)

val set_color_tag_handling : Format.formatter -> unit
(* adds functions to support color tags to the given formatter. *)

val bold_green : unit -> string
val bold_red : unit -> string
val bold_cyan : unit -> string
val bold_white : unit -> string
val bold_yellow : unit -> string
val bold_blue : unit -> string

val reset : unit -> string
