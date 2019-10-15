module Lex = Flambda_lex
module Parser = Flambda_parser

type error =
  | Lexing_error of Lex.error * Loc.location
  | Parsing_error of Loc.location

let parse_program filename =
  let ic = open_in filename in
  try
    let pos = { Lexing.pos_fname = filename; pos_lnum = 1; pos_bol = 0; pos_cnum = 0 } in
    let lb = Lexing.from_channel ic in
    let lb = { lb with lex_start_p = pos; lex_curr_p = pos } in
    let program =
      try Ok (Parser.program Lex.token lb)
      with
      | Parser.Error ->
        let loc = Lexing.lexeme_start_p lb, Lexing.lexeme_end_p lb in
        Error (Parsing_error loc)
      | Lex.Error (error, loc) ->
        Error (Lexing_error (error, loc))
    in
    close_in ic;
    program
  with
  | e ->
    let x = Printexc.get_raw_backtrace () in
    close_in ic;
    Printexc.raise_with_backtrace e x

let check_invariants program =
  try Flambda_static.Program.invariant program
  with exn ->
    Format.eprintf "Program which failed invariant check:@ %a\n%!"
      Flambda_static.Program.print program;
    raise exn

let parse_ilambda ~backend file =
    match parse_program file with
    | Ok program ->
      Format.printf "%a@.@."
        Print_fexpr.program program;
      let il = Fexpr_to_ilambda.conv program in
      Format.printf "ilambda:@.%a@.@."
        Ilambda.print_program il.program;
      let fl2 =
        Closure_conversion.ilambda_to_flambda ~backend
          ~module_ident:il.module_ident
          ~size:il.size
          ~filename:"/tmp/toto"
          il.program
      in
      Format.printf "flambda:@.%a@.@."
        Flambda_static.Program.print fl2;
      check_invariants fl2;
      let fl2' = Simplify.run ~backend ~round:1 fl2 in
      Format.printf "simplify:@.%a@."
        Flambda_static.Program.print fl2';
      exit 0
    | Error e ->
      match e with
      | Parsing_error loc ->
        Format.eprintf
          "%a:@.\
           Syntax error@."
          Loc.pp_location loc
      | Lexing_error (error, loc) ->
        Format.eprintf
          "%a:@.\
           Lex error: %a@."
          Loc.pp_location loc
          Lex.pp_error error

let parse_flambda ~backend file =
    match parse_program file with
    | Ok program ->
      Format.printf "%a@.@."
        Print_fexpr.program program;
      let fl2 = Fexpr_to_flambda.conv ~backend program in
      Format.printf "flambda:@.%a@.@."
        Flambda_static.Program.print fl2;
      check_invariants fl2;
      let fl2' = Simplify.run ~backend ~round:1 fl2 in
      Format.printf "simplify:@.%a@."
        Flambda_static.Program.print fl2';
      exit 0
    | Error e ->
      match e with
      | Parsing_error loc ->
        Format.eprintf
          "%a:@.\
           Syntax error@."
          Loc.pp_location loc
      | Lexing_error (error, loc) ->
        Format.eprintf
          "%a:@.\
           Lex error: %a@."
          Loc.pp_location loc
          Lex.pp_error error

let go ~backend () =
  let file = Sys.argv.(1) in
  let ext = Filename.extension file in
  match ext with
  | ".il" -> parse_ilambda ~backend file
  | ".fl" -> parse_flambda ~backend file
  | _ ->
    Misc.fatal_errorf "Unrecognized extension %s" ext
