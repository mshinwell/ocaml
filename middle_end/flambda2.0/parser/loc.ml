type location = Lexing.position * Lexing.position

let none = Lexing.dummy_pos, Lexing.dummy_pos

let pp_location ppf ((lstart, lend):location) =
  Format.fprintf ppf "File: \"%s\", line %i, characters %i-%i"
    lstart.pos_fname
    lstart.pos_lnum
    (lstart.pos_cnum-lstart.pos_bol)
    (lstart.pos_cnum-lend.pos_bol)
