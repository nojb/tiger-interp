type t = {
  loc_start : Lexing.position;
  loc_end : Lexing.position
}

let dummy = {
  loc_start = Lexing.dummy_pos;
  loc_end = Lexing.dummy_pos
}

let make start end_ =
  { loc_start = start; loc_end = end_ }

open Format

let pp ppf {loc_start; loc_end} =
  fprintf ppf "@[line %d@]" loc_start.Lexing.pos_lnum

let loc (_, x) = x
