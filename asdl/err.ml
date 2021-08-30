type t = string Loc.l

exception E of t

let format formatter e =
  Loc.format_loc Format.pp_print_string formatter e

let syntax loc =
  raise (E (Loc.l ~loc "Syntax error"))

let unexpected_character loc c =
  let s = String.escaped (String.make 1 c) in
  raise (E (Loc.l ~loc (Format.sprintf "Unexpected character '%s'" s)))
