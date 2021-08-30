type t = {
    lstart: Lexing.position;
    lend: Lexing.position;
  }

let mk lstart lend = { lstart; lend }

let of_lexbuf lexbuf =
  mk (Lexing.lexeme_start_p lexbuf) (Lexing.lexeme_end_p lexbuf)

let line pos = pos.Lexing.pos_lnum

let column pos = pos.Lexing.pos_cnum - pos.Lexing.pos_bol

let format formatter l =
  if line l.lstart = line l.lend then
    if column l.lend = succ (column l.lstart) then
      Format.fprintf
        formatter "@[%s:%d:%d@]" l.lstart.Lexing.pos_fname
        (line l.lstart) (column l.lstart)
    else
      Format.fprintf
        formatter "@[%s:%d:%d-%d@]" l.lstart.Lexing.pos_fname
        (line l.lstart) (column l.lstart) (column l.lend)
  else
    Format.fprintf
      formatter "@[%s:%d:%d-%d:%d@]" l.lstart.Lexing.pos_fname
      (line l.lstart) (column l.lstart) (line l.lend)
      (column l.lend)

type 'a l = {
    loc: t option;
    v: 'a;
  }

let l ?loc v = { loc; v }

let format_open formatter l =
  Format.fprintf formatter "@[%a:@ " format l

let format_loc sub formatter l =
  match l.loc with
  | None -> sub formatter l.v
  | Some loc ->
     Format.fprintf formatter "%a%a@]" format_open loc sub l.v
